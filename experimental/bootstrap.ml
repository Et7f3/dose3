(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  ADD authors here                                     *)
(*                                                                        *)
(*  Contributor(s):  ADD minor contributors here                          *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common
open Debian
open Algo

let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt
let debug fmt = Util.make_debug __FILE__ fmt
let fatal fmt = Util.make_fatal __FILE__ fmt

module Boilerplate = BoilerplateNoRpm
module Src = Sources
module Pkg = Packages

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"Detect circular build dependencies"
  include Boilerplate.MakeOptions(struct let options = options end)

  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()

  let buildarch = StdOpt.str_option ()
  let targetarch = StdOpt.str_option ()
  let dump = StdOpt.str_option ()

  open OptParser
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;

  add options ~long_name:"builarch" ~help:"Build Architecture" buildarch;
  add options ~long_name:"targetarch" ~help:"Target Architecture" targetarch;
  add options ~long_name:"dump" ~help:"dump the cudf file" dump;
end

let filter_map_results tables universe is =
  try 
    List.filter_map (fun pkg ->
      try
        let sn = Cudf.lookup_package_property pkg "source" in
        let sv = Cudf.lookup_package_property pkg "sourceversion" in
        Some((sn,sv),pkg)
      with Not_found -> None
    ) is
  with Not_found -> assert false
;;

let get_source_pkg to_cudf universe l = 
  let aux = function
    |(p,None) ->
        Cudf.lookup_packages universe (CudfAdd.encode ("src"^Src.sep^p))
    |(p,Some(c,v)) ->
        let filter = Some(c,to_cudf (CudfAdd.encode ("src"^Src.sep^p),v)) in
        Cudf.lookup_packages ~filter universe (CudfAdd.encode ("src"^Src.sep^p))
  in
  List.flatten (List.map aux l)
;;

let main () =

  let posargs = OptParse.OptParser.parse_argv Options.options in
  
  (* enable info / warning / debug information *)
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  
  (* enable a selection of progress bars *)
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;

  (* enable a selection of timers *)
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];

  if not(OptParse.Opt.is_set Options.buildarch) then
    fatal "--builarch must be specified";

  if not(OptParse.Opt.is_set Options.targetarch) then begin
    info "--targetarch must be specified assume same of buildarch";
    OptParse.Opt.set Options.targetarch (OptParse.Opt.get Options.buildarch);
  end;

  let binlist, srclist, checklist =
    match posargs with
    |[] | [_] -> fatal
      "You must provide a list of Debian Packages files and \
       a Debian Sources file"
    |l ->
        begin match List.rev l with
        |r::h::t ->
          let l = Src.input_raw [h] in
          let archs = ["linux-any";OptParse.Opt.get Options.buildarch] in
          let srcl = Src.sources2packages archs l in
          let pkgl = Pkg.input_raw t in
          let request = Boilerplate.parse_vpkg r in
          (pkgl,srcl,request)
        |_ -> failwith "Impossible"
        end
  in

  let tables = Debcudf.init_tables (srclist @ binlist) in
  let sl = List.map (fun pkg -> Debcudf.tocudf tables pkg) srclist in
  let pkglist = 
    List.fold_left (fun acc pkg -> 
      (Debcudf.tocudf tables pkg)::acc
    ) sl binlist 
  in

  let to_cudf = Debcudf.get_cudf_version tables in
  let from_cudf = Debcudf.get_real_version tables in

  let universe = Cudf.load_universe pkglist in

  let universe_size = Cudf.universe_size universe in
  info "Total packages (source + binaries) %d" universe_size;

  if OptParse.Opt.is_set Options.dump then begin
    let oc = open_out (OptParse.Opt.get Options.dump) in
    info "Dumping Cudf file";
    Cudf_printer.pp_universe oc universe
  end;

  let failure = OptParse.Opt.get Options.failures in
  let success = OptParse.Opt.get Options.successes in
  let explain = OptParse.Opt.get Options.explain in
  let fmt = Format.std_formatter in

  if OptParse.Opt.is_set Options.buildarch then
    Format.fprintf fmt "buildarch: %s@." (OptParse.Opt.get Options.buildarch);
  if OptParse.Opt.is_set Options.targetarch then
    Format.fprintf fmt "targetarch: %s@." (OptParse.Opt.get Options.targetarch);

  let pp pkg =
    let p = pkg.Cudf.package in
    let v = from_cudf (CudfAdd.decode pkg.Cudf.package,pkg.Cudf.version) in
    let l =
      List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["architecture"]
    in (p,v,l)
  in

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";

  let queue = ref checklist in
  let visited = Hashtbl.create 1023 in
  let broken = ref 0 in
  let callback d = match d with
    |{Diagnostic.result = Diagnostic.Success (f); 
                request = Diagnostic.Package r } ->
        info "Buildcheking %s" (CudfAdd.string_of_package r);
        let is = filter_map_results tables universe (f ~all:true ()) in
        List.iter (fun ((sn,sv),why) ->
          if CudfAdd.equal r why then () else
          if not(Hashtbl.mem visited (sn,sv)) then begin
            info "Scheduling (src:%s, %s) (because of %s)" 
            sn sv (CudfAdd.string_of_package why) ;
            Hashtbl.add visited (sn,sv) [(r,why)];
            queue := (sn,Some(`Eq,sv))::!queue
          end else
            let l = Hashtbl.find visited (sn,sv) in
            Hashtbl.replace visited (sn,sv) ((r,why)::l)
        ) is;
        Diagnostic.fprintf ~pp ~failure ~success ~explain fmt d
    |d -> Diagnostic.fprintf ~pp ~failure ~success ~explain fmt d
  in

  while (List.length !queue > 0) do
    let l = get_source_pkg to_cudf universe !queue in 
    let _ = queue := [] in
    let i = Depsolver.listcheck ~callback universe l in 
    broken := i + !broken;
  done;

  let module G = Defaultgraphs.PackageGraph.G in
  let module D = Defaultgraphs.PackageGraph.D in
  let module C = Graph.Components.Make(G) in
  let g = G.create () in
  Hashtbl.iter (fun (sn,sv) l -> 
    let src = List.hd (get_source_pkg to_cudf universe [(sn,Some(`Eq,sv))]) in
    G.add_vertex g src;
    List.iter (fun (root,why) ->
      G.add_edge g src root (* label why *)
    ) l
  ) visited; 
  let oc = open_out "tt.dot" in
  D.output_graph oc g;
  (*
  Array.iter (fun l ->
    List.iter (fun pkg ->
      Printf.printf "%s\n" (CudfAdd.string_of_package pkg)
    ) l;
    print_newline ();
  )  (C.scc_array g);
  *)

  if failure || success then Format.fprintf fmt "@]@.";

  let nb = universe_size in
  let nf = List.length sl in
  Format.fprintf fmt "background-packages: %d@." nb;
  Format.fprintf fmt "foreground-packages: %d@." (if nf = 0 then nb else nf);
  Format.fprintf fmt "broken-packages: %d@." !broken;
;;

main ();;
