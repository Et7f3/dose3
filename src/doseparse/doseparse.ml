(** {2 Command Line Helpers} *)
module StdDebian = struct
#1 "_build/doseparse/stdDebian.ml"
# 1 "_build/doseparse/stdDebian.ml"
(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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

let vpkg_option ?default ?(metavar = " <vpkg>") () =
  let parse_vpkg s =
    let _loc = Format822.dummy_loc in
    Pef.Packages.parse_vpkg ("cmdline <vpkg>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkg (fun _ s -> Printf.sprintf "invalid vpackage '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (<c> v)" *)
let vpkglist_option ?default ?(metavar = " <vpkglst>") () =
  let parse_vpkglist s =
    let _loc = Format822.dummy_loc in
    Pef.Packages.parse_vpkglist ("cmdline <vpkglst>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "invalid vpackage list '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (= v)" *)
let pkglist_option ?default ?(metavar = " <pkglst>") () =
  let parse_vpkglist s =
    let _loc = Format822.dummy_loc in
    List.map (function
      |((n,a),Some("=",v)) -> (n,a,v)
      |((n,a),None) ->
          raise (Format822.ParseError ([],s,"you must specify a version" ))
      |_ -> raise (Format822.ParseError ([],s,""))
    ) (Pef.Packages.parse_vpkglist ("cmdline <pkglst>",(_loc,s)))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "invalid package list '%s'" s)
;;

let pkglist tables universe vpkglist =
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  List.flatten (
    List.map (fun ((n,a),c) ->
      let (name,filter) = Pef.Pefcudf.pefvpkg to_cudf ((n,a),c) in
      Cudf.lookup_packages ~filter universe name
    ) vpkglist
  )
;;
end
module StdDebug = struct
#1 "_build/doseparse/stdDebug.ml"
# 1 "_build/doseparse/stdDebug.ml"
(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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

# 19
let label =  
# 19
               "_build/doseparse/stdDebug.ml"   
# 19
                     ;;
include Util.Logging(struct let label = label end) ;;

let enable_debug = function
  |0 -> () (* only warning messages : default *)
  |1 -> Util.Info.all_enabled ()
  |2 -> 
      begin
        Util.Info.all_enabled ();
        Util.Notice.all_enabled ()
      end
  |_ ->
      begin
        Util.Info.all_enabled () ;
        Util.Notice.all_enabled ();
        Util.Debug.all_enabled ()
      end
;;

let all_quiet t =
  if t then begin
    Util.Info.all_disabled ();
    Util.Notice.all_disabled ();
    Util.Warning.all_disabled ();
    Util.Debug.all_disabled ();
    List.iter Util.Progress.disable (Util.Progress.available ())
  end
;;

let enable_bars verbose l =
  if verbose then List.iter Util.Progress.enable l

let enable_timers verbose l = 
  at_exit (Util.Timer.dump Format.err_formatter);
  if verbose then List.iter Util.Timer.enable l
;;

end
module StdLoaders = struct
#1 "_build/doseparse/stdLoaders.ml"
# 1 "_build/doseparse/stdLoaders.ml"
(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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

# 19
let label =  
# 19
               "_build/doseparse/stdLoaders.ml"   
# 19
                     ;;
include Util.Logging(struct let label = label end) ;;

let load_list_timer = Util.Timer.create "Load" ;;
let deb_load_list_timer = Util.Timer.create "Load.Debian" ;;
let deb_load_source_timer = Util.Timer.create "Load.DebianSource" ;;

(* a list of the raw package types for all input except Cudf itself *)
(* currently, only Deb and DebSrc are used *)
type rawpackage =
  |Deb of Debian.Packages.package
  |DebSrc of Debian.Sources.source
  |Pef of Pef.Packages.package
  |Opam of Opam.Packages.package
  |Npm of Npm.Packages.package
  |Edsp of Debian.Packages.package
  |Csw of Csw.Packages.package


# 41
(** read a debian Packages file - compressed or not *)
let read_deb ?filter ?(extras=[]) fname =
  Debian.Packages.input_raw ?filter ~extras [fname]

(* fll = file list list
 * dll = deb packages list list 
 * cll = cudf package list list
 *)
let deb_load_list options ?(status=[]) ?(raw=false) dll =
  Util.Timer.start deb_load_list_timer;
  let noindep = options.Debian.Debcudf.drop_bd_indep in
  let profiles = options.Debian.Debcudf.profiles in
  let pkgll = List.map (List.map (function
      | Deb p -> p
      | DebSrc p ->
          if Option.is_none options.Debian.Debcudf.native then
            fatal "--deb-native-arch was not specified while treating Debian Sources File";
        let buildarch = Option.get options.Debian.Debcudf.native in
        let hostarch = Option.get options.Debian.Debcudf.host in
        Debian.Sources.src2pkg ~noindep ~profiles buildarch hostarch p
      | _ -> fatal "cannot handle input"
    )) dll 
  in
  let pkgl = List.flatten pkgll in
  let pkgl = if status = [] then pkgl else Debian.Packages.merge status pkgl in
  let tables = Debian.Debcudf.init_tables pkgl in
  let from_cudf (p,i) = Debian.Debcudf.get_real_version tables (p,i) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let cll = 
    List.map (fun l ->
      List.map (Debian.Debcudf.tocudf tables ~options) (Debian.Packages.merge status l)
    ) pkgll
  in
  (* if requested, connect all cudf packages representing binary packages to
   * the cudf packages representing the source package they each build from,
   * respectively *)
  let cll = if options.Debian.Debcudf.builds_from then begin
      let univ =
        Cudf.load_universe (CudfAdd.Cudf_set.elements (
            List.fold_right (
              List.fold_right CudfAdd.Cudf_set.add)
              cll CudfAdd.Cudf_set.empty))
      in
      List.map2 (List.map2 (fun cudfpkg debpkg ->
          match debpkg with
          | Deb _ ->
            let srcpkg = try Debian.Sources.get_src_package univ cudfpkg
              with Debian.Sources.NotfoundSrc ->
                failwith (Printf.sprintf "cannot find source for binary package %s"
                            (CudfAdd.string_of_package cudfpkg))
            in
            (* connect to source package as "builds-from" *)
            let srcdep = (srcpkg.Cudf.package,Some(`Eq,srcpkg.Cudf.version)) in
            { cudfpkg with Cudf.depends = [srcdep] :: cudfpkg.Cudf.depends }
          | DebSrc _ -> cudfpkg
          | _ -> failwith "impossible"
        )) cll dll
    end else cll
  in
  let preamble = Debian.Debcudf.preamble in
  let request = Cudf.default_request in
  let rawll = if raw && status = [] then Some dll else None in
  let l = (preamble,cll,request,from_cudf,to_cudf,rawll) in
  Util.Timer.stop deb_load_list_timer l
      
let npm_load_list file =
  let (request,pkglist) = Npm.Packages.input_raw file in
  let tables = Pef.Pefcudf.init_tables Versioning.SemverNode.compare pkglist in
  let from_cudf (p,i) = (p,None,Pef.Pefcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p, Pef.Pefcudf.get_cudf_version tables (p,v)) in
  let cl = List.map (Pef.Pefcudf.tocudf tables) pkglist in
  let preamble = Npm.Npmcudf.preamble in
  let request = Cudf.default_request in
  (*
  let request = Npm.Npmcudf.requesttocudf tables (Cudf.load_universe cl) request in
  *)
  (preamble,[cl;[]],request,from_cudf,to_cudf,None)

let opam_load_list ?options file =
  let (request,pkglist) = Opam.Packages.input_raw file in
  let tables = Pef.Pefcudf.init_tables Versioning.Debian.compare pkglist in
  let from_cudf (p,i) = (p,None,Pef.Pefcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p, Pef.Pefcudf.get_cudf_version tables (p,v)) in
  let options =
    match options with
    |None -> {
      Opam.Opamcudf.default_options with
      Opam.Opamcudf.switch = request.Opam.Packages.switch;
      switches = request.Opam.Packages.switches;
      profiles = request.Opam.Packages.profiles }
    |Some opt -> opt
  in
  let cl = List.flatten (List.map (Opam.Opamcudf.tocudf ~options tables) pkglist) in
  let preamble = Opam.Opamcudf.preamble in
  let request = Opam.Opamcudf.requesttocudf tables (Cudf.load_universe cl) request in
  (preamble,[cl;[]],request,from_cudf,to_cudf,None)

let pef_load_list ?compare options dll =
  let compare = match compare with Some c -> c |None -> Versioning.Debian.compare in
  let extras = [("maintainer",("maintainer",`String None))] in
  let pkglist = List.flatten dll in
  let tables = Pef.Pefcudf.init_tables compare pkglist in
  let from_cudf (p,i) = (p,None,Pef.Pefcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p, Pef.Pefcudf.get_cudf_version tables (p,v)) in
  let cll =
    List.map (fun l ->
      List.map (Pef.Pefcudf.tocudf ~extras tables) l
    ) dll
  in
  let preamble = Pef.Pefcudf.preamble in
  let request = Cudf.default_request in
  (preamble,cll,request,from_cudf,to_cudf,None)

let csw_load_list dll =
  let pkglist = List.flatten dll in
  let tables = Csw.Cswcudf.init_tables pkglist in
  let from_cudf (p,i) = (p,None,Csw.Cswcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p, Csw.Cswcudf.get_cudf_version tables (p,v)) in
  let cll = 
    List.map (fun l ->
      List.map (Csw.Cswcudf.tocudf tables) l
    ) dll
  in
  let preamble = Csw.Cswcudf.preamble in
  let request = Cudf.default_request in
  (preamble,cll,request,from_cudf,to_cudf,None)
 
let edsp_load_list options file =
  let (request,pkglist) = Debian.Edsp.input_raw file in
  let (native_arch,foreign_archs) =
    StdUtils.get_architectures
      request.Debian.Edsp.architecture
      request.Debian.Edsp.architectures
      options.Debian.Debcudf.native
      (match options.Debian.Debcudf.foreign with [] -> None | l -> Some l)
  in
  let options = { 
    options with 
    Debian.Debcudf.native = native_arch;
    Debian.Debcudf.foreign = foreign_archs
  } in
  let tables = Debian.Debcudf.init_tables pkglist in
  let preamble =
    let l = List.map snd Debian.Edsp.extras_tocudf in
    Common.CudfAdd.add_properties Debian.Debcudf.preamble l
  in  
  let univ = Hashtbl.create (2*(List.length pkglist)-1) in
  let cudfpkglist =
    List.filter_map (fun pkg ->
      let p = Debian.Edsp.tocudf tables ~options pkg in
      if not(Hashtbl.mem univ (p.Cudf.package,p.Cudf.version)) then begin
        Hashtbl.add univ (p.Cudf.package,p.Cudf.version) pkg;
        Some p
      end else begin
        warning "Duplicated package (same version, name and architecture) : (%s,%s,%s)"
          pkg#name pkg#version pkg#architecture;
        None
      end
    ) pkglist
  in
  let request = Debian.Edsp.requesttocudf tables (Cudf.load_universe cudfpkglist) request in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let from_cudf (p,i) = Debian.Debcudf.get_real_version tables (p,i) in
  (preamble,[cudfpkglist;[]],request,from_cudf,to_cudf,None)

let edsp_load_universe options file =
  let (pr,l,r,f,t,w) = edsp_load_list options file in
  (pr,Cudf.load_universe (List.hd l), r, f, t, w)

(** transform a list of debian control stanza into a cudf universe *)
let deb_load_universe options ?(raw=false) l =
  let (pr,cll,r,f,t,w) = deb_load_list options ~raw l in
  (pr,Cudf.load_universe (List.flatten cll), r, f, t, w)

(** transform a list of rpm control stanza into a cudf packages list *)
let rpm_load_list dll =
  
# 231
  failwith "librpm not available. re-configure with --with-rpm"

# 234
(** transform a list of rpm control stanza into a cudf universe *)
let rpm_load_universe l =
  let (pr,cll,r,f,t,w) = rpm_load_list [l] in
  (pr,Cudf.load_universe (List.flatten cll), r, f, t, w)

(** parse a cudf file and return a triple (preamble,package list,request
    option). If the package is not valid returns an empty list of packages *)
let parse_cudf doc =
  try
    let p = Cudf_parser.from_IO_in_channel (Input.open_file doc) in
    Cudf_parser.parse p
  with
  |Input.File_empty -> None, [], None
  |Cudf_parser.Parse_error (msg, loc) ->
    fatal "Error while parsing CUDF from %s (%s): %s" doc (Format822.string_of_loc loc) msg ;
  |Cudf.Constraint_violation _ as exn ->
    fatal "Error while loading CUDF from %s: %s" doc (Printexc.to_string exn)

(** parse a cudf file and return a triple (preamble,universe,request option).
    If the package is not valid return an empty list of packages *)
let load_cudf doc = 
  let ch = Input.open_file doc in
  let l = 
    try
      let p = Cudf_parser.from_IO_in_channel ch in
      Cudf_parser.load p
    with
    |Input.File_empty -> None, Cudf.load_universe [], None
    |Cudf_parser.Parse_error (msg, loc) ->
      fatal "Error while parsing CUDF from %s (%s): %s" doc (Format822.string_of_loc loc) msg ;
    |Cudf.Constraint_violation _ as exn -> begin
      fatal "Error while loading CUDF file %s:\n%s" doc (Printexc.to_string exn)
    end 
  in
  Input.close_ch ch;
  l
;;

let cudf_load_list file =
  let preamble, pkglist ,request =
    match parse_cudf file with
    |None, pkglist, None -> Cudf.default_preamble, pkglist, Cudf.default_request
    |None , pkglist, Some req -> Cudf.default_preamble, pkglist, req
    |Some p , pkglist, None -> p, pkglist, Cudf.default_request
    |Some p , pkglist, Some req -> p, pkglist, req
  in
  let from_cudf (p,i) = (p,None,string_of_int i) in
  let to_cudf (p,v) = (p,int_of_string v) in
  (preamble,[pkglist;[]],request,from_cudf,to_cudf,None)

let cudf_load_universe file =
  let (pr,l,r,f,t,w) = cudf_load_list file in
  (pr,Cudf.load_universe (List.hd l), r, f, t, w)

let unpack_l expected l = List.fold_left (fun acc (t,(_,_,_,_,f),_) ->
    if t = expected then f::acc
    else fatal "cannot handle input %s" (Url.scheme_to_string t)
  ) [] l

let unpack expected = function
  | (t,(_,_,_,_,f),_) when t = expected -> f
  | _ -> "cannot handle input"

let deb_parse_input options ?(status=[]) ?(raw=false) urilist =
  let archs = 
    if not(Option.is_none options.Debian.Debcudf.native) then
      (Option.get options.Debian.Debcudf.native) :: options.Debian.Debcudf.foreign 
    else []
  in
  let dll = 
    List.map (fun l ->
      List.fold_left (fun acc (t,(_,_,_,_,f),_) ->
          match t with
          | `Deb -> List.fold_left (fun acc p -> (Deb p)::acc) acc (Debian.Packages.input_raw ~archs [f])
          | `DebSrc -> List.fold_left (fun acc p -> (DebSrc p)::acc) acc (Debian.Sources.input_raw ~archs [f])
          | _ -> fatal "cannot handle input"
          ) [] l
    ) urilist
  in
  deb_load_list options ~status ~raw dll

let pef_parse_input ?compare options urilist =
  let extras = [("maintainer",None)] in
  let dll = 
    List.map (fun l ->
        let filelist = unpack_l `Pef l in
        Pef.Packages.input_raw ~extras filelist
    ) urilist
  in
  pef_load_list ?compare options dll

let npm_parse_input ?options urilist =
  match urilist with
  |[[p]] when (unpack `Npm p) = "-" -> fatal "no stdin for npm yet"
  |[[p]] -> npm_load_list (unpack `Npm p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more than one npm request file specified on the command line";
    let p = List.hd (List.flatten l) in 
    npm_load_list (unpack `Npm p)
;;

let opam_parse_input ?options urilist =
  match urilist with
  |[[p]] when (unpack `Opam p) = "-" -> fatal "no stdin for opam yet"
  |[[p]] -> opam_load_list ?options (unpack `Opam p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more than one opam request file specified on the command line";
    let p = List.hd (List.flatten l) in 
    opam_load_list ?options (unpack `Opam p)
;;

let csw_parse_input urilist =
  let dll = 
    List.map (fun l ->
        let filelist = unpack_l `Csw l in
        Csw.Packages.input_raw filelist
    ) urilist
  in
  csw_load_list dll

let cudf_parse_input urilist =
  match urilist with
  |[[p]] when (unpack `Cudf p) = "-" -> fatal "no stdin for cudf yet"
  |[[p]] -> cudf_load_list (unpack `Cudf p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more than one cudf specified on the command line";
    let p = List.hd (List.flatten l) in 
    cudf_load_list (unpack `Cudf p)
;;

let edsp_parse_input options urilist =
  match urilist with
  |[[p]] when (unpack `Edsp p) = "-" -> fatal "no stdin for edsp yet"
  |[[p]] -> edsp_load_list options (unpack `Edsp p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more than one cudf specified on the command line";
    let p = List.hd (List.flatten l) in 
    edsp_load_list options (unpack `Edsp p)
;;

(* Check that all uris are of type that is an instance of scheme *)
(* If yes return that instance of scheme, and the list of paths  *)
(* in uris.                                                      *)
(** parse a list of uris of the same type and return a cudf packages list *)
let parse_input ?(options=None) ?(raw=false) ?compare urilist =
  let filelist = List.map (List.map Input.parse_uri) urilist in
  match Input.guess_format urilist, options with
  |`Cudf, None -> cudf_parse_input filelist

  |`Deb, None
  |`DebSrc, None -> deb_parse_input Debian.Debcudf.default_options ~raw filelist
  |`Pef, None -> pef_parse_input ?compare Debian.Debcudf.default_options filelist

  |`Deb, Some (StdOptions.Deb opt)
  |`DebSrc, Some (StdOptions.Deb opt) -> deb_parse_input opt ~raw filelist
  
(*  |`Edsp, Some (StdOptions.Edsp opt) -> edsp_parse_input opt filelist *)
  |`Edsp, _ -> edsp_parse_input Debian.Debcudf.default_options filelist
  |`Opam, _ -> opam_parse_input filelist
  |`Npm, _ -> npm_parse_input filelist
(* |`Opam, Some (StdOptions.Opam options) -> opam_parse_input ~options filelist *)

  |`Pef, Some (StdOptions.Pef opt) -> pef_parse_input ?compare opt filelist

  |`Csw, None -> csw_parse_input filelist

  |`Hdlist, None -> 
    
# 414
    fatal "hdlist Not supported. re-configure with --with-rpm"

  
# 417
  |`Synthesis, None -> 
    
# 427
    fatal "synthesis input format not supported. re-configure with --with-rpm"
    
# 429
    |s,_ -> fatal "%s Not supported" (Url.scheme_to_string s)
;;

let supported_formats () =
  let standard = ["cudf://";"deb://";"deb://-";"eclipse://";"pef://"] in
  let rpm = 
     
# 438
     []
   
# 440
   in
   standard@rpm
;;

(** return a list of Debian packages from a debian source file *)
let deb_load_source ?filter ?(dropalternatives=false) ?(profiles=[]) ?(noindep=false) buildarch hostarch sourcefile =
  Util.Timer.start deb_load_source_timer;
  let l = Debian.Sources.input_raw ?filter ~archs:[hostarch] [sourcefile] in
  let r = Debian.Sources.sources2packages ~dropalternatives ~noindep ~profiles buildarch hostarch l in
  Util.Timer.stop deb_load_source_timer r
;;

(** parse and merge a list of files into a cudf package list *)
let load_list ?(options=None) ?(raw=false) ?compare urilist =
  info "Parsing and normalizing..." ;
  Util.Timer.start load_list_timer;
  let u = parse_input ~options ~raw ?compare urilist in
  Util.Timer.stop load_list_timer u
;;

(** parse and merge a list of files into a cudf universe *)
let load_universe ?(options=None) ?(raw=false) ?compare uris =
  info "Parsing and normalizing..." ;
  Util.Timer.start load_list_timer;
  let (pr,cll,r,f,t,w) = parse_input ~options ~raw ?compare [uris] in
  let u = (pr,Cudf.load_universe (List.flatten cll), r, f, t, w) in
  Util.Timer.stop load_list_timer u
;;

end
module StdOptions = struct
#1 "_build/doseparse/stdOptions.ml"
# 1 "_build/doseparse/stdOptions.ml"
(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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

# 19
let label =  
# 19
               "_build/doseparse/stdOptions.ml"   
# 19
                     ;;
include Util.Logging(struct let label = label end) ;;

module type Ot = sig
  val options :
    ?usage:string ->
    ?status:int ->
    ?version:string ->
    ?suppress_usage:bool ->
    ?suppress_help:bool ->
    ?prog:string ->
    ?formatter:OptParse.Formatter.t -> unit -> OptParse.OptParser.t
end

(* *************************************** *)

let vpkg_option ?default ?(metavar = " <vpkg>") () =
  let parse_vpkg s = 
    let _loc = Format822.dummy_loc in
    Pef.Packages.parse_vpkg ("cmdline <vpkg>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkg (fun _ s -> Printf.sprintf "Invalid vpackage '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (<c> v)" *)
let vpkglist_option ?default ?(metavar = " <vpkglst>") () =
  let parse_vpkglist s = 
    let _loc = Format822.dummy_loc in
    Pef.Packages.parse_vpkglist ("cmdline <vpkglst>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "Invalid vpackage list '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (= v)" *)
let pkglist_option ?default ?(metavar = " <pkglst>") () =
  let parse_vpkglist s = 
    let _loc = Format822.dummy_loc in
    List.map (function
      |((n,a),Some("=",v)) -> (n,a,v)
      |((n,a),None) ->
          raise (Format822.ParseError ([],s,"you must specify a version" ))
      |_ -> raise (Format822.ParseError ([],s,""))
    ) (Pef.Packages.parse_vpkglist ("cmdline <pkglst>",(_loc,s)))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "Invalid package list '%s'" s)
;;

(* this is a ,-separated list of optimization criteria *)
let criteria_option ?default ?(metavar = " <criteria>") () =
  let parse_criteria s =
    let _loc = Format822.dummy_loc in
    Criteria.parse_criteria ("cmdline <criteria>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_criteria (fun _ s -> Printf.sprintf "Invalid criteria list '%s'" s)
;;

(* *************************************** *)

let incr_str_list ?(default=Some []) ?(metavar = " <str>") =
  let acc = ref [] in 
  let coerce s = acc := s :: !acc ; !acc in
  fun () ->
  OptParse.Opt.value_option metavar default coerce 
  (fun _ s -> Printf.sprintf "Invalid String '%s'" s)
;;

(* this is a ,-separated list of strings *)
let str_list_option ?default ?(metavar = " <strlst>") =
  let sep = "," in
  let coerce s = ExtString.String.nsplit s sep in
  fun () ->
    OptParse.Opt.value_option metavar default coerce
    (fun _ s -> Printf.sprintf "Invalid String '%s'" s)
;;

(* *************************************** *)

module MakeOptions(O : Ot) = struct
  open OptParse ;;

  let verbose = StdOpt.incr_option ()
  let quiet = StdOpt.store_true ()
  let progress = StdOpt.store_true ()
  let timers = StdOpt.store_true ()
  let options = O.options ~status:64 ~version:VersionInfo.version () ;;

  open OptParser ;;
  add options ~short_name:'v' ~long_name:"verbose" ~help:"print additional information" verbose;
  add options ~long_name:"progress" ~help:"print progress bars" progress;
  add options ~long_name:"timers" ~help:"print timing information" timers;
  add options ~long_name:"quiet" ~help:"do no print any messages" quiet;

end

let create_group group descr options =
    if not (Option.is_none !group) then
      Option.get !group
    else
      let g = OptParse.OptParser.add_group options descr in
      let _ = group := Some g in
      g
;;

module DistcheckOptions = struct
  open OptParse ;;

  let success = StdOpt.store_true ()
  let failure = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let minimal = StdOpt.store_true ()
  let condense = StdOpt.store_true ()
  let summary = StdOpt.store_true ()

  let default_options = [
    "success";
    "failure";
    "explain";
    "explain-minimal";
    "explain-condense";
    "summary"
  ]

  let group = ref None 
  let descr = "Distcheck Options"

  let add_options ?(default=default_options) options =
    let open OptParser in 
    if List.length default > 0 then begin
      let group = create_group group descr options in
      if List.mem "explain" default then
        add options ~group ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
      if List.mem "explain-minimal" default then
        add options ~group ~short_name:'m' ~long_name:"explain-minimal" ~help:"Do not print dependency chains of results" minimal;
      if List.mem "explain-condense" default then
        add options ~group ~short_name:'c' ~long_name:"explain-condense" ~help:"Compress explanation graph" condense;
      if List.mem "failure" default then
        add options ~group ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failure;
      if List.mem "success" default then
        add options ~group ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" success;
      if List.mem "summary" default then
        add options ~group ~long_name:"summary" ~help:"Show Failures Summary" summary;
    end
  ;;

  let add_option ?short_name ?long_name ?help options =
    let open OptParser in
    let group = create_group group descr options in
    add options ~group ?short_name ?long_name ?help 
  ;;

end

module OutputOptions = struct
  open OptParse ;;

  let outfile = StdOpt.str_option ()
  let outdir = StdOpt.str_option ()
  let dot = StdOpt.store_true ()

  let default_options = [
    "outfile";
    "outdir";
    "dot"
  ]

  let group = ref None 
  let descr = "Output Options"

  let add_options ?(default=default_options) options =
    let open OptParser in 
    if List.length default > 0 then begin
      let group = create_group group descr options in
      if List.mem "outfile" default then
        add options ~group ~short_name:'o' ~long_name:"outfile"
        ~help:"Redirect the output to a file (default stdout)" outfile;
      if List.mem "outdir" default then
        add options ~group ~short_name:'d' ~long_name:"outdir" 
        ~help:"Set the output directory (default current directory)" outdir;
      if List.mem "dot" default then
        add options ~group ~long_name:"dot"
        ~help:"Save the explanation graph (one for each package) in dot format" dot;
    end
  ;;

  let add_option ?short_name ?long_name ?help options =
    let open OptParser in
    let group = create_group group descr options in
    add options ~group ?short_name ?long_name ?help 
  ;;

end

module InputOptions = struct
  open OptParse ;;

  let itypes = (List.map Url.scheme_to_string Url.supported_input_types)
  let in_option ?default ?(metavar = Printf.sprintf "<%s>" (String.concat "|" itypes)) () =
    let coerce s = if List.mem s itypes then s else raise Not_found in
    let supported = String.concat ", " itypes in
    let error _ s = Printf.sprintf "input format \"%s\" not supported. Must be one of: %s" s supported in
    Opt.value_option metavar default coerce error

  let vtypes = Versioning.Utils.supported_formats
  let comp_option ?default ?(metavar = Printf.sprintf "<%s>" (String.concat "|" vtypes)) () =
    let coerce s = if List.mem s vtypes then s else raise Not_found in
    let supported = String.concat ", " vtypes in
    let error _ s = Printf.sprintf "comparison function \"%s\" not supported. Must be one of: %s" s supported in
    Opt.value_option metavar default coerce error

  let trim = StdOpt.store_true ()
  let latest = StdOpt.store_true ()
  let checkonly = StdDebian.vpkglist_option ()
  let background = incr_str_list ()
  let foreground = incr_str_list ()
  let inputtype = in_option ()
  let compare = comp_option ()

  let default_options = [
    (* "trim"; *)
    "inputtype";
    "latest";
    "checkonly";
    "bg";
    "fg";
    "compare";
  ]

  let group = ref None
  let descr = "Input Options"

  (** give a list of positional arguments returns two list of resources,
      foreground and background. Positional arguments are assumed to be 
      foreground resources. *)
  let parse_cmdline (it,im) posargs = 
    let add_format t = List.map (fun s -> (Url.scheme_to_string t)^"://"^s) in
    let fg = OptParse.Opt.get foreground in
    let bg = OptParse.Opt.get background in
    let fg = (if List.length (posargs@fg) = 0 then ["-"] else posargs)@fg in
    if im then
      (add_format it fg, add_format it bg)
    else
      (fg,bg)
  ;;

  let add_options ?(default=default_options) options =
    let open OptParser in 
    if List.length default > 0 then begin
      let group = create_group group descr options in
      if List.mem "inputtype" default then
        add options ~group ~short_name:'t' ~help:"Set the input type format" inputtype;
      if List.mem "checkonly" default then
        add options ~group ~long_name:"checkonly" 
        ~help:"Check only these packages" checkonly;
      if List.mem "trim" default then
        add options ~group ~long_name:"trim" 
        ~help:"Consider only installable packages" trim;
      if List.mem "latest" default then
        add options ~group ~long_name:"latest" 
        ~help:"Check only the latest version of each package" latest;
      if List.mem "fg" default then
        add options ~group ~long_name:"fg"
        ~help:("Additional Packages lists that are checked and used "^
               "for resolving dependencies (can be repeated)") foreground;
      if List.mem "bg" default then
        add options ~group ~long_name:"bg"
        ~help:("Additional Packages lists that are NOT checked but used "^
               "for resolving dependencies (can be repeated)") background;
      if List.mem "compare" default then
        add options ~group ~long_name:"compare"
        ~help:"When used with a pef input type, selects a comparison function" compare;

    end
  ;;

  let add_option ?short_name ?long_name ?help options =
    let open OptParser in
    let group = create_group group descr options in
    add options ~group ?short_name ?long_name ?help 
  ;;

end

type options =
  |Deb of Debian.Debcudf.options
  |Pef of Debian.Debcudf.options
  |Opam of Opam.Opamcudf.options
  |Edsp of Debian.Debcudf.options
  |Csw
  |Rpm
  |Cudf

module DistribOptions = struct
  open OptParse ;;

  let deb_native_arch = StdOpt.str_option ()
  let deb_foreign_archs = str_list_option ()
  let deb_host_arch = StdOpt.str_option ()
  let deb_ignore_essential = StdOpt.store_true ()
  let deb_builds_from = StdOpt.store_true ()
  let deb_drop_bd_indep = StdOpt.store_true ()
  let deb_profiles = str_list_option ()

  let opam_switch = StdOpt.str_option ~default:"system" ()
  let opam_switches = str_list_option ()
  let opam_profiles = str_list_option ()

  let default_options = [
    "deb-native-arch";
    "deb-host-arch";
    "deb-foreign-archs";
    "deb-ignore-essential";
    "deb-builds-from";
    "deb-drop-b-d-indep";
    "deb-profiles";
    "opam-switch";
    "opam-switches";
    "opam-profiles"
  ]

  let set_deb_options () =
    let native = Opt.opt deb_native_arch in
    let host =
      if Opt.is_set deb_host_arch then begin
        (* if host arch is set, native arch must be set *)
        if Option.is_none native then
          fatal "you must specify at least the native architecture" ;
        Opt.opt deb_host_arch
      end
      else native
    in
    let foreign =
      (* if host arch is set, it is an implicit foreign arch *)
      if Opt.is_set deb_foreign_archs then begin
        let f = Opt.get deb_foreign_archs in
        if Opt.is_set deb_host_arch then
          (Option.get host)::f
        else
          f
      end else begin
        if Opt.is_set deb_host_arch then
          [Option.get host]
        else
          []
      end
    in
    let profiles =
      if Opt.is_set deb_profiles then
        Opt.get deb_profiles
      else
        try String.nsplit (Sys.getenv "DEB_BUILD_PROFILES") " "
        with Not_found -> []
    in
    {
      Debian.Debcudf.default_options with
      Debian.Debcudf.native = native;
      foreign = foreign;
      host = host;
      ignore_essential = Opt.get deb_ignore_essential;
      builds_from = Opt.get deb_builds_from;
      drop_bd_indep = Opt.get deb_drop_bd_indep;
      profiles = profiles;
    }
  ;;

  let set_opam_options () =
    let switch = Opt.get opam_switch in
    let switches =
      if Opt.is_set opam_switches then
        Opt.get opam_switches
      else []
    in
    let profiles =
      if Opt.is_set opam_profiles then
        Opt.get opam_profiles
      else []
    in
    { Opam.Opamcudf.default_options with
      Opam.Opamcudf.switch = switch;
      switches = switches;
      profiles = profiles;
    }
  ;;

  let set_default_options = function
    |`Deb -> Some (
      Deb { 
        Debian.Debcudf.default_options with
        Debian.Debcudf.ignore_essential = true
      })
    |`Edsp -> Some (
      Edsp { 
        Debian.Debcudf.default_options with
        Debian.Debcudf.ignore_essential = true
      })
    |`Pef -> Some (Pef Debian.Debcudf.default_options)
    |`Opam -> Some (Opam Opam.Opamcudf.default_options)
    |_ -> None

  let set_options = function
    |`Deb |`DebSrc -> Some (Deb (set_deb_options ()))
    |`Edsp -> Some (Edsp (set_deb_options ()))
    |`Pef -> Some (Pef Debian.Debcudf.default_options)
    |`Opam -> Some (Opam (set_opam_options ()))
    |_ -> None
  ;;

  let deb_group =
    let g = ref None in
    fun options ->
      match !g with
      |Some group -> group
      |None -> begin
          let group = OptParser.add_group options "Debian Specific Options" in
          g := Some group;
          group
      end

  let opam_group =
    let g = ref None in
    fun options ->
      match !g with
      |Some group -> group
      |None -> begin
          let group = OptParser.add_group options "Opam Specific Options" in
          g := Some group;
          group
      end

  let add_debian_options ?(default=default_options) options =
    let open OptParser in
    if List.length default > 0 then begin
      let group = deb_group options in
      if List.mem "deb-native-arch" default then
        add options ~group ~long_name:"deb-native-arch"
          ~help:"Native architecture" deb_native_arch;
      if List.mem "deb-host-arch" default then
        add options ~group ~long_name:"deb-host-arch" 
          ~help:"Native/cross compile host architecture, defaults to native architecture" deb_host_arch;
      if List.mem "deb-foreign-archs" default then
        add options ~group ~long_name:"deb-foreign-archs" 
          ~help:"Foreign architectures in addition to native and host architectures" deb_foreign_archs;
      if List.mem "deb-ignore-essential" default then
        add options ~group ~long_name:"deb-ignore-essential" 
          ~help:"Ignore Essential Packages" deb_ignore_essential;
      if List.mem "deb-builds-from" default then
        add options ~group ~long_name:"deb-builds-from"
          ~help:"Add builds-from relationship of binary packages on source packages as dependency" deb_builds_from;
      if List.mem "deb-drop-b-d-indep" default then
        add options ~group ~long_name:"deb-drop-b-d-indep"
          ~help:"Drop the Build-Depends-Indep field from source packages (build no Architecture:all packages)" deb_drop_bd_indep;
      if List.mem "deb-profiles" default then
        add options ~group ~long_name:"deb-profiles"
          ~help:"comma separated list of activated build profiles" deb_profiles;
    end

  let add_opam_options ?(default=default_options) options =
    let open OptParser in
    if List.length default > 0 then begin
      let group = opam_group options in
      if List.mem "opam-switch" default then
        add options ~group ~long_name:"opam-switch"
          ~help:"Active Switch" opam_switch;
      if List.mem "opam-switches" default then
        add options ~group ~long_name:"opam-switches"
          ~help:"Available Switches" opam_switches;
      if List.mem "opam-profiles" default then
        add options ~group ~long_name:"opam-profiles"
          ~help:"Build Profiles" opam_profiles;

    end

  let add_option ?group ?short_name ?long_name ?help options v =
    let open OptParser in
    match group with
    |None -> add options ?short_name ?long_name ?help v
    |Some group -> add options ~group ?short_name ?long_name ?help v
  ;;

end
end
module StdUtils = struct
#1 "_build/doseparse/stdUtils.ml"
# 1 "_build/doseparse/stdUtils.ml"
(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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

# 19
let label =  
# 19
               "_build/doseparse/stdUtils.ml"   
# 19
                     ;;
include Util.Logging(struct let label = label end) ;;

let get_architectures native_edsp foreign_edsp native_opt foreign_opt =
  let cmd = "apt-config dump" in
  let arch = ref None in
  let archs = ref [] in
  let aux () =
    let out = Std.input_list (Unix.open_process_in cmd) in
    List.iter (fun s ->
      let key, value =  ExtString.String.split s " " in
      if key = "APT::Architecture" then
        arch := Some(ExtString.String.slice ~first: 1 ~last:(-2) value)
      else if key = "APT::Architectures::" || key = "APT::Architectures" then
        let s = ExtString.String.slice ~first:1 ~last:(-2) value in
        if s <> "" then
          archs := (ExtString.String.slice ~first:1 ~last:(-2) value)::!archs
    ) out;
    debug "Automatically set native as %s and foreign archs as %s" (Option.get !arch) (String.concat "," !archs);
  in
  let (na,fa) =
    match (native_edsp,foreign_edsp),(native_opt,foreign_opt) with
    |(None,[]),(None,None)    -> aux () ; (!arch,List.filter ((<>) (Option.get !arch)) !archs) (* EDSP 0.4 + no options *)
    |(_,l),(Some a,None)      -> (Some a,l)  (* EDSP 0.5 + overrride options *)
    |(Some a,_),(None,Some l) -> (Some a,l)  (* EDSP 0.5 + overrride options *)
    |(_,_),(Some a,Some l)    -> (Some a,l)  (* EDSP 0.5 / 0.4 + overrride options *)
    |(Some a,l),(None,None)   -> (Some a,l)  (* EDSP 0.5 + no options *)
    |(None,[]),(None,_)       -> fatal "Native arch is missing while Foregin archs are specified"
    |_,_ -> fatal "Unable to compute native and foreign arch information"
  in
  begin match (na,fa) with
  |(Some a, l) when (native_edsp,foreign_edsp) = (None,[]) || (native_edsp,foreign_edsp) = (Some a, l) ->
      notice "Setting Native Architecture to %s and Foreign Architectures to %s" a (String.concat "," l)
  |(Some a, l) ->
      info "Overriding EDSP. Setting Native Architecture to %s and Foreign Architectures to %s" a (String.concat "," l)
  |_ -> fatal "Error Setting Native Architecture"
  end;
  (na,fa)
;;

let pp_versions_table fmt (from_cudf, pkglist) =
  List.iter (fun pkg ->
    let (p,v) = from_cudf (pkg.Cudf.package,pkg.Cudf.version) in
    Format.fprintf fmt "%s=%d=%s@." p pkg.Cudf.version v
  ) pkglist

(* exit code policy : 
Exit codes 0-63 indicate a normal termination of the program, codes
64-127 indicate abnormal termination of the program (such as parse
errors, I/O errors).

In case of normal program termination:
- exit code 0 indicates that all foreground packages are found
  installable;
- exit code 1 indicates that at least one foreground package is found
  uninstallable.
*)
let if_application ?(alternatives=[]) filename main =
  let normalize f = 
    let bf = Filename.basename f in
    try
      if String.ends_with bf ".p.byte" then
        String.slice ~last:(String.find bf ".p.byte") bf
      else if String.ends_with bf ".p.native" then
        String.slice ~last:(String.find bf ".p.native") bf
      else
        Filename.chop_extension bf
    with Invalid_argument _ -> bf
  in
  let names = List.map normalize (filename::alternatives) in
  let invoked_as = normalize Sys.argv.(0) in
  if List.exists ((=) invoked_as) names then 
    try (if main () = 0 then exit(0) else exit(1)) with
      |Unix.Unix_error(err, _, arg) -> begin
          Printf.eprintf "%s %s" (Unix.error_message err) arg;
          Pervasives.exit(64) end
      |exn -> begin
          Printexc.print_backtrace stderr; 
          Printf.eprintf "The applications raised this exception : ";
          Printf.eprintf "%s\n" (Printexc.to_string exn);
          Pervasives.exit(64) end
  else begin
    Printf.eprintf "you are using %s as a module and not as an executable\n" Sys.argv.(0);
    Printf.eprintf "%s can be run as an exactable if named : %s\n" Sys.argv.(0) 
    (ExtString.String.join " , " names)
  end
end
