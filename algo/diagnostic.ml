(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

module OcamlHash = Hashtbl
open ExtLib
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

(** One un-installability reason for a package *)
type reason =
  |Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list) 
  (** Not strictly a un-installability, Dependency (a,vpkglist,pkglist) is used
      to recontruct the the dependency path from the root package to the
      offending un-installable package *)
  |Missing of (Cudf.package * Cudf_types.vpkg list) 
  (** Missing (a,vpkglist) means that the dependency
      [vpkglist] of package [a] cannot be satisfied *)
  |Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg) 
  (** Conflict (a,b,vpkg) means that the package [a] is in conflict
      with package [b] because of vpkg *)

(** The request provided to the solver *)
type request =
  |Package of Cudf.package
  (** Check the installability of one package *)
  |PackageList of Cudf.package list
  (** Check the installability of a list of packages *)

(** The result of an installability query *)
type result =
  |Success of (?all:bool -> unit -> Cudf.package list) 
  (** If successfull returns a function that will
      return the installation set for the given query. Since
      not all packages are tested for installability directly, the
      installation set might be empty. In this case, the solver can
      be called again to provide the real installation set 
      using the parameter [~all:true] *)
  |Failure of (unit -> reason list) 
  (** If unsuccessful returns a function containing the list of reason *)

type diagnosis = { result : result ; request : request }

type pp = (Cudf.package -> string * string * (string * string) list)

module ResultHash = OcamlHash.Make (
  struct
    type t = reason

    let equal v w = match (v,w) with
    |Missing (_,v1),Missing (_,v2) -> v1 = v2
    |Conflict(i1,j1,_),Conflict (i2,j2,_) -> i1 = i2 && j1 = j2
    |_ -> false

    let hash = function
      |Missing (_,vpkgs) -> OcamlHash.hash vpkgs
      |Conflict (i,j,_) -> OcamlHash.hash (i,j)
      |_ -> assert false
  end
)

type summary = {
  mutable missing : int;
  mutable conflict : int;
  mutable unique_missing : int;
  mutable unique_conflict : int;
  summary : (Cudf.package list ref) ResultHash.t 
}

let default_result n = {
  missing = 0;
  conflict = 0;
  unique_missing = 0;
  unique_conflict = 0;
  summary = ResultHash.create n;
}

(** given a list of dependencies, return a list of list containg all
 *  paths in the dependency tree starting from [root] *)
let build_paths deps root =
  let bind m f = List.flatten (List.map f m) in
  let rec aux acc deps root =
    match List.partition (fun (i,_,_) -> CudfAdd.equal i root) deps with
    |([],_) when (List.length acc) = 1 -> [] 
    |(rootlist,_) ->
        bind rootlist (function
          |(i,v,[]) -> [List.rev acc]
          |(i,v,l) -> bind l (fun r -> aux ((i,v)::acc) deps r)
        )
  in
  aux [] deps root
;;

let pp_package ?(source=false) pp fmt pkg =
  let (p,v,fields) = pp pkg in
  Format.fprintf fmt "package: %s@," p;
  Format.fprintf fmt "version: %s" v;
  List.iter (function
    |(("source"|"sourcenumber"),_) -> ()
    |(k,v) -> Format.fprintf fmt "@,%s: %s" k v
  ) fields;
  if source then begin 
    try
      let source = List.assoc "source" fields in
      let sourceversion = 
        try "(= "^(List.assoc "sourcenumber" fields)^")" 
        with Not_found -> ""
      in
      Format.fprintf fmt "@,source: %s %s" source sourceversion
    with Not_found -> ()
  end
;;

let pp_vpkglist pp fmt = 
  (* from libcudf ... again *)
  let pp_list fmt ~pp_item ~sep l =
    let rec aux fmt = function
      | [] -> assert false
      | [last] -> (* last item, no trailing sep *)
          Format.fprintf fmt "@,%a" pp_item last
      | vpkg :: tl -> (* at least one package in tl *)
          Format.fprintf fmt "@,%a%s" pp_item vpkg sep ;
          aux fmt tl
    in
    match l with
    | [] -> ()
    | [sole] -> pp_item fmt sole
    | _ -> Format.fprintf fmt "@[<h>%a@]" aux l
  in
  let string_of_relop = function
      `Eq -> "="
    | `Neq -> "!="
    | `Geq -> ">="
    | `Gt -> ">"
    | `Leq -> "<="
    | `Lt -> "<"
  in
  let pp_item fmt = function
    |(p,None) -> 
        let (p,_,_) = pp {Cudf.default_package with Cudf.package = p} in
        Format.fprintf fmt "%s" p
    |(p,Some(c,v)) ->
        debug "pp_vpkglist %s %s %i" p (string_of_relop c) v;
        let (p,v,_) = pp {Cudf.default_package with Cudf.package = p ; version = v} in
        Format.fprintf fmt "%s (%s %s)" p (string_of_relop c) v
  in
  pp_list fmt ~pp_item ~sep:" | "

let pp_dependency pp ?(label="depends") fmt (i,vpkgs) =
  Format.fprintf fmt "%a" (pp_package pp) i;
  if vpkgs <> [] then
    Format.fprintf fmt "@,%s: %a" label (pp_vpkglist pp) vpkgs;
;;

let rec pp_list pp fmt = function
  |[h] -> Format.fprintf fmt "@[<v 1>-@,%a@]" pp h
  |h::t ->
      (Format.fprintf fmt "@[<v 1>-@,%a@]@," pp h ;
      pp_list pp fmt t)
  |[] -> ()
;;

let create_pathlist root deps =
  let dl = List.map (function Dependency x -> x |_ -> assert false) deps in
  build_paths (List.unique dl) root
;;

let pp_dependencies pp fmt pathlist =
  let rec aux fmt = function
    |[path] -> Format.fprintf fmt "@[<v 1>-@,@[<v 1>depchain:@,%a@]@]" (pp_list (pp_dependency pp)) path
    |path::pathlist ->
        (Format.fprintf fmt "@[<v 1>-@,@[<v 1>depchain:@,%a@]@]@," (pp_list (pp_dependency pp)) path;
        aux fmt pathlist)
    |[] -> ()
  in
  aux fmt pathlist
;;

(* IF HASOCAMLGRAPH THEN *)
(** Build a SyntacticDependencyGraph from the solver output. *)
let build_explanation_graph root l =
  let open Defaultgraphs.SyntacticDependencyGraph in
  let module UG = Graph.Imperative.Graph.Concrete(G.V) in
  let add_node p =
    G.V.create (PkgV.Pkg {
      value = p;
      root = (CudfAdd.equal p root)
        }
      )
  in
  let gr = G.create () in
  let ugr = UG.create () in
  let c = ref 0 in
  let conflicts = ref [] in
  let missing = ref [] in
  (* remove duplicate dependencies/reasons. XXX with long
     list of packages Hashtbl.hash could give wrong results *)
  let dup_reasons_table = Hashtbl.create 10 in
  let dup_or_table = Hashtbl.create 10 in
  List.iter (function
    |e when Hashtbl.mem dup_reasons_table e -> ()
    |e -> begin
      Hashtbl.add dup_reasons_table e ();
      match e with
      |Dependency(pkg,vpkgs,[p]) ->
          let vpid = add_node pkg in
          let vp = add_node p in
          add_edge gr vpid (PkgE.DirDepends vpkgs) vp
      |Dependency(pkg,vpkgs,l) ->
          let vpid = add_node pkg in
          let vor =
            try Hashtbl.find dup_or_table (pkg.Cudf.package,vpkgs)
            with Not_found -> begin
              let vor = G.V.create (PkgV.Or !c) in incr c;
              Hashtbl.add dup_or_table (pkg.Cudf.package,vpkgs) vor;
              vor
            end
          in
          add_edge gr vpid (PkgE.OrDepends vpkgs) vor;
          List.iter (fun p ->
            let vp = add_node p in
            add_edge gr vor (PkgE.OrDepends vpkgs) vp
          ) l;
          let s =
            List.fold_left (fun acc p ->
              CudfAdd.StringSet.add p.Cudf.package acc
            ) CudfAdd.StringSet.empty l
          in
          let missingvpkgs =
            List.fold_left (fun acc (n,c) ->
              if not(CudfAdd.StringSet.mem n s) then ((n,c)::acc) else acc
            ) [] vpkgs
          in
          (* we add this node if a package depends disjuctively on one
             or more packages that exists in the repository, but are not
             installable, and one that do not exists in the repository. For
             the latter we add a missing node to the graph. *)
          if List.length missingvpkgs > 0 then begin
            let vp = G.V.create (PkgV.Missing missingvpkgs) in incr c;
            missing := (vp,missingvpkgs) :: !missing;
            add_edge gr vor (PkgE.MissingDepends missingvpkgs) vp
          end
      |Missing(pkg,vpkgs) ->
          let vpid = add_node pkg in
          let vp = G.V.create (PkgV.Missing vpkgs) in
          missing := (vp,vpkgs) :: !missing;
          add_edge gr vpid (PkgE.MissingDepends vpkgs) vp
      |Conflict(pkg_i,pkg_j,vpkg) ->
          let vpid = add_node pkg_i in
          let vp = add_node pkg_j in
          UG.add_edge ugr vpid vp;
          incr c;
          conflicts := (vpid,vp,vpkg) :: !conflicts;
          add_edge gr vpid PkgE.Conflict vp
      end
  ) l;
  ignore(all_paths gr (add_node root));
  (gr,!conflicts,!missing)
;;

(** condense nodes in the graph that have the same package name
    but different versions using the following transformation rules:
    - Or -> (a, v 1,2) -> Node 

let condense gr =
  let module Bfs = Traverse.Bfs(G) in
  let visit = function 
    G.V.label v
    |PkgV.Missing vpkgs ->

  in
  Bfs.iter visit g;

*)

(* END *)

let print_error pp root fmt l =
  let (deps,res) = List.partition (function Dependency _ -> true |_ -> false) l in
  let pp_reason fmt = function
    |Conflict (i,j,vpkg) ->
        Format.fprintf fmt "@[<v 1>conflict:@,";
        Format.fprintf fmt "@[<v 1>pkg1:@,%a@," (pp_package ~source:true pp) i;
        Format.fprintf fmt "unsat-conflict: %a@]@," (pp_vpkglist pp) [vpkg];
        Format.fprintf fmt "@[<v 1>pkg2:@,%a@]" (pp_package ~source:true pp) j;
        if deps <> [] then begin
          let pl1 = create_pathlist root (Dependency(i,[],[])::deps) in
          let pl2 = create_pathlist root (Dependency(j,[],[])::deps) in
          if pl1 <> [[]] then
            Format.fprintf fmt "@,@[<v 1>depchain1:@,%a@]" (pp_dependencies pp) pl1;
          if pl2 <> [[]] then
            Format.fprintf fmt "@,@[<v 1>depchain2:@,%a@]" (pp_dependencies pp) pl2;
          Format.fprintf fmt "@]"
        end else
          Format.fprintf fmt "@,@]"
    |Missing (i,vpkgs) ->
        Format.fprintf fmt "@[<v 1>missing:@,";
        Format.fprintf fmt "@[<v 1>pkg:@,%a@]" 
          (pp_dependency ~label:"unsat-dependency" pp) (i,vpkgs);
        let pl = create_pathlist root (Dependency(i,vpkgs,[])::deps) in
        if pl <> [[]] then begin
          Format.fprintf fmt "@,@[<v 1>depchains:@,%a@]" (pp_dependencies pp) pl;
          Format.fprintf fmt "@]"
        end else
          Format.fprintf fmt "@,@]"
    (* only two failures reasons. Dependency describe the 
     * dependency chain to a failure witness *)
    |_ -> assert false 
  in
  pp_list pp_reason fmt res
;;

(* XXX unplug your imperative brain and rewrite this as a tail recoursive
 * function ! *)
let minimize roots l =
  let module H = Hashtbl in
  let h = H.create (List.length l) in
  List.iter (fun p -> H.add h p.Cudf.package p) l;
  let acc = H.create 1023 in
  let rec visit pkg =
    if not (H.mem acc pkg) then begin
      H.add acc pkg ();
      List.iter (fun vpkgformula ->
        List.iter (fun (name,constr) ->
          try
            let p = H.find h name in
            if Cudf.version_matches p.Cudf.version constr then
              visit p
          with Not_found -> ()
        ) vpkgformula
      ) pkg.Cudf.depends
    end
  in
  begin match roots with 
  |Package r -> visit r
  |PackageList rl -> List.iter visit l end;
  H.fold (fun k _ l -> k::l) acc []
;;

let get_installationset ?(minimal=false) = function
  |{result = Success f ; request = req} -> 
     let s = f ~all:true () in
     if minimal then minimize req s else s
  |{result = Failure _ } -> raise Not_found
;;

let is_solution = function
  |{result = Success _ } -> true
  |{result = Failure _ } -> false
;;

(** [default_pp] default package printer. If the version of the package is
  * a negative number, the version version if printed as "nan" *)
let default_pp pkg =
  let v = 
    if pkg.Cudf.version > 0 then 
      CudfAdd.string_of_version pkg
    else "nan"
  in
  (pkg.Cudf.package,v,[])
;;

let print_error_human ?(prefix="") pp root fmt l =
  let (deps,res) = List.partition (function Dependency _ -> true |_ -> false) l in
  let pp_package pkg =
    let (p,v,fields) = pp pkg in
    Format.sprintf "(%s %s)" p v
  in
  let pp_dependencies fmt pathlist =
    List.iter (fun path ->
      List.iter (fun (i,vpkgs) ->
        if i.Cudf.package <> "dose-dummy-request" then
          Format.fprintf fmt "%s%s@." prefix (pp_package i)
      ) path
    ) pathlist
  in
  let pp_reason fmt = function
    |Conflict (i,j,vpkg) -> begin
      Format.printf "%sThere is a conflict " prefix;
      Format.printf "between package %s and package %s@." (pp_package i) (pp_package j);
      if deps <> [] then begin
          let pl1 = create_pathlist root (Dependency(i,[],[])::deps) in
          let pl2 = create_pathlist root (Dependency(j,[],[])::deps) in
          if pl1 <> [[]] then pp_dependencies fmt pl1;
          if pl2 <> [[]] then pp_dependencies fmt pl2;
        end
    end
    |Missing (i,vpkgs) -> begin
      Format.printf "%sThe dependency %a of package %s cannot be satisfied@." prefix (pp_vpkglist pp) vpkgs (pp_package i);
      let pl = create_pathlist root (Dependency(i,vpkgs,[])::deps) in
      if pl <> [[]] then pp_dependencies fmt pl
    end
    |_ -> assert false 
  in
  if root.Cudf.package = "dose-dummy-request" then
    Format.printf "%sThe request cannot be satisfied@," prefix
  else
    Format.printf "%sThe request for package %s cannot be satisfied@," prefix (pp_package root);
  List.iter (pp_reason fmt) res
;;

let fprintf_human ?(pp=default_pp) ?(prefix="") fmt = function
  |{result = Failure f ; request = Package r } -> 
         print_error_human ~prefix pp r fmt (f ());
  |{result = Failure f ; request = PackageList rl } -> 
      let n = List.length rl in
      List.iteri (fun i r ->
        print_error_human ~prefix pp r fmt (f ());
        if i <> (n-1) then Format.fprintf fmt "@."
      ) rl;
  |_ -> ()
;;

let fprintf ?(pp=default_pp) ?(failure=false) ?(success=false) ?(explain=false) ?(minimal=false) fmt d = 
  match d with
  |{result = Success f; request = req } when success ->
       Format.fprintf fmt "@[<v 1>-@,";
       begin match req with
       |Package r -> 
           Format.fprintf fmt "@[<v>%a@]@," (pp_package ~source:true pp) r
       |PackageList rl -> 
           Format.fprintf fmt "coinst: %s@," 
           (String.concat " , " (List.map CudfAdd.string_of_package rl));
       end;
       Format.fprintf fmt "status: ok@,";
       if explain then begin
         let is = get_installationset ~minimal d in
         if is <> [] then begin
           Format.fprintf fmt "@[<v 1>installationset:@," ;
           Format.fprintf fmt "@[<v>%a@]" (pp_list (pp_package pp)) is;
           Format.fprintf fmt "@]"
         end
       end;
       Format.fprintf fmt "@]@,"
  |{result = Failure f; request = Package r } when failure -> 
       Format.fprintf fmt "@[<v 1>-@,";
       Format.fprintf fmt "@[<v>%a@]@," (pp_package ~source:true pp) r;
       Format.fprintf fmt "status: broken@,";
       if explain then begin
         Format.fprintf fmt "@[<v 1>reasons:@,";
         Format.fprintf fmt "@[<v>%a@]" (print_error pp r) (f ());
         Format.fprintf fmt "@]"
       end;
       Format.fprintf fmt "@]@,"
  |{result = Failure f; request = PackageList rl } when failure -> 
       Format.fprintf fmt "@[<v 1>-@,";
       Format.fprintf fmt "coinst: %s@," (String.concat " , " (List.map CudfAdd.string_of_package rl));
       Format.fprintf fmt "status: broken@,";
       Format.fprintf fmt "@]@,";
       if explain then begin
         Format.fprintf fmt "@[<v 1>reasons:@,";
         List.iter (fun r -> 
           Format.fprintf fmt "@[<v>%a@]@," (print_error pp r) (f ());
         ) rl;
        Format.fprintf fmt "@]@,"
       end;
  |_ -> ()
;;

let printf_dot ?(pp=default_pp) ?(failure=false) ?(success=false) ?(explain=false) ?(minimal=false) fmt d = 
  match d with
  |{result = Success f; request = req } when success ->
       Format.fprintf fmt "@[<v 1>-@,";
       begin match req with
       |Package r -> 
           Format.fprintf fmt "@[<v>%a@]@," (pp_package ~source:true pp) r
       |PackageList rl -> 
           Format.fprintf fmt "coinst: %s@," 
           (String.concat " , " (List.map CudfAdd.string_of_package rl));
       end;
       Format.fprintf fmt "status: ok@,";
       if explain then begin
         let is = get_installationset ~minimal d in
         if is <> [] then begin
           Format.fprintf fmt "@[<v 1>installationset:@," ;
           Format.fprintf fmt "@[<v>%a@]" (pp_list (pp_package pp)) is;
           Format.fprintf fmt "@]"
         end
       end;
       Format.fprintf fmt "@]@,"
  |{result = Failure f; request = Package r } when failure -> 
      let fmt =
        let n = (r.Cudf.package)^(string_of_int (r.Cudf.version)) in
        let oc = open_out (CudfAdd.decode ("/tmp/"^n^".dot")) in
        Format.formatter_of_out_channel oc
      in
      let (gr,_,_) = build_explanation_graph r (f ()) in
      Defaultgraphs.SyntacticDependencyGraph.DotPrinter.print fmt gr

  |{result = Failure f; request = PackageList rl } when failure -> 
       Format.fprintf fmt "@[<v 1>-@,";
       Format.fprintf fmt "coinst: %s@," (String.concat " , " (List.map CudfAdd.string_of_package rl));
       Format.fprintf fmt "status: broken@,";
       Format.fprintf fmt "@]@,";
       if explain then begin
         Format.fprintf fmt "@[<v 1>reasons:@,";
         List.iter (fun r -> 
           Format.fprintf fmt "@[<v>%a@]@," (print_error pp r) (f ());
         ) rl;
        Format.fprintf fmt "@]@,"
       end;
  |_ -> ()
;;

let printf ?(pp=default_pp) ?(failure=false) ?(success=false) ?(explain=false) d =
  fprintf ~pp ~failure ~success ~explain Format.std_formatter d

let collect results d = 
  let add h k v =
    try let l = ResultHash.find h k in l := v :: !l
    with Not_found -> ResultHash.add h k (ref [v])
  in
  match d with 
  |{result = Failure (f) ; request = Package r } -> 
      List.iter (fun reason ->
        match reason with
        |Conflict (i,j,_) ->
            add results.summary reason r;
            results.conflict <- results.conflict + 1
        |Missing (i,vpkgs) ->
            add results.summary reason r;
            results.missing <- results.missing + 1
        |_ -> ()
      ) (f ())
  |_  -> ()
;;

let pp_summary_row explain pp fmt = function
  |(Conflict (i,j,_),pl) ->
      Format.fprintf fmt "@[<v 1>conflict:@,";
      Format.fprintf fmt "@[<v 1>pkg1:@,%a@]@," (pp_package pp) i;
      Format.fprintf fmt "@[<v 1>pkg2:@,%a@]@," (pp_package pp) j;
      Format.fprintf fmt "@[<v 1>broken-by: %d@]@," (List.length pl);
      if explain then begin
        Format.fprintf fmt "@[<v 1>packages:@," ;
        pp_list (pp_package ~source:true pp) fmt pl;
        Format.fprintf fmt "@]"
      end;
      Format.fprintf fmt "@]"
  |(Missing (i,vpkgs) ,pl) -> 
      Format.fprintf fmt "@[<v 1>missing:@,";
      Format.fprintf fmt "@[<v 1>unsat-dependency: %a@]@," (pp_vpkglist pp) vpkgs;
      Format.fprintf fmt "@[<v 1>broken-by: %d@]@," (List.length pl);
      if explain then begin
        Format.fprintf fmt "@[<v 1>packages:@," ;
        pp_list (pp_package ~source:true pp) fmt pl;
        Format.fprintf fmt "@]"
      end;
      Format.fprintf fmt "@]"
  |_ -> ()
;;

let pp_summary ?(pp=default_pp) ?(explain=false) () fmt result = 
  let l =
    ResultHash.fold (fun k v acc -> 
      let l1 = Util.list_unique !v in
      begin match k with
        |Conflict(_,_,_) -> result.unique_conflict <- result.unique_conflict + 1;
        |Missing(_,_) -> result.unique_missing <- result.unique_missing +1;
        |_ -> ()
      end;
      if List.length l1 > 1 then (k,l1)::acc else acc 
    ) result.summary [] 
  in
  let l = List.sort ~cmp:(fun (_,l1) (_,l2) -> (List.length l2) - (List.length l1)) l in

  Format.fprintf fmt "@[";
  Format.fprintf fmt "missing-packages: %d@." result.missing;
  Format.fprintf fmt "conflict-packages: %d@." result.conflict;
  Format.fprintf fmt "unique-missing-packages: %d@." result.unique_missing;
  Format.fprintf fmt "unique-conflict-packages: %d@." result.unique_conflict;
  Format.fprintf fmt "@]";

  Format.fprintf fmt "@[<v 1>summary:@," ;
  pp_list (pp_summary_row explain pp) fmt l;
  Format.fprintf fmt "@]"
;;

