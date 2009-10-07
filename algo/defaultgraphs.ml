(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

(** Specialized Ocamlgraph modules *)

open Graph
open Common

let print_package = CudfAdd.print_package

(** generic operation over graphs *)
module GraphOper (G : Sig.I) = struct

  (** transitive reduction.  Uses the transitive reduction algorithm from The
      Transitive Reduction of a Directed Graph, Aho, Garey and Ullman, 1972 - 
      with the proviso that we know that our graph already is a transitive 
      closure *)
  let transitive_reduction graph =
    G.iter_vertex (fun v ->
      List.iter (fun v' ->
        if v <> v' then
        List.iter (fun v'' ->
          if v' <> v'' then
            G.remove_edge graph v v''
        ) (G.succ graph v')
      ) (G.succ graph v);
    ) graph

  module O = Oper.I (G) 
  
end

(** syntactic dependency graph. Vertex are Cudf packages and
    are indexed considering only the pair name,version .
    Edges are labelled with
    - [OrDepends] : disjuctive dependency
    - [DirDepends] : direct dependecy 
    - [Conflict] : conflict
    *) 
module SyntacticDependencyGraph = struct

  module PkgV = struct
      type t = Pkg of Cudf.package | Or of (Cudf.package * int)
      let compare = Pervasives.compare 
      let hash p =
        match p with
        |Pkg p -> Hashtbl.hash (p.Cudf.package,p.Cudf.version)
        |Or (p,i) -> Hashtbl.hash (p.Cudf.package,p.Cudf.version,i)
      let equal x y = ((compare x y) = 0)
  end

  module PkgE = struct
    type t = OrDepends | DirDepends | Conflict

    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal x y = ((compare x y) = 0)
    let default = DirDepends
  end

  module G = Imperative.Digraph.ConcreteLabeled(PkgV)(PkgE) 

  let string_of_vertex vertex =
    match G.V.label vertex with
    |PkgV.Pkg p -> Printf.sprintf "Pkg %s" (print_package p)
    |PkgV.Or (p, _) -> Printf.sprintf "Or %s" (print_package p)

  let string_of_edge edge =
    let label =
      match G.E.label edge with
      |PkgE.DirDepends -> "Direct"
      |PkgE.OrDepends -> "Disjunctive"
      |PkgE.Conflict -> "Conflict"
    in
    let src = G.E.src edge in
    let dst = G.E.dst edge in
    Printf.sprintf "%s %s %s"
    (string_of_vertex src)
    label
    (string_of_vertex dst)

  module Display = struct
      include G
      let vertex_name v =
        match G.V.label v with
        |PkgV.Pkg i -> Printf.sprintf "\"%s\"" (print_package i)
        |PkgV.Or (i,c) -> Printf.sprintf "\"Or%s-%d\"" (print_package i) c

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v =
        match G.V.label v with
        |PkgV.Or _ -> [`Label "Or"]
        |_ -> []

      let edge_attributes e =
        let t =
          match G.E.label e with
          |PkgE.DirDepends -> [`Style `Dotted]
          |PkgE.OrDepends -> [`Style `Dotted]
          |PkgE.Conflict -> [`Style `Dotted; `Label "#"]
        in
        t
    end

  (** Graphviz outoput module *)
  module D = Graph.Graphviz.Dot(Display) 
  module S = Set.Make(PkgV)

  (** Build a Syntactic Dependency Graph graph from the give cudf universe *)
  let dependency_graph universe =
    let maps = CudfAdd.build_maps universe in
    let gr = G.create () in
    Cudf.iter_packages (fun pkg ->
      let vpid = G.V.create (PkgV.Pkg pkg) in
      let c = ref 0 in
      List.iter (function
        |[(pkgname,constr)] ->
            List.iter (fun p ->
              let vp = G.V.create (PkgV.Pkg p) in
              let edge = G.E.create vpid PkgE.DirDepends vp in
              G.add_edge_e gr edge
            ) (maps.CudfAdd.who_provides (pkgname,constr))
        |l ->
            match List.flatten (List.map maps.CudfAdd.who_provides l) with 
            |[] -> ()
            |[p] ->
                let vp = G.V.create (PkgV.Pkg p) in
                let edge = G.E.create vpid PkgE.DirDepends vp in
                G.add_edge_e gr edge
            |l ->
                begin
                  let vor = G.V.create (PkgV.Or (pkg,!c)) in
                  let edgeor = G.E.create vpid PkgE.OrDepends vor in
                  G.add_edge_e gr edgeor;
                  incr c;
                  List.iter (fun p ->
                    let vp = G.V.create (PkgV.Pkg p) in
                    let oredge = G.E.create vor PkgE.OrDepends vp in
                    G.add_edge_e gr oredge
                  ) l
                end
      ) pkg.Cudf.depends
      ;
      List.iter (fun p ->
        if not(CudfAdd.equal p pkg) then
          let vp = G.V.create (PkgV.Pkg p) in
          let edge = G.E.create vpid PkgE.Conflict vp in
          G.add_edge_e gr edge
      ) (maps.CudfAdd.who_conflicts pkg)
    ) universe
    ;
    gr
  ;;

end

(******************************************************)

(** Imperative bidirectional graph. This graph should be preferred when 
    using operation like nb_edges, inter_prec, iter_succ *)
module PackageGraph = struct

  module PkgV = struct
      type t = Cudf.package
      let compare = CudfAdd.compare
      let hash = CudfAdd.hash
      let equal = CudfAdd.equal
  end

  module G = Imperative.Digraph.ConcreteBidirectional(PkgV)

  module Display =
    struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (print_package v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v = []

      let edge_attributes e = []
    end
  
  module D = Graph.Graphviz.Dot(Display)
  module S = Set.Make(PkgV)
end

(******************************************************)

(** Integer matrix graph. Space efficient, but not so effient for
    general operations like nb_edges, iter_prec *)
module MatrixGraph(Pr : sig val pr : int -> string end) = struct

  module G = Imperative.Matrix.Digraph

  module Display =
    struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (Pr.pr v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v = []

      let edge_attributes e = []
    end
  
  module D = Graph.Graphviz.Dot(Display)
  module S = Set.Make(struct type t = int let compare = compare end)
end

(******************************************************)

(** Integer Imperative Bidirectional Graph *)
module IntGraph(Pr : sig val pr : int -> string end) = struct

  module PkgV = struct
      type t = int
      let compare = compare
      let hash = Hashtbl.hash
      let equal = (=)
  end

  module G = Imperative.Digraph.ConcreteBidirectional(PkgV)

  module Display =
    struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (Pr.pr v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v = []

      let edge_attributes e = []
    end

  module D = Graph.Graphviz.Dot(Display)
  module S = Set.Make(PkgV)
end

(******************************************************)

(** Imperative bidirectional graph. This graph should be preferred when 
    using operation like nb_edges, inter_prec, iter_succ *)
module StrongDepGraph = struct

  module PkgV = struct
      type t = (string * string)
      let compare = compare
      let hash = Hashtbl.hash
      let equal = (=)
  end

  module G = Imperative.Digraph.ConcreteBidirectional(PkgV)

  module Display =
    struct
      include G
      let vertex_name (n,v) = Printf.sprintf "\"(%s,%s)\"" n v

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v = []

      let edge_attributes e = []
    end
  
  module D = Graph.Graphviz.Dot(Display)

  module DIn = Dot.Parse (Builder.I(G))(
    struct
      let node (id,_) _ =
        match id with
        |Graph.Dot_ast.String s -> 
            let rex = Str.regexp "(\\([a-zA-Z0-9_-.]+\\),\\([a-zA-Z0-9.-]+\\))" in
            if Str.string_match rex s 0 then
              (Str.matched_group 1 s , Str.matched_group 2 s)
            else (s,"")
        |_ -> assert false
      let edge _ = ()
    end
  )

  module S = Set.Make(PkgV)
end
