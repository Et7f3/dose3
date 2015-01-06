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

(** The aggregated result from the solver *)
type diagnosis = { result : result; request : request; }

module ResultHash : Hashtbl.S with type key = reason

type summary = {
  mutable missing : int;
  mutable conflict : int;
  mutable unique_missing : int;
  mutable unique_conflict : int;
  summary : Cudf.package list ref ResultHash.t;
}
val default_result : int -> summary

(** [collect summary result]. Callback function to collect result 
    information in the [summary] data structure. Can be used to build
    a custom callback function for [Depsolver.listcheck] or [Depsolver.univcheck] *)
val collect : summary -> diagnosis -> unit

(* Function signature for cudf package printer. The output represents
   a triple (name, version, (field name, value) list *)
type pp = (Cudf.package -> string * string * (string * string) list)

(** [default_pp] default package printer. Extracts string values from a 
    cudf package : Name, Version, Fields. Where Fields is a list of 
    field name , value pairs . If the version of the package is
    a negative number, the version version if printed as "nan". *)
val default_pp : pp

(** Default package pretty printer. *)
val pp_package : ?source:bool -> pp -> Format.formatter -> Cudf.package -> unit

(** Cudf Vpkglist printer. *)
val pp_vpkglist : pp -> Format.formatter -> Cudf_types.vpkglist -> unit

val pp_dependency :
  pp ->
  ?label:string ->
  Format.formatter ->
  Cudf.package * Cudf_types.vpkglist -> unit

(** Print the list of dependencies of a package . The label specifies the
    type of dependency ("depends" by default) *)
val pp_dependencies : pp ->
  Format.formatter -> (Cudf.package * Cudf_types.vpkglist) list list -> unit

val pp_list :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

val print_error : pp ->
  Cudf.package -> Format.formatter -> reason list -> unit

(** If the installablity query is successfull, [get_installationset] return 
    the associated installation set . If minimal is true (false by default),
    the installation set is restricted to the dependency cone of the packages
    specified in the installablity query. @Raise [Not_found] if the result is
    a failure. *)
val get_installationset : ?minimal:bool -> diagnosis -> Cudf.package list

(** True is the result of an installablity query is successfull. False otherwise *)
val is_solution : diagnosis -> bool

val pp_summary :
  ?pp:(Cudf.package -> Cudf_types.pkgname * string * (string * string) list) ->
  ?explain:bool -> unit -> Format.formatter -> summary -> unit

val print_error_human :
  ?prefix:string -> pp ->
  Cudf.package -> Format.formatter -> reason list -> unit

val fprintf_human :
  ?pp:pp ->
  ?prefix:string -> Format.formatter -> diagnosis -> unit

val fprintf :
  ?pp:pp ->
  ?failure:bool ->
  ?success:bool ->
  ?explain:bool -> ?minimal:bool -> Format.formatter -> diagnosis -> unit

val printf :
  ?pp:pp ->
  ?failure:bool -> ?success:bool -> ?explain:bool -> diagnosis -> unit

val printf_dot :
  ?pp:pp ->
  ?failure:bool ->
  ?success:bool ->
  ?explain:bool -> ?minimal:bool -> Format.formatter -> diagnosis -> unit




