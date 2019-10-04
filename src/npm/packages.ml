(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

open ExtLib
open Dose_common
module Pcre = Re_pcre

include Util.Logging (struct
  let label = "dose_npm.packages"
end)

let lexbuf_wrapper type_parser v =
  try Format822.lexbuf_wrapper type_parser Npm_lexer.token v
  with Format822.ParseError _ -> (
    match v with
    | (_, (_, package)) ->
        let splited = Pcre.split ~rex:(Pcre.regexp " : ") package in
        let base64 =
          Bytes.to_string (Base64.str_encode (List.nth splited 1))
        in
        [[(("", None), Some ("=", base64))]] )

let parse_dependlist v = lexbuf_wrapper Npm_parser.dependlist_top v

let parse_depends v = lexbuf_wrapper Npm_parser.depends_top v

let parse_depend v = lexbuf_wrapper Npm_parser.depend_top v

type request = {
  install : Dose_pef.Packages_types.vpkg list;
  remove : Dose_pef.Packages_types.vpkg list;
  upgrade : Dose_pef.Packages_types.vpkg list;
  preferences : string;
}

let default_request =
  {install = []; remove = []; upgrade = []; preferences = ""}

let parse_req =
  Dose_pef.Packages.lexbuf_wrapper Dose_pef.Packages_parser.vpkglist_top

let parse_request_stanza par =
  try
    {
      install = Dose_pef.Packages.parse_s ~default:[] parse_req "install" par;
      remove = Dose_pef.Packages.parse_s ~default:[] parse_req "remove" par;
      upgrade = Dose_pef.Packages.parse_s ~default:[] parse_req "upgrade" par;
      preferences =
        Dose_pef.Packages.parse_s
          ~default:""
          Dose_pef.Packages.parse_string
          "preferences"
          par;
    }
  with Format822.ParseError (cl, f, err) ->
    let c = "Parser Error in Preamble" in
    raise (Format822.ParseError (c :: cl, f, err))

class package ?(name = ("package", None)) ?(version = ("version", None))
  ?(depends = ("depends", None)) ?(conflicts = ("conflicts", None))
  ?(provides = ("provides", None)) ?(extras = ([], None)) par =
  object
    inherit
      Dose_pef.Packages.package
        ~name ~version ~depends ~conflicts ~provides ~extras par

    method! pp oc =
      Dose_pef.Printer.pp_string_wl oc name ;
      Dose_pef.Printer.pp_string_wl oc version ;
      Dose_pef.Printer.pp_vpkgformula_wl oc depends ;
      Dose_pef.Printer.pp_vpkglist_wl oc conflicts ;
      Printf.fprintf oc "\n"
  end

let parse_package_stanza ?(extras = []) par =
  try
    let pkg =
      let depends =
        let f = Dose_pef.Packages.parse_s ~default:[] parse_depends in
        ("depends", Some (f "depends" par))
      in
      (* let extras = List.map (fun (f,v) -> (f,(Format822.dummy_loc,v))) extras in *)
      new package ~depends ~extras:(extras, None) par
    in
    Some pkg
  with
  | Dose_pef.Packages.IgnorePackage s ->
      let n =
        Dose_pef.Packages.parse_s
          ~default:"?"
          Dose_pef.Packages.parse_name
          "package"
          par
      in
      let v =
        Dose_pef.Packages.parse_s
          ~default:"?"
          Dose_pef.Packages.parse_version
          "version"
          par
      in
      warning "Ignoring Package (%s,%s) : %s" n v s ;
      None
  | Format822.ParseError (cl, f, err) ->
      let n =
        Dose_pef.Packages.parse_s
          ~default:"?"
          Dose_pef.Packages.parse_name
          "package"
          par
      in
      let v =
        Dose_pef.Packages.parse_s
          ~default:"?"
          Dose_pef.Packages.parse_version
          "version"
          par
      in
      let c = Printf.sprintf "Parser Error in Package (%s,%s)" n v in
      raise (Format822.ParseError (c :: cl, f, err))

let rec packages_parser ?(request = false) (req, acc) p =
  match
    Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf
  with
  | None ->
      (req, acc) (* end of file *)
  | Some stanza when request = true ->
      let req = parse_request_stanza stanza in
      packages_parser (req, acc) p
  | Some stanza -> (
    match parse_package_stanza stanza with
    | None ->
        packages_parser (req, acc) p
    | Some st ->
        packages_parser (req, st :: acc) p )

let input_raw_in ic =
  Format822.parse_from_ch
    (packages_parser ~request:true (default_request, []))
    ic

let input_raw file =
  try
    let ch =
      match file with
      | "-" ->
          IO.input_channel stdin
      | _ ->
          Input.open_file file
    in
    let l = input_raw_in ch in
    let _ = Input.close_ch ch in
    l
  with
  | Input.File_empty ->
      (default_request, [])
  | Format822.ParseError (cl, field, errmsg) ->
      fatal
        "Filename %s\n %s\n %s : %s"
        file
        (String.concat "\n " cl)
        field
        errmsg
