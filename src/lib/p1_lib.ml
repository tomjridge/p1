

type 'a substring = 'a P1_core.substring
let mk_ss = P1_core.mk_ss

let content = P1_core.content

let ss_len = P1_core.len

let ss_concat = P1_core.ss_concat

type nonterm = string

type 'a ty_input = 'a P1_core.ty_input

let toinput = P1_core.toinput
let substring_of_input = P1_core.substring_of_input

type ('a,'b) ty_parser = ('a,'b) P1_core.ty_parser

let ( >> ) = P1_core.( >> )
let ( ||| ) = P1_core.( ||| )
let ( **> ) = P1_core.( **> )

let check_and_upd_lctxt = P1_core.check_and_upd_lctxt


type hashkey = P1_core.hashkey
let hashkey_of_input = P1_core.hashkey_of_input
let memo = P1_core.memo

let run_parser_string = P1_core.run_parser_string



let a = P1_extra.a
let until_a = P1_extra.until_a
let parse_RE = P1_extra.parse_RE
let parse_regexp = P1_extra.parse_regexp
let parse_not_RE = P1_extra.parse_not_RE
let parse_not_regexp = P1_extra.parse_not_regexp
let parse_EOF = P1_extra.parse_EOF
let until_EOF = P1_extra.until_EOF
let parse_eps  = P1_extra.parse_eps


(* some simple code to read and write a file, using minimal technology *)

(* copied from ocaml/utils/misc.ml *)

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

let read_file_as_string :string -> string option = (fun fn -> 
  try
    let ic = open_in fn in
    let s = string_of_file ic in
    let _ = close_in ic in
    Some s
  with _ -> None)

let write_string_to_file : string -> string -> bool = (fun s fn ->
  try
    let oc = open_out fn in
    let _ = output_string oc s in
    let _ = close_out oc in
    true
  with _ -> false)
