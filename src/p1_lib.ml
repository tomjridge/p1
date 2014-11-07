type 'a substring = 'a P1_core.substring
let mk_ss = P1_core.mk_ss

let content = P1_core.content

let ss_len = P1_core.len

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



let a = P1_parsers.a
let until_a = P1_parsers.until_a
let parse_RE = P1_parsers.parse_RE
let parse_not_RE = P1_parsers.parse_not_RE
let parse_EOF = P1_parsers.parse_EOF
let parse_eps  = P1_parsers.parse_eps

let read_file_as_string = P1_util.read_file_as_string
let write_string_to_file = P1_util.write_string_to_file

