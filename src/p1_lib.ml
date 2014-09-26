type 'a substring = 'a P1_core.substring

type nonterm = string

type 'a ty_input = 'a P1_core.ty_input

let toinput = P1_core.toinput
let substring_of_input = P1_core.substring_of_input

type ('a,'b) ty_parser = ('a,'b) P1_core.ty_parser

let ( >> ) = P1_core.( >> )
let ( ||| ) = P1_core.( ||| )
let ( **> ) = P1_core.( **> )

let check_and_upd_lctxt = P1_core.check_and_upd_lctxt

let a = P1_parsers.a
let until_a = P1_parsers.until_a


let read_file_as_string = P1_util.read_file_as_string
let write_string_to_file = P1_util.write_string_to_file

