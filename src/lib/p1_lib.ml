

type 'a substring = 'a P1_core.Substring.t

type nonterm = string

type 'a input = 'a P1_core.input_t

type ('a,'b) result = ('a,'b) P1_core.result

type ('a,'b) parser_t = ('a,'b) P1_core.parser_t

let ( >> ) = P1_core.( >> )
let ( ||| ) = P1_core.( ||| )
let ( **> ) = P1_core.( **> )

let check = P1_core.Context.check


let run_parser_string = P1_core.run_parser_string

module Basic_parsers = struct
  type p = (string, string) P1_core.parser_t
  (* FIXME include P1_extra.Basic_parsers *)
end

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

let memo = P1_core.memo

type hashkey = P1_core.hashkey
let hashkey_of_input = P1_core.hashkey_of_input
