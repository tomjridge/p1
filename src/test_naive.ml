(*
#require "str";;
#require "astring";;
#mod_use "p0.ml";;
#mod_use "p0_template.ml";;
*)

(* test p0 ---------------------------------------- *)

open P0
open P

let p0 = (a "tom") **> (until_eof)

let r0 = parse_string p0 "tom ridge xxx"


let p1 = (
    a "theory " **>
    until_a_then_a "imports" **>
    until_a_then_a "begin" **>
    until_a_then_a "end" **>
    until_eof)

let r1 = parse_string p1 {|theory X imports Y Z begin ... end|}

let p1 = (
    a "theory " **>
    until_a_then_a "imports" **>
    until_a_then_a "begin" **>
    until_a_then_a "end" **>
    until_eof)

let r1 = parse_string p1 {|theory X imports Y Z begin ... end|}

let rec list_to_parser ps = (
  match ps with
    [] -> failwith "list_to_parser"
  | [p] -> (p >> (fun x -> [x]))
  | p::ps -> (p **> (list_to_parser ps)) >> (fun (x,y) -> x::y))

let p2 = (
    [a "theory" >> (fun _ -> `Theory_kw);
     until_a_then_a "imports" >> (fun x -> `Theory x);
     until_a_then_a "begin" >> (fun x -> `Imports x);
     until_a_then_a "end" >> (fun x -> `Body x);
     until_eof >> (fun _ -> `End)] |>
    list_to_parser)

let r2 = parse_string p2 {|theory X imports Y Z begin ... end|}


(* test templates ---------------------------------------- *)

open P0_template

let def = 
  (T.of_strings ["definition"; "::"; "where"; {|"|}; {|"|} ])
  >> (fun m -> `Def(find "definition" m, find "::" m, find {|"|} m))

let defs = longest (sep_by def (until_a "definition"))

let thy = 
  let defs' = 
    (until_a "definition" **> defs **> until_eof) >> 
    (fun (x,(y,z)) -> y)
  in
  T.of_strings ["theory"; "imports"; "begin"; "end" ] 
  >> (fun m -> `Theory(
      find "theory" m, 
      find "imports" m, 
      find "begin" m |> parse_results defs'
    ))

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

let Some s = read_file_as_string "/tmp/l/git/github/isa_btree/src/Delete_tree_stack.thy"

let [`Theory x1] = s|>parse_results thy
