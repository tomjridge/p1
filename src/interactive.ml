(*
#mod_use "p1_util.ml";;
#mod_use "p1_prelude.ml";;
#mod_use "p1_core.ml";;
#mod_use "p1_parsers.ml";;
#mod_use "p1_lib.ml";;
#mod_use "p1_extra_combinators.ml";;
#mod_use "p1_parse_grammar.ml";;
#mod_use "p1_cl.ml";;
#mod_use "p1_gen_shared.ml";;


#mod_use "p1_examples.ml";;
*)

#require "str";;
#require "astring";;
#mod_use "p0.ml";;


(* test p0 ---------------------------------------- *)

open P0

let p0 = (P.a "tom") **> (P.until_eof)

let r0 = parse_string p0 "tom ridge xxx"


let p1 = P.(
    a "theory " **>
    until_a_then_a "imports" **>
    until_a_then_a "begin" **>
    until_a_then_a "end" **>
    until_eof)

let r1 = parse_string p1 {|theory X imports Y Z begin ... end|}

let p1 = P.(
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

let p2 = P.(
    [a "theory" >> (fun _ -> `Theory_kw);
     until_a_then_a "imports" >> (fun x -> `Theory x);
     until_a_then_a "begin" >> (fun x -> `Imports x);
     until_a_then_a "end" >> (fun x -> `Body x);
     until_eof >> (fun _ -> `End)] |>
    list_to_parser)

let r2 = parse_string p2 {|theory X imports Y Z begin ... end|}
