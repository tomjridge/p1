(** Very basic command line parsing *)

open P1_lib
open P1_extra_combinators

let get_args parse_CL argv = (
  let argv = List.tl (Array.to_list argv) in
(*  let _ = print_endline ("Command line: "^(String.concat " " argv)) in *)
  let rs = (parse_CL (toinput (mk_ss (String.concat "\x00" argv)))) in
  match rs with
    | [] -> failwith "get_args: failed to parse command line (0 parses)"
    | [(r,_)] -> r
    | _ -> failwith "get_args: failed to parse command line (>=2 parses)")

(* parsers for command line parsing *)

(* we use "\x00" as an arg separator - assumes this char does not appear on the cl *)
let parse_FLAG = ((a "-") **> (parse_RE "[^\x00]*")) >> (fun (_,s) -> "-"^(content s))

(*
let _ = "ab\x00c" |> mk_ss |> toinput |> parse_RE "[^\x00]*" 
*)

(* first char should not be a - *)
let parse_ARG = ( 
  let parse_not_minus = parse_RE "[^-]" in
  parse_not_minus **> (parse_RE "[^\x00]*") >> (fun (s1,s2) -> ((content s1)^(content s2))))

let parse_FLARGS = (
  let sep = a "\x00" in
  (parse_FLAG >> fun f -> (f,[]))
  ||| ((parse_FLAG **> sep **> (listof parse_ARG sep)) >> (fun (f,(_,xs)) -> (f,xs))))

let get_args parse_CL argv = (
  let argv = List.tl (Array.to_list argv) in
(*  let _ = print_endline ("Command line: "^(String.concat " " argv)) in *)
  let rs = (parse_CL (toinput (mk_ss (String.concat "\x00" argv)))) in
  match rs with
    | [] -> failwith "get_args: failed to parse command line (0 parses)"
    | [(r,_)] -> r
    | _ -> failwith "get_args: failed to parse command line (>=2 parses)")
