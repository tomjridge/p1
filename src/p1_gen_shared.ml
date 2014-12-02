(* code common to p1_gen_* *)

open P1_lib
open P1_extra_combinators
open P1_cl
open P1_parse_grammar

let parse_file = 
  (((until_a "<<g<<") **> (a "<<g<<") **> (until_a ">>g>>") **> (a ">>g>>") **> until_EOF)
   >> (fun (pre,(_,(mid,(_,post)))) -> (content pre,content mid,content post)))

let ws = (parse_RE "[ \n]*")

(* throw away the header *)
let parse_embedded_grammar : (string,(nonterm * symbol list * string) list) ty_parser = 
  ws **> parse_GRAMMAR_WITH_ACTIONS **> ws >> (fun (_,((h,g),_)) -> assert(h=""); g)


(* so we need to map a nt E to a string parse_E *)
let nonterm_to_parse_fun_name = fun nt -> "parse_"^nt

(* assume terms are like "1" or ?xyz? for now *)
let term_to_ocaml = (fun x0 -> 
    let l0 = String.length x0 in
    match l0 <= 1 with
    | true -> (failwith "term_to_ocaml: t0x")
    | false -> (
        match x0.[0],x0.[l0-1] with
        | '"','"' -> ("(a "^x0^")")
        | '?','?' -> ("("^(String.sub x0 1 (l0-2))^")")
        | _ -> (failwith "term_to_ocaml: fjc")))        

let rhs_symbol_to_ocaml sym = (
   match sym with 
  | `TM x -> (term_to_ocaml x)
  | `NT x -> (nonterm_to_parse_fun_name x))
