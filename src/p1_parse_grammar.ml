(* Parse a grammar file *)

(* FIXME code generation doesn't work correctly if we have a nt with no rules *)

open P1_lib
open P1_extra_combinators

type nonterm = string
type symbol = [ `NT of string | `TM of string ]

type grammar = (nonterm * symbol list) list

type grammar_with_actions = 
  string (* header *)
  * (nonterm * symbol list * string) list (* rule with action *)

let syms_of_grammar : grammar -> symbol list = fun g0 ->
  let syms_of_rule = (fun (nt,syms) -> (`NT nt)::syms) in
  g0 |> List.map syms_of_rule |> List.concat

let is_NT x = match x with | `NT x -> true | _ -> false

let dest_NT x = match x with | `NT x -> x | _ -> failwith "dest_NT"

let unique xs =
  let f1 acc x = if List.mem x acc then acc else x::acc in
  List.fold_left f1 [] xs

let nts_of_grammar g = g |> syms_of_grammar |> List.filter is_NT |> List.map dest_NT |> unique

let tm_of_lit quote lit = `TM(quote^lit^quote)

let parse_comm = fun i -> ((a "(*") **> until_a "*)" **> (a "*)")) i

let parse_ws = parse_RE "[ \n]*" (* do we want more than one whitespace? *)

let noteps : ('a,'b) ty_parser -> ('a,'b) ty_parser = (fun p i0 ->
    i0 |> p |> (List.filter (fun (x,ss) -> ss_len ss <> (i0 |> substring_of_input |> ss_len))))

let eps = parse_eps

(*
let _ = "a b c" |> mk_ss |> toinput |> ((listof (parse_RE "[abc]") (a " ")) >> List.map content)
*)

(* FIXME only one comment in ws? *)
let parse_wscomm =
  ((parse_ws >> (fun _ -> ""))
   ||| ((parse_ws **> parse_comm **> parse_ws) >> (fun _ -> "")))

(* allow _ in NT *)
let parse_AZS = parse_RE "[A-Z_]+"

let parse_azAZs = parse_RE "[A-Za-z_]+"

let dest_NT x = (match x with
    | `NT x -> x | _ -> (failwith "dest_NT"))

let parse_notdquote = until_a "\""
let parse_notsquote = until_a "'"

let rec parse_GRAMMAR = fun i ->
  ((parse_RULES **> parse_wscomm **> parse_EOF) >> (fun (rs,(_,_)) -> rs)) i

and parse_RULES = fun i -> (
  ((listof parse_RULE parse_wscomm) >> (fun xs -> List.concat xs))
  (*||| ((parse_epsws **> (listof parse_RULE parse_wscomm)) >> (fun (_,xs) -> List.concat xs)) *)
    i)

and parse_RULE = fun i ->
  ((parse_SYM **> parse_wscomm **> (a "->") **> parse_wscomm **> parse_SYMSLIST)
    >> (fun (nt,(_,(_,(_,symss)))) -> (List.map (fun syms -> (dest_NT nt,syms)) symss))) i

and parse_SYMSLIST = fun i ->
  (listof parse_SYMS (parse_wscomm **> (a "|") **> parse_wscomm)) i

(* N.B. we do not allow empty lists here *)
and parse_SYMS = fun i ->
  (noteps (listof parse_SYM parse_wscomm))  i

and parse_SYM = fun i ->
  ((((a "\"") **> parse_notdquote **> (a "\"")) >> (fun (_,(s,_)) -> tm_of_lit "\"" (content s)))
  ||| (((a "'") **> parse_notsquote **> (a "'")) >> (fun (_,(s,_)) -> tm_of_lit "'" (content s)))
  ||| (parse_AZS >> (fun s -> `NT (content s)))
  ||| (((a "?") **> parse_azAZs **> (a "?")) >> (fun (_,(s,_)) -> `TM("?" ^ (content s) ^ "?"))))
    i

let (_:(string,grammar) ty_parser) = parse_GRAMMAR

(*
let _ = "E" |> mk_ss |> toinput |> parse_SYM
let _ = "E E E" |> mk_ss |> toinput |> parse_SYMS
let _ = "E -> E E E | E E" |> mk_ss |> toinput |> parse_RULE
let _ = "E -> E E E  E -> E E" |> mk_ss |> toinput |> parse_RULES
let _ = "E -> E E E\n| \"1\"  E -> E E" |> mk_ss |> toinput |> parse_RULES


let (Some t0) = read_file_as_string "../resources/example_grammar"
let g0 = t0 |> mk_ss |> toinput |> parse_GRAMMAR
*)



(* parse_SYM identifies terminals using quotes and question marks;
   here we provide an auxiliary to detect which case we are in;
   FIXME this is quick and dirty! *)

let dest_parsed_TM tm = (
  if String.length tm < 2 then failwith "dest_parsed_TM: string length < 2" else
    let stripped = String.sub tm 1 (String.length tm - 2) in
    if tm.[0] = '\"' then `Dquote stripped else
    if tm.[0] = '\'' then `Squote stripped else
    if tm.[0] = '?' then `Question stripped else
      failwith ("dest_parsed_TM: unidentified character: "^(Char.escaped (tm.[0]))))


(* FIXME version with actions; we allow parsing multiple action blocks *)

let rec parse_GRAMMAR_WITH_ACTIONS' = fun i ->
  ((parse_HG **> parse_wscomm **> parse_EOF) >> (fun (h,_) -> h)) i

and parse_HG = fun i ->
  (parse_RULES >> (fun rs -> ("",rs))
  ||| ((parse_HEADER **> parse_wscomm **> parse_RULES) >> (fun (h,(_,rs)) -> (h,rs)))) i

and parse_HEADER = fun i -> parse_CODE i

and parse_RULES = fun i ->
  ((listof parse_RULE parse_wscomm) >> (fun xs -> List.concat xs)) i

and parse_RULE = fun i ->
  ((parse_SYM **> parse_wscomm **> (a "->") **> parse_wscomm **> parse_RHS)
    >> (fun (nt,(_,(_,(_,symss)))) -> (List.map (fun syms -> (dest_NT nt,syms)) symss))) i

and parse_RHS = fun i ->
  (listof parse_SYMSACT (parse_wscomm **> (a "|") **> parse_wscomm)) i

and parse_SYMSACT = fun i ->
  ((parse_SYMS **> parse_wscomm **> parse_ACT) >> (fun (syms,(_,act)) -> (syms,act))) i

and parse_ACT = fun i -> parse_CODES i

and parse_CODES = fun i -> listof parse_CODE parse_wscomm i

and parse_CODE = fun i ->
  (((a "{{") **> until_a "}}" **> (a "}}")) >> (fun (_lt,(act,_gt)) -> (content act))) i

let (_:(string,(string * (nonterm * (symbol list * string list)) list)) ty_parser)
    = parse_GRAMMAR_WITH_ACTIONS'

(* 
let (Some t0) = read_file_as_string "../resources/example_grammar_with_actions"
let g0 = t0 |> mk_ss |> toinput |> parse_GRAMMAR_WITH_ACTIONS'
*)

(* return only the first action for each alternative *)
let parse_GRAMMAR_WITH_ACTIONS = fun i ->
  let rs = parse_GRAMMAR_WITH_ACTIONS' i in
  let f2 (nt,(syms,acts)) = (nt,syms,List.hd acts) in
  let f1 (h,rules) = (h,List.map f2 rules) in
  let f3 (v,srem) = (f1 v,srem) in
  List.map f3 rs

let (_:(string,grammar_with_actions) ty_parser)
    = parse_GRAMMAR_WITH_ACTIONS

let get_grammar : string -> grammar = (fun fname ->
  let txt = read_file_as_string fname in
  let txt = (match txt with | Some x -> x | _ -> failwith ("get_grammar: no file: "^fname)) in
  let rs = txt |> mk_ss |> toinput |> parse_GRAMMAR in
  let _ = if List.length rs = 0 then (
      failwith ("get_grammar: failed to parse grammar file: "^fname^""))
  in
  let _ = if List.length rs > 1 then (failwith ("get_grammar: ambiguous grammar file: "^fname)) in
  let (g,_) = List.hd rs in
  g)

let get_grammar_with_actions : string -> grammar_with_actions = (fun fname ->
  let txt = read_file_as_string fname in
  let txt = (match txt with | Some x -> x | _ -> failwith ("get_grammar: no file: "^fname)) in
  let rs = txt |> mk_ss |> toinput |> parse_GRAMMAR_WITH_ACTIONS in
  let _ = if List.length rs = 0 then (
      failwith ("get_grammar: failed to parse grammar file: "^fname^""))
  in
  let _ = if List.length rs > 1 then (failwith ("get_grammar: ambiguous grammar file: "^fname)) in
  let (g,_) = List.hd rs in
  g)

let drop_actions g = g |> List.map (fun (nt,syms,act) -> (nt,syms))

(* following functions are for printing a grammar with altered actions *)
(*
let unparse_GRAMMAR_WITH_ACTIONS : grammar_with_actions -> string = (fun (h,g) ->
  let nts : nonterm list = nts_of_grammar (drop_actions g) in
  let rhss : nonterm -> (symbol list * string) list = fun nt -> 
    g |> (List.filter (fun (nt',_,_) -> nt' = nt)) |> List.map (fun (nt,syms,act) -> (syms,act))
  in
  let f1 : nonterm * (symbol list * string) list -> string = (fun (nt,xs) ->
    let string_of_symbol sym = (match sym with
      | `NT nt -> nt
      | `TM tm -> tm)
    in
    let string_of_syms rhs = String.concat " " (List.map string_of_symbol rhs) in
    let f2 (syms,act) = (string_of_syms syms)^" {{"^act^"}}" in
    nt^" -> "^(String.concat "\n| " (List.map f2 rhss))^"\n")
  in
  "{{"^h^"}}\n"
  ^(String.concat "\n" (nts |> List.map rhss |> List.map f1 rhss)))
*)

(*
let pt_fun_of_rhs nt syms = (
  let rec upto a b = if a=b then [] else a::(upto (a+1) b) in
  let var n = "x"^(string_of_int n) in
  let ns = upto 0 (List.length syms) in
  let vars = List.map var ns in
  let rec pat1 vars = (match vars with
    | [] -> (failwith "pt_fun_of_rhs")
    | [x] -> x
    | x::xs -> ("("^x^","^(pat1 xs)^")"))
  in
  let pat = pat1 vars in
  let vars = List.combine vars syms in
  let dquote="\"" in
  let _LF = "P1_lib.P1_core.Types.LF" in
  let _LF = "`LF" in
  let _NODE = "`NODE" in
  let vars = List.map (fun (v,sym) -> if is_TM sym then _LF^"("^dquote^(String.escaped(dest_TM sym))^dquote^","^v^")" else v) vars in
  let list_pat = "["^(String.concat ";" vars)^"]" in
  " fun "^pat^" -> "^_NODE^"(\""^nt^"\","^list_pat^") ")
*)

(*
let mk_pt_actions (h,g) = (
  let f1 (nt,(syms,s)) = (nt,(syms,pt_fun_of_rhs nt syms)) in
  let g = List.map f1 g in
  (h,g))
*)
