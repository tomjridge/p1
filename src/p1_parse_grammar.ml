(* Parse a grammar file *)

(* FIXME code generation doesn't work correctly if we have a nt with no rules *)

open P1_lib

type nonterm = string
type sym = [ `NT of string | `TM of string ]

type grammar = (nonterm * sym list) list

let tm_of_lit quote lit = `TM(quote^lit^quote)

let parse_comm = fun i -> ((a "(*") **> until_a "*)" **> (a "*)")) i

let parse_ws = parse_RE " *" (* do we want more than one whitespace? *)

let noteps : ('a,'b) ty_parser -> ('a,'b) ty_parser = (fun p i0 ->
    i0 |> p |> (List.filter (fun (x,ss) -> ss_len ss <> 0)))

let eps = parse_eps

(* list of p separated by sep *)
let listof : ('a,'b) ty_parser -> ('a,'c) ty_parser -> ('a,'b list) ty_parser = (fun p sep ->
    let rec f1 i0 = 
      ((p >> (fun x -> [x]))
       ||| ((p **> sep **> f1) >> (fun (x,(_,xs)) -> x::xs))) 
        i0
    in
    f1 ||| (eps >> (fun _ -> [])))

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

let str = {abc|
E -> E E E
| "1" 
| ""
|abc}




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

let (_:ty_input1 -> ((string * (nonterm * (symbol list * string list)) list) * substring) list)
    = parse_GRAMMAR_WITH_ACTIONS'

let drop_actions g = List.map (fun (nt,y) -> (nt,fst y)) g

let parse_GRAMMAR_WITH_ACTIONS = fun i ->
  let rs = parse_GRAMMAR_WITH_ACTIONS' i in
  let f2 (nt,(syms,acts)) = (nt,(syms,List.hd acts)) in
  let f1 (h,rules) = (h,List.map f2 rules) in
  let f3 (v,srem) = (f1 v,srem) in
  List.map f3 rs

let (_:ty_input1 -> ((string * (nonterm * (symbol list * string)) list) * substring) list)
    = parse_GRAMMAR_WITH_ACTIONS

let get_grammar fname = (
  let open P1_prelude in
  let open P1_core.Common in
  let open P1_core.Substring in
  let rs = (parse_GRAMMAR (toinput (full (read_file_as_string fname)))) in
  let _ = if List.length rs = 0 then (
    failwith ("Failed to parse grammar file: "^fname^""))
  in
  let _ = if List.length rs > 1 then (failwith ("Ambiguous grammar file: "^fname)) in
  let (g,_) = List.hd rs in
  g)

let get_grammar_with_actions fname = (
  let open P1_prelude in
  let open P1_core.Common in
  let open P1_core.Substring in
  let rs = (parse_GRAMMAR_WITH_ACTIONS (toinput (full (read_file_as_string fname)))) in
  let _ = if List.length rs = 0 then (
    failwith ("Failed to parse grammar file: "^fname^""))
  in
  let _ = if List.length rs > 1 then (failwith ("Ambiguous grammar file: "^fname)) in
  let (g,_) = List.hd rs in
  g)



(* following functions are for printing a grammar with altered actions *)
let unparse_GRAMMAR_WITH_ACTIONS (h,g) = (
  let nts = P1_core.Common.nts_of_grammar (drop_actions g) in
  let rhss nt = List.map snd (List.filter (fun (nt',_) -> nt' = nt) g) in
  let rhss = List.map (fun nt -> (nt,rhss nt)) nts in
  let f1 (nt,rhss) = (
    let string_of_symbol sym = (match sym with
      | `NT nt -> nt
      | `TM tm -> tm)
    in
    let string_of_rhs rhs = String.concat " " (List.map string_of_symbol rhs) in
    let f2 (rhs,s) = (string_of_rhs rhs)^" {{"^s^"}}" in
    nt^" -> "^(String.concat "\n| " (List.map f2 rhss))^"\n")
  in
  "{{"^h^"}}\n"
  ^(String.concat "\n" (List.map f1 rhss)))

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

let mk_pt_actions (h,g) = (
  let f1 (nt,(syms,s)) = (nt,(syms,pt_fun_of_rhs nt syms)) in
  let g = List.map f1 g in
  (h,g))

