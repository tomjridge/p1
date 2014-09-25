(* some simple code to read and write a file, using minimal technology *)

(* copied from ocaml/utils/misc.ml *)
let string_of_ic' ic =
  let b = Buffer.create 0x10000 in
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_substring b buff 0 n; copy())
  in copy()

let read_ic_as_string ic = (
  try
    string_of_ic' ic
  with _ -> failwith "read_ic_as_string")

(* get contents of file as string; return option Some s if successful, None otherwise *)
let read_file_as_string fn = (
  try
    let ic = open_in fn in
    let s = string_of_ic' ic in
    let _ = close_in ic in
    Some s
  with _ -> None)

let write_string_to_oc s oc = (
  try
    let () = output_string oc s in
    true
  with _ -> false)

(* write a string to a file ; return true if successful, false otherwise *)
let write_string_to_file s fn = (
  try
    let oc = open_out fn in
    let _ = output_string oc s in
    let _ = close_out oc in
    true
  with _ -> false)

(*
let Some(s) = string_of_file "/tmp/alldates.txt"
let _ = write_string_to_file s "/tmp/a2.txt"
*)



(**
{2 `grammar_to_parser` and caneps}

This is the plain version that appears in the paper and the HOL4 formalization.

*)

module GrammarToParser = struct

  open P1_prelude
  open P1_core.Types
  open P1_core.Combinator
  open P1_core.Context

  let rec grammar_to_parser p_of_tm g sym i = (match sym with
    `TM tm -> ((p_of_tm tm) >> (fun v -> LF(tm,v))) i | `NT nt ->
    let rules = List.filter (fun (nt',rhs) -> nt' = nt) g in
    let alts1 = List.map snd rules in
    let alts2 = List.map (List.map (grammar_to_parser p_of_tm g)) alts1 in
    let p = or_list (List.map (then_list2 nt) alts2) in
    check_and_upd_lctxt nt p i)

  let (_: (term -> substring ty_parser) -> grammar -> symbol -> parse_tree ty_parser) = grammar_to_parser

  let g2p_params p_of_tm = {
    p_of_tm3=(fun tm -> (p_of_tm tm) >> (fun v -> LF(tm,v)));
    then_list3=(fun nt -> then_list2 nt);
    check_and_upd_lctxt3=(fun nt -> check_and_upd_lctxt nt);
    unique3=(fun p -> p);
  }

  let rec g2p params g sym i = (match sym with
    `TM tm -> params.p_of_tm3 tm i | `NT nt ->
    let rules = List.filter (fun (nt',rhs) -> nt' = nt) g in
    let alts1 = List.map snd rules in
    let alts2 = List.map (List.map (g2p params g)) alts1 in
    let p = or_list (List.map (params.then_list3 nt) alts2) in
    let q = params.unique3 p in
    params.check_and_upd_lctxt3 nt q i)

  (* version via parameterization *)
  let grammar_to_parser p_of_tm = g2p (g2p_params p_of_tm)


  let toinput = P1_core.Common.toinput
  let full = P1_core.Substring.full

  let caneps p_of_tm g start_sym =
    let tbl = MyHashtbl.create 100 in
    let memo_grammar_to_parser tbl p_of_tm =
      let ps = {
        p_of_tm3=(fun tm -> (p_of_tm tm) >> (fun _ -> tm));
        then_list3=(fun nt -> fun alt -> then_list alt >> (fun _ -> nt));
        check_and_upd_lctxt3=(fun nt -> memo_check_and_upd_lctxt tbl nt);
        unique3=(fun p i -> unique (p i));
      } in
      (fun g sym i -> g2p ps g sym i)
    in
    let p = memo_grammar_to_parser tbl p_of_tm g start_sym in
    let rs = p (toinput (full "")) in
    rs <> []

end

;;






(**
{2 Parse a grammar file}
*)

module ParseGrammar = struct

  (* FIXME code generation doesn't work correctly if we have a nt with no rules *)

  open P1_core.Types
  open P1_core.BasicParsers
  open P1_core.Combinator
  let content = P1_core.Substring.content

  let tm_of_lit quote lit = `TM(quote^lit^quote)

  let parse_comm = fun i -> ((a "(*") **> until_a "*)" **> (a "*)")) i

  (* FIXME only one comment in ws? *)
  let parse_wscomm =
    ((parse_ws >> (fun _ -> ""))
     ||| ((parse_ws **> parse_comm **> parse_ws) >> (fun _ -> "")))

  (* allow _ in NT *)
  let parse_AZS =
    let pred c =
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || (String.compare c "_" = 0)
      || ((String.compare "0" c <=0) && (String.compare c "9" <= 0))
    in
    noteps (parse_while pred)

  let parse_azAZs =
    let pred = fun c ->
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
      || (String.compare c "_" = 0)
      || ((String.compare "0" c <=0) && (String.compare c "9" <= 0))
    in
    noteps (parse_while pred)

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

  let (_:ty_input1 -> (grammar * substring) list) = parse_GRAMMAR

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

end

(**
{2 Command line helper functions}
*)

module CommandLine = struct

  let toinput = P1_core.Common.toinput
  let full = P1_core.Substring.full

  let get_args parse_CL argv = (
    let argv = List.tl (Array.to_list argv) in
  (*  let _ = print_endline ("Command line: "^(String.concat " " argv)) in *)
    let rs = (parse_CL (toinput (full (String.concat "\x00" argv)))) in
    match rs with
      | [] -> failwith "get_args: failed to parse command line (0 parses)"
      | [(r,_)] -> r
      | _ -> failwith "get_args: failed to parse command line (>=2 parses)")

  open P1_core.Substring
  open P1_core.BasicParsers
  open P1_core.Combinator

  (* parsers for command line parsing *)

  (* we use "\x00" as an arg separator - assumes this char does not appear on the cl *)
  let parse_FLAG = ((a "-") **> parse_while (fun s -> s <> "\x00")) >> (fun (_,s) -> "-"^(content s))

  (* first char should not be a - *)
  let parse_ARG =
    let parse_not_minus = parse1 (fun c -> c <> "-") in
    (parse_not_minus **> parse_while (fun s -> s <> "\x00")) >> (fun (s1,s2) -> ((content s1)^(content s2)))

  let parse_FLARGS = (
    let sep = a "\x00" in
    (parse_FLAG >> fun f -> (f,[]))
    ||| ((parse_FLAG **> sep **> (listof parse_ARG sep)) >> (fun (f,(_,xs)) -> (f,xs))))

end



;;
