(**********************************************************************)
(* substrings *)


let concatenate_two (`SS(s1,i1,j1)) (`SS(s2,i2,j2)) = (
  match (s1=s2 && (j1=i2)) with
  | true -> Some (`SS(s1,i1,j2))
  | false -> None)

let rec concatenate_list ss = (
  match ss with
  | [] -> None
  | [s1] -> (Some s1)
  | s1::ss -> (
    match concatenate_list ss with
    | None -> None
    | Some s2 -> concatenate_two s1 s2))



  let rec string_of_pt pt = (match pt with
    | LF(x,s) -> ("LF("^x^",\""^(String.escaped (content s))^"\")")
    | NODE(x,pts) -> (
        "NODE("^
         x^","^"["^
         (String.concat "," (List.map string_of_pt pts))^
         "])"))



  type raw_parser = substring -> (substring * substring) list

  (* packaging a grammar with the relevant terminal parsers - here restricted to be finite *)
  (* type grammar8 = { g8:grammar; raw_parsers8:(term * raw_parser) list } *)

  type ty_p_of_tm = term -> substring ty_parser


  (* memoization *)
  type key = (nonterm * local_context * (int * int))

  type ty_compact_form = (nonterm * lc_substring) list

  (* moved to earley
  (* parse results *)
  type parsed_sym = (symbol * lc_substring)
  type parse_result =
    | PNODE of (nonterm * lc_substring) * (parsed_sym list)
    | PLEAF of (term * lc_substring)
  *)

  (* grammar_to_parser parameterization *)

  type 'a g2p_params = {
    p_of_tm3: term -> 'a ty_parser;
    then_list3: nonterm -> 'a ty_parser list -> 'a ty_parser;
    check_and_upd_lctxt3: nonterm -> 'a ty_parser -> 'a ty_parser;
    unique3: 'a ty_parser -> 'a ty_parser;
  }


  let string_of_substring (s,l,h) = "("^s^","^(string_of_int l)^","^(string_of_int h)^")"

  let lc_substring_of (s,l,h) = (l,h)

  let eps = `TM(eps) (* fix one particular terminal for eps *)

  let syms_of_rhs rhs = rhs

  let syms_of_parse_rule (nt,rhs) = insert (`NT(nt)) (syms_of_rhs rhs)

  let syms_of_grammar g = unions (List.map syms_of_parse_rule g)

  let nts_of_grammar g = (
    let syms = syms_of_grammar g in
    let nts = List.map dest_NT (List.filter is_NT syms) in
    nts)


  (* sometimes we need to get the underlying function from substrings rather than inputs *)
  let raw_parser_of_parser (p:substring ty_parser) s = (p (toinput s))

  let (_:substring ty_parser -> raw_parser) = raw_parser_of_parser

  let parser_of_raw_parser p i = p (i.sb1)

  let (_:raw_parser -> substring ty_parser) = parser_of_raw_parser


(**
{2 Basic parsers}

We lift the raw parsers to take inputs of type `ty_input1` rather than `substring`. N.B. these bindings shadow those from RawParsers - don't get confused! As with raw parsers, basic parsers correspond to terminals in the grammar. However, here they take a type `ty_input1` rather than `substring`.

*)

module BasicParsers = struct

  (* desired instantiation *)
  module Input = struct
    open Types
    type ty_input = ty_input1
    let substr i = i.sb1
    let with_substr i s = {i with sb1=s}
  end

  include P1_terminal_parsers.FunctorBasicParsers(Input)

end



  let not_epsilon p = fun i ->
    List.filter (fun (v,_) -> not (len v = 0)) (p i)

  let (_:substring ty_parser -> substring ty_parser) = not_epsilon

  let noteps p = fun i ->
    List.filter (fun (_,srem) -> srem <> substr i) (p i)

  let (_:'a ty_parser -> 'a ty_parser) = noteps

  let peps = (fun i ->
    let (s,l,h) = i.sb1 in
    let s1 = (s,l,l) in
    [(s1,i.sb1)])

  (* FIXME following allows: item sep as a list *)
  let rec listof item sep = fun i ->
    ((peps >> (fun _ -> []))
     ||| (item >> (fun x -> [x]))
     ||| ((item **> sep **> (listof item sep)) >> (fun (x,(_,xs)) -> x::xs))) i

  let rec star item = fun i ->
    ((peps >> (fun _ -> []))
     ||| ((item **> (star item)) >> (fun (x,xs) -> x::xs))) i

  let rec itern item n = (match n with
    | 0 -> (peps >> (fun _ -> []))
    | _ -> ((item **> (itern item (n-1))) >> (fun (x,xs) -> x::xs)) )

  let just a = (always >> (fun _ -> a))

  (* consume until another parser produces results - other parser MUST produce a result! *)
  (* FIXME very inefficient *)
  let rec until p = fun i -> (
    let rs = p i in
    let (s,l,h) = i.sb1 in
    if rs <> [] then [(s,l,l),(s,l,h)] else
      if len i.sb1 = 0 then [] else
        let rs = until p (lift (inc_low 1) i) in
        let f1 (s1,s2) = (dec_low 1 s1,s2) in
        List.map f1 rs)

  let (_:(ty_input1 -> 'a list) -> ty_input1 -> (substring * substring) list) = until

  (* FIXME following is tail recursive; not sure about correctness *)
  let until p = fun i -> (
    let (s,l,h) = i.sb1 in
    let rec f1 n = (
      if l+n <= h then (
        let rs = p {i with sb1=(s,l+n,h)} in
        if rs <> [] then [(s,l,l+n),(s,l+n,h)] else f1 (n+1)
      ) else [])
    in
    f1 0)

  (* consume until another parser produces results, or end of string *)
  let rec until' p = fun i -> (
    let rs = p i in
    let (s,l,h) = i.sb1 in
    if rs <> [] then [(s,l,l),(s,l,h)] else
      if len i.sb1 = 0 then [(s,l,h),(s,l,h)] else
        let rs = until' p (lift (inc_low 1) i) in
        let f1 (s1,s2) = (dec_low 1 s1,s2) in
        List.map f1 rs)

  (* not combinator *)
  (* FIXME what should be the correct semantics?
  let parse_not p = fun i -> (
    let (s,l,h) = i.sb1 in
    let rs = p i in
    if rs = [] then [(s,l,l),(s,l,h)] else [])
  *)

  (* and combinator; results are those from first parser; matched substrings have to match each parser *)
  let (&&&) p1 p2 = fun i -> (
    let rs1 = p1 i in
    let rs2 = p2 i in
    let rs3 = List.map snd rs2 in
    let f1 = fun (r,s) -> mem s rs3 in
    let rs1 = List.filter f1 rs1 in
    rs1)













open P1_prelude
open Types
open Substring
let lc_substring_of = Common.lc_substring_of
let ignr_last = Combinator.ignr_last
let substr = Common.substr
let lift = Common.lift


  (* simple memoization *)

  let generic_memo tbl key_of_input f i = (
    let k = key_of_input i in
    match k with
    | None -> (f i)
    | Some k -> (
      if (MyHashtbl.mem tbl k) then (MyHashtbl.find tbl k) else
        let v = f i in
        let _ = MyHashtbl.add tbl k v in
        v))

  let key_of_input nt = (fun i ->
    let i = { i with lc1=(normalize_context i.lc1 (lc_substring_of i.sb1)) } in
    let k = (nt,i.lc1,lc_substring_of i.sb1) in
    Some k)

  (* (key,('a * substring)list) MyHashtbl.t -> nonterm -> 'a ty_parser -> 'a ty_parser *)
  let memo_check_and_upd_lctxt tbl nt p i = (
    generic_memo tbl (key_of_input nt) (check_and_upd_lctxt nt p) i)

  let (_:(key,('a * substring)list) MyHashtbl.t -> nonterm -> 'a ty_parser -> 'a ty_parser) =
    memo_check_and_upd_lctxt


  (* parameterization by ignr_last *)

  (* f1 argument takes a (nt,(i,j)) and return an int option k < j *)
  (* FIXME change name of following *)
  let sti_ignr_last f1 nt p = fun i ->
    if len (substr i) = 0 then [] else (
      let k = f1 (nt,lc_substring_of i.sb1) in
      match k with | None -> [] | Some k ->
        let delta = high i.sb1 - k in
        let inc_high (e,s) = (e,inc_high delta s) in
        ((List.map inc_high) $ p $ (lift (dec_high delta))) i)
  let (_:(nonterm * (int * int) -> int option) -> nonterm -> 'a ty_parser -> 'a ty_parser) = sti_ignr_last

  (* FIXME change name of following *)
  let il_memo_check_and_upd_lctxt ignr_last =
    let check_and_upd_lctxt nt p = fun i ->
      let should_trim = context_contains i.lc1 (nt,lc_substring_of i.sb1) in
      if should_trim && (len i.sb1 = 0) then
        []
      else if should_trim then
        (ignr_last nt (update_lctxt nt p)) i
      else
        (update_lctxt nt p) i
    in
    (fun tbl nt p i ->
      generic_memo tbl (key_of_input nt) (check_and_upd_lctxt nt p) i)

