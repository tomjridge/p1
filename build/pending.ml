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


















(**
Raw parsers, of type substring -> (substring * substring) list

*)

(**
{2 RawParsers}

Raw parsers are typically those parsers corresponding to terminals.

*)

module RawParsers = struct

  (* begin import *)
  (* prelude *)
  let ($) f g x = f(g x)

  let dest_Some x = match x with `Some y -> y | _ -> failwith "dest_Some"

  (* types *)
  type substring = string * int * int

  type term = string

  (* substring *)
  let string (s,l,h) = s

  let (low,high,len) = (
    (fun (s,l,h) -> l),
    (fun (s,l,h) -> h),
    (fun (s,l,h) -> h-l))

  let content s =
    String.sub (string s) (low s) (len s)

  let concatenate_two s1 s2 =
    if (string s1 = string s2) && (high s1 = low s2) then
      `Some (string s1, low s1, high s2)
    else
      `None

  let rec concatenate_list ss = match ss with
    [] -> `None
  | s1::ss -> (match ss with
      [] -> `Some s1
    | _ -> (match concatenate_list ss with
        `None -> `None
    |   `Some s2 -> concatenate_two s1 s2))
  (* end import *)

  let wf_substring (s,i,j) = (
    let wf = i <= String.length s && j <= String.length s && 0 <= i && i <= j in
    if not wf then failwith ("wf_substring!: "^s^" "^(string_of_int i)^" "^(string_of_int j)^"!") else ())

  type raw_parser = substring -> (substring * substring) list

  (* these combinators are only for raw parsers *)
  let ( **>@ ) p1 p2 = (fun s ->
    let f (e1,s1) =
      List.map (fun (e2,s2) -> ((e1,e2),s2)) (p2 s1)
    in
    ((List.concat $ (List.map f) $ p1) s))

  let ( |||@ ) p1 p2 = (fun s -> List.append (p1 s) (p2 s))

  let (>>@) p f = (List.map (fun (e,s) -> (f e, s))) $ p

  let never = fun i -> []

  let noteps p = (fun s ->
    List.filter (fun (_,srem) -> srem <> s) (p s))


  let a lit = fun s ->
    let n = String.length lit in
    if
      (n <= len s)
      && (String.sub (string s) (low s) n = lit)
    then
      let (s1,l,h) = s in
      [((s1,l,l+n),(s1,l+n,h))]
    else
      []

  (* this appears no faster than the naive approach above...
  (* s is matched by m from posn i *)
  let string_matches s m i = (
    let rec smatch j = (
      j >= String.length m || ((String.get s (i+j) = String.get m j) && smatch (j+1)))
    in
    (i + String.length m <= String.length s) && smatch 0)

  let a lit = (
    let n = String.length lit in
    fun s ->
      if
        (n <= len s)
        && (string_matches (string s) lit (low s))
      then
        let (s1,l,h) = s in
        let s2 = (s1,l,l+n) in
        [(s2,inc_low n s)]
      else
        [])
  *)

  let (_:raw_parser) = (a "1")

  (* FIXME change this to take an underlying parser *)
  let until_a lit = fun s ->
    let llit = String.length lit in
    let rec f1 n =
      if
        n+llit <= len s
        && (String.sub (string s) ((low s)+n) llit) = lit
      then
        let (s1,l,h) = s in
        (* let s2 = (s1,l,l+n) in *)
        [((s1,l,l+n),(s1,l+n,h))]
      else if
          n+llit <= len s
      then
        f1 (n+1)
      else
        let (s1,l,h) = s in
        [(s,(s1,h,h))]
    in
    f1 0

  (* pred is a function from a string of length 1 to a bool *)
  let parse1 pred = fun s ->
    if (1 <= len s && pred (String.sub (string s) (low s) 1)) then
      let (s,i,j) = s in
      [((s,i,i+1),(s,i+1,j))]
    else
      []

  let parse_EOF = fun s -> (
    if (low s = high s) && (high s = String.length (string s)) then
      (a "") s
    else
      never s)

  (* can return eps; FIXME this is incredibly dangerous, and breaks wf of terminal parsers *)
  let parse_while pred = fun s ->
    let rec f = fun n ->
      if n = len s then len s else
      let c = String.sub (string s) ((low s)+n) 1 in
      if pred c then f (n+1) else n
    in
    let n = f 0 in
    let (s,i,j) = s in
    [((s,i,i+n),(s,i+n,j))]

  let (_:(string -> bool) -> raw_parser) = parse_while

  (* FIXME could tidy up the following *)

  let parse_azAZ =
    let pred = fun c ->
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    in
    parse1 pred

  let (_:raw_parser) = parse_azAZ

  let parse_AZ =
    let pred c =
      (String.compare "A" c <= 0)
      && (String.compare c "Z" <= 0)
    in
    parse1 pred

  let parse_az =
    let pred c =
      (String.compare "a" c <= 0)
      && (String.compare c "z" <= 0)
    in
    parse1 pred

  let parse_azs =
    let pred c =
      (String.compare "a" c <= 0)
      && (String.compare c "z" <= 0)
    in
    parse_while pred

  let parse_AZS =
    let pred c =
      (String.compare "A" c <= 0)
      && (String.compare c "Z" <= 0)
    in
    noteps (parse_while pred)

  let parse_ws = noteps (parse_while (fun s -> s = " " || s = "\n"))

  let parse_epsws = (parse_while (fun s -> s = " " || s = "\n"))

  let parse_newline = a "\n"

  let parse_azAZs =
    let pred = fun c ->
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    in
    noteps (parse_while pred)

  let parse_notdquote =
    parse_while (fun c -> not (c = "\""))

  let parse_notsquote =
    parse_while (fun c -> not (c = "'"))

  let parse_notlt =
    parse_while (fun c -> not (c = "<"))

  let parse_notgt =
    parse_while (fun c -> not (c = ">"))

  let parse_notltgt =
    parse_while (fun c -> not ((c = "<") || (c = ">")))

  let parse_notbracket =
    parse_while (fun c -> not ((c = "(") || (c = ")")))

  (* FIXME if parse_ws includes \n, then notws should also *)
  let parse_notws =
    parse_while (fun c -> not (c = " "))

  let parse_notcurlyr = parse_while (fun c -> not (c = "}"))

  let parse_all =
    parse_while (fun c -> true)

  let parse_num =
    let pred = fun c ->
      (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
    in
    noteps (parse_while pred)

  (* the following is hopeless, obviously; nums are non-empty  *)
  let parse_float = ((parse_num **>@ (a ".") **>@ parse_num) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z])))

  let parse_ident =
    let pred = fun c ->
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
      || (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
      || (c = "_") || (c = "'")
    in
    noteps (parse_while pred)

  let term_to_parser s = (match s with
    | "?all?"   -> parse_all
    | "?AZS?" -> parse_AZS
    | "?AZ?" -> parse_AZ
    | "?az?" -> parse_az
    | "?azs?" -> parse_azs
    | "?azAZ?" -> parse_azAZ
    | "?azAZs?" -> parse_azAZs
    | "?EOF?" -> parse_EOF
    | "?epsws?" -> parse_epsws
    | "?ident?" -> parse_ident
    | "?newline?" -> parse_newline
    | "?notbracket?" -> parse_notbracket
    | "?notcurlyr?" -> parse_notcurlyr
    | "?notdquote?" -> parse_notdquote
    | "?notgt?" -> parse_notgt
    | "?notlt?" -> parse_notlt
    | "?notltgt?" -> parse_notltgt
    | "?notsquote?" -> parse_notsquote
    | "?num?" -> parse_num
    | "?float?" -> parse_float
    | "?ws?" -> parse_ws
    | "\"\"" -> a ""
    | _ -> ( (* interpret as a literal *)
        if String.length s < 2 then failwith ("term_to_parser: "^s)
        else
    let _ = () (* print_string ("term_to_parser: treating "^s^" as a literal\n") *) in
    (a (String.sub s 1 (String.length s - 2)))))

  let (_:term -> raw_parser) = term_to_parser

end


(**
{2 Functorized raw parsers}

We lift the raw parsers to operate over an arbitrary input type. In BasicParsers We then instantiate the input type to `ty_input1`.

*)

module type SubstringPlus = sig
  type ty_input
  val substr:ty_input -> (string*int*int)
  val with_substr:ty_input -> (string*int*int) -> ty_input
end

module FunctorBasicParsers = functor(A:SubstringPlus) -> struct

  open RawParsers

  type ty_input = A.ty_input

  type 'a myparser = A.ty_input -> ('a * substring) list

  let seq p1 p2 = (fun i ->
    let f (e1,s1) =
      List.map (fun (e2,s2) -> ((e1,e2),s2)) (p2 (A.with_substr i s1))
    in
    ((List.concat $ (List.map f) $ p1) i))

  let alt p1 p2 = (fun i -> List.append (p1 i) (p2 i))

  let with_action p f = (fun i -> List.map (fun (e,s) -> (f e, s)) (p i))

  let wrap p i = (p (A.substr i))

  let (_:raw_parser -> substring myparser) = wrap

  (* these combinators are only for raw_parsers *)
  let ( **>@ ) p1 p2 = seq p1 p2

  let ( |||@ ) p1 p2 = alt p1 p2

  let (>>@) p f = with_action p f

  (* string -> substring ty_parser *)
  let a lit = wrap (a lit)

  let (_:substring myparser) = (a "1")

  (* FIXME change this to take an underlying parser *)
  let until_a lit = wrap (until_a lit)

  (* pred is a function from a string of length 1 to a bool *)
  let parse1 pred = wrap (parse1 pred)

  let parse_EOF = wrap (parse_EOF)

  let parse_while pred = wrap (parse_while pred)

  let (_:(string -> bool) -> substring myparser) = parse_while

  (* FIXME could tidy up the following *)

  let parse_azAZ = wrap parse_azAZ

  let (_:substring myparser) = parse_azAZ

  let parse_AZ = wrap parse_AZ

  let parse_az = wrap parse_az

  let parse_azs = wrap parse_azs

  let parse_AZS = wrap parse_AZS

  let parse_ws = wrap parse_ws

  let parse_epsws = wrap parse_epsws

  let parse_newline = wrap parse_newline

  let parse_azAZs = wrap parse_azAZs

  let parse_notdquote = wrap parse_notdquote

  let parse_notsquote = wrap parse_notsquote

  let parse_notlt = wrap parse_notlt

  let parse_notgt = wrap parse_notgt

  let parse_notltgt = wrap parse_notltgt

  let parse_notbracket = wrap parse_notbracket

  let parse_notws = wrap parse_notws

  let parse_notcurlyr = wrap parse_notcurlyr

  let parse_all = wrap parse_all

  let parse_num = wrap parse_num

  let parse_float = wrap parse_float

  let parse_ident = wrap parse_ident

  let term_to_parser s = wrap (term_to_parser s)

  let (_:term -> substring myparser) = term_to_parser

end


