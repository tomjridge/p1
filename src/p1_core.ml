

type 'a substring = [ `SS of 'a * int * int ]

let mk_ss : string -> string substring = fun s -> `SS(s,0,String.length s)

let dest_substring : 'a substring -> string * int * int = 
  fun (`SS(s,i,j)) -> (s,i,j)

let ss_to_string = fun (`SS(s,i,j)) -> Printf.sprintf "(%s,%d,%d)" s i j

let content : string substring -> string = 
  fun (`SS(s,l,h)) -> String.sub s l (h-l)

let len : 'a substring -> int = (
  fun (`SS(s,i,j)) -> (j-i))

(* convenience *)
let ss_concat_2 : 'a substring -> 'a substring -> 'a substring = fun (`SS(s,i,k)) (`SS(s',k',j)) ->
  assert(k'=k && s'=s);
  `SS(s,i,j)



(* assumes the substrings are adjacent *)
let rec ss_concat xs = (match xs with
    | [] -> None 
    | [x] -> (Some x)
    | [x;y] -> (Some(ss_concat_2 x y))
    | x::xs -> (match ss_concat xs with
        | None -> None
        | Some xs -> Some(ss_concat_2 x xs)))




type nonterm = string

type lc_substring = int * int (* FIXME really need an extra arg int for the string *)

let lc_substring_of : 'a substring -> lc_substring =
  fun (`SS(s,i,j)) -> (i,j)

type local_context = LC of (nonterm * lc_substring) list

let empty_context = LC []



type 'a ty_input = { lc1 : local_context; sb1 : 'a substring }
let substring_of_input i = i.sb1

let toinput s = { lc1=empty_context; sb1=s }
let (_:'a substring -> 'a ty_input) = toinput



type ('a,'b) ty_parser = 'a ty_input -> ('b * 'a substring) list



(**********************************************************************)
(* combinators *)

(* It is worth noting that nothing in the following definitions
   depends on the notion of context. Context comes later, and is
   modularly combined with the following. *)

let unique = P1_prelude.unique

let ( >> ) p f = (fun i0 ->
  i0 |> p |> List.map (fun (e,s) -> (f e, s)) |> unique)
let (_:('a,'b) ty_parser -> ('b -> 'c) -> ('a,'c) ty_parser) = ( >> )

let ( ||| ) p1 p2 = fun s -> List.append (p1 s) (p2 s) |> unique
let (_: ('a,'b) ty_parser -> ('a,'b) ty_parser -> ('a,'b) ty_parser) = ( ||| )

(* a version of the combinator that ignores duplicate entries FIXME *)
let ( **> ) p1 p2 = (fun i ->
  let f (e1,s1) =
    { lc1=i.lc1; sb1=s1 } |> p2 |> List.map (fun (e2,s2) -> ((e1,e2),s2))
  in
  i |> p1 |> List.map f |> List.concat)
let (_:('a,'b) ty_parser -> ('a,'c) ty_parser -> ('a, 'b*'c) ty_parser) = ( **> )


let lift f i = {i with sb1=(f i.sb1) } 
let dec_high i = lift (fun (`SS(s,i,j)) -> `SS(s,i,j-1)) i 
let inc_high : ('a * 'b substring) -> ('a * 'b substring) = (
  fun (e,`SS(s,i,j)) -> (e,`SS(s,i,j+1)))

let ignr_last p i = (
  match len i.sb1 with
  | 0 -> []
  | _ -> i |> dec_high |> p |> List.map inc_high)
let (_:('a,'b) ty_parser -> ('a,'b) ty_parser) = ignr_last


(**********************************************************************)
(* context *)


(* debug version; assumes s1 = s2 (since the only part of the context that matters is...) *)
let lc_cmp (nt1,(l1,h1)) (nt2,(l2,h2)) =
  if (l1,h1) <> (l2,h2) then failwith "lc_cmp" else Pervasives.compare nt1 nt2

(* when parsing the input between l and h, the only part of the
 context that matters are those entries (nt,(l',h')) st (l',h') =
 (l,h); so there is a notion of a normalized context (important
 for memoization) *)

let normalize_context (LC(lc)) (l,h) = (
  LC(List.filter (fun (nt',(l',h')) -> (l',h') = (l,h)) lc))

let update_context c (nt,(l,h)) = (
  let LC(lc) = normalize_context c (l,h) in
  LC(P1_prelude.myinsert lc_cmp (nt,(l,h)) lc))

let context_contains (LC(lc)) (nt,(l,h)) = (
  List.exists ((=) (nt,(l,h))) lc)


(* remember what NT is called on what input *)
(* nonterm -> 'a ty_parser -> 'a ty_parser *)
let update_lctxt nt p = (fun i0 ->
  p { i0 with lc1=(update_context i0.lc1 (nt,lc_substring_of i0.sb1)) })

let (_:nonterm -> ('a,'b) ty_parser -> ('a,'b) ty_parser) = update_lctxt

let check_and_upd_lctxt nt p = fun i0 ->
  let should_trim = context_contains i0.lc1 (nt,lc_substring_of i0.sb1) in
  if should_trim && (len i0.sb1 = 0) then
    []
  else if should_trim then
    (ignr_last (update_lctxt nt p)) i0
  else
    (update_lctxt nt p) i0

let (_:nonterm -> ('a,'b) ty_parser -> ('a,'b) ty_parser) = check_and_upd_lctxt



(**********************************************************************)
(* memoization support *)

(* FIXME really this needs to have an gensymed int identifier for the
   string component *)
type hashkey = (local_context * (int * int))

let hashkey_of_input i0 = (i0.lc1,lc_substring_of i0.sb1)
  
(* generic memo function *)
let memo tbl key_of_input f i = (
  let k = key_of_input i in
  if (Hashtbl.mem tbl k) then (Hashtbl.find tbl k) else
    let v = f i in
    let _ = Hashtbl.add tbl k v in
    v)


(**********************************************************************)
(* convenience *)

(* return those results for which entire input is parsed *)
let run_parser_string : (string,'b) ty_parser -> string -> 'b list = fun p s -> 
  s |> mk_ss |> toinput |> p |> List.filter (fun (_,`SS(s,i,j)) -> i=j) |> List.map fst
