
module Substring : (sig

  type 'a t

  val mk: string -> string t
      
  val with_i: int -> 'a t -> 'a t

  val with_j: int -> 'a t -> 'a t

  val s: 'a t -> 'a
  val i: 'a t -> int
  val j: 'a t -> int
  val len: 'a t -> int

end) = struct

  type 'a t = ('a*int*int*int)

  let mk s = (s,String.length s,0,String.length s)

  let with_i i' s = (
    let (s,l,i,j) = s in
    (s,l,i',j))

  let with_j j' s = (
    let (s,l,i,j) = s in
    (s,l,i,j'))
  
  let s s = (
    let (s,l,i,j) = s in
    s)

  let i s = (
    let (s,l,i,j) = s in
    i)

  let j s = (
    let (s,l,i,j) = s in
    j)

  let len s = (
    let (s,l,i,j) = s in
    j-i)

end

module S = Substring

let dec_j : 'a S.t -> 'a S.t = (fun s -> s|>S.with_j (S.j s -1))
let inc_j : 'a S.t -> 'a S.t = (fun s -> s|>S.with_j (S.j s +1))


type nonterm = string


module Pre_context = struct
  
  type elt = (nonterm * int * int)  (* FIXME really need an extra arg int for the string *)
  type t = elt list
      
  let empty = []

end



type c = Pre_context.t

type 'a s = 'a Substring.t


type 'a input_t = { c:Pre_context.t; s : 'a s }

let to_input s = { c=Pre_context.empty; s=s }

let lift f i = {i with s=(f i.s) } 

type ('a,'b) result = ('b * 'a s) list

type ('a,'b) parser_t = 'a input_t -> ('a,'b) result




(* combinators ---------------------------------------- *)

(* It is worth noting that nothing in the following definitions
   depends on the notion of context. Context comes later, and is
   modularly combined with the following. *)

let unique = P1_prelude.unique

let ( >> ) p f = (fun i0 ->
  i0 |> p |> List.map (fun (e,s) -> (f e, s)) |> unique)

let (_:('a,'b) parser_t -> ('b -> 'c) -> ('a,'c) parser_t) = ( >> )

let ( ||| ) p1 p2 = fun s -> List.append (p1 s) (p2 s) |> unique

let (_: ('a,'b) parser_t -> ('a,'b) parser_t -> ('a,'b) parser_t) = ( ||| )

(* a version of the combinator that ignores duplicate entries FIXME *)
let ( **> ) p1 p2 i = (
  let f (e1,s1) =
    { c=i.c; s=s1 } |> p2 |> List.map (fun (e2,s2) -> ((e1,e2),s2))
  in
  i |> p1 |> List.map f |> List.concat)

let (_:('a,'b) parser_t -> ('a,'c) parser_t -> ('a, 'b*'c) parser_t) = ( **> )


let ignr_last p i = (
  match (i.s|>S.len) with
  | 0 -> []
  | _ -> {i with s = i.s|>dec_j} |> p |> List.map (fun (v,s) -> (v,s|>inc_j)))

let (_:('a,'b) parser_t -> ('a,'b) parser_t) = ignr_last



(* context ---------------------------------------- *)

module Context = struct

  include Pre_context

  (* debug version; assumes s1 = s2 (since the only part of the
     context that matters is...) *)
  let lc_cmp (nt1,l1,h1) (nt2,l2,h2) = (
    if (l1,h1) <> (l2,h2) then failwith "lc_cmp" else Pervasives.compare nt1 nt2)

  (* when parsing the input between l and h, the only part of the
     context that matters are those entries (nt,(l',h')) st (l',h') =
     (l,h); so there is a notion of a normalized context (important
     for memoization) *)

  let normalize c (l,h) = (
    c |> List.filter (fun (nt',l',h') -> (l',h') = (l,h)) )

  let update c (nt,l,h) = (
    let c' = normalize c (l,h) in
    (P1_prelude.myinsert lc_cmp (nt,l,h) c'))

  let contains c (nt,l,h) = (
    List.exists ((=) (nt,l,h)) c)


  (* remember what NT is called on what input *)
  (* nonterm -> 'a parser_t -> 'a parser_t *)
  (* was update_lctxt *)
  let update_p nt p = (fun i0 ->
      p { i0 with c=(update i0.c (nt,i0.s|>S.i,i0.s|>S.j)) })

  let (_:nonterm -> ('a,'b) parser_t -> ('a,'b) parser_t) = update_p

  let check nt p = (fun i0 ->
    let should_trim = contains i0.c (nt,i0.s|>S.i,i0.s|>S.j) in
    if should_trim && (i0.s|>S.len = 0) then
      []
    else if should_trim then
      (ignr_last (update_p nt p)) i0
    else
      (update_p nt p) i0)

  let (_:nonterm -> ('a,'b) parser_t -> ('a,'b) parser_t) = check

end


(* memoization support ---------------------------------------- *)

(* FIXME really this needs to have an gensymed int identifier for the
   string component *)
type hashkey = (Context.t * int * int)

let hashkey_of_input i0 = (i0.c,i0.s|>S.i,i0.s|>S.j)
  
(* generic memo function *)
let memo tbl key_of_input f i = (
  let k = key_of_input i in
  if (Hashtbl.mem tbl k) then (Hashtbl.find tbl k) else
    let v = f i in
    let _ = Hashtbl.add tbl k v in
    v)


(* convenience ---------------------------------------- *)

(* return those results for which entire input is parsed *)
let run_parser_string : (string,'b) parser_t -> string -> 'b list = (
  fun p s -> 
    s |> Substring.mk |> to_input |> p |> 
    List.filter (fun (_,s) -> s|>S.len=0) |> List.map fst)

