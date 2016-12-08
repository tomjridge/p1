(* Very simple combinator parser *)

module Pre_substring = struct

  (* string functions ---------------------------------------- *)

  let starts_with s s' = (
    let (l,l') = (String.length s, String.length s') in
    l'<=l && String.sub s 0 l' = s')

  (* FIXME inefficient *)
  let find_sub : string -> int -> string -> int option = (
    fun s0 i0 s ->
      let (l0,l) = (String.length s0, String.length s) in    
      let i = ref i0 in
      let r = ref None in
      let _ = 
        while (!r = None && !i + l <= l0) do 
          if (String.sub s0 !i l = s) then r:=Some(!i) else i:=!i+1
        done
      in
      !r)


  (* substrings ---------------------------------------- *)

  (* backing is the backing string; length is String.length backing; sub
     returns the substring from i *)
  type 'a substring_t = < 
    backing:'a; 
    length: int; 
    i:int; 
    j:int;
    sub:string; 
    with_i:int -> 'a substring_t;
    dec_j: 'a substring_t;
    inc_j: 'a substring_t
  >

  let mk s =     
    object (self)
      method backing = s
      val i0 = 0
      val j0 = String.length s
      method i = i0
      method j = j0
      method length = (j0 - i0)
      method sub = String.sub s i0 (j0 - i0)
      method with_i j = {< i0=j >}
      method dec_j = {< j0=j0-1 >}
      method inc_j = {< j0=j0+1 >}
    end

end


open Pre_substring

type s = string substring_t (* only use within this file *)

(* main types ---------------------------------------- *)


(* monad for results with remaining parts of input *)
type 'a m = ('a * s) list

type 'a parser_t = s -> 'a m


(* combinators ---------------------------------------- *)

let ( **> ) : 'a parser_t -> 'b parser_t -> ('a * 'b) parser_t = (fun p1 p2 ->
  fun s -> 
    s|>p1|>
    List.map (
      function (v1,s) -> 
        s|>p2|>List.map (function (v2,s) -> ((v1,v2),s))) |>
    List.concat)

let ( ||| ) : 'a parser_t -> 'a parser_t -> 'a parser_t = (fun p1 p2 ->
    fun s -> (p1 s)@(p2 s))

let ( >> ) : 'a parser_t -> ('a -> 'b) -> 'b parser_t = (fun p f s -> 
    p s|>List.map (fun (x,y) -> (f x,y)))


(* more "combinators" ---------------------------------------- *)

(* needs at least one item *)
let rec sep_by : 'a parser_t -> 'b parser_t -> 'a list parser_t = (fun item sep s ->
    ((item >> (fun x -> [x])) ||| 
    (item **> sep **> (sep_by item sep) >> (fun (x,(y,z)) -> x::z))) s
  )

let longest : 'a parser_t -> 'a parser_t = (fun p ->
  fun s -> (
    p s |> (
      fun rs ->
        match rs with
          [] -> []
        | r::rs' -> 
          [List.fold_left (fun a b -> if (snd a)#i > (snd b)#i then a else b) r rs']
    )))


(* running the parse ---------------------------------------- *)

(* FIXME rename run_parse_string? and return only those matching whole stirng? *)
let parse_string : 'a parser_t -> string -> 'a m = (fun p s -> s|>mk|>p)

(* return 'a such that the whole input was parsed *)
let parse_results : 'a parser_t -> string -> 'a list = (fun p s ->
    parse_string p s |> List.filter (fun (_,s) -> String.length s#sub=0) |> List.map fst)
    


(* exporting substring ---------------------------------------- *)

module Substring = struct

  include Pre_substring

  let eps s = [("",s)]

  (* literal *)
  let a s0 = (
      fun s -> 
        match (starts_with s#sub s0) with
          false -> []
        | true -> [(s0,s#with_i (s#i + String.length s0))])

  (* consume chars until we get to s0; if we reach end, fail *)
  let upto_a s0 = (
    fun s -> 
      match (find_sub s#backing s#i s0) with
        None -> []
      | Some(j) -> [(String.sub s#backing s#i (j-s#i), s#with_i j)]
  )

  let to_a s0 = (upto_a s0 **> a s0) >> (fun (x,_) -> x)

  let eof = (fun s -> if (s#i = s#length) then [((),s)] else [])

  let upto_eof s = [(s#sub, s#with_i s#length)]

  (* f is a fun that takes a string, and returns an option;
     if the option is Some(x), then f should ensure s#sub starts with x *)
  let lift f = (
    fun s ->
      match f s#sub with
        None -> []
      | Some(x) -> [(x,s#with_i (s#i + String.length x))])
      
  (* parse a re at start of substring *)
  let re : Str.regexp -> string parser_t = (fun re0 ->
    lift (
      fun sub -> 
        if (Str.string_match re0 sub 0) then 
          Some(String.sub sub 0 (Str.match_end ()))
      else
        None
    ))

  let upto_re : Str.regexp -> string parser_t = (fun re0 ->
    lift (
      fun sub ->
        try (
          let k = Str.search_forward re0 sub 0 in
          Some(String.sub sub 0 k))
        with Not_found -> None))

end
