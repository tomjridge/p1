(* Very simple combinator parser *)

(* backing is the backing string; length is String.length backing; sub
   returns the substring from i *)
type substring_t = < 
  backing:string; length: int; i:int; sub:string; with_i:int -> substring_t >

(* monad for results with remaining parts of input *)
type 'a m = ('a * substring_t) list

type 'a parser_t = substring_t -> 'a m


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


(* basic parsers ---------------------------------------- *)

module P = struct 

  (* literal *)
  let a s0 = Astring.String.(
      fun s -> 
        match (find_sub ~start:s#i ~sub:s0 s#backing) = Some(s#i) with
          false -> []
        | true -> [(s0,s#with_i (s#i + String.length s0))])

  (* consume chars until we get to s0; if we reach end, fail *)
  let until_a s0 = (
    fun s -> 
      match Astring.String.(find_sub ~start:s#i ~sub:s0 s#backing) with
        None -> []
      | Some(j) -> [(String.sub s#backing s#i (j-s#i), s#with_i j)]
  )

  let until_a_then_a s0 = (until_a s0 **> a s0) >> (fun (x,_) -> x)

  let eof = (fun s -> if (s#i = s#length) then [((),s)] else [])

  let until_eof s = [(s#sub, s#with_i s#length)]

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

  let until_re : Str.regexp -> string parser_t = (fun re0 ->
    lift (
      fun sub ->
        try (
          let k = Str.search_forward re0 sub 0 in
          Some(String.sub sub 0 k))
        with Not_found -> None))
  
end

let parse_string : 'a parser_t -> string -> 'a m = (fun p s -> p (
    object 
      method backing = s
      val i0 = 0
      method i = i0
      method length = String.length s
      method sub = String.sub s i0 (String.length s - i0)
      method with_i j = {< i0=j >}
    end))

