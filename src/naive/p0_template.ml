(* Simple template text extraction *)

(* a template is a match: a function to an substitution/map option;
   here we use singleton lists/empty list instead of option *)

(* vars, substitutions ---------------------------------------- *)

type var_t = string

module Subst = struct

  include Map.Make(struct type t = var_t let compare = Pervasives.compare end)

end

type 'a subst = 'a Subst.t

let find = Subst.find

(* generic json-like datastructure for results -------------------- *)

type res = 
    S of string 
  | L of res list
  | M of res subst

type templater_t = res P0.parser_t

module T = struct
  
  open P0
  open Substring

  let of_s : string -> templater_t = (fun s -> a s >> (fun x -> S x))

  let rec of_l : templater_t list -> templater_t = (
    function
      [] -> failwith "of_l"
    | [t] -> t >> (fun x -> L [x])
    | t::ts -> (t **> (of_l ts)) >> (function (r,L rs) -> L(r::rs)))
  
  let rec of_strings : string list -> string subst parser_t = (fun xs ->
      let module S = Subst in
      let prime s = s^"'" in
      match xs with
        [] -> (failwith "")
      | [s] -> (
          (to_a s **> (a s)) >> (fun (x,y) -> (S.empty |> S.add s x)))
      | s1::s2::xs -> (
          (a s1 **> to_a s2 **> of_strings (s2::xs)) >> (
            fun (x,(y,m1)) -> (m1 |> S.add s1 y)))
    )

end


