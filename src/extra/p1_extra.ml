(** Extra definitions that are occasionally useful but shouldn't be in core *)

open P1_core

type 'a parser_t = (string,'a) P1_core.parser_t

(* FIXME
let p0_to_p1 : 'a P0.parser_t -> 'a parser_t = (
  fun p0 -> 
    fun i0 ->
      p0 i0.s
)

module Basic_parsers = struct

  open P0.Substring

  let a1 = a1|>p0_to_p1

  let eps = eps|>p0_to_p1

  let a s = ((a s)|>p0_to_p1)

  let upto_a s = ((upto_a s)|>p0_to_p1)

  let to_a : string -> string parser_t = (
    fun s -> 
      (to_a s) |> p0_to_p1)

  let eof = eof|>p0_to_p1

  let upto_eof = upto_eof|>p0_to_p1

  let re x = (re x)|>p0_to_p1

  let upto_re x = (upto_re x)|>p0_to_p1

end

include Basic_parsers


module Combinators = struct

  (* needs at least one *)
  let rec sep_by p sep i = (
    (
      (p >> (fun x -> [x])) 
      ||| ((p **> sep **> sep_by p sep) >> (fun (x,(_,xs)) -> x::xs))
    ) i
  )

end



*)
