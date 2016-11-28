(** Simple combinator parsing library that can handle all context-free grammars.

    This code is based on: Tom Ridge, Simple, functional, sound and
    complete parsing for all context-free grammars. In Jean Pierre
    Jouannaud and Zhong Shao, editors, Certified Programs and Proofs -
    First International Conference, CPP 2011, Kenting, Taiwan,
    December 7-9, 2011. Proceedings, volume 7086 of Lecture Notes in
    Computer Science, pages 103--118. Springer, 2011

    See the file {p1_examples.ml} for examples.
*)

type 'a substring 

type nonterm = string

type 'a input

type ('a,'b) result

type ('a, 'b) parser_t = 'a input -> ('a,'b) result

val ( >> ) : ('a,'b) parser_t -> ('b -> 'c) -> ('a,'c) parser_t
val ( ||| ) : ('a,'b) parser_t -> ('a,'b) parser_t -> ('a,'b) parser_t
val ( **> ) : ('a,'b) parser_t -> ('a,'c) parser_t -> ('a, 'b*'c) parser_t


(** Any nonterminal for which there is (direct or indirect)
    left-recursion must pass through check_and_upd_lctxt *)
val check : nonterm -> ('a,'b) parser_t -> ('a,'b) parser_t

(** Convenience function, return results for which whole of the input was consumed *)
val run_parser_string : (string,'b) parser_t -> string -> 'b list


(*
module Basic_parsers : sig

  type p = (string, string) parser_t

  val a : string -> p
  val upto_a: string -> p
  val to_a : string -> p

  val eof: (string,unit) parser_t
  val upto_eof: p

  (** Take a regexp (following Str) and parse that regexp at beginning
      of input *)
  val re : Str.regexp -> p
  val upto_re: Str.regexp -> p

end
*)
 
val read_file_as_string: string -> string option
val write_string_to_file: string -> string -> bool

(** Second argument maps inputs to keys *)
val memo : ('k, 'v) Hashtbl.t -> ('i -> 'k) -> ('i -> 'v) -> 'i -> 'v

type hashkey
val hashkey_of_input: 'a input -> hashkey
