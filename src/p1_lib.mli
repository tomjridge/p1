(** Simple combinator parsing library that can handle all context-free grammars.

    This code is based on: Tom Ridge, Simple, functional, sound and
    complete parsing for all context-free grammars. In Jean Pierre
    Jouannaud and Zhong Shao, editors, Certified Programs and Proofs -
    First International Conference, CPP 2011, Kenting, Taiwan,
    December 7-9, 2011. Proceedings, volume 7086 of Lecture Notes in
    Computer Science, pages 103--118. Springer, 2011

    See the file {p1_examples.ml} for examples.
*)

type 'a substring = [ `SS of 'a * int * int ]
val mk_ss : string -> string substring

val content : string substring -> string

val ss_len : 'a substring -> int

(** Concatenate a list of adjacent substrings; assumes 'string components are equal and substrings are adjacent; returns none iff empty list *)
val ss_concat : 'a substring list -> 'a substring option

(*
val dest_substring : 'a substring -> string * int * int
val len : 'a substring -> int
*)

type nonterm = string

(*
type term = string
type symbol = [ `NT of nonterm | `TM of term ]

type rhs = symbol list
type parse_rule = nonterm * rhs
type grammar = parse_rule list
*)

(*
type lc_substring
val lc_substring_of : 'a substring -> lc_substring
*)

(*
type local_context
val empty_context : local_context
*)

type 'a ty_input
val toinput : 'a substring -> 'a ty_input
val substring_of_input : 'a ty_input -> 'a substring

type ('a, 'b) ty_parser = 'a ty_input -> ('b * 'a substring) list

val ( >> ) : ('a,'b) ty_parser -> ('b -> 'c) -> ('a,'c) ty_parser
val ( ||| ) : ('a,'b) ty_parser -> ('a,'b) ty_parser -> ('a,'b) ty_parser
val ( **> ) : ('a,'b) ty_parser -> ('a,'c) ty_parser -> ('a, 'b*'c) ty_parser

(*
val ignr_last : ('a,'b) ty_parser -> ('a,'b) ty_parser)
*)

(** Any nonterminal for which there is (direct or indirect)
    left-recursion must pass through check_and_upd_lctxt *)
val check_and_upd_lctxt : nonterm -> ('a,'b) ty_parser -> ('a,'b) ty_parser

(** Convenience function *)
val run_parser_string : (string,'b) ty_parser -> string -> ('b*string substring) list


type hashkey
val hashkey_of_input: 'a ty_input -> hashkey




val a : string -> (string, string substring) ty_parser
val until_a : string -> (string, string substring) ty_parser

(** Take a regexp (following Str) and parse that regexp at beginning
    of input *)
val parse_RE : string -> (string, string substring) ty_parser

(** Take a regexp, and parse until that regexp occurs in the input. If
    the regexp does not occur, consume all the input. *)
val parse_not_RE : string -> (string, string substring) ty_parser

val parse_EOF : ('a, 'a substring) ty_parser
val until_EOF : ('a, 'a substring) ty_parser

(** Parse an empty substring *)
val parse_eps : ('a,'a substring) ty_parser

val read_file_as_string: string -> string option
val write_string_to_file: string -> string -> bool

(** Second argument maps inputs to keys *)
val memo : ('k, 'v) Hashtbl.t -> ('i -> 'k) -> ('i -> 'v) -> 'i -> 'v
