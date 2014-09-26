type 'a substring = [ `SS of string * int * int ]
val mk_ss : string -> string substring
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


val a : string -> (string, string substring) ty_parser
val until_a : string -> (string, string substring) ty_parser


val read_file_as_string: string -> string option
val write_string_to_file: string -> string -> bool
