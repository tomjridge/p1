(** Parse a concrete representation of a grammar with actions *)

open P1_lib

type nonterm = string
type symbol = [ `NT of string | `TM of string ]

type grammar = (nonterm * symbol list) list

(** The first component is the header; the second is a list of rules,
    with actions *)
type grammar_with_actions = 
  string (* header *)
  * (nonterm * symbol list * string) list (* rule with action *)

val parse_GRAMMAR : (string,grammar) ty_parser

val parse_GRAMMAR_WITH_ACTIONS : (string,grammar_with_actions) ty_parser


(** Get grammar from file; may throw exception *)
val get_grammar: string -> grammar 

(** Get grammar with actions from file; may throw exception *)
val get_grammar_with_actions: string -> grammar_with_actions
