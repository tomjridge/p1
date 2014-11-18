(** Extra definitions that are occasionally useful but shouldn't be in core *)

open P1_core

let ss_to_string = function (`SS(s,i,j)) -> (
    String.concat "" [
      "(";
      string_of_int i;
      ",";
      string_of_int j;
      ")" ])
