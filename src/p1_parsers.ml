open P1_core

(* Parse a literal *)
let a : string -> (string,string substring) ty_parser = (
  fun lit i0 -> 
    let (`SS(s,i,j)) = substring_of_input i0 in
    let len = j-i in
    let n = String.length lit in
    if
      (n <= len)
      && (String.sub s i n = lit)
    then
      [(`SS(s,i,i+n),`SS(s,i+n,j))]
    else
      [])

(** Parse chars until a literal, or the end of the string *)
let until_a : string -> (string,string substring) ty_parser = (
  fun lit i0 -> 
    let (`SS(s,i,j)) = substring_of_input i0 in
    let len = j-i in
    let llit = String.length lit in
    let rec f1 n =
      if
        n+llit <= len
        && (String.sub s (i+n) llit) = lit
      then
        [(`SS(s,i,i+n),`SS(s,i+n,j))]
      else if
          n+llit <= len 
      then
        f1 (n+1)
      else
        [(`SS(s,i,j),`SS(s,j,j))]
    in
    f1 0)


