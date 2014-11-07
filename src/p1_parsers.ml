open P1_core

(** Parse a literal *)
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


let parse_EOF : (string, string substring) ty_parser = (fun i0 ->
    i0 |> substring_of_input |> (
      function (`SS(s,i,j)) -> 
        if i=j then [`SS(s,i,j),`SS(s,i,j)] (* FIXME note that this assumes j is the end of input *) 
        else []))

let parse_eps : ('a,'a substring) ty_parser = (fun i0 ->
    i0 |> substring_of_input |> (function (`SS(s,i,j)) -> [`SS(s,i,i),`SS(s,i,j)]))


let parse_RE : string -> (string, string substring) ty_parser = (fun re -> 
  let re = Str.regexp re in
  let f2 = fun (`SS(s,i,j)) -> (
    let b = Str.string_match re s i in
    if b then
      let e = Str.match_end () in
      if e<=j then 
        [(`SS(s,i,e)),`SS(s,e,j)]
      else
        []
    else
      [])
  in
  fun i0 -> i0 |> substring_of_input |> f2)

(*
let _ = "abc" |> mk_ss |> toinput |> (parse_RE "a")
let _ = "abc" |> mk_ss |> toinput |> (parse_RE "b")
let _ = assert ("aabc" |> mk_ss |> toinput |> (parse_RE "a*") = [(`SS("aabc",0,2), `SS ("aabc", 2, 4))])
*)

let parse_not_RE : string -> (string, string substring) ty_parser = (fun re -> 
  let re = Str.regexp re in
  let f2 = fun (`SS(s,i,j)) -> (
    try
      let k = Str.search_forward re s i in
      if k<=j then [`SS(s,i,k),`SS(s,k,j)] else []
    with Not_found -> [`SS(s,i,j),`SS(s,j,j)])  (* read till end of string *)
  in
  fun i0 -> i0 |> substring_of_input |> f2)

(*
let _ = "abc" |> mk_ss |> toinput |> (parse_not_RE "b")
let _ = "abc" |> mk_ss |> toinput |> (parse_not_RE "x")
let _ = assert ("aabc" |> mk_ss |> toinput |> (parse_not_RE "b+") =
  [(`SS ("aabc", 0, 2), `SS ("aabc", 2, 4))])
*)

