(* header *)
open P1_lib

let eof = parse_EOF
let ws = parse_RE "[ \n]*"

<<g<<
S -> E ?ws? ?eof?  {{ fun (x,_) -> x |> string_of_int |> print_endline }}

E -> E E E   {{ fun (x,(y,z)) -> x+y+z }}
  | "1"      {{ fun _ -> 1 }}
  | ""       {{ fun _ -> 0 }}

>>g>>

(* footer *)
let main () = 
  let fname = Sys.argv.(1) in
  let Some txt = read_file_as_string fname in
  let _ = run_parser_string parse_S txt in
  ()

let _ = main ()

