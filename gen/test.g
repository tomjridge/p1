(* header *)
open P1_lib

<<g<<
S -> E       {{ fun x -> x |> string_of_int |> print_endline }}

E -> E E E   {{ fun (x,(y,z)) -> x+y+z }}
  | "1"      {{ fun _ -> 1 }}
  | ""       {{ fun _ -> 0 }}
  | F        {{ fun x -> x }}

F -> E       {{ fun x -> x }}
>>g>>

(* footer *)
let main () = 
  let fname = Sys.argv.(1) in
  let Some txt = read_file_as_string fname in
  let _ = txt |> mk_ss |> toinput |> parse_start in
  ()

let _ = main ()

