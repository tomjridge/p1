(* header *)
open P1_lib



let rec parse_E = (fun i0 -> (
  check_and_upd_lctxt "E" (*vnl*)(
    ((parse_E**>parse_E**>parse_E) >> ( fun (x,(y,z)) -> x+y+z ))
    ||| (((a "1")) >> ( fun _ -> 1 ))
    ||| (((a "")) >> ( fun _ -> 0 ))
    ||| ((parse_F) >> ( fun x -> x )) )(*vnl*) i0))
  and parse_F = (fun i0 -> (
  check_and_upd_lctxt "F" (*vnl*)(
    ((parse_E) >> ( fun x -> x )) )(*vnl*) i0))
  and parse_S = (fun i0 -> (
  check_and_upd_lctxt "S" (*vnl*)(
    ((parse_E) >> ( fun x -> x |> string_of_int |> print_endline )) )(*vnl*) i0))

let parse_start = parse_S


(* footer *)
let main () = 
  let fname = Sys.argv.(1) in
  let Some txt = read_file_as_string fname in
  let _ = txt |> mk_ss |> toinput |> parse_start in
  ()

let _ = main ()

