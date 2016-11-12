open P1_lib

(* to get a visual indication of runtime *)
let start_stop s f = 
  let t1 = Sys.time () in
  let _ = print_string ("Start "^s^" ...") in
  let _ = f () in
  let t2 = Sys.time () in
  let _ = print_endline ("...stop in "^(string_of_float (t2 -. t1))^" seconds") in
  ()
  

let set_equal xs ys = (
  let subset xs ys = List.for_all (fun x -> List.mem x ys) xs in
  subset xs ys && subset ys xs)

(* a simple terminal parser *)

let a1 : (string,int) ty_parser = (fun i0 ->
    let `SS(s,i,j) = substring_of_input i0 in
    if i < j && s.[i] = '1' then 
      [(1,`SS(s,i+1,j))]
    else
      [])


(**********************************************************************)
(* example grammar: E -> E E E | "1" | eps *)

let rec parse_E = (fun i -> 
  check_and_upd_lctxt "E" (
    ((parse_E **> parse_E **> parse_E) >> (fun (x,(y,z)) -> x+y+z))
    ||| ((a "1") >> (fun _ -> 1))
    ||| ((a "") >> (fun _ -> 0))) i)

let f () = "111" |> mk_ss |> toinput |> parse_E 
let _ = start_stop "example muv" f

let _ = assert (
  let result = "111" |> mk_ss |> toinput |> parse_E in
  let expected = [
    (0, `SS ("111", 0, 3)); 
    (1, `SS ("111", 1, 3)); 
    (2, `SS ("111", 2, 3));
    (3, `SS ("111", 3, 3));]
  in
  set_equal result expected)

let f () = "1111111" |> mk_ss |> toinput |> parse_E 
let _ = start_stop "example b1q" f


(**********************************************************************)
(* with memo *)


(* we want to create a new hashtable for each new string that we
   parse, hence unit argument *)
let parse_E () =
  let tbl = Hashtbl.create 100 in
  let rec parse_E = 
    (fun i -> 
      check_and_upd_lctxt "E" (
        let p = 
          ((parse_E **> parse_E **> parse_E) >> (fun (x,(y,z)) -> x+y+z))
          ||| ((a "1") >> (fun _ -> 1))
            ||| ((a "") >> (fun _ -> 0))
        in
        memo tbl hashkey_of_input p) i)
  in
  parse_E
      
let f () = "111" |> mk_ss |> toinput |> parse_E ()
let _ = start_stop "example 63i" f

(* we can now handle much longer inputs with relatively little
   slowdown; the following takes less than 1s compiled *)
let f () = "1111111111111111111111111111111111111111" |> mk_ss |> toinput |> parse_E ()
let _ = start_stop "example 6my" f


(**********************************************************************)
(* with memo and dummy actions, i.e. just parsing *)

(* we want to create a new hashtable for each new string that we
   parse, hence unit argument *)
let parse_E () =
  let tbl = Hashtbl.create 100 in
  let rec parse_E = 
    (fun i -> 
      check_and_upd_lctxt "E" (
        let p = 
          ((parse_E **> parse_E **> parse_E) >> (fun _ -> ()))
          ||| ((a "1") >> (fun _ -> ()))
          ||| ((a "") >> (fun _ -> ()))
        in
        memo tbl hashkey_of_input p) i)
  in
  parse_E
      
let f () = "111" |> mk_ss |> toinput |> parse_E ()
let _ = start_stop "example 1cq" f

(* we can now handle much longer inputs with relatively little
   slowdown; the following takes a small fraction of a second *)
let f () = (String.make 20 '1') |> mk_ss |> toinput |> parse_E () |> (fun _ -> ())
let _ = start_stop "example yn7" f

let f () = (String.make 40 '1') |> mk_ss |> toinput |> parse_E ()
let _ = start_stop "example rxi" f

let f () = (String.make 60 '1') |> mk_ss |> toinput |> parse_E ()
let _ = start_stop "example vgx" f




(* Sample output from ./p1_examples.native:

Start example muv ......stop in 0.000389 seconds
Start example b1q ......stop in 0.171137 seconds
Start example 63i ......stop in 3.5e-05 seconds
Start example 6my ......stop in 0.648735 seconds
Start example 1cq ......stop in 3.4e-05 seconds
Start example yn7 ......stop in 0.031351 seconds
Start example rxi ......stop in 1.058452 seconds
Start example vgx ......stop in 10.121685 seconds

*)
