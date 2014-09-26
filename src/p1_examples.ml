open P1_lib

let set_equal xs ys = (
  let subset xs ys = List.for_all (fun x -> List.mem x ys) xs in
  subset xs ys && subset ys xs)

let rec parse_E = (fun i -> 
  check_and_upd_lctxt "E" (
    ((parse_E **> parse_E **> parse_E) >> (fun (x,(y,z)) -> x+y+z))
    ||| ((a "1") >> (fun _ -> 1))
    ||| ((a "") >> (fun _ -> 0))) i)

let _ = "111" |> mk_ss |> toinput |> parse_E 

let _ = assert (
  let result = "111" |> mk_ss |> toinput |> parse_E in
  let expected = [
    (0, `SS ("111", 0, 3)); 
    (1, `SS ("111", 1, 3)); 
    (2, `SS ("111", 2, 3));
    (3, `SS ("111", 3, 3));]
  in
  set_equal result expected)

let _ = "1111111" |> mk_ss |> toinput |> parse_E 


(**********************************************************************)
(* with memo *)

(* generic memo function *)
let memo tbl key_of_input f i = (
  let k = key_of_input i in
  if (Hashtbl.mem tbl k) then (Hashtbl.find tbl k) else
    let v = f i in
    let _ = Hashtbl.add tbl k v in
    v)

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
      
let _ = "111" |> mk_ss |> toinput |> parse_E ()

(* we can now handle much longer inputs with relatively little
   slowdown; the following takes less than 1s compiled *)
let _ = "1111111111111111111111111111111111111111" |> mk_ss |> toinput |> parse_E ()
