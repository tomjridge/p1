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
