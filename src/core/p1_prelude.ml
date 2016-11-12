(* disable debugging *)
let debugging = ref false
let debug_endline s = (if !debugging then (print_endline s) else ())

(* careful - this is taken from hol_light_lib.ml and maybe overwritten (with different mutable state) when including hol_light_lib.ml *)

let report_timing = ref false;;

let report s =
  Format.print_string s; Format.print_newline();;

(* removed so that we can cut and paste p3_lib.ml into http://try.ocamlpro.com/
  let time f x =
    if not (!report_timing) then f x else
    let start_time = Sys.time() in
    try let result = f x in
        let finish_time = Sys.time() in
        report("CPU time (user): "^(string_of_float(finish_time -. start_time)));
        result
    with e ->
        let finish_time = Sys.time() in
        Format.print_string("Failed after (user) CPU time of "^
                            (string_of_float(finish_time -. start_time))^": ");
        raise e;;
*)

module type MYSET = sig
  type elt
  type t
  val add : elt -> t -> t
  val choose : t -> elt
  val diff : t -> t -> t
  val elements : t -> elt list
  val empty : t
  val filter : (elt -> bool) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val from_list : elt list -> t
  val is_empty : t -> bool
  val list_union : elt list -> t -> t
  val map : (elt -> elt) -> t -> t
  val maximal_less_than : elt -> t -> elt option
  val mem : elt -> t -> bool
  val remove : elt -> t -> t
  val split : elt -> t -> t * bool * t
  val union : t -> t -> t
end

module MySet_Make = functor (Ord:Set.OrderedType) -> (struct
  include Set.Make(Ord)
  let maximal_less_than e s = (
    let (smaller,_,larger) = split e s in
    if (is_empty smaller) then None else (Some(max_elt smaller)))
  let rec itlist f l b =
    match l with
      [] -> b
    | (h::t) -> f h (itlist f t b)
  let list_union xs s =
    itlist (fun x -> fun s -> (*Set_earley_item.*)add x s) xs s
  let map f s =
    let f1 x s = (*Set_earley_item.*)add (f x) s in
    (*Set_earley_item.*)fold f1 s (*Set_earley_item.*)empty
  let from_list elts =
    let f1 elt s = add elt s in
    itlist f1 elts empty
end : MYSET with type elt = Ord.t)

(* we want to override the create function to provide a global_clear method *)
module MyHashtbl = struct
  include Hashtbl
  let reset_funs = ref []
  let original_create = create
  let create n = (
    let tbl = original_create n in
    let _ = (reset_funs := (fun () -> Hashtbl.clear tbl)::(!reset_funs)) in (* NB not concurrent safe; prefer reset to clear *)
    tbl)
  let global_reset () = (ignore (List.map (fun f -> f ()) (!reset_funs)); ())
end

(* basic library functions *)

type ('a,'b) sum = Inl of 'a | Inr of 'b

let dest_Inl x = (match x with | Inl x -> x | _ -> failwith "dest_Inl")
let dest_Inr x = (match x with | Inr x -> x | _ -> failwith "dest_Inr")

(* FIXME change names of predefined combinators to reflect use of not_epsilon (i.e. default is epsilon) *)

let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b);;

let rec mem x lis =
  match lis with
    [] -> false
  | (h::t) -> Pervasives.compare x h = 0 || mem x t;;

let insert x l =
  if mem x l then l else x::l;;

let union l1 l2 = itlist insert l1 l2;;

let unions l = itlist union l [];;


let ($) f g x = f(g x)

(*
let read_file_as_string fn =
  let f = open_in fn in
  let s = ref "" in
  let _ = try (while(true) do s := (!s) ^ (input_line f) ^ "\n" done) with _ -> () in
  let _ = close_in f in
  !s
*)

let lines fname =
  let lines = ref [] in
  let chan = if fname="-" then Pervasives.stdin else open_in fname in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines

let read_file_as_string fn =
  let ls = lines fn in
  ((String.concat "\n" ls)^"\n")


(* our contexts are sorted; we need insertion into a sorted list; we expect no duplicates  *)
let rec myinsert cmp elt lst = match lst with
  [] -> [elt]
| head :: tail -> let r = cmp elt head in if r < 0  then elt :: lst else (
  if r = 0 then failwith "myinsert" else head :: myinsert cmp elt tail)

(* get a list with no duplicates; inefficient? FIXME do we mean List.memq? *)
let unique_f res e = if List.mem e res then res else e::res

(* this is insertion sort; alternatives? *)
let unique = fun e -> List.fold_left unique_f [] e

let is_Some x = x <> None

let dest_Some x = match x with Some y -> y | _ -> failwith "dest_Some"

let rec allpairs f l1 l2 =
  match l1 with
   h1::t1 ->  itlist (fun x a -> f h1 x :: a) l2 (allpairs f t1 l2)
   | [] -> []
