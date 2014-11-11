(** Generate code given a concrete representation of the grammar *)

(* the grammar file has the format:

ocaml defns

<<p3<<
grammar defn
>>p3>>

ocaml defns

*)

open P1_lib
open P1_extra_combinators
open P1_cl
open P1_parse_grammar

let parse_file = 
  (((until_a "<<p3<<") **> (a "<<p3<<") **> (until_a ">>p3>>") **> (a ">>p3>>") **> parse_RE ".*")
   >> (fun (pre,(_,(mid,(_,post)))) -> (content pre,content mid,content post)))

(* command line args *)
type ty_cl_args = { grammar: string;  }
let cl0 = { grammar="";  }

(* precedence to earlier args *)
let rec parse_CL = fun i -> (
  let f1 cl (f,xs) = (match (f,xs) with
    | (x,[]) -> {cl with grammar=x }
    | _ -> (failwith ("parse_CL: unrecognized flag/arg combination: "^f^" "^(String.concat " " xs))))
  in
  let sep = a "\x00" in
  (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> List.fold_left f1 cl0 xs))) i

let str_of_GRAMMAR g = ""

let str_of_SYM sym = ""

(* FIXME following should have same checks on input wellformedness as other mains *)
let main () =
  let command = "p1_gen" in
  let ws = (parse_RE "[ \n]*") in
  let args = get_args parse_CL Sys.argv in
  let _ = (if (args.grammar="") then
      (failwith (command^": require a grammar file as argument"))
    else
      ())
  in
  let txt = 
    match read_file_as_string args.grammar with 
    | Some x -> x 
    | None -> failwith (command^": couldn't read file: "^args.grammar) 
  in
  let rs = txt |> mk_ss |> toinput |> parse_file in
  let (pre,mid,post) = (match rs with
    | [(pre,mid,post),_] -> (pre,mid,post)
    | [] -> (failwith (command^": failed to parse grammar file: "^args.grammar))
    | _ -> (failwith (command^": failed to parse grammar file unambiguously: "^args.grammar)))
  in
  let rs : (grammar_with_actions * string substring) list = 
    mid |> mk_ss |> toinput |> 
    ((ws **> parse_GRAMMAR_WITH_ACTIONS **> ws) >> (fun (_,(g,_)) -> g)) 
  in
  let ((header,g),_) = (match rs with 
    | [x] -> x | _ -> failwith (command^": failed to parse p3 component of grammar file: "^mid)) 
  in
  let (start_sym0,_1,_2) = (List.hd g) in
  let start_sym = `NT start_sym0 in
  let nts = nts_of_grammar (drop_actions g) in 
  let s = str_of_GRAMMAR g in
  let s = s ^ "\n" in 
  let _ = print_string (
    pre^"\n"^
      header^"\n"^
      s^"\n"^
      "let parse_start = "^(str_of_SYM start_sym)^"\n"^
      post)
  in
  ()

let _ = main ()
