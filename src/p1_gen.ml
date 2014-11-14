(** Generate code given a concrete representation of the grammar *)

(* the grammar file has the format:

ocaml defns

<<g<<
grammar defn
>>g>>

ocaml defns

*)

open P1_lib
open P1_extra_combinators
open P1_cl
open P1_parse_grammar

let parse_file = 
  (((until_a "<<g<<") **> (a "<<g<<") **> (until_a ">>g>>") **> (a ">>g>>") **> until_EOF)
   >> (fun (pre,(_,(mid,(_,post)))) -> (content pre,content mid,content post)))

let ws = (parse_RE "[ \n]*")

(* throw away the header *)
let parse_embedded_grammar : (string,(nonterm * symbol list * string) list) ty_parser = 
  ws **> parse_GRAMMAR_WITH_ACTIONS **> ws >> (fun (_,(x,_)) -> snd x)



(* so we need to map a nt E to a string parse_E *)
let nonterm_to_parse_fun_name = fun nt -> "parse_"^nt

(* assume terms are like "1" for now *)
let term_to_ocaml = fun x -> "(a "^x^")"

let rhs_symbol_to_ocaml sym = (
   match sym with 
  | `TM x -> (term_to_ocaml x)
  | `NT x -> (nonterm_to_parse_fun_name x))

let rhs_to_ocaml = (fun (syms,act) ->
   String.concat "" [
     "(";
     String.concat " >> "
     ["(" ^ (syms |> List.map rhs_symbol_to_ocaml |> String.concat "**>") ^ ")";
       "(" ^ act ^ ")"];
     ")"])

let rules_for_nt_to_ocaml = (fun (nt,xs) -> 
   (* let rec *)
   (nonterm_to_parse_fun_name nt)^" = (fun i0 -> (
  check_and_upd_lctxt \""^nt^"\" (*vnl*)(
    "^
   (xs |> List.map rhs_to_ocaml |> String.concat "\n    ||| ")^" )(*vnl*) i0))"
)

let rhs_for_nt nt g0 = (nt,g0 |> List.filter (fun (x,_,_) -> x=nt) |> List.map (fun (_,x,y) -> (x,y)))




(*
let Some txt = read_file_as_string "../resources/example_embedded_grammar_with_acts"

let [((s1,s2,s3),_)] = txt |> mk_ss |> toinput |> parse_file

let [(g0,_)] = s2 |> mk_ss |> toinput |> parse_embedded_grammar 

let xs0 = rhs_for_nt "E" g0

let _ = rules_for_nt_to_ocaml xs0 |> print_endline
*)

let grammar_to_ocaml = (fun g0 ->
   let nts = g0 |> drop_actions |> nts_of_grammar in
   String.concat "" [
     "let rec ";
     String.concat "\n  and " (
       nts |> List.map (fun nt -> rhs_for_nt nt g0) |> List.map rules_for_nt_to_ocaml)])

(*
let _ = grammar_to_ocaml g0 |> print_endline
*)

let rec parse_E = (fun i0 -> (
  check_and_upd_lctxt "E" (*vnl*)(
    ((parse_E**>parse_E**>parse_E) >> ( fun (x,(y,z)) -> x+y+z ))
    ||| (((a "1")) >> ( fun _ -> 1 ))
    ||| (((a "")) >> ( fun _ -> 0 ))
    ||| ((parse_F) >> ( fun x -> x )) )(*vnl*) i0))
  and parse_F = (fun i0 -> (
  check_and_upd_lctxt "F" (*vnl*)(
    ((parse_E) >> ( fun x -> x )) )(*vnl*) i0))


(* command line args *)
type ty_cl_args = { grammar: string;  }
let cl0 = { grammar="";  }

(* precedence to earlier args *)
let rec parse_CL = fun i -> (
  let f1 cl (f,xs) = (match (f,xs) with
    | ("-g",[x]) -> {cl with grammar=x }
    | _ -> (failwith ("parse_CL: unrecognized flag/arg combination: "^f^" "^(String.concat " " xs))))
  in
  let sep = a "\x00" in
  (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> List.fold_left f1 cl0 xs))) i

(* FIXME following should have same checks on input wellformedness as other mains *)
let main () =
  let command = "p1_gen" in
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
    | [x] -> x | _ -> failwith (command^": failed to parse <<g<<...>>g>> component of grammar file: "^mid)) 
  in
  let (start_sym0,_1,_2) = (List.hd g) in
  let start_sym = `NT start_sym0 in
  let nts = nts_of_grammar (drop_actions g) in 
  let s = g |> grammar_to_ocaml in
  let s = s ^ "\n" in 
  let _ = print_string (
      String.concat "\n" [
        pre;
        header;
        s;
        "let parse_start = "^(nonterm_to_parse_fun_name start_sym0);
        post])
  in
  ()

let _ = main ()
