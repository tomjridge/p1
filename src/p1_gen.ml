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
  ws **> parse_GRAMMAR_WITH_ACTIONS **> ws >> (fun (_,((h,g),_)) -> assert(h=""); g)



(* so we need to map a nt E to a string parse_E *)
let nonterm_to_parse_fun_name = fun nt -> "parse_"^nt

(* assume terms are like "1" or ?xyz? for now *)
let term_to_ocaml = (fun x0 -> 
    let l0 = String.length x0 in
    match l0 <= 1 with
    | true -> (failwith "term_to_ocaml: t0x")
    | false -> (
        match x0.[0],x0.[l0-1] with
        | '"','"' -> ("(a "^x0^")")
        | '?','?' -> ("("^(String.sub x0 1 (l0-2))^")")
        | _ -> (failwith "term_to_ocaml: fjc")))        

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
   (nonterm_to_parse_fun_name nt)^" = 
  let tbl = Hashtbl.create 100 in
  (fun i0 -> (
  check_and_upd_lctxt \""^nt^"\" (*vnl*)(
    memo tbl hashkey_of_input (
    "^
   (xs |> List.map rhs_to_ocaml |> String.concat "\n    ||| ")^" ))(*vnl*) i0))"
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

let rec parse_E = (fun i0 -> (
  check_and_upd_lctxt "E" (*vnl*)(
    ((parse_E**>parse_E**>parse_E) >> ( fun (x,(y,z)) -> x+y+z ))
    ||| (((a "1")) >> ( fun _ -> 1 ))
    ||| (((a "")) >> ( fun _ -> 0 ))
    ||| ((parse_F) >> ( fun x -> x )) )(*vnl*) i0))
  and parse_F = (fun i0 -> (
  check_and_upd_lctxt "F" (*vnl*)(
    ((parse_E) >> ( fun x -> x )) )(*vnl*) i0))

*)

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

type 'a or_error = OK of 'a | Error of string

let ( |>> ) x f = (match x with
    | OK x -> f x
    | Error y -> Error y)

(* FIXME following should have same checks on input wellformedness as other mains *)
let main () =
  let command = "p1_gen" in
  (try OK (get_args parse_CL Sys.argv) with Failure s -> Error s)
  |>> (fun args -> 
      if (args.grammar="") then
        Error (command^": require a grammar file as argument")
      else
        OK args)
  |>> (fun args -> 
      match read_file_as_string args.grammar with 
      | Some x -> OK (x,args) 
      | None -> Error (command^": couldn't read file: "^args.grammar))
  |>> (fun (txt,args) ->
      let rs = txt |> mk_ss |> toinput |> parse_file in
      match rs with
      | [(pre,mid,post),_] -> OK(pre,mid,post)
      | [] -> (Error (command^": failed to parse grammar file: "^args.grammar))
      | _ -> (Error (command^": failed to parse grammar file unambiguously: "^args.grammar)))
  |>> (fun (pre,mid,post) -> 
      let rs : ((nonterm * symbol list * string) list * string substring) list = 
        mid |> mk_ss |> toinput |> parse_embedded_grammar
      in
      OK(pre,mid,rs,post))
  |>> (fun (pre,mid,rs,post) -> 
      match rs with 
      | [(g,_)] -> OK(pre,g,post) 
      | [] -> Error (command^": failed to parse <<g<<...>>g>> component of grammar file: "^mid)
      | _ -> Error (command^": failed to parse <<g<<...>>g>> component of grammar file unambiguously: "^mid))
  |>> (fun (pre,g,post) -> 
      print_string (
        String.concat "\n" [
          pre;
          g |> grammar_to_ocaml;
          post]);
      OK ())
  |> (function
      | Error s -> failwith s
      | OK () -> ())

let _ = main ()
