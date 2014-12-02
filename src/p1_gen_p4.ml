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
open P1_gen_shared


let rhs_symbol_to_ocaml sym = (
   match sym with 
  | `TM x -> (term_to_ocaml x)
  | `NT x -> ("(!_"^x^")"))

let concat syms = 
  syms |> List.map rhs_symbol_to_ocaml 
  |> (fun xs -> (match xs with
      | [x] -> "(rhs "^x^")"
      | (x::xs) -> x^" >-- "^(String.concat " >- " xs)))

let rhs_to_ocaml = (fun (syms,act) ->
   String.concat "" [
     "(";
     String.concat ""
       ["(" ^ (syms |> concat) ^ ")";
        " >> ";
        "(" ^ act ^ ")"];
     ")"])

let rules_for_nt_to_ocaml = (fun (nt,xs) -> 
    let _nt = "_"^nt in
String.concat ";\n" [
_nt^" := mkntparser_lazy (!"^_nt^") (lazy(alts[
  "^(xs |> List.map rhs_to_ocaml |> String.concat ";\n    ")^" ]))";
"  "^_nt^" := memo_p (Hashtbl.create 100) !_E"
])

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
nts |> List.map (fun nt -> "let _"^nt^" = ref (mk_pre_parser()) in") |> String.concat "\n";
"\n";
"let _ = \n  ";
nts |> List.map (fun nt -> rhs_for_nt nt g0) |> List.map rules_for_nt_to_ocaml |> String.concat ";\n  "; "\n";
"in\n"
])


(*
let _ = grammar_to_ocaml g0 |> print_endline

let rec parse_E = (
  let _E = ref (mk_pre_parser()) in
  let _ = _E := mkntparser_lazy (!_E) (lazy(alts[
  ((parse_E**>parse_E**>parse_E) >> ( fun (x,(y,z)) -> x+y+z ));
    (((a "1")) >> ( fun _ -> 1 ));
    (((a "")) >> ( fun _ -> 0 ));
    ((parse_F) >> ( fun x -> x )) ])) )
  and parse_F = (
  let _F = ref (mk_pre_parser()) in
  let _ = _F := mkntparser_lazy (!_F) (lazy(alts[
  ((parse_E) >> ( fun x -> x )) ])) )

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
  let command = "p1_gen_p4" in
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
