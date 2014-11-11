open P1_lib

(* list of p separated by sep *)
let listof : ('a,'b) ty_parser -> ('a,'c) ty_parser -> ('a,'b list) ty_parser = (fun p sep ->
    let rec f1 i0 = 
      ((p >> (fun x -> [x]))
       ||| ((p **> sep **> f1) >> (fun (x,(_,xs)) -> x::xs))) 
        i0
    in
    f1 ||| (parse_eps >> (fun _ -> [])))
