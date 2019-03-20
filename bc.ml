(*Global scope/scope for each function*)
(* times and plus(commutative), put constants as first arguments and combine subsequently *)
open Core

type sExpr = 
    | Atom of string
    | List of sExpr list
;;

type value = 
    | Float of float
    | Error of string
;;

type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list
;;

type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | Break of string
    | Continue of string
    | Ret
    | Error of string
    | FctDef of string * string list * statement list 
;;

type block = statement list ;;

type env = (string, float) Stdlib.Hashtbl.t ;;(* variable list*)
(* Stdlib.Hashtbl.add my_hash "h" "hello";
# Stdlib.Hashtbl.find my_hash "h";;
# Stdlib.Hashtbl.remove my_hash "h";; *)

type envQueue = env list ;;(*variable list stack*)

(* let get_key (a,_) = a

let get (k:string) (l:env) = List.Assoc.find ~equal:String.equal l k

let remove (k: string) (m: env) : env = List.filter m (fun x -> not(String.equal (get_key x) k)) *)

(* let put (k:string) (v:float) (st:envQueue) : envQueue = match st with
    | q::[] ->  ((k,v)::(remove k q))::st
    | q::tl ->  match (get k q) with
                 | Some(flt) -> ((k,v)::(remove k q))::st
                 | None      -> q::((k,v)::(remove k (List.hd(List.rev tl)))) *)

let store (k:string) (v:float) (q:envQueue) : envQueue =  
    Stdlib.Hashtbl.add (Stdlib.List.hd(Stdlib.List.rev q)) k v ;
    q;
;;

let varEval (k: string) (q:envQueue) : float  = match q with
    | hd::[] -> ( 
            match Stdlib.Hashtbl.find hd k with
                | float ->  Stdlib.Hashtbl.find hd k
                | _ -> 0. 
            )
    | hd::tl -> (
            match (Stdlib.Hashtbl.find (Stdlib.List.hd(Stdlib.List.rev tl)) k)  with
                | float -> Stdlib.Hashtbl.find (Stdlib.List.hd(Stdlib.List.rev tl)) k
                | _ -> 0.
            )
    | _ -> 0.
;;


let%expect_test "getvar" = 
    let my_hash = Stdlib.Hashtbl.create 2 in
    Stdlib.Hashtbl.add my_hash "one" 1.;
    varEval "one" [my_hash]|>
    printf "%F";
    [%expect {| 1. |}]


let%expect_test "getvar" = 
    let my_hash = Stdlib.Hashtbl.create 2 in
    Stdlib.Hashtbl.add my_hash "two" 2.;
    varEval "two" [my_hash] |>
    printf "%F";
    [%expect {| 2. |}]
;;

let evalOp1 (s: string) (op1: float) : float =
    match s with    
        | "++" -> op1+.1.
        | "--" -> op1-.1.
        | _ -> 0.0
;;

let evalOp2 (s: string) (op1: float) (op2: float) : float =
    match s with    
        | "+" -> op1+.op2
        | "-" -> op1-.op2
        | "*" -> op1*.op2
        | "/" -> op1/.op2
        | "^" -> op1**op2
        | "==" -> if(op1==op2) then 1. else 0.
        | _ -> 0.0
;;

let rec evalExpr (e: expr) (q:envQueue): float  = match e with
    | Num(flt) -> flt 
    | Var(v) -> varEval v q
    (* | Op1(s,expr) -> *)
    | Op2(s,ex1,ex2) -> evalOp2 s (evalExpr ex1 q) (evalExpr ex2 q)
    (* | Fct -> *)
;;
(* Test for expression *)
let%expect_test "evalNum" = 
    evalExpr (Num 10.0) [] |>
    printf "%F";
    [%expect {| 10. |}]

let rec evalBlock (code: block) (q:envQueue): envQueue = match code with
    | stat::tl -> let q = evalStatement stat q in evalBlock tl q;
    | stat::[] -> evalStatement stat q;
    | _        -> q;

and evalStatement (s: statement) (q:envQueue) : envQueue =
    match s with 
        | Assign(v, e) -> print_float(evalExpr e q); store v (evalExpr e q) q;
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e q in
                if(cond>0.0) then
                    evalBlock codeT q
                else
                    evalBlock codeF q
        | Expr(e) -> let f = evalExpr e q in printf "%F" f ; q
        | _ -> q (*ignore *)
;;

let rec evalCode (code: block) (q:envQueue): unit =
    (* create new environment *)
    let currLocal = Stdlib.Hashtbl.create 1000 in
    let q = evalBlock code (q@[currLocal]) in
    match code with 
        | state::tl -> let q = evalStatement state q in evalCode tl q
        | [] -> print_endline "done"
    (* function state list *)
    (* pop the local environment *)
;;
(* 
    v = 10; 
    v // display v
 *)
let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

let%expect_test "p1" =
    evalCode p1 [];
    [%expect {| 1. |}]

(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("a++", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p1" =
    evalCode p2 []; 
    [%expect {| 3628800. |}]

(*  Fibbonaci sequence
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
 *)
let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
        2. 
        5.      
    |}]



