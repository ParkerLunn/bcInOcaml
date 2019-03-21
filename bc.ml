(*Global scope/scope for each function*)
(* times and plus(commutative), put constants as first arguments and combine subsequently *)
open Core

(* Top-level expression *)
type sExpr = 
    | Atom of string
    | List of sExpr list
;;

(* Our floats, prints an error if not given a float *)
type value = 
    | Float of float
    | Error of string
;;

(* Allows for basic arithmetic *)
type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list
;;

(* Statements for the BC language *)
type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement * expr * statement * statement list
    | Break of string
    | Continue of string
    | Error of string
    | FctDef of string * string list * statement list 
;;

(* Contains any number of statements *)
type block = statement list ;;

(* Environments for storing variables *)
type env = (string, float) Stdlib.Hashtbl.t ;;(* variable list*)
(* Stdlib.Hashtbl.add my_hash "h" "hello";
# Stdlib.Hashtbl.find my_hash "h";;
# Stdlib.Hashtbl.remove my_hash "h";; *)

(* Stack of environments / scopes *)
type envQueue = env list ;;(*variable list stack*)

exception Cont
exception Brk

(* Stores value (v) with name (k) and returns q *)
let store (k:string) (v:float) (q:envQueue) : envQueue =  
    Stdlib.Hashtbl.add (Stdlib.List.hd(Stdlib.List.rev q)) k v ;
    q;
;;

let varEval (k: string) (q:envQueue) : float  = try ( match q with
    | hd::[] -> ( 
            match Stdlib.Hashtbl.find hd k with
                | float ->  Stdlib.Hashtbl.find hd k
                | _ -> 0. 
            )
    | _::tl -> (
            match (Stdlib.Hashtbl.find (Stdlib.List.hd (Stdlib.List.rev tl)) k)  with
                | float -> Stdlib.Hashtbl.find (Stdlib.List.hd(Stdlib.List.rev tl)) k
                | _ -> 0.
            )
    | _ -> 0.)
    with Not_found -> 0.
;;


let%expect_test "getvar" = 
    let my_hash = Stdlib.Hashtbl.create 2 in
    Stdlib.Hashtbl.add my_hash "one" 1.;
    varEval "one" [my_hash]|>
    printf "%F";
    [%expect {| 1. |}]


let%expect_test "getar" = 
    let my_hash = Stdlib.Hashtbl.create 2 in
    Stdlib.Hashtbl.add my_hash "two" 2.;
    varEval "two" [my_hash] |>
    printf "%F";
    [%expect {| 2. |}]
;;

let evalOp1 (s: string) (e: expr) (q:envQueue) : float =
    match s with    
        | "++" -> ( 
            match e with
                | Var(v) -> let f = (varEval v q) in 
                            let q = (store v (f+.1.) q) in f;
                | _ -> failwith "must call increment on a variable"; 
             )
        | "--" -> ( 
            match e with
                | Var(v) -> let f = (varEval v q) in 
                            let q = (store v (f-.1.) q) in f;
                | _ -> failwith "must call increment on a variable"; 
             )
        | _ -> failwith "must call increment on a variable"; 
;;

let evalOp1 (s: string) (e: expr) (q:envQueue) : float =
    match s with    
        | "++" -> ( 
            match e with
                | Var(v) -> let f = (varEval v q) in 
                            let q = (store v (f+.1.) q) in f;
                | _ -> failwith "must call increment on a variable"; 
             )
        | "--" -> ( 
            match e with
                | Var(v) -> let f = (varEval v q) in 
                            let q = (store v (f-.1.) q) in f;
                | _ -> failwith "must call increment on a variable"; 
             )
        | _ -> failwith "must call increment on a variable"; 
;;

let evalOp2 (s: string) (op1: float) (op2: float) : float =
    match s with    
        | "+" -> op1+.op2
        | "-" -> op1-.op2
        | "*" -> op1*.op2
        | "/" -> op1/.op2
        | "^" -> op1**op2
        | "==" -> if(op1==op2) then 1. else 0.
        | ">" -> if(op1>op2) then 1. else 0.
        | "<" -> if(op1<op2) then 1. else 0.
        | ">=" -> if(op1>=op2) then 1. else 0.
        | "<=" -> if(op1<=op2) then 1. else 0.
        | _ -> 0.0
;;

let rec evalExpr (e: expr) (q:envQueue): float  = match e with
    | Num(flt) -> flt 
    | Var(v) -> varEval v q
    | Op1(s,expr) -> evalOp1 s expr q
    | Op2(s,ex1,ex2) -> let f = evalOp2 s (evalExpr ex1 q) (evalExpr ex2 q) in printf "%F" f; f
    (* | Fct -> *)
;;

(* Test for expression *)
let%expect_test "evalNum" = 
    evalExpr (Num 10.0) [] |>
    printf "%F";
    [%expect {| 10. |}]

    (* While loop implementation *)
let rec while_loop (e: expr) (code: block) (q:envQueue) : envQueue =
    let cond = evalExpr e q in
        if(cond>0.0) then (* evalute code block *)
            try (
                let q = evalBlock code q in
                while_loop e code q
            )
            with Cont -> while_loop e code q
            | Brk -> q
        else
            q;

and for_loop (e: expr) (inc: statement) (code: block) (q:envQueue) : envQueue =
    let cond = evalExpr e q in
        if(cond>0.0) then
            try (
                let q = evalBlock code q in
                let q = evalStatement inc q in 
                for_loop e inc code q
            )
            with Cont -> (let q = evalStatement inc q in for_loop e inc code q;)
            | Brk -> q
        else
            q;

and evalBlock (code: block) (q:envQueue): envQueue = match code with
    | stat::tl -> let q = evalStatement stat q in evalBlock tl q;
    | stat::[] -> evalStatement stat q;
    | _        -> q;

and evalStatement (s: statement) (q:envQueue) : envQueue =
    match s with 
        | Assign(v, e) -> store v (evalExpr e q) q;
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e q in
                if(cond>0.0) then
                    evalBlock codeT q
                else
                    evalBlock codeF q
        | Expr(e) -> let f = evalExpr e q in printf "%F" f ; q
        | While(e, code) -> while_loop e code q
        | For(assign, e, inc, code) -> let q = evalStatement assign q in
                                    for_loop e inc code q
        | Break(s) -> raise Brk
        | Continue(s) -> raise Cont
        | _ -> q (*ignore *)
;;

let rec evalCode (code: block) (q:envQueue): unit =
    (* create new environment *)
    (* let currLocal = Stdlib.Hashtbl.create 1000 in
    let q = evalBlock code (q@[currLocal]) in *)
    match code with 
        | state::tl -> let q = evalStatement state q in evalCode tl q
        | [] -> print_endline ""
    (* function state list *)
    (* pop the local environment *)
;;
(* 
    v = 1; 
    v // display v
 *)
let p4: block = [
        Assign("v", Num(1.0));
        Expr(Op1("++", (Var "v")));
        Expr(Var("v")) 
]

let%expect_test "incVar" =
    let my_hash = Stdlib.Hashtbl.create 5 in
    evalCode p4 [my_hash];
    [%expect {| 2. |}]

(* v=1
   v++
   v
*)
let p1: block = [
        Assign("v", Num(1.0));

        Expr(Var("v")) 
]

let%expect_test "p1" =
    let my_hash = Stdlib.Hashtbl.create 5 in
    evalCode p1 [my_hash];
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
            Expr(Op1("++", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p2" =
    let currLocal = Stdlib.Hashtbl.create 1000 in
    evalCode p2 [currLocal];
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

let p4: block = 
    [
        Assign("v",Num(0.0));
        While(
            Op2("<=", Var("v"), Num(3.0)),
            [Assign("v", Op2("+", Var("v"), Num(1.0)))]
        );
        Expr(Var("v"))
    ]

let%expect_test "p4" =
    let currLocal = Stdlib.Hashtbl.create 1000 in
    evalCode p4 [currLocal];
    [%expect {| 4. |}]
