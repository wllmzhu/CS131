(* From HW1 *)
let rec subset a b = 
    match a with
    | [] -> true
    | h::t -> (List.exists (fun x -> x = h) b) && (subset t b)

let equal_sets a b = 
    (subset a b) && (subset b a)


(* Borrowing the grammar from HW1's sample *)
type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let new_awksub_grammar = convert_grammar awksub_grammar

let convert_grammar_test0 = equal_sets
                                ((snd new_awksub_grammar) Num)
                                ([[T "0"]; [T "1"]; [T "2"]; [T "3"]; [T "4"]; [T "5"];
                                [T "6"]; [T "7"]; [T "8"]; [T "9"]])

let parse_tree_leaves_test0 tree = 

