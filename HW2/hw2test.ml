(* From HW1 *)
let rec subset a b = 
    match a with
    | [] -> true
    | h::t -> (List.exists (fun x -> x = h) b) && (subset t b)

let equal_sets a b = 
    (subset a b) && (subset b a)

(* Modifying the grammar from HW1's sample *)
type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num | Factorial

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
   Expr, [T"["; N Expr; T"]"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"]]

let awksub_grammar = Expr, awksub_rules

let new_awksub_grammar = convert_grammar awksub_grammar

let awkish_grammar =
    (Expr,
    function
        | Expr ->
            [[N Term; N Binop; N Expr];
            [N Term]]
        | Term ->
        [[N Num];
        [N Lvalue];
        [N Incrop; N Lvalue];
        [N Lvalue; N Incrop];
        [T"("; N Expr; T")"];
        [T"["; N Expr; T"]"];
        [N Num; N Factorial]]
        | Lvalue ->
        [[T"$"; N Expr]]
        | Incrop ->
        [[T"++"];
        [T"--"]]
        | Binop ->
        [[T"+"];
        [T"-"]]
        | Num ->
        [[T"0"]; [T"1"]]
        | Factorial ->
        [[T"!"]])

let awk_frag = ["$"; "["; "$"; "1"; "++"; "]"; "-"; "0"; "!"]

let acceptor_accept_nonempty frag =
    match frag with
    | [] -> None
    | x -> Some x

let convert_grammar_test = equal_sets
                                ((snd new_awksub_grammar) Num)
                                ([[T "0"]; [T "1"]])

let make_matcher_test = 
    (make_matcher awkish_grammar acceptor_accept_nonempty awk_frag) =
    Some ["!"]

let parse_tree_leaves_test = 
    match make_parser awkish_grammar awk_frag with
    | Some tree -> parse_tree_leaves tree = awk_frag
    | _ -> false

let parse_tree_leaves_test1 = 
    (make_parser awkish_grammar awk_frag) = 
      Some
        (Node (Expr,
            [Node (Term,
            [Node (Lvalue,
                [Leaf "$";
                Node (Expr,
                [Node (Term,
                    [Leaf "[";
                    Node (Expr,
                    [Node (Term,
                        [Node (Lvalue,
                        [Leaf "$";
                            Node (Expr, [Node (Term, [Node (Num, [Leaf "1"])])])]);
                        Node (Incrop, [Leaf "++"])])]);
                    Leaf "]"])])])]);
            Node (Binop, [Leaf "-"]);
            Node (Expr,
            [Node (Term, [Node (Num, [Leaf "0"]); Node (Factorial, [Leaf "!"])])])]))
