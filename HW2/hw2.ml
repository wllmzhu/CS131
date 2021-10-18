(* Type definition *)
type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
    | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
    | Leaf of 'terminal

(* Question 1, convert HW1 style grammer to HW2 style grammar *)
let convert_grammar (start, rules) =
    let rec get_alt_list rules nt =
        (* include a list item (rule) in the returned list if its LHS equals
        to the provided nt *)
        match rules with
        | [] -> []
        | h::t -> if (fst h) = nt then (snd h)::(get_alt_list t nt)
                  else get_alt_list t nt
    in
    (* partially applying get_alt_list to result in prod_function *)
    let prod_function = get_alt_list rules in
    (start, prod_function)

(* Question 2, traverses the parse tree left to right and yields a list of the
leaves encountered *)
let parse_tree_leaves tree = 
    let rec parse_trees trees = 
        match trees with
        | [] -> []
        | h::t -> get_traversal_list h t 
    and get_traversal_list h t =
        match h with
        | Leaf leaf -> leaf::(parse_trees t)
        | Node (nonterminal, subtree) -> (parse_trees subtree) @ (parse_trees t)
    in
    parse_trees [tree]

(* Question 3, a function make_matcher 'gram' that returns a matcher for the 
grammar 'gram'. the matcher must try the grammar rules in order and return the
result of calling accept on the suffix corresponding to the first acceptable
matching prefix of frag*)
let make_matcher (start, prod_function) = 
    let rec match_rules rules acceptor frag =
        match rules with
        | [] -> None
        | h::t -> match_rules_helper h t acceptor frag
    and match_rules_helper h t acceptor frag =
        match (evaluate_suffix h acceptor frag) with
        | None -> match_rules t acceptor frag
        | x -> x
    and evaluate_suffix expr acceptor frag =
        if expr = [] then acceptor frag else
        match frag with
        | [] -> None
        | h::t -> evaluate_suffix_helper h t expr acceptor frag
    and evaluate_suffix_helper h t expr acceptor frag =
        match expr with
        | [] -> None
        | x::y -> match_type x y h t acceptor frag
    and match_type x y h t acceptor frag = 
        match x with 
        | T x -> if x = h then evaluate_suffix y acceptor t else None
        | N x -> match_rules (prod_function x) (evaluate_suffix y acceptor) frag
    in
    let matcher acceptor frag = 
        evaluate_suffix [(N start)] acceptor frag
    in
    matcher






    