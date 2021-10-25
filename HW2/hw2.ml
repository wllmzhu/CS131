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
matching prefix of frag *)
let make_matcher (start, prod_function) = 
    (* iterate over set of rhs (alt_list) to find an applicable one, evaluate 
    each rhs and return the result of evaluation *)
    let rec find_rhs alt_list acceptor frag =
        match alt_list with
        | [] -> None
        | rhs::rest_rhs ->
            (* if evaluate_rhs gives None, then iterate to the next rhs. 
            If not, then we found a good rule. Directly return the result *)
            match (evaluate_rhs rhs acceptor frag) with
            | None -> find_rhs rest_rhs acceptor frag
            | Some x -> Some x
    (* evaluate_rhs iterate over prefix and frag, eventually apply acceptor onto
    the suffix (frag - prefix) and return the result (None or frag) *)
    and evaluate_rhs prefix acceptor frag =
        (* if finished iterating through prefix, call acceptor to accept or reject
        the suffix. Else, call match_type to switch on whether prefix is terminal
        or nonterminal *)
        match prefix with
        | [] -> acceptor frag
        | prefix_h::prefix_t ->
            match frag with
            | [] -> None
            | frag_h::frag_t -> match_type prefix_h prefix_t frag_h frag_t acceptor frag
            
    and match_type prefix_h prefix_t frag_h frag_t acceptor frag = 
        match prefix_h with 
        (* if terminal, and if prefix head match with frag head, then iterate over prefix and
        frag by getting rid of these two heads *)
        | T prefix_h -> 
            if prefix_h = frag_h 
            then evaluate_rhs prefix_t acceptor frag_t
            else None
        (* if nonterminal, try to expand by trying all rules that are applicable to prefix head, 
        use a partially applied evaluate_rhs as the new acceptor *)
        | N prefix_h -> 
            find_rhs (prod_function prefix_h) (evaluate_rhs prefix_t acceptor) frag
    in
    evaluate_rhs [(N start)]



(* Question 4, a function make_parser gram that returns a parser for the grammar gram. When applied
to a fragment frag, the parser returns an optional parse tree. If frag cannot be parsed entirely
(that is, from beginning to end), the parser returns None. Otherwise, it returns Some tree where
tree is the parse tree corresponding to the input fragment. Your parser should try grammar rules
in the same order as make_matcher *)

let make_parser (start, prod_function) =
    (* inherited from question 3 *)
    let rec find_rhs start prod_function alt_list acceptor frag children =
        match alt_list with
        | [] -> None
        | rhs::rest_rhs ->
            match (parse_rhs_tree start prod_function rhs acceptor frag children) with
            | None -> find_rhs start prod_function rest_rhs acceptor frag children
            | Some x -> Some x
    (* modified from question 3 *)
    and parse_rhs_tree start prod_function prefix acceptor frag children =
        match prefix with
        | [] -> acceptor frag (Node (start, children))
        | prefix_h::prefix_t ->
            match frag with
            | [] -> None
            | frag_h::frag_t -> 
                match prefix_h with 
                | T prefix_h -> 
                    if prefix_h = frag_h 
                    then parse_rhs_tree start prod_function prefix_t acceptor frag_t (children @ [Leaf prefix_h])
                    else None
                | N prefix_h -> 
                    let acceptor1 frag1 tree1 =
                        parse_rhs_tree start prod_function prefix_t acceptor frag1 (children @ [tree1]) 
                    in
                    find_rhs prefix_h prod_function (prod_function prefix_h) acceptor1 frag []       
    in
    let acceptor_accept_empty frag tree =
        match frag with
        | [] -> Some tree
        | _ -> None
    in
    fun frag -> find_rhs start prod_function (prod_function start) acceptor_accept_empty frag []  

