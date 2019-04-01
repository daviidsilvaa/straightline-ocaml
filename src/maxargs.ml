open Absyn

let rec maxargs statement =
   match statement with
   | CompoundStm (stm1, stm2) -> max (maxargs stm1) (maxargs stm2)
   | AssignStm (_, e) -> maxargs_exp e
   | PrintStm args -> List.fold_left max (List.length args) (List.map maxargs_exp args)

and maxargs_exp expression =
	match expression with
	| IdExp id -> 0
	| NumExp num -> 0
	| OpExp (e1, _, e2) -> max (maxargs_exp e1) (maxargs_exp e2)
	| EseqExp (s, e) -> max (maxargs s) (maxargs_exp e)
