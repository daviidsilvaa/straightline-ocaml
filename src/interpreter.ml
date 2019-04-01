
open Absyn

type memory = (id, float) Hashtbl.t

let rec run mem prog =
	match prog with
	| CompoundStm (stm1, stm2) -> run mem stm1; run mem stm2
	| AssignStm (var, e) -> Hashtbl.replace mem var (eval e)
	| PrintStm list -> List.iter (fun e -> print_float (eval mem e); print_char ' ') list
	and eval mem exp =
		match exp with
		| IdExp var -> Hashtbl.find mem var
      | NumExp cte -> cte
      (*| OpExp (cte1, op, cte2) -> of exp * binop * exp*)
      (*| EseqExp of stm * exp*)

let interpret prog =
	let mem = Hashtbl.create 0 in
	run mem prog	
	
