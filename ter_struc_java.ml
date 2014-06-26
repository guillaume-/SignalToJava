(* type direction = Local | In | Out *)

type constant = {
	c_id : int; (* id unique *)
	c_value : string;
	c_type : string;
}

type variable = {
	v_type : string;
	v_name : string;
(* 	v_usage : direction; pour faciliter le traitement plutot 3 listes différentes*)
	v_isPassedUsed : bool;
	(* usage de la variable à t-1,
	   ce qui implique la création d'une variable en t-1
	   de même type, et dont le nom sera name^"1"
	   S'il existe déjà une variable de même nom, elle sera renommée.
	*)
}

type expression =
	V of variable
	| C of constant
	| Bin_op of expression*string*expression
	| Una_op of string*expression
	| Call of string*(expression list)

type thread = {
	t_ins : variable list;
	t_outs : variable list;
	t_locals : variable list;
	t_exp : expression;
	t_const : const list;
}
type java = {
	j_ins : variable list;
	j_outs : variable list;
	j_locals : variable list;
	j_constants : constant list;
	j_threads : thread list;
}
