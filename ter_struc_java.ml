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
	   il ne pourra pas exister de variable de même nom :
	   toute variable doit avoir de base un nom = nomOrigine^"0"
	*)
}

type expression =
	V of variable
	| C of constant
	| Bin_op of expression*string*expression
	| Una_op of string*expression
	| Call of string*(expression list)

type const =
	cV of variable
	| cBin_op of const*string*const
	| cUna_op of string*const

type enum = {
	e_name : string;
	e_values : string list;
}

type thread = {
	t_ins : variable list;
	t_out : variable;
	t_locals : variable list;
	t_exp : expression;
	t_const : (variable*string*const) list;
}

type procedure = {
	p_in_types : string list;
	p_out_type : string;
}

type java = {
	j_ins : variable list;
	j_outs : variable list;
	j_locals : variable list;
	j_constants : constant list;
	j_threads : thread list;
	j_enumT : enum list;
	j_procedures : procedure list;
}
