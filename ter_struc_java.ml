(* type direction = Local | In | Out *)

type variable = {
	v_type : string;
	v_name : string;
(* 	v_usage : direction; pour faciliter le traitement plutot 3 listes différentes*)
	v_valuesPassedUsed : string list;
	(* usage de la variable à t-1,
	   ce qui implique la création de variables en t-1
	   de même type, et dont les noms seront name^"num_used"^"1"
	   toute variable ayant de base un nom = nomOrigine^"0",
	   il ne pourra pas exister de variable de même nom
	*)
}

type konstraint =
	K_V of string
	| K_Bin_op of konstraint*string*konstraint
	| K_Una_op of string*konstraint

type expression =
	V of string (* pour stocker constantes et variables de tout type *)
	| Bin_op of expression*string*expression
	| Una_op of string*expression
	| Call of string*(expression list)

type enum = {
	e_name : string;
	e_values : string list;
}

type thread = {
	t_variables : variable list; (* in, out, local *)
	t_exp : (string*expression) list; (* plusieurs si cas séquentiel *)
	t_const : (string*konstraint) list;
}

type procedure = {
	p_in_types : string list;
	p_name : string;
	p_out_type : string;
}

type java = {
	j_enumT : enum list; (* sauvegarde des types énumérés *)
	j_procedures : procedure list; (* sauvegarde des procedure à prédéfinir dans Usable.java *)
	j_ins : variable list;
	j_outs : variable list;
	j_locals : variable list;
	j_threads : thread list;
	j_nbThreads : int;
	(* souvent appelé : diminue la complexité plutot que List.length j_threads *)
}
