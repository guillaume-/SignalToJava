open Ms_syntax_tree
open SyntaxTree
open Ms_identifier
open Ter_exception
open Ter_util
open Ter_iterateurs
open Ter_identite_p
open Ter_struc_java

(*	Module permettant de convertir une structure spécification telle que proposée par l'INRIA en structure java destinée à coder du Java
	On suppose la transformation NoSubmodule déjà appliquée sur la spécification en entrée.
*)

type ref = {
	spec: specification;
	res: java;
	assigns: (string*expression) list;
	kons: (string*konstraint) list;
}

	module MbParam : tRef with type r = ref = struct
	type r = ref
	let creerRef s =
		let init_java = {
			j_enumT = []; (* sauvegarde des types énumérés *)
			j_procedures = []; (* sauvegarde des procedure à prédéfinir dans Usable.java *)
			j_ins = [];
			j_outs = [];
			j_locals = [];
			j_threads = [];
			j_nbThreads = 0;
		}
		in {spec = s; res = init_java; assigns = []; kons = [];}
	end

	include Identite(MbParam)

	let init_var v_n v_t = {
		v_name = v_n;
		v_type = v_t;
		v_valuesPassedUsed = [];
	}

	let rec exp_to_kons = function
		SignalAtom(id) -> K_V(id)
		| ClockPlus(e1, e2) -> K_Bin_op(exp_to_kons e1, "^+", exp_to_kons e2)
		| ClockMinus(e1, e2) -> K_Bin_op(exp_to_kons e1, "^-", exp_to_kons e2)
		| ClockTimes(e1, e2) -> K_Bin_op(exp_to_kons e1, "^*", exp_to_kons e2)
		| _ -> raise (Clock_Error("^+, ^-, ^* used to set clock but with incompatible operations"))

	let rec exp_to_exp = function
		IntegerConstant(i) -> V(string_of_int i)
		| EnumVariantAtom(id) -> V(id)
		| SignalAtom(id) -> V(id)
		| WhenNotAtom(id) -> Una_op("when not", V(id))
		| NotAtom(id) -> Una_op("not", V(id))
		| WhenAtom(id) -> Una_op("when", V(id))
		| When(e1, e2) -> Bin_op(exp_to_exp e1, "when", exp_to_exp e2)
		| EqualityAtom(e1, e2) -> Bin_op(exp_to_exp e1, "=", exp_to_exp e2)
		| Delay(e1, e2) -> Bin_op(exp_to_exp e1, "$1 init ", exp_to_exp e2)
		| Default(e1, e2) -> Bin_op(exp_to_exp e1, "default", exp_to_exp e2)
		| AndExp(e1, e2) -> Bin_op(exp_to_exp e1, "&&", exp_to_exp e2)
		| OrExp(e1, e2) -> Bin_op(exp_to_exp e1, "||", exp_to_exp e2)
		| Plus(e1, e2) -> Bin_op(exp_to_exp e1, "+", exp_to_exp e2)
		| Minus(e1, e2) -> Bin_op(exp_to_exp e1, "-", exp_to_exp e2)
		| Times(e1, e2) -> Bin_op(exp_to_exp e1, "*", exp_to_exp e2)
		| FunctionCall(id, sigL) -> Call(id, List.map (exp_to_exp) sigL)
		| _ -> raise (Clock_Error("^+, ^-, ^* used to set clock but with incompatible operations"))


	let compute_threads res assi ks = []
(*		let rec var_dependences assi ks res =
			let rec*)

	let apl_proced_decla s pn pi po = {
		s with
		res = { 	s.res with
					j_procedures = {
						p_in_types = pi;
						p_name = pn;
						p_out_type = po;
					}::s.res.j_procedures
		}
	}

	let apl_typed_var_set s ttn vs =
		if(List.exists (fun x -> x = ttn) ["Integer"; "integer"; "int"; "Boolean"; "boolean"; "bool"; "Event"; "event"])then
			s
		else {
			s with
			res =	{ 	s.res with
						j_enumT = {
							e_name = ttn;
							e_values = vs;
						}::s.res.j_enumT
			}
		}

	let apl_sig_decla s sn st sd = {
		s with
		res = match sd with
				Input -> {s.res with j_ins = (init_var sn st)::s.res.j_ins;}
				| Output-> {s.res with j_outs = (init_var sn st)::s.res.j_outs;}
				| Local	-> {s.res with j_locals = (init_var sn st)::s.res.j_locals;}
	}

	let apl_sconstr s ck lsn rsn = {
		s with
		kons = (lsn, K_Una_op(ck, K_V(rsn)))::s.kons
	}

	let apl_assign s asn ae = match(ae)with
		ClockPlus(_)
		| ClockMinus(_)
		| ClockTimes(_) -> {
			s with
			kons = (asn, exp_to_kons ae)::s.kons
		}
		| _ -> {
			s with
			assigns = (asn, exp_to_exp ae)::s.assigns
		}

	let apl_spec s pl tdl pdl = {
		s with
		res = { s.res with
				j_threads = compute_threads s.res s.assigns s.kons
		}
	}
