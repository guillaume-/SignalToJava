open Ms_syntax_tree
open SyntaxTree
open Ms_identifier
open Ter_exception
open Ter_util
open Ter_iterateurs
open Ter_identite_p
open Ter_struc_java

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

	let apl_assign s asn ae = s
