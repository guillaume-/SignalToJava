(*
Compile : ocamlc ter_struc_graph.ml ter_manip_graph.ml ter_make_graph.ml
*)
open Ms_syntax_tree
open SyntaxTree
open Ms_identifier
open Ter_exception
open Ter_struc_java
open Ter_struc_graph
open Ter_manip_graph

type ref = {
	java: java;
	assigns: (string*expression) list;
	res: graph;
}

let creerRef j a = {java = j; assigns = a; res = [];}


	(* ici, 1 thread =  1 assign :
	** ALGO génération des liens
	** pour chaque thread t :
	**  pour chaque assign a, fst(a) != t.p_path.o_name
	**  pour chaque V(s) dans scd(a),
	**  	si s = t.p_path.o_name
	**  	alors 
	**  		pour tout thread de threads
	**  			si thread.p_path.o_name = fst(a)
	**  			alors ajouter à t.p_path.links thread.p_path
	**  			sinon continuer
	**  	sinon continuer
	*)
	let compute_links_graph param ins threads =
		let rec compute_links assigns =
			let rec compute_t t =
				let tname = t.p_path.o_name
				in let tpath = t.p_path
				in let rec compute_t_a t a_out = function
					V(s)->	if(s==tname)
							then {t with p_path =
									{tpath with o_links = (
								(List.find (fun x -> x.p_path.o_name = a_out) threads).p_path
							::tpath.o_links) } }
							else t
					|Bin_op(e1, s, e2) -> compute_t_a (compute_t_a t a_out e1) a_out e2
					|Una_op(s, e) -> compute_t_a t a_out e
					|Call(s, eL) -> rec_compute_t_a t a_out eL
				and rec_compute_t_a t a_out = function
					[] -> t
					|a::aL -> rec_compute_t_a (compute_t_a t a_out a) a_out aL
				in function
				[] -> t
				|a :: aL -> compute_t (compute_t_a t (fst a) (snd a)) aL
			in function
			[] -> []
			|t::l -> (compute_t t assigns)::(compute_links assigns l)
		in (compute_links param.assigns threads)

	(* premier temps : on définit 1 thread par assignation *)
	let mk_graph param =
		let rec mk_ins = function
			[] -> []
			|i::ins -> (new_in_port i.v_name)::(mk_ins ins)
		in let ins = (mk_ins param.java.j_ins)
		in let rec threads i = 
			let thread i assign = {
				p_id = i;
				p_path = new_out_port (fst assign)(snd assign)ins;
			}
			in function
			[] -> []
			|e::l -> (thread i e)::(threads (i+1) l)
		in compute_links_graph param ins (threads 0 param.assigns)
