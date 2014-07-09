open Ter_struc_graph
open Ter_struc_java

let new_in_port name = {
	in_name = name;
	in_thread_used = [];
}

let rec detect_ins ins =
	let rec call_ins ins = function
		[] -> []
		|e::l -> (call_ins ins l)@(detect_ins ins e)
	in function
	V(s) -> if(List.exists (fun x -> x.in_name = s) ins)
			then [(List.find (fun x -> x.in_name = s) ins)]
			else []
	| Bin_op(e1, s, e2) -> (detect_ins ins e1)@(detect_ins ins e2)
	| Una_op(s, e) -> detect_ins ins e
	| Call(s, eL) -> call_ins ins eL

let new_out_port name expr ins = {
	o_name = name;
	o_ins = detect_ins ins expr;
	o_outs = [];
	o_next = None;
	o_links = [];
	o_waited = false;
}

(*
Prend 2 threads, retourne un booleen "est-il intéressant de séquencialiser"
*)
let is_fusion_needed t1 t2 = false

(*
Prend 2 chemins, retourne un seul chemin union des 2 tel que les ordres de définition de p1 et de p2 soient les plus optimisés
*)
let assigns_priority p1 p2 =
	let is_possible p1 p2 = not (List.exists (fun x -> x=p1) p2.o_links)
	in let rec tmp_res p1 p2 res = (*créé une liste de out dans l'ordre de définition souhaité*)
		match(p2)with
		Some(p) -> if(is_possible p1 p)
					then tmp_res p p1.o_next (p1::res)
					else tmp_res p1 p.o_next (p::res)
		|None -> match(p1.o_next)with
					Some(x) -> tmp_res x None (p1::res)
					|None -> res 
	in let rec ordonner last_Value = function (*indique à tout out que son prochain est celui qui se trouve en i+1 dans la liste*)
		[]-> last_Value
		|e::l -> ordonner (Some({e with o_next = last_Value})) l
	in let tmp = List.rev (tmp_res p1 (Some(p2)) [])
	in match(ordonner None tmp)with
		Some x -> x
		|None -> failwith("at ter_manip_graph.ml : assigns_priority error")
(*
Prend 2 threads, retourne un thread union des 2
*)
let fusion t1 t2 = {
	p_id = t1.p_id;
	p_path = assigns_priority t1.p_path t2.p_path;
}
