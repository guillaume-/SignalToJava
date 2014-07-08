open Ter_struc_graph

let new_in_port name = {
	in_name = name;
	in_thread_used = [];
	in_transi_links = [];
}

let new_out_port name = {
	o_name = name;
	o_thread_num = -1;
	o_link = None;
	o_transi_links = [];
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
	let is_possible p1 p2 = not (List.exists (fun x -> x=p1) p2.o_transi_links)
	in let rec tmp_res p1 p2 res = (*créé une liste de out dans l'ordre de définition souhaité*)
		match(p2)with
		Some(p) -> if(is_possible p1 p)
					then tmp_res p p1.o_link (p1::res)
					else tmp_res p1 p.o_link (p::res)
		|None -> match(p1.o_link)with
					Some(x) -> tmp_res x None (p1::res)
					|None -> res 
	in let rec ordonner last_Value = function (*indique à tout out que son prochain est celui qui se trouve en i+1 dans la liste*)
		[]-> last_Value
		|e::l -> ordonner (Some({e with o_link = last_Value})) l
	in let tmp = List.rev (tmp_res p1 (Some(p2)) [])
	in match(ordonner None tmp)with
		Some x -> x
		|None -> failwith("at ter_manip_graph.ml : assigns_priority error")
(*
Prend 2 threads, retourne un thread union des 2
*)
let fusion t1 t2 = 
	let rec union ins = function
		[] -> ins
		|e::l -> if(List.exists (fun x -> x == e) ins)then (union ins l) else e::(union ins l)
	in {
	p_in = union t1.p_in t2.p_in;
	p_path = assigns_priority t1.p_path t2.p_path;
}
