(*
We know we have one uniq output signal per assign
We also can target assigns with their uniq output signal
*)

type in_port = {
	in_name : string;
	in_thread_used : int list; (* local signal if List size == 1 *)
}

type out_port = {
	o_name : string; (* nom de la variable calculée dans l'assignation *)
	o_ins : in_port list;
	o_outs : out_port list; (* /!\ comme ins : variables d'entrées *)
	o_links : out_port list; (* ensemble des variables dépendantes de cette variable *)
	o_waited : bool; (* if true we'll have to use a syn_set function to define the variable *)
	o_next : out_port option; (* if None end of a thread else compute the out_port *)
}

type thread = {
	p_id : int;
	p_path : out_port; (* follow o_next to generate the path *)
}

type graph = thread list;; (* 2 paths <=> 2 lists without any recurrent out_port following p_path and o_next *)
