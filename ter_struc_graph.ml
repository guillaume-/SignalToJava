(*
We know we have one uniq output signal per assign
We also can target assigns with their uniq output signal
*)

type in_port = {
	in_name : string;
	in_thread_used : int list; (* local signal if size == 1 *)
}
and out_port = {
	out_name : string;
	out_thread_num : int; (* while not treated -1 *)
	link : Some out_port; (* if None end of a thread else compute the out_port *)
	transitive_links : out_port list; (* *)
	waited : bool; (* if true we'll have to notifyAll *)
}
and port = In of in_port | Out of out_port;;

type path = {
	p_in : in_port list;
	(* all In imply the first out_port, which imply the next outport, ... *)
	p_path : out_port;
}

type graph = {
	paths : path list; (* 2 paths <=> the input ports not counted, 2 lists without any recurrent port *)
}
