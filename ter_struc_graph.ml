(*
We know we have one uniq output signal per assign
We also can target assigns with their uniq output signal
*)

type in_port = {
	in_name : string;
	in_thread_used : int list; (* local signal if size == 1 *)
	in_transi_links : out_port list;
}
and out_port = {
	o_name : string;
	o_thread_num : int; (* while not treated -1 *)
	o_link : out_port option; (* if None end of a thread else compute the out_port *)
	o_transi_links : out_port list;
	o_waited : bool; (* if true we'll have to notifyAll *)
}

type thread = {
	p_in : in_port list;
	(* all In imply the first out_port, which imply the next outport, ... *)
	p_path : out_port;
}

type graph = thread list;; (* 2 paths <=> the input ports not counted, 2 lists without any recurrent port *)
