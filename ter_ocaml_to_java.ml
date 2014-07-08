open Ter_struc_java
open Printf

let rec str_ins = function
	|[] -> ""
	|v::l ->"\tpublic Signal<"^v.v_type^">"^v.v_name^"0 = new Signal<>(SET_VALUE);\n"
			^(if(v.v_valuesPassedUsed = [])then ""
			else "\tpublic Signal<"^v.v_type^">"^v.v_name^"1 = new Signal<>();\n")
			^(str_ins l)

let rec str_outs = function
	|[] -> ""
	|v::l ->"\tpublic Signal<"^v.v_type^">"^v.v_name^"0 = new Signal<>();\n"
			^(if(v.v_valuesPassedUsed = [])then ""
			else "\tpublic Signal<"^v.v_type^">"^v.v_name^"1 = new Signal<>();\n")
			^(str_ins l)

let rec str_syn = function
	[] -> ""
	|v::l -> (if(v.v_is_syn)then "\t\t"^v.v_name^".new_loop();\n" else "")^(str_syn l)

let rec enumL_to_string =
	let enum_to_string e =
		let rec variants_to_string = function
			|[] -> ""
			|e::[] -> e^"\n"
			|e::l -> e^", "^(variants_to_string l)
		in "public enum "^e.e_name^"{\n"^(variants_to_string e.e_values)^"}\n"
	in function
	[] -> ""
	|e::l -> (enum_to_string e)^"\n"^(enumL_to_string l)

let rec proc_to_string =
	let rec p_in_to_string = function
		[] -> ""
		|e::[]-> e
		|e::l -> e^", "^(p_in_to_string l)
	in let p_to_string p = "public "^p.p_out_type^" "^p.p_name^"("^(p_in_to_string p.p_in_types)^")throws Uncoded_function{\n\t\tthrow new Uncoded_function();\n}\n"
	in function
	[] -> ""
	|p::l -> (p_to_string p)^"\n"^(proc_to_string l)

let rec main_to_string nbThreads =
	let rec import = function
		 0 -> "import thread.T0;\n"
		|x -> "import thread.T"^(string_of_int x)^";\n"^(import (x-1))
	in
	let rec declarations = function
		 0 -> "\t\tthread t0 = new T0(0, data);\n"
		|x -> "\t\tthread t"^(string_of_int x)^" = new T"^(string_of_int x)^"("^(string_of_int x)^", data);\n"^(declarations (x-1))
	in
	let rec globals = function
		 0 -> "\t\tdata.add_thread(t0);\n"
		|x -> "\t\tdata.add_thread(t"^(string_of_int x)^");\n"^(globals (x-1))
	in
	let rec start = function
		 0 -> "\t\tt0.start();\n"
		|x -> "\t\tt"^(string_of_int x)^".start();\n"^(start (x-1))
	in
	"package main;\nimport thread.thread;\nimport data.GlobalData;\n"
	^(import (nbThreads-1))
	^"public class Main{\n\tpublic static void main(String[]args){\n\t\tGlobalData data = new GlobalData();\n"
	^(declarations (nbThreads-1))
	^(globals (nbThreads-1))
	^(start (nbThreads-1))
	^"\t}\n}\n"

let create_Usable enumL procedureL =
	let content =
		let c_beg = "package usable;\n/*\n * C'est ici que doivent être définies les procédures dont la signature est donnée en miniSignal\n */\n\npublic final class Usable{\n"
		and c_end = "\n}\n"
		in let c_mid = (enumL_to_string enumL)^"\n"^(proc_to_string procedureL)
		in (c_beg^c_mid^c_end)
	in let file = open_out "Usable.java"
	in fprintf file "%s" content;
	close_out file

let create_Constantes nbThreads =
	let content = "package data;\n\npublic final class Constantes{\n\tpublic static final int NB_THREADS = "^string_of_int nbThreads^";\n}\n"
	in let file = open_out "Constantes.java"
	in fprintf file "%s" content;
	close_out file

let create_GlobalData ins outs locals =
	let gdata_to_string =
		let str_reset outs locals =
			"\tpublic void reset(){\n"^(str_syn outs)^(str_syn locals)^"\n\t}\n"
		in
		(str_ins ins)^(str_outs locals)^(str_outs outs)^(str_reset outs locals)
	in
	let content = "package data;\nimport java.util.ArrayList;\nimport java.util.List;\nimport thread.thread;\npublic class GlobalData{\n\tpublic boolean end_loop = false;\n\tpublic List<thread> threads = new ArrayList<thread>();\n"
					^(gdata_to_string)
					^"\n\n\tpublic void add_thread(thread t){\n\t\tthreads.add(t);\n\t}\n\n\tpublic boolean allThreadLoopEnded(){\n\t\tfor(int i=0; i<Constantes.NB_THREADS; i++)\n\t\t\tif(!threads.get(i).has_looped)\n\t\t\t\treturn false;\n\t\treturn true;\n\t}\n\n\tpublic boolean anyThreadLoopEnded(){\n\t\tfor(int i=0; i<Constantes.NB_THREADS; i++)\n\t\t\tif(threads.get(i).has_looped)\n\t\t\t\treturn false;\n\t\treturn true;\n\t}\n}\n"
	in let file = open_out "GlobalData.java"
	in fprintf file "%s" content;
	close_out file

let rec expr_to_string t = function
	V(s) -> if((List.exists (fun x -> x.v_name = s) t.t_inits)
			|| (List.exists (fun x -> x.v_name = s) t.t_others))
			then s^"0.getT() "
			else s
	|Bin_op(e1, s, e2) -> "("^(expr_to_string t e1)^")"^s^"("^(expr_to_string t e2)^")"
	|Una_op(s, e) -> s^"("^(expr_to_string t e)^")"
	|Call(s, eL) -> s^"("^(exprL_to_string t eL)^")"
and exprL_to_string t = function
	[] -> ""
	|e::[] -> expr_to_string t e
	|e::l -> (expr_to_string t e)^", "^(exprL_to_string t l)

let compute_uniq t =
	let content assign =
	"\t\ttry{\n\t\t"
	^(fst assign)^"0.setT("^(expr_to_string t (snd assign))^");\n"
	^"\t\t}catch(UnpresentSignalException e){\n\t\t"
	^(fst assign)^"0.unsetT();\n"
	^"\t\t}\n"
	in let rec rule_assigns = function
		[] -> ""
		|a::aL -> (content a)^(rule_assigns aL)
	in rule_assigns t.t_assigns
	

let create_thread t =
	let content =
		"package thread;\nimport data.GlobalData;\nimport data.Signal;\nimport exceptions.UnpresentSignalException;\npublic class T"
		^(string_of_int t.t_id)^" extends thread{\n"
		^(str_ins t.t_inits)^(str_outs t.t_others)
		^"\n\tpublic T0(int n, GlobalData gd){\n\t\tsuper(n, gd);\n\t}\n\n\t@Override\n\tpublic void reset(){\n\t\tsuper.reset();\n"
		^(str_syn t.t_inits)^(str_syn t.t_others)
		^"\t}\n\n\t@Override\n\tpublic void uniq(){\n"
		^(compute_uniq t)
		^"\t}\n}\n"
	in let file = open_out ("T"^(string_of_int t.t_id)^".java")
	in fprintf file "%s" content;
	close_out file

let rec create_threads = function
	[] -> ()
	|t::l -> (create_thread t); (create_threads l)

let create_main nbThreads =
	let content = main_to_string nbThreads
	in let file = open_out "Main.java"
	in fprintf file "%s" content;
	close_out file

(*
data
	-> Constantes.java
	Event.java
	-> GlobalData.java
	Signal.java
exception
	Uncoded_function.java
	UnpresentSignalException.java
main
	-> Main.java
thread
	thread.java
	-> T0.java .. TN.java
usable
	-> Usable.java /!\ CREATE VOID FUNCTIONS /!\
*)
let ocaml_to_java (j:java) =
	create_Usable j.j_enumT j.j_procedures;
	create_Constantes j.j_nbThreads;
	create_GlobalData j.j_ins  j.j_outs j.j_locals;
	create_threads j.j_threads;
	create_main j.j_nbThreads
;;
