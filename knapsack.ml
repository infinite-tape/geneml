
open Simulator
open Population
open Chromosome
open Selection
open Bigarray
open Util

let load_items filename =
  let temp_list = ref [] in
  let readfile fn =
    let process_line name weight value = (name, weight, value)
    and build_list (name, w, v) lst =
      let n = List.length lst in (n, name, w, v) :: lst
    in
    let ic = open_in fn in
      try
	while true do
	  let (n, w, v) = Scanf.fscanf ic "%S %d %d" process_line in
	    temp_list := build_list (n, w, v) !temp_list
	done;
	List.rev !temp_list
      with End_of_file -> List.rev !temp_list in
    readfile filename

class ['a] zero_one_knapsack_simulator items capacity (select: 'a) =
object(self)
  inherit ['a] basic_simulator select
  val item_list = items
  val num_items = List.length items
  val capacity = capacity
  val mutable total_weight = 0
  val mutable total_value = 0
  val mutable selected_items = []
  val mutable outfile = "knapsack1.txt"

  initializer 
  let v, w = self#calc_value_and_weight item_list in
    total_value <- v;
    total_weight <- w;
  initializer selection_system#set_survival_rate 0.10
  initializer selection_system#set_mutation_rate 0.05

  method calc_value_and_weight lst =
    let rec do_calc l v w =
      match l with
	  ((n: int), nm, wt, vl)::tl -> do_calc tl (v + vl) (w + wt)
	| [] -> v, w 
    in
      do_calc lst 0 0

  (* over-ride fitness_func with our own fitness function *)
  method fitness_func chrom_string = 
    selected_items <- [];
    let score = ref 0 in
      for i=0 to (Array1.dim chrom_string) - 1 do
	if chrom_string.{i} = 1 then
	  let this_item = List.nth item_list i in
	    selected_items <- [this_item] @ selected_items;
      done;
      let selected_value, selected_weight =
	self#calc_value_and_weight selected_items in
	if selected_weight <= capacity then begin
	  float_of_int (total_value - selected_value);
	end
	else float_of_int (2*total_value)
	
  method print_items =
    let rec print_item_list lst =
      match lst with
	  (num, name, weight, value)::tl -> 
	    Printf.printf "%s\t %d\t %d\n" name weight value;
	    print_item_list tl
	| [] -> ()
    in
      print_string "Name\t\tWeight\tValue\n";
      print_item_list item_list;
      flush stdout

  method print_info =
    print_string "Total value: "; print_int total_value; pn ();
    print_string "Total weight: "; print_int total_weight; pn ();

  method print_statistics = 
    print_string "Number of generations: ";
    print_int selection_system#get_num_gens; print_newline ()

  method write_data gen =
    let oc = open_out_gen [Open_append; Open_creat; Open_text;] 0o755 outfile
    in
    let s = self#get_best_score in
      Printf.fprintf oc "%d\t%d\n" gen (int_of_float s);
      close_out oc

  method handle_quit (n: int) = self#print_statistics; ignore (exit 0)
end

let main () =
  let size = ref 10000 
  and tfil = ref "items.txt"
  and capacity = ref 500
  and iterations = ref 100 
  and mutrate = ref 0.05
  and surrate = ref 0.1
  and logging = ref false 
  and quiet = ref false
  and log_file = ref "knapsack.log" in
  let speclist =
    [("-p", Arg.Int (fun x -> size := x), "population size (10000)");
     ("-f", Arg.String (fun x -> tfil := x), "knapsack file (items.txt)");
     ("-i", Arg.Int (fun x -> iterations := x), "number of iterations (100)");
     ("-c", Arg.Int (fun x -> capacity := x), "capacity (500)");
     ("-s", Arg.Float (fun x -> surrate := x), "survival rate (0.1)");
     ("-m", Arg.Float (fun x -> mutrate := x), "mutation rate (0.05)");
     ("-o", Arg.String (fun x -> log_file := x), "log file for -l (knapsack.log)");
     ("-l", Arg.Set logging, "enable statistics output (disabled)");
     ("-q", Arg.Set quiet, "operate quietly, supress output (disable)")]
  and usage_msg = "usage: knapsack [-l] [-s <size>] [-f <file>] [-i <iter>] [-m <mut>] [-s <sur>] [-o <logfile>]" in
    Arg.parse speclist (fun s -> ()) usage_msg;
    let items = load_items !tfil in
    let selector = new basic_selector !size (List.length items) MIN in
    let sim = new zero_one_knapsack_simulator items !capacity selector
    and gen = ref 0 in
      Sys.set_signal Sys.sigint (Sys.Signal_handle sim#handle_quit);
      Random.self_init ();
      if !logging then begin
	sim#logging_on;
	sim#set_log_file !log_file;
      end;
      selector#set_mutation_rate !mutrate;
      selector#set_survival_rate !surrate;
      sim#print_items;
      sim#print_info;
      sim#evaluate;
      if not !quiet then
	sim#print_best !gen;

	let the_best = ref (sim#get_best_score) in
	  while !gen < !iterations do
	    sim#new_generation;
	    gen := !gen + 1;
	    sim#evaluate;
	    if not !quiet then
	      sim#print_best !gen;
	  done;;

main ()
