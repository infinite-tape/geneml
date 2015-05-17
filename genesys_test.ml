
open Genesys
open Genesys_selection
open Util

let stat_main size tfil iter mut sur cxr log_file =
  let selector = new genesys_selector size 453 MAX in
  let sim = new genesys_simulator tfil selector and gen = ref 0 in
    Sys.set_signal Sys.sigint (Sys.Signal_handle sim#handle_quit);
    Random.self_init ();
    sim#print_sim_info;
    sim#set_scores_file log_file;
    selector#set_mutation_rate mut;
    selector#set_survival_rate sur;
    selector#set_crossover_rate cxr;
    sim#set_scoring true;
    for i=1 to iter do
      sim#evaluate;
      sim#respawn;
      if i mod 100 = 0 then begin 
	ps "Iteration #"; pi i; pn (); flush stdout;
	sim#write_scores_table;
      end
    done;
    sim#write_scores_table;;
    
let main () =
  let size = ref 65536 
  and tfil = ref "muir.txt"
  and iterations = ref 100 
  and mutrate = ref 0.005
  and surrate = ref 0.01
  and cxrrate = ref 0.005
  and logging = ref false 
  and log_file = ref "genesys.log" 
  and dist = ref false in
  let speclist =
    [("-p", Arg.Int (fun x -> size := x), "population size (65536)");
     ("-t", Arg.String (fun x -> tfil := x), "trail file (muir.txt)");
     ("-i", Arg.Int (fun x -> iterations := x), "number of iterations (100)");
     ("-m", Arg.Float (fun x -> mutrate := x), "mutation rate (0.005)");
     ("-s", Arg.Float (fun x -> surrate := x), "survival rate (0.01)");
     ("-c", Arg.Float (fun x -> cxrrate := x), "crossover rate (0.005)");
     ("-o", Arg.String (fun x -> log_file := x), "log file for -l or -d (genesys.log)");
     ("-l", Arg.Set logging, "enable statistics output, GA-only (disabled)");
     ("-d", Arg.Set dist, "perform score distribution random sample (disabled)")]
  and usage_msg = "usage: genesys [-ld] [-p <size>] [-t <file>] [-i <iter>] [-m <mut>] [-s <sur>] [-c <cxr>] [-o <logfile>]" in
    Arg.parse speclist (fun s -> ()) usage_msg;
    if !dist then 
      stat_main !size !tfil !iterations !mutrate !surrate !cxrrate !log_file;
    let selector = new genesys_selector !size 453 MAX in
    let sim = new genesys_simulator !tfil selector and gen = ref 0 in
      Sys.set_signal Sys.sigint (Sys.Signal_handle sim#handle_quit);
      Random.self_init ();
      sim#print_sim_info;
      if !logging then begin
	sim#logging_on;
	sim#set_log_file !log_file;
      end;
      selector#set_mutation_rate !mutrate;
      selector#set_survival_rate !surrate;
      selector#set_crossover_rate !cxrrate;
      sim#evaluate;
      sim#print_best !gen;
      let the_best = ref (sim#get_best_score) in
	while !gen < !iterations do
	  gen := !gen + 1;
	  sim#new_generation;
	  sim#evaluate;
	  if !gen mod 100 = 0 then begin ps "Generation #"; pi !gen; pn (); 
	    flush stdout end;
	  if sim#get_best_score > !the_best then begin
	    the_best := sim#get_best_score;
	    sim#print_best !gen;
	  end;
	done;;

main ()
