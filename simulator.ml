

open Population
open Chromosome
open Selection
open Util

class ['a] basic_simulator (select: 'a) =
object(self)
  val selection_system = select
	
  (* generic fitness function that scores bitstrings 
     as the integer they represent - overload in child classes *)
  method fitness_func chrom_string = 
    let score = ref 0 and pof2 = ref 1 and len = selection_system#get_length in
      for i=0 to len - 1 do
	if chrom_string.{(len-1) - i} = 1 then begin
	  score := !score + !pof2;
	  pof2 := !pof2 * 2;
	end
	else pof2 := !pof2 * 2
      done;
      float_of_int !score 

  method logging_on = selection_system#set_collect_stats true
  method logging_off = selection_system#set_collect_stats false
  method set_log_file f = selection_system#set_stats_file f
  method evaluate = selection_system#evaluate_population self#fitness_func;
  method new_generation = selection_system#select
  method respawn = selection_system#respawn
  method print_chromosomes = selection_system#print_chromosomes

  method get_best_score =
    let m = selection_system#find_best in m#get_score
				      
  method print_best gen =
    let m = selection_system#find_best in
      m#print_gene_string; 
      Printf.printf " scores: %f %d\n" (m#get_score) gen;
      flush stdout
end
