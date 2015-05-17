
open Chromosome
open Util
open Bigarray

class population size length goal =
  let init_pop =
    let tmp = ref [] in
      for i = 1 to size do
	let random = new binary_chromosome length in
	  random#randomize;
	  tmp := random :: !tmp
      done;
      !tmp
  in
object(self)
  val mutable population = init_pop
  val size = size
  val len = length
  val goal = goal
  val mutable generation_number = 0
  val mutable pop_stats = (0.0, 0.0, 0.0, 0.0)
  val mutable collect_stats = false
  val mutable stats_file = "pop-stats"
  method get_length = len
  method get_size = size
  method set_population x = population <- x
  method get_population = population
  method get_scores = List.rev_map (fun x -> x#get_score) population
  method set_collect_stats b = collect_stats <- b
  method set_stats_file f = stats_file <- f

  method calc_pop_stats =
    let raw_scores = self#get_scores and peak = ref 0.0 in
      if goal = MAX then
	peak := List.fold_left 
	  (fun x a -> if x > a then x else a) 0.0 raw_scores
      else
	peak := List.fold_left 
	  (fun x a -> if x < a then x else a) (List.hd raw_scores) raw_scores;
      let modetbl = Hashtbl.create 100 in
	List.iter 
	  (fun x -> if Hashtbl.mem modetbl x
	   then Hashtbl.replace modetbl x ((Hashtbl.find modetbl x) + 1)
	   else Hashtbl.add modetbl x 1) raw_scores;
	let tmp = ref (0.0, 0) in
	  Hashtbl.iter (fun k v -> if v > (snd !tmp) then tmp := (k, v)) modetbl;
	  let mode = fst !tmp in
	  let sum = List.fold_left (fun x a -> a +. x) 0.0 raw_scores in
	  let mean = sum /. float_of_int (List.length raw_scores) in
	  let sumdifsqr = ref 0.0 in
	    List.iter (fun x -> sumdifsqr := !sumdifsqr +. ((x -. mean) ** 2.0)) raw_scores;
	  let stddev = 
	    sqrt (!sumdifsqr /. float_of_int ((List.length raw_scores) - 1)) in
	    pop_stats <- (!peak, mode, mean, stddev)

  method write_pop_stats = 
    let oc = open_out_gen [Open_append; Open_creat;] 0o755 stats_file in
    let p, md, mn, std = pop_stats in
      Printf.fprintf oc "%d\t%f\t%f\t%f\t%f\n" generation_number p md mn std;
      close_out oc;
	      
  method respawn =   
    let new_pop =
    let tmp = ref [] in
      for i = 1 to size do
	let random = new binary_chromosome length in
	  random#randomize;
	  tmp := random :: !tmp
      done;
      !tmp
    in
      generation_number <- generation_number + 1;
      population <- new_pop;

  method sort_population =
    let member_test m1 m2 =
      let s1 = m1#get_score and s2 = m2#get_score in
	if s1 = s2 then 0 else
	  if s1 > s2 then 1 else -1 
    in
      population <- List.sort member_test population;
      population <- List.rev population

  method evaluate_population f =
    let eval_member m = 
      let chrom_copy = Array1.create int8_unsigned c_layout self#get_length in
	Array1.blit m#get_chrom_string chrom_copy;
	let s = f chrom_copy in m#set_score s 
    in
      List.iter eval_member population;
      if collect_stats then begin
	self#calc_pop_stats;
	self#write_pop_stats
      end
	
  (* find_best:
     return the lowest scoring member of a chromosome list *)
  method find_best =
    let rec best_in_pop (p:binary_chromosome list) best = 
      match p with
	  m::tl ->
	    let s = m#get_score and t = best#get_score in 
	      if goal = MAX then
		if s > t then 
		  best_in_pop tl m
		else best_in_pop tl best
	      else
		if s < t then
		  best_in_pop tl m
		else best_in_pop tl best
	| [] -> best 
    in best_in_pop population (List.hd population)

  method print_chromosomes =
    let print_info c =
      c#print_gene_string; print_string ": "; 
      print_float c#get_score; print_newline ()
    in List.iter print_info population

end
