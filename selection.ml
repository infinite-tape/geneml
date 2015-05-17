
open Population
open Chromosome
open Bigarray
open Util

class basic_selector size length goal =
object(self)
  inherit population size length goal
  val mutable survival_rate = 0.10
  val mutable mutation_rate = 0.01
  method set_survival_rate x = survival_rate <- x
  method set_mutation_rate x = mutation_rate <- x
  method get_num_gens = generation_number

  method select = 
    self#roulette_wheel_selection;
    generation_number <- generation_number + 1

  method find_parent (p: binary_chromosome list) partsum random_goal =
    match p with
	m::tl -> let s = m#get_score in 
	  if (partsum +. s) >= random_goal then m
	  else self#find_parent tl (partsum +. s) random_goal
      | [] -> self#find_parent population partsum random_goal

  method calc_survivors =
    (* handle the case with very small populations where survival rate
       produces < 1 survivor *)
    int_of_float (if float_of_int size *. survival_rate > 1.0 
		  then float_of_int size *. survival_rate else 2.0)

  method generate_parents =
    (* generate a list of parents based on survival rate for this
       population *)
    let parents = ref [] 
    and sumfitness = List.fold_left 
		       (fun x y -> x +. y#get_score) 0.0 population in
      for i = 1 to self#calc_survivors do
	let p = self#find_parent population 0.0 (Random.float sumfitness) in
	  parents := p :: !parents;
      done;
      !parents

  (* generate a new population using a simple roulette wheel *)
  method roulette_wheel_selection =
    (* select one parent from the population using partial sums
       and a randomly selected goal *)
    let next_gen = ref [] in
      (* always keep the best member in the current population *)
      next_gen := self#find_best :: !next_gen;
      let parent_list = self#generate_parents in
	next_gen := parent_list @ !next_gen;
	for i = 1 to int_of_float (ceil (float_of_int size /. 2.0)) do
	  (* select two random parents from our parent list and mate them *)
	  let p1 = List.nth parent_list (Random.int self#calc_survivors)
	  and p2 = List.nth parent_list (Random.int self#calc_survivors) in

	  let c1, c2 = self#one_point_crossover p1 p2 in
	    (* every crossover returns two children, we use 
	       both if it won't exceed our population size *)
	    if (Random.float 1.0) < mutation_rate then (* mutation *)
	      self#bitflip_mutate c1;
	    next_gen := [c1] @ !next_gen;
	    
	    if (Random.float 1.0) < mutation_rate then
	      self#bitflip_mutate c2;

	    next_gen := [c2] @ !next_gen;
	done;
	population <- !next_gen
	  
  method one_point_crossover (chrom1: binary_chromosome) (chrom2: binary_chromosome) =
    let gene1 = chrom1#get_chrom_string
    and gene2 = chrom2#get_chrom_string
    and length = chrom1#get_length in

    let split = Random.int (length - 1) 
    and child1 = Array1.create int8_unsigned c_layout length
    and child2 = Array1.create int8_unsigned c_layout length in
      for i=0 to split do
	child1.{i} <- gene1.{i};
	child2.{i} <- gene2.{i}
      done;
      for i=split to length - 1 do
	child1.{i} <- gene2.{i};
	child2.{i} <- gene1.{i}
      done;
      
      let o1 = new binary_chromosome length 
      and o2 = new binary_chromosome length in
	o1#set_chrom_string child1;
	o2#set_chrom_string child2;
	o1, o2

  method bitflip_mutate (chrom: binary_chromosome) =
    let gene = chrom#get_chrom_string in
    let i = Random.int (Array1.dim gene) in
      if gene.{i} = 1 then gene.{i} <- 0
      else gene.{i} <- 1;
      chrom#set_chrom_string gene  
end
