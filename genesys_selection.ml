
open Population
open Chromosome
open Bigarray
open Selection
open Util

class genesys_selector size length goal =
object(self)
  inherit basic_selector size length goal
  method select = 
    self#genesys_select;
    generation_number <- generation_number + 1
    
  val mutable crossover_rate = ((Random.float 0.005) +. 0.005)
  method set_crossover_rate x = crossover_rate <- x
  initializer survival_rate <- 0.10
  initializer mutation_rate <- ((Random.float 0.01) +. 0.001)

  method genesys_select = 
    let parents = self#generate_parents in
    let p1 = ref (List.hd parents) 
    and p2 = ref (List.hd parents)
    and next_gen = ref [] 
    and offspring = ref (List.hd parents) in
      for i=1 to size do
	p1 := List.nth parents (Random.int self#calc_survivors);
	p2 := List.nth parents (Random.int self#calc_survivors);
	offspring := self#recombination !p1 !p2;
	offspring := self#point_mutation !offspring;
	next_gen :=  !offspring :: !next_gen;
      done;
      population <- !next_gen;
      
  method generate_parents =
    self#sort_population;
    let pop = Array.of_list population 
    and parents = ref [] and num = self#calc_survivors in
      for i=0 to num - 1 do
	parents := pop.(i) :: !parents
      done;
      !parents

  method point_mutation member =
    let chrom = member#get_chrom_string in
      for i=0 to (length - 1) do
	if (Random.float 1.0) <= mutation_rate then
	  if chrom.{i} = 1 then chrom.{i} <- 0 else chrom.{i} <- 1
      done;
      member#set_chrom_string chrom;
      member
	
  method recombination p1 p2 =
    let g1 = p1#get_chrom_string and g2 = p2#get_chrom_string in
    let c = Array.make length 0 in
    let current = ref g1 and next = ref g2 and temp = ref g1 in
      for i=0 to (length - 1) do
	if (Random.float 1.0) <= crossover_rate then 
	  begin
	    temp := !current;
	    current := !next;
	    next := !temp
	  end;
	c.(i) <- !current.{i};
      done;
      let chrom = Array1.of_array int8_unsigned c_layout c
      and child = new binary_chromosome length in
	child#set_chrom_string chrom;
	child
end
