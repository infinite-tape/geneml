
open Simulator
open Population
open Chromosome
open Selection
open Genesys_selection
open Util
open Ant

class ['a] genesys_simulator filename (select: 'a) =
object(self)
  inherit ['a] basic_simulator select

  val mutable trail_file = filename
  val mutable ant_trail = Array.make 0 NoTrail
  val mutable gridx = 0
  val mutable gridy = 0
  val mutable trail_length = 0
  val mutable num_of_evals = 0
  val scores_table = Hashtbl.create 89
  val mutable scoring = false
  val mutable scores_file = "scores-data"

  initializer ant_trail <- self#parse_map trail_file
  method set_scoring x = scoring <- x
  method set_scores_file f = scores_file <- f

  method new_position (x, y) dir =
    match dir with
	EAST -> if x+1 < gridx then (x+1, y) else (0, y)
      | WEST -> if x-1 >= 0 then (x-1, y) else (gridx - 1, y)
      | NORTH -> if y-1 >= 0 then (x, y-1) else (x, gridy - 1)
      | SOUTH -> if y+1 < gridy then (x, y+1) else (x, 0)

  method left_turn dir =
    match dir with
	EAST -> NORTH
      | WEST -> SOUTH
      | NORTH -> WEST
      | SOUTH -> EAST

  method right_turn dir =
    match dir with
	EAST -> SOUTH
      | WEST -> NORTH
      | NORTH -> EAST
      | SOUTH -> WEST

  method observe_trail (x, y) dir  =
    let coordx, coordy = self#new_position (x, y) dir in
      ant_trail.(coordx + (gridx*coordy))
	
  method parse_map fn =
    let ic = open_in fn in
    let sizex, sizey = Scanf.fscanf ic "%d %d" (fun x y -> x, y) in
    let grid = Array.make (sizex*sizey) NoTrail in
      ignore (input_char ic);
      gridx <- sizex; gridy <- sizey;
      for i=0 to sizey do
	for c=0 to sizex do
	  try let char = input_char ic in
	    if char = '1' then begin
	      grid.(c + i*sizex) <- Trail;
	      trail_length <- trail_length + 1;
	    end
	    else if char = '0' then grid.(c + i*sizex) <- NoTrail
	  with End_of_file -> ()
	done;
      done;
      grid

(* load_trail:
   receives: filename: string
   description: loads a trail from a plaintext file with 0's and 1's representing
                the trail and its on/off positions *)
  method load_trail filename =
    let ic = open_in filename in
    let sizex, sizey = Scanf.fscanf ic "%d %d" (fun x y -> x, y) in
      if is_power_of_2 (float_of_int sizex) && 
	is_power_of_2 (float_of_int sizey) then
	  let grid = Array.make (sizex*sizey) NoTrail in
	    gridx <- sizex; gridy <- sizey;
	    for i=0 to (sizex*sizey - 1) do
	      try let num = Scanf.fscanf ic " %d " (fun x -> x) in
		if num = 0 then grid.(i) <- NoTrail;
		if num = 1 then begin 
		  grid.(i) <- Trail; trail_length <- trail_length + 1
		end
	      with End_of_file -> ()
	    done;

	    grid
      else begin
	print_string "Error: dimensions not a power of two!"; 
	Array.make 0 NoTrail; end

  (* evaluate:
     iterates the fitness function across the entire population *)
  method evaluate =
    selection_system#evaluate_population self#fitness_func;
    if scoring then
      self#collect_scores
	
  (* fitness_func:
     Receives: bigarray  (chromosome string)
     Returns: float (the score)
  *)
  method fitness_func chrom_string =
    let pos = ref (0, 0)
    and this_ant = new antFSA chrom_string
    and step = ref 0 and score = ref 0 and this_trail = Array.copy ant_trail in
      while !step < 200 do 
	let sensor_input = self#observe_trail !pos this_ant#get_direction in
	let action = this_ant#automaton sensor_input in
	  if action = GO_FORWARD then begin
	    pos := self#new_position !pos this_ant#get_direction end;
	  if action = TURN_LEFT then begin
	    this_ant#set_direction (self#left_turn this_ant#get_direction) end;
	  if action = TURN_RIGHT then begin
	    this_ant#set_direction (self#right_turn this_ant#get_direction) end;
	  let x = fst !pos and y = snd !pos in
	    if this_trail.(x + y*gridx) = Trail then begin
	      score := !score + 1;
	      this_trail.(x + y*gridx) <- NoTrail
	    end;
	    step := !step + 1;
      done;
      float_of_int !score

  method execute_ant chrom_string =
    let pos = ref (0, 0)
    and this_ant = new antFSA chrom_string
    and step = ref 0 and score = ref 0 and this_trail = Array.copy ant_trail in
      while !step < 200 do 
	step := !step + 1;
	let sensor_input = self#observe_trail !pos this_ant#get_direction in
	let action = this_ant#automaton sensor_input in
	  if action = GO_FORWARD then begin
	    pos := self#new_position !pos this_ant#get_direction end;
	  if action = TURN_LEFT then begin
	    this_ant#set_direction (self#left_turn this_ant#get_direction) end;
	  if action = TURN_RIGHT then begin
	    this_ant#set_direction (self#right_turn this_ant#get_direction) end;
	  self#print_trail_with_ant !pos;
	  let x = fst !pos and y = snd !pos in
	    if this_trail.(x + y*gridx) = Trail then begin
	      score := !score + 1;
	      this_trail.(x + y*gridx) <- NoTrail
	    end;
      done;
      float_of_int !score

  method print_trail_with_ant pos =
    for i=0 to (gridy-1) do
      for c=0 to (gridx-1) do
	let sqr = ant_trail.(c + i*gridx) in
	  if fst pos = c && snd pos = i then
	    print_string "#"
	  else
	    if sqr = Trail then print_string ":"
	    else print_string "\""
      done;
      print_newline ();
    done;

  method print_trail =
    for i=0 to (gridy-1) do
      for c=0 to (gridx-1) do
	let sqr = ant_trail.(c + i*gridx) in
	  if sqr = Trail then print_string ":" 
	  else print_string "\""
      done;
      print_newline ();
    done;

  method collect_scores =
    let scores = selection_system#get_scores in
    let add_score s =
      try let amt = Hashtbl.find scores_table s in
	Hashtbl.replace scores_table s (amt+1)
      with Not_found ->
	Hashtbl.add scores_table s 1;
    in
      List.iter add_score scores;
      
  method write_scores_table =
    let oc = 
      open_out_gen [Open_wronly; Open_trunc; Open_creat;] 0o755 scores_file in
    let write_table_entry scr num =
      Printf.fprintf oc "%d\t%d\n" (int_of_float scr) num
    in
      Printf.fprintf oc "Scores Table\n--------------\n";
      Hashtbl.iter write_table_entry scores_table;
      close_out oc

  method print_sim_info =
    ps "Trail filename: \""; ps trail_file; ps "\"\n";
    ps "Trail length: "; pi trail_length; pn ();
    ps "Total grid squares: "; pi (Array.length ant_trail); pn ();
    self#print_trail

  method print_statistics = 
    print_string "Number of generations: ";
    print_int selection_system#get_num_gens; print_newline ()

  method handle_quit (n: int) = 
    self#print_statistics; 
    if scoring then
      self#write_scores_table;
    ignore (exit 0)
end
