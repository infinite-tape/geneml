
open Bigarray
open Util

let rec assoc e a =
  match a with
      [] -> raise Not_found
    | (state, next, output)::rest -> 
	if state=e then (next, output) else assoc e rest

let chomp_n_bits n bnum =
  let len = Array1.dim bnum in
  let num = ref 0 and pof2 = ref 1 in
    for i=0 to n-1 do
      num := !num + (bnum.{(n-1)-i} * !pof2);
      pof2 := !pof2 * 2;
    done;
    !num, (Array1.sub bnum n (len-n))
and chomp_action bnum =
  let len = Array1.dim bnum 
  and num = ref 0 and pof2 = ref 1 in
    for i=0 to 1 do
      num := !num + (bnum.{1-i} * !pof2);
      pof2 := !pof2 * 2;
    done;
    let new_array = if len >= 2 then
      Array1.sub bnum 2 (len - 2)
    else Array1.create int8_unsigned c_layout 0 in
    match !num with
	0 -> NOOP, new_array
      | 1 -> TURN_LEFT, new_array
      | 2 -> TURN_RIGHT, new_array
      | 3 -> GO_FORWARD, new_array
      | _ -> begin print_string "ERROR! Bad input to automaton!\n";
	  raise Not_found end
and first_bit_empty bnum =
  if Array1.dim bnum = 0 then true else false
    
class antFSA chrom_string =
object(self)
  (* delta function: ((state, input), newstate, output) *)
  val mutable delta = []
  val mutable bitstring = chrom_string
  val mutable moves = 0
  val mutable startState = 0
  val mutable currentState = (0, NOOP)
  val mutable currentDirection = EAST

  method get_direction = currentDirection

  method set_direction d = currentDirection <- d

  method automaton (input: trail_square) = 
    currentState <- assoc (fst currentState, input) delta;
    moves <- moves + 1;
    snd currentState
      
  method print_action a = match a with
      NOOP -> ps " NOOP ";
    | GO_FORWARD -> ps " FWD ";
    | TURN_LEFT -> ps " LEFT ";
    | TURN_RIGHT -> ps " RGHT ";
    | ERROR -> ps " ERR ";
  method print_input i = match i with
      Trail -> ps " T "
    | NoTrail -> ps " N "
  method print_state ((n, i), nx, o) =
    ps "("; pi n; ps ", "; self#print_input i; ps ") -> "; pi nx; self#print_action o; pn ()
  
  method print_info =
    ps "Start state: "; pi startState; pn ();
    ps "State transition table: \n";
    List.iter self#print_state delta

  method make_delta_from_bits =
    let bits = ref bitstring in
    let n, b = chomp_n_bits 5 !bits in
      bits := b;
      startState <- n;
      let stateNum = ref 0 in
	while not (first_bit_empty !bits) do
	  let n, b = chomp_n_bits 5 !bits in
	    bits := b;
	    let a, b = chomp_action !bits in
	      bits := b;
	      delta <- ((!stateNum, NoTrail), n, a) :: delta;
	      let n, b = chomp_n_bits 5 !bits in
		bits := b;
		let a, b = chomp_action !bits in
		  bits := b;
		  delta <- ((!stateNum, Trail), n, a) :: delta;
		  stateNum := !stateNum + 1;
	done;
	currentState <- (startState, NOOP);
      
  initializer self#make_delta_from_bits
end
