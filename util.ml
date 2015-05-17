
(* ==========================================================================
   basic print functions
   ========================================================================== *)
let pC () = ()
let pF () = flush stdout
let pn () = print_string "\n"
let pT i  = if (i>0) then for x=1 to i do print_string "  " done else ()
let pnT i = if (i=0) then () else (pn (); pT i)
let ps s  = print_string s
let pc () = print_string ", "
let pi i  = print_string (string_of_int i)
let pf f  = print_string (string_of_float f)
let pb b  = print_string (if b then "true" else "false")

let rec pl_basic fx tab = function
    []      -> ()
  | [h]     -> pnT tab; (fx h)
  | h::l    -> pnT tab; (fx h); ps " ::";
	       (pl_basic (fx) tab l)

let pl_string tab l = (pl_basic (ps)  tab l)
and pl_int    tab l = (pl_basic (pi)  tab l)
and pl_bool   tab l = (pl_basic (pb)  tab l)
and pl_float  tab l = (pl_basic (pf)  tab l)

type optimize = MAX | MIN
type direction = NORTH | SOUTH | EAST | WEST
type ant_action = GO_FORWARD | TURN_LEFT | TURN_RIGHT | NOOP | ERROR
type trail_square = Trail | NoTrail

let is_power_of_2 num =
  let lb2 = log10 num /. log10 2. in
    if mod_float lb2 2. = 1. then true else false
