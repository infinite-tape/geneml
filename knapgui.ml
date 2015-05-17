
open Tk
open Knapsack

class knapman ~parent = object(self)
  val frame = Frame.create parent
  val items = load_items "knapsack-items"
  val mutable simulator = new zero_one_knapsack_simulator items 65000 500
end

let _ = 

let top = openTk () in 
(* Title setting *)
  Wm.title_set top "Zero-One Knapsack Solver";

(* Base frame *)
let base = Frame.create top in
  pack [base];

(* Menu bar *)
let bar = Frame.create ~borderwidth:2 ~relief:`Raised  base in 
  pack ~fill:`X [bar];
  let meb = Menubutton.create ~text:"File" bar in
  let men = Menu.create meb in
    Menu.add_command ~label:"Quit" ~command:(fun () -> closeTk (); exit 0) men;
    Menubutton.configure ~menu:men meb;
  let base2 = Frame.create base in
    pack [base2];
  let but = Button.create ~text:"Start Simulation" base2 in
  let lab = Label.create ~text:"Population size:" base2 in
  let che = Entry.create ~width:5 base2 in
    Entry.insert che ~index:`End ~text:"65000";
  let sr = Scale.create ~label:"Survival rate" ~resolution:0.01 ~min:0.0 ~max:1.0 ~digits:3 ~orient:`Horizontal ~length:100 ~showvalue:true base2 in
    Scale.set sr 0.15;
  let mr = Scale.create ~label:"Mutation rate" ~resolution:0.01 ~min:0.0 ~max:1.0 ~digits:3 ~orient:`Horizontal ~length:100 ~showvalue:true base2 in
    Scale.set mr 0.10;
    pack ~side:`Left [meb];
    pack [coe lab; coe che; coe but; coe sr; coe mr];


Printexc.print mainLoop ()
