open Core
open Set


type formula = FAtomic of int
             | FAnd of formula*formula
             | FNot of formula
             | FNext of formula
             | FUntil of formula*formula
                                   [@@deriving compare, sexp]

(* module FormulaSet: Set.S (\* OK *\) *)
module AlphSet: Set.S with type Elt.t = int
module FormulaSet: Set.S with type Elt.t = formula 
(* module FormulaSet: Set.S with type Elt.t = int | does not work! *)

val parse: string -> formula option
val get_subformulae: formula -> formula list
val reduce_doublenegs: formula -> formula
val gen_states_for_aba: formula -> FormulaSet.t

(* module type LtlAba = (Aba.MAKE (AlphSet) (StateSet)) *)
module type LTLABA = Aba.ABA
  with module AlphSet = AlphSet
  with module StateSet = FormulaSet

module LtlAba : LTLABA



