open Core
open Set


type formula = FAtomic of int
             | FAnd of formula*formula
             | FNot of formula
             | FNext of formula
             | FUntil of formula*formula
                                   [@@deriving compare, sexp]

(* module FormulaSet: Set.S (\* OK *\) *)
module PropSet: Set.S with type Elt.t = int
module AlphSet: Set.S with type Elt.t = PropSet.t
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

val formula_to_aba: formula -> LtlAba.t

module type BEUCHISTATE = sig
  type t = FormulaSet.t * FormulaSet.t
  val sexp_of_t: t -> Sexp.t
  val t_of_sexp: Sexp.t -> t
end
module BeuchiState: BEUCHISTATE
module BeuchiStateSet: Set.S with type Elt.t = FormulaSet.t * FormulaSet.t
module Beuchi: Beuchi.BEUCHI with module StateSet = BeuchiStateSet
       with module AlphSet = AlphSet
module ToBeuchi: LtlAba.TOBEUCHI'
       with module BeuchiStateSet = BeuchiStateSet
       with module Beuchi = Beuchi

val aba_to_beuchi: LtlAba.t -> Beuchi.t
val formula_to_beuchi: formula -> Beuchi.t                                
