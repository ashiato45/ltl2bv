open Core
open Set


type 'a altFormula =
  | AFAtomic of 'a
  | AFTrue
  | AFFalse
  | AFAnd of 'a altFormula*'a altFormula
  | AFOr of 'a altFormula*'a altFormula

module type ABA = sig
  module AlphSet: Set.S
  module StateSet: Set.S
  type t = {
      alph: AlphSet.t;
      states: StateSet.t;
      trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.Elt.t altFormula;
      init: StateSet.Elt.t;
      final: StateSet.t}
end

module type MAKE = functor (AlphSet: Set.S) (StateSet: Set.S) -> ABA 
                                                                 with module AlphSet = AlphSet
                                                                 with module StateSet = StateSet

module Make = (functor (AlphSet: Set.S) (StateSet: Set.S) -> struct
                 module AlphSet = AlphSet
                 module StateSet = StateSet
                 type t = {
                     alph: AlphSet.t;
                     states: StateSet.t;
                     trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.Elt.t altFormula;
                     init: StateSet.Elt.t;
                     final: StateSet.t}
                            end: MAKE)
