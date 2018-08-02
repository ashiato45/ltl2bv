open Core
open Set

type 'a altFormula = AFTrue
                   | AFFalse
                   | AFAnd
                   | AFOr
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

module Make : MAKE
