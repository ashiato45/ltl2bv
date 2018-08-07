open Core
open Set

module type BEUCHI = sig
  module AlphSet: Set.S
  module StateSet: Set.S
  type t = {
      alph: AlphSet.t;
      states: StateSet.t;
      trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.t;
      init: StateSet.t;
      final: StateSet.t}
  val to_dot: t -> string             
end

module type MAKE = functor (AlphSet: Set.S) (StateSet: Set.S) -> BEUCHI
                                                                 with module AlphSet = AlphSet
                                                                 with module StateSet = StateSet

module Make : MAKE
