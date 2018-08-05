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

  val is_valid: AlphSet.Elt.t altFormula -> AlphSet.t -> bool
             
  module type TOBEUCHI' = sig
    module BeuchiStateSet: Set.S with type Elt.t = StateSet.t * StateSet.t
    module Beuchi: Beuchi.BEUCHI
           with module StateSet = BeuchiStateSet
           with module AlphSet = AlphSet
                                    
    val convert: t -> Beuchi.t
  end
  module type TOBEUCHI = functor (BeuchiStateSet: Set.S with type Elt.t = StateSet.t * StateSet.t)
                                   (Beuchi: Beuchi.BEUCHI with module StateSet = BeuchiStateSet
                                    with module AlphSet = AlphSet) ->
                         TOBEUCHI'
                         with module BeuchiStateSet = BeuchiStateSet
                         with module Beuchi = Beuchi

  module ToBeuchi: TOBEUCHI
end

module type MAKE = functor (AlphSet: Set.S) (StateSet: Set.S) -> ABA 
                                                                 with module AlphSet = AlphSet
                                                                 with module StateSet = StateSet

module Make : MAKE

