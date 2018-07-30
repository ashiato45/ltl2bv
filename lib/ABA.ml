open Core
open Set

type 'a altFormula = AFTrue
                   | AFFalse
                   | AFAnd
                   | AFOr

module Make = functor (AlphSet: Set.S) (StateSet: Set.S) -> struct
                     type t = {
                         alph: AlphSet.t;
                         states: StateSet.t;
                         trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.Elt.t altFormula;
                         init: StateSet.Elt.t;
                         final: StateSet.t}

              end
