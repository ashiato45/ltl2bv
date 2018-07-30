open Core
open Set

type 'a altFormula = AFTrue
                   | AFFalse
                   | AFAnd
                   | AFOr
module type ABA = sig
  type ast
  type sst
  type aset
  type sset
  type t = {
      alph: ast;
      states: sst;
      trans: sset -> aset -> sset altFormula;
      init: sset;
      final: sst}
end

module type MAKE = functor (AlphSet: Set.S) (StateSet: Set.S) -> ABA 
                                                                 with type ast = AlphSet.t 
                                                                 with type sst = StateSet.t
                                                                 with type aset = AlphSet.Elt.t
                                                                 with type sset = StateSet.Elt.t

module Make = (functor (AlphSet: Set.S) (StateSet: Set.S) -> struct
                 type ast = AlphSet.t
                 type sst = StateSet.t
                 type aset = AlphSet.Elt.t
                 type sset = StateSet.Elt.t
                 type t = {
                     alph: AlphSet.t;
                     states: StateSet.t;
                     trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.Elt.t altFormula;
                     init: StateSet.Elt.t;
                     final: StateSet.t}
                            end: MAKE)
