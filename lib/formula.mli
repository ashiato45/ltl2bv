type formula = FAtomic of int
             | FAnd of formula*formula
             | FNot of formula
             | FNext of formula
             | FUntil of formula*formula
                                   [@@deriving compare, sexp]

val parse: string -> formula option
val get_subformulae: formula -> formula list
val reduce_doublenegs: formula -> formula
