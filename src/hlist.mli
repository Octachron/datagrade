type ('a, 'b) pair = 'a -> 'b
module type s =
  sig
    type 'a x
    type _ t =
        (::) : 'a x * ('b * 'tail) t -> (('a, 'b) pair * 'tail) t
      | [] : ('elt * 'elt) t
  end
type empty = Empty
module type s' =
  sig
    type 'a x
    type _ t = (::) : 'a x * 'b t -> ('a, 'b) pair t | [] : empty t
  end
module type s2 =
  sig
    type ('a, 'b) x
    type (_, _) t =
        (::) : ('a, 'b) x * ('c, 'd) t -> (('a, 'c) pair, ('b, 'd) pair) t
      | [] : (empty, empty) t
  end
module type arg = sig type 'a t end
module Make :
  functor (T : arg) ->
    sig
      type 'a x = 'a T.t
      type _ t =
          (::) : 'a x * ('b * 'tail) t -> (('a, 'b) pair * 'tail) t
        | [] : ('elt * 'elt) t
      type 'b map = { map : 'a. 'a x -> 'b; }
      type proj = { map : 'a. 'a x -> 'a; }
      type bin = { map : 'a. 'a x -> 'a x -> 'a x; }
      val length : 'a t -> int
      val map : 'c map -> 'a t -> 'c list
      val fold_map : 'c map -> ('d -> 'c -> 'd) -> 'a t -> 'd -> 'd
      val append : ('a * 'b) t -> ('b * 'c) t -> ('a * 'c) t
      val ( @ ) : ('a * 'b) t -> ('b * 'c) t -> ('a * 'c) t
      val tail : (('a, 'b) pair * 'c) t -> ('b * 'c) t
      val head : (('a, 'b) pair * 'c) t -> 'a x
      val map2 : bin -> 'a t -> 'a t -> 'a t
    end
module MakeS :
  functor (T : arg) ->
    sig
      type 'a x = 'a T.t
      type _ t = (::) : 'a x * 'b t -> ('a, 'b) pair t | [] : empty t
      type 'b map = { map : 'a. 'a x -> 'b; }
      type bin = { map : 'a. 'a x -> 'a x -> 'a x; }
      val length : 'a t -> int
      val map : 'c map -> 'a t -> 'c list
      val fold_map : 'c map -> ('d -> 'c -> 'd) -> 'a t -> 'd -> 'd
      val tail : ('a, 'b) pair t -> 'b t
      val head : ('a, 'b) pair t -> 'a x
      val map2 : bin -> 'a t -> 'a t -> 'a t
    end
module Make2 :
  functor (T : sig type ('a, 'b) t end) ->
    sig
      type ('a, 'b) x = ('a, 'b) T.t
      type (_, _) t =
          (::) : ('a, 'b) x * ('c, 'd) t -> (('a, 'c) pair, ('b, 'd) pair) t
        | [] : (empty, empty) t
      type 'a map = { map : 'c 'b. ('c, 'b) x -> 'a; }
      type bin = { map : 'a 'b. ('a, 'b) x -> ('a, 'b) x -> ('a, 'b) x; }
      val length : ('a, 'b) t -> int
      val map : 'c map -> ('a, 'b) t -> 'c list
      val fold_map : 'c map -> ('d -> 'c -> 'd) -> ('a, 'b) t -> 'd -> 'd
      val tail : (('a, 'b) pair, ('c, 'd) pair) t -> ('b, 'd) t
      val head : (('a, 'b) pair, ('c, 'd) pair) t -> ('a, 'c) x
      val map2 : bin -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    end
module Cross :
  functor (L1 : s) (L2 : s) ->
    sig
      type t = { f : 'a. 'a L1.x -> 'a L2.x; }
      val map : t -> 'a L1.t -> 'a L2.t
    end
module Cross2 :
  functor (L1 : s2) (L2 : s2) ->
    sig
      type t = { f : 'a 'b. ('a, 'b) L1.x -> ('a, 'b) L2.x; }
      val map : t -> ('a, 'b) L1.t -> ('a, 'b) L2.t
    end
module Cross21 :
  functor (L1 : s2) (L2 : s') ->
    sig
      type fst = { f : 'a 'b. ('a, 'b) L1.x -> 'a L2.x; }
      type 'r bin = { f : 'a 'b. ('a, 'b) L1.x -> 'a L2.x -> 'r; }
      val fold_map :
        'r bin ->
        ('acc -> 'r -> 'acc) -> 'acc -> ('a, 'b) L1.t -> 'a L2.t -> 'acc
      val to_list : 'a bin -> 'a list -> ('b, 'c) L1.t -> 'b L2.t -> 'a list
      val iter : unit bin -> ('a, 'b) L1.t -> 'a L2.t -> unit
      val map : fst -> ('a, 'b) L1.t -> 'a L2.t
    end
