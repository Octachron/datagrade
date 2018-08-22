(** {1 DSL type system} *)
type _ typ =
    Int : (int * int array) typ
  | Float : (float * Float.Array.t) typ
  | Bool : (bool * bool array) typ
  | String : (string * string array) typ
  | Char : (char * bytes) typ
type dyntyp = Dyn : 'a typ -> dyntyp [@@unboxed]
exception Type_error


type ('a, 'b) eq = Refl : ('a, 'a) eq
val ( === ) : 'a typ -> 'b typ -> ('a, 'b) eq

(** Packed array *)
module Dynarray :
  sig
    type t = A : ('a * 'b) typ * 'b -> t
    val pack : ('a * 'b) typ -> 'b -> t
    val unpack : ('a * 'b) typ -> t -> 'b
    val len : t -> int
    val map : ('a * 'b) typ -> ('a -> 'c) -> 'b -> 'c array
    val make : ('a * 'b) typ -> int -> 'a -> 'b
    val create : ('a * 'b) typ -> int -> 'b
    val init : ('a * 'b) typ -> int -> (int -> 'a) -> 'b
    val getter : ('a * 'b) typ -> 'b -> int -> 'a
    val setter : ('a * 'b) typ -> 'b -> int -> 'a -> unit
    val concat : t -> t -> t
    val ( .%() ) : t -> ('a * 'b) typ * int -> 'a
    val ( .%()<- ) : t -> ('a * 'b) typ * int -> 'a -> unit
  end

(** Packed element *)
type packed =
  | I of int
  | F of float
  | C of char
  | B of bool
  | S of string


val pack : ('a * 'b) typ -> 'a -> packed
val unpack : ('a * 'b) typ -> packed -> 'a


(** {1 Dynamic frame } *)
module Frame :
  sig
    type t
    type key = string
    val empty : t
    val ( .%[] ) : t -> key -> Dynarray.t
    val ( .%() ) : t -> key * int -> packed
    val ( .%()<- ) : t -> key * int -> packed -> unit
    val ( .|[] ) : t -> key -> packed array
    val ( .--[] ) : t -> int -> (key * packed) list
    val add : key -> Dynarray.t -> t -> t
    val hcat : t -> t -> t
    val vcat : t -> t -> t
  end


(** Typed keys *)
type 'a key
val key : string -> 'a typ -> 'a key
val ( $: ) : string -> 'a typ -> 'a key

(** A row is an heterogeneous list of values  *)
module Row : Hlist.s' with type 'a x = 'a

(** A header is a heterogeneous list of keys *)
module Header : Hlist.s2 with type ('a,'b) x = ('a * 'b) key


(** A header is a heterogeneous list of pair string * type *)
module Spec : Hlist.s2 with type ('a,'b) x = string * ('a * 'b) typ

(** Typed description of columns *)
module Col :
  sig
    val keytype : 'a key -> dyntyp
    val dyn : 'a typ -> dyntyp
    val typ : Frame.key -> Frame.t -> dyntyp
    val check : 'a key -> Frame.t -> unit
    val ckey : string -> 'a typ -> Frame.t -> 'a key
    val ( .%() ) : Frame.t -> ('a * 'b) key * int -> 'a
    val ( .%()<- ) :
      Frame.t -> ('a * 'b) key * int -> 'a -> unit
    val ( .|[] ) : Frame.t -> ('a * 'b) key -> 'b
    val add :
      ('a * 'b) key -> 'b -> Frame.t -> Frame.t
    val add_many : Frame.t -> ('a, 'b) Spec.t -> 'b Row.t -> Frame.t
    val indices :
      ('a, 'b) Spec.t -> Frame.t -> ('a, 'b) Header.t
    val header : ('a, 'b) Spec.t -> ('a, 'b) Header.t
  end

(** {1 Fully typed table} *)
module Typed :
  sig
    type ('a, 'b) t = { header : ('a, 'b) Header.t; frame : Frame.t; }
    val check : Frame.t -> ('a, 'b) Header.t -> unit
    val promote : ('a, 'b) Header.t -> Frame.t -> ('a, 'b) t
    val get_row : ('a, 'b) t -> int -> 'a Row.t
    val ( .%() ) : ('a, 'b) t -> int -> 'a Row.t
    val add_row : ('a, 'b) t -> 'a Row.t -> ('a, 'b) t
    val blit_row : ('a, 'b) t -> int -> 'a Row.t -> unit
    val untype : ('a, 'b) t -> Frame.t
    val columns : ('a, 'b) Spec.t -> 'b Row.t -> ('a, 'b) t
    val rows : ('a, 'b) Spec.t -> 'a Row.t list -> ('a, 'b) t
  end


type 'c case
val case : ('a, 'b) Spec.t -> (('a, 'b) Typed.t -> 'c) -> 'c case
val ( => ) : ('a, 'b) Spec.t -> (('a, 'b) Typed.t -> 'c) -> 'c case

exception Match_failure

val match' :
  Frame.t -> 'a case list -> (Frame.t -> 'a) -> 'a
