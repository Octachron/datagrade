module type s = sig
  type 'a x
  type _ t =
    | (::) : 'a x * ('b * 'tail) t -> (('a * 'b) * 'tail) t
    | []: ('elt*'elt) t
end

type empty = Empty

module type s' = sig
  type 'a x
  type _ t =
    | (::) : 'a x * 'b t -> ('a * 'b) t
    | []: empty t
end

module type s2 = sig
  type ('a,'b) x
  type (_,_) t =
    | (::) : ('a,'b) x * ('c,'d) t -> ('a * 'c, 'b * 'd) t
    | []: (empty, empty) t
end

module Make(T:sig type 'a t end) = struct
  type 'a x = 'a T.t
  type _ t =
    | (::) : 'a x * ('b * 'tail) t -> (('a * 'b) * 'tail) t
    | []: ('elt *'elt) t
  type 'b map = { map: 'a. 'a x -> 'b }
  type proj = { map: 'a. 'a x -> 'a }
  type bin = {map: 'a. 'a x -> 'a x -> 'a x }
  let rec length: type a. a t -> int = function
    | [] -> 0
    | _ :: b -> 1 + length b
  let rec map: type a. 'c map -> a t -> 'c list =
    fun m -> function
      | [] -> []
      | a :: q -> m.map a :: map m q
  let rec fold_map: type a. 'c map -> ('d -> 'c -> 'd) -> a t -> 'd -> 'd =
    fun m f l acc -> match l with
      | [] -> acc
      | a :: q -> fold_map m f q @@ f acc (m.map a)
  let rec append: type a b c. (a * b) t -> (b * c) t -> (a * c)t =
    fun x y -> match x with
      | a :: q -> a :: append q y
      | [] -> y
  let (@) = append
  let tail =function
  | _ :: q -> q
  | _ -> invalid_arg "tail"

  let head =function
    | a :: _ -> a
    | _ -> invalid_arg "head"

  let rec map2: type a. bin -> a t -> a t -> a t  = fun f x y ->
    match x, y with
    | [], _ -> []
    | _, [] -> []
    | a :: q, b :: r -> (f.map a b) :: map2 f q r

end

module MakeS(T:sig type 'a t end) = struct
  type 'a x = 'a T.t
  type _ t =
    | (::) : 'a x * 'b t -> ('a * 'b) t
    | []: empty t
  type 'b map = { map: 'a. 'a x -> 'b }
  type bin = {map: 'a. 'a x -> 'a x -> 'a x }
  let rec length: type a. a t -> int = function
    | [] -> 0
    | _ :: b -> 1 + length b
  let rec map: type a. 'c map -> a t -> 'c list =
    fun m -> function
      | [] -> []
      | a :: q -> m.map a :: map m q
  let rec fold_map: type a. 'c map -> ('d -> 'c -> 'd) -> a t -> 'd -> 'd =
    fun m f l acc -> match l with
      | [] -> acc
      | a :: q -> fold_map m f q @@ f acc (m.map a)

  let tail =function
  | _ :: q -> q

  let head =function
    | a :: _ -> a

  let rec map2: type a. bin -> a t -> a t -> a t  = fun f x y ->
    match x, y with
    | [], _ -> []
    | a :: q, b :: r -> (f.map a b) :: map2 f q r

end

module Make2(T:sig type ('a,'b) t end) = struct
  type ('a,'b) x = ('a,'b) T.t
  type (_,_) t =
    | (::) : ('a,'b) x * ('c,'d) t -> ('a * 'c, 'b * 'd ) t
    | []: (empty , empty) t
  type 'a map = { map: 'c 'b. ('c,'b) x -> 'a }

  type bin = {map: 'a 'b. ('a,'b) x -> ('a,'b) x -> ('a,'b) x }
  let rec length: type a b. (a,b) t -> int = function
    | [] -> 0
    | _ :: b -> 1 + length b
  let rec map: type a b. 'c map -> (a,b) t -> 'c list =
    fun m -> function
      | [] -> []
      | a :: q -> m.map a :: map m q
  let rec fold_map: type a b.
    'c map -> ('d -> 'c -> 'd) -> (a,b) t -> 'd -> 'd =
    fun m f l acc -> match l with
      | [] -> acc
      | a :: q -> fold_map m f q @@ f acc (m.map a)

  let tail =function
  | _ :: q -> q

  let head =function
    | a :: _ -> a

  let rec map2: type a b. bin -> (a,b) t -> (a,b) t ->
    (a,b) t  = fun f x y ->
    match x, y with
    | [], _ -> []
    | a :: q, b :: r -> (f.map a b) :: map2 f q r

end


module Cross(L1:s)(L2:s) = struct
  type t = {f:'a. 'a L1.x -> 'a L2.x}

  let rec map: type a. t -> a L1.t -> a L2.t  =
    fun c -> function
      | [] -> []
      | a :: q -> c.f a :: map c q

end



module Cross2(L1:s2)(L2:s2) = struct
  type t = {f:'a 'b. ('a,'b) L1.x -> ('a,'b) L2.x}

  let rec map: type a b. t -> (a,b) L1.t -> (a,b) L2.t  =
    fun c -> function
      | [] -> []
      | a :: q -> c.f a :: map c q

end

module CrossFst(L1:s2)(L2:s') = struct
 type t = {f:'a 'b. ('a,'b) L1.x -> 'a L2.x}

  let rec map: type a b. t -> (a,b) L1.t -> a L2.t  =
    fun c -> function
      | [] -> []
      | a :: q -> c.f a :: map c q

end
