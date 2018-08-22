type _ typ =
  | Int: (int * int array) typ
  | Float: (float * Float.Array.t) typ
  | Bool: (bool * bool array) typ
  | String: (string * string array) typ
  | Char: (char * bytes) typ

type dyntyp = Dyn: _ typ -> dyntyp [@@unboxed]
exception Type_error
type ('a, 'b) eq = Refl: ('a,'a) eq
let (===) (type a b) (x:a typ) (y: b typ): (a,b) eq =
  match x, y with
    | Int, Int -> Refl
    | Float, Float -> Refl
    | Bool, Bool -> Refl
    | String, String -> Refl
    | _ -> raise Type_error


module Dynarray = struct

  type t = A: ('a * 'b) typ * 'b -> t


  let pack (type a b) (ty: (a * b) typ) (x:b) = A(ty,x)
  let unpack (type a b) (ty:(a * b) typ) (A(ty',x)): b =
    match  ty === ty' with
    | Refl -> x

  let len (A(ty,x)) = match ty with
    | Float -> Float.Array.length x
    | Char -> Bytes.length x
    | Int -> Array.length x
    | Bool -> Array.length x
    | String -> Array.length x

  let map (type a b) (ty:(a*b) typ) (f:a -> 'c) (a:b) =
    match ty with
    | Float -> Array.init (Float.Array.length a)
                 (fun n -> f @@ Float.Array.get a n)
    | Char -> Array.init (Bytes.length a) (fun n -> f @@ Bytes.get a n)
    | Int -> Array.map f a
    | Bool -> Array.map f a
    | String ->Array.map f a


  let float_make n x =
    let a = Float.Array.create n in
    for i = 0 to n - 1 do
      Float.Array.set a i x
    done;
    a

  let float_init n f =
    let a = Float.Array.create n in
    for i = 0 to n - 1 do
      Float.Array.set a i (f i)
    done;
    a

  let make (type a b) (ty:(a*b) typ): int -> a -> b =
    match ty with
    | Float -> float_make
    | Char -> BytesLabels.make
    | Int -> Array.make
    | Bool -> Array.make
    | String ->Array.make

  let create (type a b) (ty:(a*b) typ) n: b =
    match ty with
    | Float -> Array.Floatarray.create n
    | Char -> Bytes.create n
    | Int -> Array.make n 0
    | Bool -> Array.make n false
    | String ->Array.make n ""

  let init (type a b) (ty:(a*b) typ): int -> (int -> a) -> b =
    match ty with
    | Float -> float_init
    | Char -> Bytes.init
    | Int -> Array.init
    | Bool -> Array.init
    | String ->Array.init


  let getter (type a b) (ty:(a*b) typ) (a:b) n: a =
    match ty with
    | Float -> Float.Array.get a n
    | Char -> Bytes.get a n
    | Int -> a.(n)
    | Bool -> a.(n)
    | String -> a.(n)

  let setter (type a b) (ty:(a*b) typ) (a:b) n (x:a) =
    match ty with
    | Float -> Float.Array.set a n x
    | Char -> Bytes.set a n x
    | Int -> a.(n) <- x
    | Bool -> a.(n) <- x
    | String -> a.(n) <- x


  let concat (A(tyx,x) as ax) (A(tyy,y) as ay) =
    match tyx === tyy with
    | Refl ->
      let nx, ny = len ax, len ay in
      let a =
        init tyx (nx + ny)
          (fun i -> if i < nx then
              getter tyx x i
            else
              getter tyy y (i-nx)
          ) in
      A(tyx,a)

  let (.%()) (type a b) (A(wit,array)) ((ty:((a*b) typ)),n): a =
    match ty === wit with
    | Refl -> getter ty array n

  let (.%()<-) (type a b) (A(wit,array)) ((ty:((a*b) typ)),n) (x:a) =
    match ty === wit with
    | Refl -> setter ty array n x
end



type packed =
  | I of int
  | F of float
  | C of char
  | B of bool
  | S of string

let pack (type a b) (ty:(a * b) typ) (x:a) =
  match ty with
  | Int -> I x
  | Float -> F x
  | Char -> C x
  | Bool -> B x
  | String -> S x

let unpack (type a b) (ty:(a * b) typ) x: a =
  match ty, x with
  | Int, I x -> x
  | Float, F x -> x
  | Char, C x -> x
  | Bool, B x -> x
  | String, S x -> x
  | _ -> raise Type_error


module Frame = struct
  module M = Map.Make(String)
  let empty = M.empty
  type key = M.key
  type t = Dynarray.t M.t


  let ( .%[] ) dict name =
    M.find name dict

  let ( .%() ) dict (name,n) =
    let Dynarray.A(ty,x) = dict.%[name] in
    pack ty (Dynarray.getter ty x n)

  let ( .%()<- ) dict (name,n) x =
    let Dynarray.A(ty,a) = dict.%[name] in
    Dynarray.setter ty a n @@ unpack ty x

  let ( .|[] ) dict name =
    let Dynarray.A(ty, a) = dict.%[name] in
    Dynarray.map ty (pack ty) a



  let ( .--[] ) dict n =
    M.fold (fun k (Dynarray.A(ty,a)) acc ->
        let v = pack ty (Dynarray.getter ty a n) in
        (k, v) :: acc) dict []

  let add name col frame = M.add name col frame
  let hcat x = M.union (fun _ _ y -> Some y) x
  let vcat x = M.union (fun _ x y -> Some (Dynarray.concat x y)) x

end

type 'a key = K: string * 'a typ -> 'a key
module Row = Hlist.MakeS(struct type 'c t = 'c end)
module Header = Hlist.Make2(struct type ('a,'b) t = ('a*'b) key end)
module Spec =
  Hlist.Make2(struct type ('a,'b) t = string * ('a*'b) typ end)


module Cross = Hlist.Cross2(Spec)(Header)

let key x ty = K(x,ty)


module Col = struct
  let keytype (K(_,ty)) = Dyn ty
  let dyn x = Dyn x
  let typ name dict =
    let Dynarray.A(ty, _ ) = Frame.M.find name dict in
    Dyn ty

  let check (type a) (K(x,ty): a key) dict =
    let Dynarray.A(tyc, _) = Frame.M.find x dict in
    match ty === tyc with
    | Refl -> ()

  let ckey x ty dict =
    let k = key x ty in
    check k dict;
    k

  let ( .%() ) dict (K(name, ty),n) =
    (Frame.M.find name dict).Dynarray.%(ty,n)

  let ( .%()<- ) dict (K(name, ty),n) x =
    (Frame.M.find name dict).Dynarray.%(ty,n) <- x

  let ( .|[] ) (type a b) dict (K(name, ty):(a * b) key): b =
    let Dynarray.A(tyc, a ) = Frame.M.find name dict in
    match ty === tyc with
    | Refl -> a

  let add (K(name,ty)) col dict =
    Frame.M.add name (Dynarray.A(ty,col)) dict

  let rec add_many: type a b.
    Frame.t -> (a,b) Spec.t -> b Row.t -> Frame.t
    = fun frame specs cols ->
      match specs, cols with
      | [], [] -> frame
      | (name,ty) :: q, b :: r ->
        add_many (add (key name ty) b frame) q r


  let indices spec dict = Cross.map
      {f=(fun (name,ty) -> ckey name ty dict)} spec

  let header spec = Cross.map
      {f=(fun (name,ty) -> key name ty )} spec

end


let ( $: ) x = key x

let int = "integer" $: Int
let en = "en" $: String

let dict =
  Frame.empty
  |> Col.add int [|0;1;2|]
  |> Col.add en [|"0";"1";"2"|]

let x = dict.Col.|[int]


module Typed = struct
  module Cross = Hlist.Cross21(Header)(Row)
  type ('a,'b) t =
    { header:('a,'b) Header.t; frame: Frame.t }

  let rec check: type a b. Frame.t -> (a,b) Header.t -> unit =
    fun frame -> function
    | [] -> ()
    | a :: q -> Col.check a frame; check frame q

  let promote header frame =
    check frame header;
    {header;frame}

  let get_row x n = Cross.map
      { f=(fun k -> x.frame.Col.%(k,n)) } x.header

  let (.%()) x n = get_row x n


  let rec add_row: type a b. (a,b) t -> a Row.t -> (a,b) t =
    fun frame r ->
      let header = frame.header in
      match header, r with
      | [],[] -> {frame with header= [] }
      | K(b,ty):: r, a :: q ->
        let frame = (add_row { frame with header=r} q).frame in
        let col = frame.Frame.%[b] in
        let col = Dynarray.(concat col @@ A(ty,make ty 1 a)) in
        let frame = Frame.add b col frame in
        { frame; header }

  let rec blit_row frame n row =
    Cross.iter {f = (fun k x ->
        frame.frame.Col.%(k,n) <- x
      )} frame.header row

  let rec rows header rows =
      let n = List.length rows in
      let frame = Header.fold_map
          {map=(fun (K(name,ty)) ->
               Frame.add name Dynarray.(A(ty,create ty n))
             )
          }
          (|>)
          header
          Frame.empty in
      let frame = { frame; header} in
      List.iteri (blit_row frame) rows;
      frame


  let untype x = x.frame

  let columns spec cols =
    let header = Col.header spec in
    let frame = Col.add_many Frame.empty spec cols in
    promote header frame

  let rows spec rows =
    let frame = promote (Col.header spec) Frame.empty in
    List.fold_left add_row frame rows
end

type 'c case = Case: ('a,'b) Spec.t *
                     (('a,'b) Typed.t -> 'c) ->
  'c case

let case x y = Case(x,y)
let ( => ) = case

exception Match_failure
let rec match' frame cases default =
  match cases with
  | [] -> default frame
  |  Case(h,f) :: q ->
    match Col.indices h frame with
    | x -> f (Typed.promote x frame)
    | exception Type_error -> match' frame q default


let header: _ Spec.t = [ "int", Int; "string", String]
let f = Typed.columns header
    [
      [| 1; 2; 3 |];
      [| "one"; "two"; "three" |]
    ]

let y = Typed.rows header
    [
      [1; "one"];
      [2; "two"];
      [3; "three"];

    ]

let row = match' (Typed.untype f) [
    ["string", String; "int", Int] => fun frame ->
      frame.Typed.%(0)
  ]
    (fun _ -> raise Match_failure)
