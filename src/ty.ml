type _ elt =
  | Int: int elt
  | Float: float elt
  | Bool: bool elt
  | String: string elt


module Row = Hlist.Make(struct type 'a t = 'a end)

module Header = Hlist.Make(struct type 'a t = 'a elt end)

module Columns = Hlist.Make(struct type 'a t = 'a array end)

type 'a table =
  | Rows of 'a Row.t array
  | Columns of 'a Columns.t

let rows x = Rows x
let cols x = Columns x

let rec row_of_columns: type a. a Columns.t -> int -> a Row.t =
  fun cols i -> match cols with
    | [] -> []
    | a :: q -> a.(i) :: row_of_columns q i

let ( .%[] ) tbl i = match tbl with
  | Rows x -> x.(i)
  | Columns x -> row_of_columns x i

type (_,_) index =
  | Z: ('a * 'b , 'a) index
  | S: ('s,'r) index -> ( _ * 's, 'r) index

type empty = |


let rec (.%()) : type a b. (a * empty) Row.t -> (a,b) index -> b =
  fun row index -> match row,index with
    | a :: _ , Z -> a
    | _ :: q, S n -> q.%(n)
    | _ -> .

module Cross = Hlist.Cross(Columns)(Row)
let ( .%{}) m n = match m with
  | Rows m -> m.(n)
  | Columns cols -> Cross.map {Cross.f=(fun x -> x.(n))} cols


let take_col: type a b c.  ( (a * b) * c) Row.t array -> a array * (b * c) Row.t array  =
  fun rows ->
  let n = Array.length rows in
  if n = 0 then invalid_arg "take_col: 0 rows";
  Array.init n (fun n -> Row.head rows.(n)),
  Array.map Row.tail rows

let cols_to_row cols =
  Array.init
    Columns.(fold_map {map=Array.length} min cols max_int)
    (row_of_columns cols)

let rec rows_to_col: type a.  a Row.t array -> a Columns.t  =
  fun rows ->
  let n = Array.length rows in
  if n = 0 then invalid_arg "take_col: 0 rows";
  match rows.(0) with
  | [] -> []
  | _ :: _ ->
    let first, rest = take_col rows in
    first :: rows_to_col rest

let transpose = function
  | Columns c -> Rows (cols_to_row c)
  | Rows r -> Columns (rows_to_col r)

let hcat x y =  match x, y with
| Rows x, Rows y -> Rows (Array.map2 Row.(@) x y)
| Columns x, Columns y -> Columns Columns.(x @ y)
| Rows x, Columns y -> Columns Columns.(rows_to_col x @ y)
| Columns x, Rows y -> Columns Columns.(x @ rows_to_col y)

let array_cat x y =
  let nx, ny = Array.(length x, length y) in
  Array.init (nx+ny) (fun n -> if n < nx then x.(n) else y.(n-nx))
let vcat x y = match x, y with
  | Columns x, Columns y -> Columns (Columns.map2 {map=array_cat} x y)
  | Rows x, Rows y -> Rows(array_cat x y)
  | Columns x, Rows y -> Rows (array_cat (cols_to_row x) y)
  | Rows x, Columns y -> Rows (array_cat x (cols_to_row y))

type _ eq = Eq: ('a * 'a) eq
let hregularize: (_ * empty) Header.t -> _ = fun x -> x


let example : (_ * empty) table = cols [
    [| "one"; "two"; "three" |];
    [| 1 ; 2 ; 3 |];
    [| 1.; 2.; 3. |]
  ]

let rec take: type input output.
  (input, output) index -> (input * unit) Header.t -> output elt =
  fun n l -> match n, l with
    | _ , [] -> .
    | Z,  a :: _ -> (a:output elt)
    | S k, _ :: q -> take k q

module Filter = struct
  type ('input,'output,'tail) f =
    | [] : ('input,'output,'output) f
    | (::): ('input,'output) index * ('input, 'a, 'tail) f -> ('input, 'output * 'a, 'tail) f
end

let rec filter: type indices t list. (list,indices,t) Filter.f -> (list * unit) Header.t -> (indices * t) Header.t =
  fun indices list -> match indices with
    | Filter.[] -> []
    | Filter.( a :: q ) -> Header.( (take a list) :: filter q list )

open Header
let data: _ Header.t = [Int;Int;Float]
let data2: _ Header.t = [Float;Int]
let data3: (_ * empty) t = data @ data2

let regularize : ((_ * empty) t as 'x) -> 'x = fun x -> x
let data' = regularize @@ filter Filter.[Z; S (S Z); S Z] data

let Int = take Z data

type dyn = Dyn: ('a * 'b) Header.t -> dyn [@@unboxed]
let rec cast: type a. a Header.t -> dyn -> a Header.t option =
  fun spec (Dyn d) -> match spec, d with
    | [], [] -> Some []
    | Float :: q, Float :: q' ->
      begin
        match cast q (Dyn q') with | None -> None | Some t -> Some(Float::t)
      end
    | Int :: q, Int :: q' ->
      begin
        match cast q (Dyn q') with | None -> None | Some t -> Some(Int::t)
      end
    | _ -> None

let [Float;Float] = regularize (filter Filter.[Z; Z] data2)

type _ witness = ..

type 'a key = Key: 'a witness * ('a,'b) index -> 'b key
type row = R: 'a witness * ('a * empty) Row.t -> row

module X = struct
  let l = Header.[Int;Float]
  let row = Row.[2;3.]
  type _ witness += K: (int * (float * empty)) witness
end

let key = Key( X.K, Z)

let get (type a) (key: a key) row: a = match key, row with
  | Key(X.K, n), R(X.K, row) -> row.%(n)
  | _ -> raise Not_found
