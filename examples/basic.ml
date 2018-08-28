open Datagrade.Dy

let int = "integer" $: Int
let en = "en" $: String

let dict =
  Frame.empty
  |> Col.add int [|0;1;2|]
  |> Col.add en [|"0";"1";"2"|]

let x = dict.Col.|[int]

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

let print x = Format.printf "%a@." (Typed.pp header) x

let%expect_test _ =
   print y.Typed.%(0);
[%expect {|[1; one]|}]


let%expect_test _ =
   print f.Typed.%(0);
[%expect {|[1; one]|}]


let row = match' (Typed.untype f) [
    header => fun frame ->
      frame.Typed.%(0)
  ]
    (fun _ -> raise Match_failure)

let%expect_test _ =
  print row;
[%expect {|[1; one]|}]
