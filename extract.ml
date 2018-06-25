open Csv
open Notty
open Notty.Infix

let s = I.string A.empty
let b = I.string A.(st bold)
let u = I.string A.(st underline)
let lpad ?(f=s) i x = x |> f |> I.hsnap ~align:`Right i
let rpad ?(f=s) i x = x |> f |> I.hsnap ~align:`Left i
let endl = Notty_unix.output_image_endline

let marks ~idx ~len qs =
  let sub ~idx ~len l = CCList.(l |> take (idx+len) |> drop idx) in
  qs |> sub ~idx ~len |>
  List.mapi (fun i -> function
      | "" -> I.empty
      | x -> lpad 2 (string_of_int (i+1)) <|> s ":" <|> lpad 3 x
    )
  |> I.vcat

let g x = (match x with
    | "1"  -> "I"
    | "21" -> "II.i"
    | "22" -> "II.ii"
    | "3"  -> "III"
    | x    -> x
  ) |> rpad ~f:b 6

let format_results ~part p1 p2 p3 p4 answers =
  let (t1,l1), (t2,l2), (t3,l3), (t4,l4) = match part with
    | "II"
      -> ("Paper 7", 14), ("Paper 8", 15), ("Paper 9", 16), ("Dissertation", 0)
    | "IB"
      -> ("Paper 3",  8), ("Paper 4",  9), ("Paper 5",  9), ("Paper 6", 10)
    | _ -> assert false
  in
  let p123_result =
    (
      rpad ~f:u 7 t1 <|> s (" ("^p1^"/100)   ")
      <->
      marks ~idx:0 ~len:l1 answers
    ) <|> (
      rpad ~f:u 7 t2 <|> s (" ("^p2^"/100)   ")
      <->
      marks ~idx:l1 ~len:l2 answers
    ) <|> (
      rpad ~f:u 7 t3 <|> s (" ("^p3^"/100)   ")
      <->
      marks ~idx:(l1+l2) ~len:l3 answers
    )
  in
  let p4_result = (match part with
      | "II" -> u t4 <|> s (" ("^p4^"/100)")
      | "IB" ->
        rpad ~f:u 7 t4 <|> s (" ("^p4^"/100)   ")
        <->
        marks ~idx:(l1+l2+l3) ~len:l4 answers
      | _ -> assert false
    )
  in (match part with
      | "II" -> p123_result <-> p4_result
      | "IB" -> p123_result <|> p4_result
      | _ -> assert false
    )

let format_row ~part = function
  | name :: college :: grade :: rank :: total :: p7 :: p8 :: p9 :: dis :: _
    :: answers -> (
      assert (college = "CHR");
      (rpad ~f:b 18 name <|> (g grade) <|> s (
          if (int_of_string rank < 50) then "rank:"^rank^"/94" else ""
        )
      )
      <->
      format_results ~part p7 p8 p9 dis answers
    )
  | _ -> assert false

let () =
  let path = Sys.argv.(1) in
  let csv = Csv.load path ~separator:'\t' |> List.tl |> List.tl in
  match csv with
  | [] -> assert false
  | header :: rows ->
    let part = match List.hd header with
      | "Computer Science Tripos Part IB" -> "IB"
      | "Computer Science Tripos Part II" -> "II"
      | _ -> assert false
    in
    List.iter (fun row -> row |> format_row ~part |> endl) rows
