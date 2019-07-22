open Notty
open Notty.Infix

let n_ib = "100"
let n_ii = "98"
let l0_offset = 9
let papers = [
  ("Paper 1", 10);
  ("Paper 2", 10);
  ("Paper 3",  9);
  ("Paper 4",  9);
  ("Paper 5",  8);
  ("Paper 6", 10);
  ("Paper 7",  8); (* IB 75% *)
  ("Paper 7", 14); (* II *)
  ("Paper 8", 14);
  ("Paper 9", 15);
  ("Dissertation", 0);
]

let s = I.string A.empty
let b = I.string A.(st bold)
let u = I.string A.(st underline)
let lpad ?(f=s) i x = x |> f |> I.hsnap ~align:`Right i
let rpad ?(f=s) i x = x |> f |> I.hsnap ~align:`Left i
let endl line = Notty_unix.(line |> eol |> output_image)

let marks ~idx ~len qs =
  let sub ~idx ~len l = CCList.(l |> take (idx+len) |> drop idx) in
  qs |> sub ~idx ~len |> List.mapi (fun i -> function
      | "" -> I.empty
      | x -> lpad 2 (string_of_int (i+1)) <|> s ":" <|> lpad 3 x
    )
  |> I.vcat

let g x = rpad ~f:b 6 (match x with
    | "1"  -> "I"
    | "21" -> "II.i"
    | "22" -> "II.ii"
    | "3"  -> "III"
    | x    -> failwith ("Unknown grade: "^x)
  )

let format_results ~part pA pB pC pD answers =
  let[@warning "-8"] [ (t1,l1); (t2,l2); (t3,l3); (t4,l4) ] =
    let open CCList in
    match part with
    | "II"   -> papers |> drop 7 |> take 4
    | "IB50" -> papers |> drop 2 |> take 4
    | "IB75" -> papers |> drop 3 |> take 4
    | _ -> assert false
  in
  let l0 = if part = "IB75" then l0_offset else 0 in

  let pABC_result =
    (
      rpad ~f:u 7 t1 <|> s (" ("^pA^"/100)   ")
      <->
      marks ~idx:l0 ~len:l1 answers
    ) <|> (
      rpad ~f:u 7 t2 <|> s (" ("^pB^"/100)   ")
      <->
      marks ~idx:(l0+l1) ~len:l2 answers
    ) <|> (
      rpad ~f:u 7 t3 <|> s (" ("^pC^"/100)   ")
      <->
      marks ~idx:(l0+l1+l2) ~len:l3 answers
    )
  in

  let pD_result = (match part with
      | "II" -> u t4 <|> s (" ("^pD^"/100)")
      | "IB50" | "IB75" ->
        rpad ~f:u 7 t4 <|> s (" ("^pD^"/100)   ")
        <->
        marks ~idx:(l0+l1+l2+l3) ~len:l4 answers
      | _ -> assert false
    )
  in

  (match part with
   | "II" -> pABC_result <-> pD_result
   | "IB50" | "IB75" -> pABC_result <|> pD_result
   | _ -> assert false
  )

let format_row = function
  | "IB" -> (function
      | name :: college :: grade :: rank :: _total :: p3 :: p4 :: p5 :: p6 :: p7 :: _penalty :: answers ->
          assert (college = "CHR");
          (rpad ~f:b 18 name <|> (g grade) <|> s ("rank:"^rank^"/"^n_ib))
          <->
          (match Astring.String.to_int p7 with
           | None -> assert false
           | Some 0 ->
             format_results ~part:"IB50" p3 p4 p5 p6 answers
           | Some _ ->
             format_results ~part:"IB75" p4 p5 p6 p7 answers
          )
      | _ -> assert false
    )

  | "II" as part -> (function
      | name :: college :: grade :: rank :: _total :: p7 :: p8 :: p9 :: dis :: _penalty :: answers ->
          assert (college = "CHR");
          (rpad ~f:b 18 name <|> (g grade) <|> s ("rank:"^rank^"/"^n_ii))
          <->
          format_results ~part p7 p8 p9 dis answers
      | _ -> assert false
    )

  | _ -> assert false

let () =
  let csvs = Sys.argv.(1) |> Csv.load in
  match csvs with
  | [] -> assert false
  | header :: rows ->
    let part = match List.hd header with
      | "Computer Science Tripos Part IB" -> "IB"
      | "Computer Science Tripos Part II" -> "II"
      | _ -> assert false
    in
    List.iter (fun row -> row |> format_row part |> endl)
      (rows |> List.tl)
