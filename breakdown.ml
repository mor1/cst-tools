open Notty
open Notty.Infix

let modules = [
    "CAT";
    "IRO";
    "MSP";
    "MVP";
    "AGIP";
    "ADS";
    "DSP";
    "DSPCM";
    "NLP";
    "AOS";
    "CSM";
    "CYC";
    "DNN";
    "IML";
    "CC"
  ]


module P = struct
  let grade = function
    | "1" | "I"  -> Some "I"
    | "21" | "II.1" -> Some "II.i"
    | "22" | "II.2" -> Some "II.ii"
    | "3" | "III"  -> Some "III"
    | "W" -> Some "W" (* withdrawn *)
    | "" -> None
    | _ as g -> failwith ("unknown grade: "^g)

  let cst_part = function
    | "ia.csv" -> "IA"
    | "ib.csv" -> "IB"
    | "ii.csv" -> "II"
    | _ as s -> failwith ("unknown Part of Tripos: " ^ s)

  let ia_candidates out_of rows =
    rows
    |> CCList.drop 3
    |> CCList.filter (function
           | _bcn :: _name :: "CHR" :: _ -> true
           | _ -> false
         )
    |> CCList.map (function
           | bcn :: _crsid :: "CHR" :: name :: rank :: total :: total_p1 :: total_p2 :: total_p3 :: _maths_p1 :: _maths_p2 :: _total_maths :: _scicomp :: grade :: columns
             ->
              let open CCList in
              let bcn1, columns = columns |> take_drop 1 in
              let bcn1 = List.hd bcn1 in
              let paper1, columns = columns |> drop 3 |> take_drop 10 in

              let bcn2, columns = columns |> drop 1 |> take_drop 1 in
              let bcn2 = List.hd bcn2 in
              let paper2, columns = columns |> drop 3 |> take_drop 10 in

              let bcn3, columns = columns |> drop 1 |> take_drop 1 in
              let bcn3 = List.hd bcn3 in
              let paper3 = columns |> drop 3 |> take 9 in

          if not (bcn = bcn1 && bcn1 = bcn2 && bcn2 = bcn3) then
            failwith (Printf.sprintf "mismatched BCNs bcn=%s bcn1=%s bcn2=%s bcn3=%s" bcn bcn1 bcn2 bcn3);

          name, grade, Some (rank, out_of), total,
          [ "1", paper1, total_p1;
            "2", paper2, total_p2;
            "3", paper3, total_p3
          ]

        | _ as row
          ->
          Printf.printf "ERR: %s\n%!" (String.concat "; " row);
          failwith "unexpected row"
      )

  let ib_candidates out_of rows =
    rows
    |> CCList.drop 3
    |> CCList.filter (function
           | _bcn :: _name :: "CHR" :: _ -> true
           | _ -> false
         )
    |> CCList.map (function
        | _bcn :: name :: "CHR" :: _gender :: grade :: rank :: total :: total_p4 :: total_p5 :: total_p6 :: total_p7 :: columns
          ->
           let open CCList in
          let columns = drop 2 columns in
          let paper4, columns = columns |> take_drop  9 in
          let paper5, columns = columns |> take_drop  8 in
          let paper6, columns = columns |> take_drop 10 in
          let paper7          = columns |> take      10 in
          name, grade, Some (rank, out_of), total,
          [ "4", paper4, total_p4;
            "5", paper5, total_p5;
            "6", paper6, total_p6;
            "7", paper7, total_p7
          ]

        | _ as row
          ->
          Printf.printf "ERR: %s\n%!" (String.concat "; " row);
          failwith "unexpected row"
      )

  let ii_candidates out_of rows =
    rows
    |> CCList.drop 3
    |> CCList.filter (function
           | _bcn :: _name :: "CHR" :: _ -> true
           | _ -> false
         )
    |> CCList.map (function
        | _bcn :: name :: "CHR" :: _gender :: _ :: grade :: _outof :: _distinction
          :: rank :: _total_nodis :: total :: _total_p7 :: total_p8 :: total_p9 :: dis
          :: _penalties :: _bonus :: columns
          ->
          let open CCList in
          let uoas,   columns = columns |> take_drop (List.length modules) in
          let         columns = columns |> drop      15 in
          let paper8, columns = columns |> take_drop 13 in
          let paper9          = columns |> take      13 in
          name, grade, Some (rank, out_of), total,
          [ "dis", [ dis ], "";
            "uoas", uoas, "";
            "8", paper8, total_p8;
            "9", paper9, total_p9
          ]

        | _ as row
          ->
          Printf.printf "ERR: %s\n%!" (String.concat "; " row);
          failwith "unexpected row"
      )
end

module F = struct
  let s = I.string A.empty
  let b = I.string A.(st bold)
  let u = I.string A.(st underline)

  let lpad ?(f=s) i x = x |> f |> I.hsnap ~align:`Right i
  let rpad ?(f=s) i x = x |> f |> I.hsnap ~align:`Left i

  let endl line = Notty_unix.(line |> eol |> output_image)

  let sep = s (String.make 80 '-')

  let name n = rpad ~f:b 18 n

  let grade g = match P.grade g with
    | None -> I.empty
    | Some g -> rpad ~f:b 6 g

  let total t = lpad ~f:b 18 ("Total "^t^"/400")

  let rank = function
    | None -> I.empty
    | Some (r, s) -> I.strf "rank:%s/%d" r s

  let paper p =
    p |> List.mapi (fun i -> function
        | "" -> I.empty
        | v -> lpad 5 (string_of_int (i+1)) <|> s ":" <|> lpad 3 v
      )
    |> I.vcat

  let uoas p =
    p |> List.mapi (fun i -> function
        | "X" -> I.empty
        | v -> lpad 5 (CCList.nth modules i) <|> s ":" <|> lpad 3 (v |> float_of_string |> int_of_float |> string_of_int)
      )
    |> I.vcat

  let papers ps =
    let papers ps = ps |> List.map (fun (i, p, t) ->
        match CCList.count (fun x -> x <> "") p with
        | 0 -> I.empty
        | _ -> rpad ~f:u 20 ("Paper " ^ i)
               <->
               paper p
               <->
               rpad ~f:u 20 ("Total: " ^ t)
      )
    in
    match ps with
    | ("dis",dis,_) :: ("uoas",scores,_) :: ps ->
      ((rpad ~f:u 12 "Dissertation" <|> (List.hd dis |> I.strf ": %s"))
       <->
       I.void 22 1
       <->
       (match CCList.count (fun x -> x <> "X") scores with
        | 0 -> I.empty
        | _ -> rpad ~f:u 22 ("Units of Assessment")
               <->
               uoas scores
       )
      ) <|> (papers ps |> I.hcat)

    | _ -> (papers ps |> I.hcat)

  let candidate (n, g, r, t, ps) =
    sep
    <->
    (name n <|> grade g <|> rank r <|> total t)
    <->
    (match P.grade g with Some "W" -> I.empty | _ -> papers ps)

end

let ( >> ) f g x = g (f x)
let ( <| ) f x = f x

let () =
  let filename = Sys.argv.(1) in
  let out_of = Sys.argv.(2) |> int_of_string in
  let process = match P.cst_part filename with
       | "IA" -> P.ia_candidates out_of
       | "IB" -> P.ib_candidates out_of
       | "II" -> P.ii_candidates out_of
       | _ as s -> failwith ("unsupported Part: " ^ s)
  in
  (Csv.load
   >> process
   >> List.iter F.(candidate >> endl)
  )
  |> Array.iter <| [| filename |]
