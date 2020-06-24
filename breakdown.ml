open Notty
open Notty.Infix

let modules = [
  "CC";
  "MRS";
  "MSP";
  "AGIP";
  "DS";
  "DSP";
  "NLP";
  "PC";
  "TC"
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

  let cst_part rows = match List.hd rows with
    | "Seq" :: _ -> "IA", List.tl rows

    | ("Computer Science Tripos Part IB" :: _
      | "" :: "" :: "E2018" :: _
      ) -> "IB", List.tl rows
    | "" :: "" :: "" :: "" :: "E2020" :: _ -> "II", List.tl rows
    | _ as s -> failwith ("unknown Part of Tripos: "^ (String.concat ", " s))

  let ia_candidates rows =
    rows |> List.map (function
        | _ :: name :: "CHR" :: _gender :: _tripos :: _cid :: columns
          ->
          let open CCList in
          let paper1, columns = columns |>           take_drop 10 in
          let paper2, columns = columns |> drop 2 |> take_drop 10 in
          let paper3, columns = columns |> drop 1 |> take_drop  9 in
          let grade = CCList.nth columns 39 in
          name, grade, None, [ "1", paper1; "2", paper2; "3", paper3 ]

        | _ as row
          ->
          Printf.printf "ERR: %s\n%!" (String.concat "; " row);
          failwith "unexpected row"
      )

  let ib_candidates rows =
    rows |> CCList.drop 2 |> CCList.map (function
        | name :: "CHR" :: grade :: rank :: columns
          ->
          let open CCList in
          let columns = drop 7 columns in
          let paper3, columns = columns |> take_drop  9 in
          let paper4, columns = columns |> take_drop  9 in
          let paper5, columns = columns |> take_drop  8 in
          let paper6, columns = columns |> take_drop 10 in
          let paper7          = columns |> take      10 in
          name, grade, Some (rank, 102),
          [ "3",paper3; "4",paper4; "5",paper5; "6",paper6; "7",paper7 ]

        | _ as row
          ->
          Printf.printf "ERR: %s\n%!" (String.concat "; " row);
          failwith "unexpected row"
      )

  let ii_candidates rows =
    rows |> CCList.drop 2 |> CCList.map (function
        | _blind :: name :: "CHR" :: _gender :: clas :: _safety :: _safeclass
          :: rank :: _tot :: _p7 :: _p8 :: _p9 :: columns
          ->
          let open CCList in
          let dis,    columns = take_drop  1 columns in
          let         columns = drop       1 columns in
          let uoas,   columns = take_drop (List.length modules) columns in
          let paper7, columns = take_drop 11 columns in
          let paper8, columns = take_drop 15 columns in
          let paper9          = take      15 columns in
          name, clas, Some (rank, 97),
          ["dis",dis; "uoas", uoas; "7",paper7; "8",paper8; "9",paper9]

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

  let rank = function
    | None -> I.empty
    | Some (r, s) -> I.strf "rank:%s/%d" r s

  let paper p =
    p |> List.mapi (fun i -> function
        | "" -> I.empty
        | v -> lpad 2 (string_of_int (i+1)) <|> s ":" <|> lpad 3 v
      )
    |> I.vcat

  let uoas p =
    p |> List.mapi (fun i -> function
        | "X" -> I.empty
        | v -> lpad 5 (CCList.nth modules i) <|> s ":" <|> lpad 3 v
      )
    |> I.vcat

  let papers ps =
    let papers ps = ps |> List.map (fun (i, p) ->
        match CCList.count (fun x -> x <> "") p with
        | 0 -> I.empty
        | _ -> rpad ~f:u 20 ("Paper " ^ i)
               <->
               paper p
      )
    in
    match ps with
    | ("dis",dis) :: ("uoas",scores) :: ps ->
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

  let candidate (n, g, r, ps) =
    sep
    <->
    (name n <|> grade g <|> rank r)
    <->
    (match P.grade g with Some "W" -> I.empty | _ -> papers ps)

end

let ( >> ) f g x = g (f x)
let ( <| ) f x = f x

let () =
  let input = Array.(sub Sys.argv 1 ((length Sys.argv) - 1)) in
  (Csv.load
   >> P.cst_part
   >> (function
       | "IA", rows -> P.ia_candidates rows
       | "IB", rows -> P.ib_candidates rows
       | "II", rows -> P.ii_candidates rows
       | _ -> failwith "unsupported Part"
     )
   >> List.iter F.(candidate >> endl)
  )
  |> Array.iter <| input
