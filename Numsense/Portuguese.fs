module internal Ploeh.Numsense.Portuguese

open Ploeh.Numsense.InternalDsl
open System.Diagnostics

let rec internal toPortugueseImp x =
    Some ""

let rec internal tryParsePortugueseImp (x:string) =
    let rec conv acc candidate =
        match candidate with
        | ""                          -> Some acc
        | StartsWith "-"            t
        | StartsWith "E"            t -> conv                acc  t
        | StartsWith "ZERO"         t -> conv          (0  + acc) t
        | StartsWith "UM"           t -> conv          (1  + acc) t
        | StartsWith "DOIS"         t -> conv          (2  + acc) t
        | StartsWith "TRÊS"         t -> conv          (3  + acc) t
        | StartsWith "QUATRO"       t -> conv          (4  + acc) t
        | StartsWith "CINCO"        t -> conv          (5  + acc) t
        | StartsWith "SEIS"         t -> conv          (6  + acc) t
        | StartsWith "OITO"         t -> conv          (8  + acc) t
        | StartsWith "ONZE"         t -> conv         (11  + acc) t
        | StartsWith "DOZE"         t -> conv         (12  + acc) t
        | StartsWith "CATORZE"      t
        | StartsWith "QUATORZE"     t -> conv         (14  + acc) t
        | StartsWith "QUINZE"       t -> conv         (15  + acc) t
        | StartsWith "DEZESSEIS"    t -> conv         (16  + acc) t
        | StartsWith "DEZASSETE"    t -> conv         (17  + acc) t
        | StartsWith "DEZOITO"      t -> conv         (18  + acc) t
        | StartsWith "DEZANOVE"     t -> conv         (19  + acc) t
        | StartsWith "DEZ"          t -> conv         (10  + acc) t
        | StartsWith "VINTE"        t -> conv         (20  + acc) t
        | StartsWith "TRINTA"       t -> conv         (30  + acc) t
        | StartsWith "QUARENTA"     t -> conv         (40  + acc) t
        | StartsWith "CINQUENTA"    t -> conv         (50  + acc) t
        | StartsWith "SESSENTA"     t -> conv         (60  + acc) t
        | StartsWith "SETENTA"      t -> conv         (70  + acc) t
        | StartsWith "SETE"         t -> conv          (7  + acc) t
        | StartsWith "OITENTA"      t -> conv         (80  + acc) t
        | StartsWith "NOVENTA"      t -> conv         (90  + acc) t
        | StartsWith "NOVE"         t -> conv          (9  + acc) t
        | StartsWith "CENTOS"       t -> conv        (100 %* acc) t
        | StartsWith "DUZENTOS"     t -> conv         (200 + acc) t
        | StartsWith "TREZENTOS"    t -> conv         (300 + acc) t
        | StartsWith "TREZE"        t -> conv         (13  + acc) t
        | StartsWith "QUINHENTOS"   t -> conv         (500 + acc) t
        | StartsWith "CENTO"        t
        | StartsWith "CEM"          t -> conv         (100 + acc) t
        | StartsWith "MILHÃO"       t 
        | StartsWith "MILHÕES"      t -> conv    (1000000 %* acc) t
        | StartsWith "MILMILHÕES"   t ->
               conv (if acc = 0 then 1000000000 else 1000000000 %* acc) t
        | StartsWith "MIL"          t -> 
               conv (if acc = 0 then 1000 else      1000 %* acc) t
        | _ -> failwith(candidate) 

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "pt-PT")
    match canonicalized with
    | StartsWith "MENOS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized