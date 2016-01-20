module internal Ploeh.Numsense.Portuguese

open Ploeh.Numsense.InternalDsl
open System.Diagnostics

let rec internal toPortugueseImp x =

    // Esentially simplifies expressions like 'twenty-zero' to 'twenty',
    // 'one-milion-zero' to 'one-million', and so on.
    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s-%s" prefix (toPortugueseImp (remainder))

    let format' suffixPlural suffixSingular factor x biform =
        let firstDigit = x / factor
        match firstDigit, biform with
        | 1, false ->
            let prefix = sprintf "%s-%s" (toPortugueseImp firstDigit) suffixSingular
            simplify prefix factor x
        | 1, true ->
            let form = if x % factor > 0 then suffixPlural else suffixSingular
            simplify form factor x
        | _ ->
            let prefix = sprintf "%s-%s" (toPortugueseImp firstDigit) suffixPlural
            simplify prefix factor x

    let formatPlural suffixPlural suffixSingular factor x =
        format' suffixPlural suffixSingular factor x false

    let formatBiform suffixPlural suffixSingular factor x =
        format' suffixPlural suffixSingular factor x true

    match x with
    |  x when x < 0 -> sprintf "menos %s" (toPortugueseImp -x)
    |  0 -> "zero"
    |  1 -> "um"
    |  2 -> "dois"
    |  3 -> "três"
    |  4 -> "quatro"
    |  5 -> "cinco"
    |  6 -> "seis"
    |  7 -> "sete"
    |  8 -> "oito"
    |  9 -> "nove"
    | 10 -> "dez"
    | 11 -> "onze"
    | 12 -> "doze"
    | 13 -> "treze"
    | 14 -> "catorze"
    | 15 -> "quinze"
    | 16 -> "dezasseis"
    | 17 -> "dezassete"
    | 18 -> "dezoito"
    | 19 -> "dezanove"
    | Between 20  30 x -> simplify "vinte" 20 x
    | Between 30  40 x -> simplify "trinta" 30 x
    | Between 40  50 x -> simplify "quarenta" 40 x
    | Between 50  60 x -> simplify "cinquenta" 50 x
    | Between 60  70 x -> simplify "sessenta" 60 x
    | Between 70  80 x -> simplify "setenta" 70 x
    | Between 80  90 x -> simplify "oitenta" 80 x
    | Between 90  100 x -> simplify "noventa" 90 x
    | Between 100 200 x -> formatBiform "cento" "cem" 100 x
    | Between 200 300 x -> simplify "duzentos" 200 x
    | Between 300 400 x -> simplify "trezentos" 300 x
    | Between 400 500 x -> simplify "quatrocentos" 400 x
    | Between 500 600 x -> simplify "quinhentos" 500 x
    | Between 600 700 x -> simplify "seiscentos" 600 x
    | Between 700 800 x -> simplify "setecentos" 700 x
    | Between 800 900 x -> simplify "oitocentos" 800 x
    | Between 900 1000 x -> simplify "novecentos" 900 x
    | Between 1000 1000000 x -> formatBiform "mil" "mil" 1000 x
    | Between 1000000 1000000000 x -> formatPlural "milhões" "milhão" 1000000 x
    | _ -> formatBiform "mil-milhões" "mil-milhões" 1000000000 x

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
        | StartsWith "DEZASSEIS"    t -> conv         (16  + acc) t
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
