module internal Ploeh.Numsense.BrazilianPortuguese

open Ploeh.Numsense.InternalDsl

let rec internal toBrazilianImp x =
    let formatPrefix prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s e %s" prefix <| toBrazilianImp remainder

    let formatSuffix suffix factor x =
        let prefix = sprintf "%s%s" (toBrazilianImp (x / factor)) suffix
        formatPrefix prefix factor x

    match x with
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
    | 14 -> "quatorze"
    | 15 -> "quinze"
    | 16 -> "dezesseis"
    | 17 -> "dezessete"
    | 18 -> "dezoito"
    | 19 -> "dezenove"
    | Between 20 30 x -> formatPrefix "vinte" 10 x
    | Between 30 40 x -> formatPrefix "trinta" 10 x
    | Between 40 50 x -> formatPrefix "quarenta" 10 x
    | Between 50 60 x -> formatPrefix "cinquenta" 10 x
    | Between 60 70 x -> formatPrefix "sessenta" 10 x
    | Between 70 80 x -> formatPrefix "setenta" 10 x
    | Between 80 90 x -> formatPrefix "oitenta" 10 x
    | Between 90 100 x -> formatPrefix "noventa" 10 x
    | 100 -> "cem"
    | Between 100 200 x -> formatPrefix "cento" 100 x
    | Between 200 300 x -> formatPrefix "duzentos" 100 x
    | Between 300 400 x -> formatPrefix "trezentos" 100 x
    | Between 500 600 x -> formatPrefix "quinhentos" 100 x
    | Between 400 1000 x -> formatSuffix "centos" 100 x
    | Between 1000 2000 x -> formatPrefix "mil" 1000 x
    | Between 1000 1000000 x -> formatSuffix " mil" 1000 x
    | _  -> ""

let internal tryParseBrazilianImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                          -> Some acc
        | StartsWith " "            t
        | StartsWith ","            t
        | StartsWith "E"            t -> conv acc t
        | "ZERO"                      -> Some          (0  + acc)
        | StartsWith "BILHÃO"       t 
        | StartsWith "BILHÕES"      t -> conv (1000000000 %* acc) t
        | StartsWith "MILHÃO"       t
        | StartsWith "MILHÕES"      t -> conv    (1000000 %* acc) t
        | StartsWith "MIL"          t -> conv       (1000 %* acc) t
        | StartsWith "CEM"          t -> conv        (100  + acc) t
        | StartsWith "CENTOS"       t -> conv        (100 %* acc) t
        | StartsWith "CENTO"        t -> conv        (100  + acc) t
        | StartsWith "DUZENTOS"     t -> conv        (200  + acc) t
        | StartsWith "TREZENTOS"    t -> conv        (300  + acc) t
        | StartsWith "QUINHENTOS"   t -> conv        (500  + acc) t
        | StartsWith "VINTE"        t -> conv         (20  + acc) t
        | StartsWith "TRINTA"       t -> conv         (30  + acc) t
        | StartsWith "QUARENTA"     t -> conv         (40  + acc) t
        | StartsWith "CINQUENTA"    t -> conv         (50  + acc) t
        | StartsWith "CINQÜENTA"    t -> conv         (50  + acc) t
        | StartsWith "SESSENTA"     t -> conv         (60  + acc) t
        | StartsWith "SETENTA"      t -> conv         (70  + acc) t
        | StartsWith "OITENTA"      t -> conv         (80  + acc) t
        | StartsWith "NOVENTA"      t -> conv         (90  + acc) t
        | StartsWith "ONZE"         t -> conv         (11  + acc) t
        | StartsWith "DOZE"         t -> conv         (12  + acc) t
        | StartsWith "TREZE"        t -> conv         (13  + acc) t
        | StartsWith "CATORZE"      t -> conv         (14  + acc) t
        | StartsWith "QUATORZE"     t -> conv         (14  + acc) t
        | StartsWith "QUINZE"       t -> conv         (15  + acc) t
        | StartsWith "DEZESSEIS"    t -> conv         (16  + acc) t
        | StartsWith "DEZESSETE"    t -> conv         (17  + acc) t
        | StartsWith "DEZOITO"      t -> conv         (18  + acc) t
        | StartsWith "DEZENOVE"     t -> conv         (19  + acc) t
        | StartsWith "DEZ"          t -> conv         (10  + acc) t
        | StartsWith "UM"           t -> conv          (1  + acc) t
        | StartsWith "DOIS"         t -> conv          (2  + acc) t
        | StartsWith "TRÊS"         t -> conv          (3  + acc) t
        | StartsWith "QUATRO"       t -> conv          (4  + acc) t
        | StartsWith "CINCO"        t -> conv          (5  + acc) t
        | StartsWith "SEIS"         t -> conv          (6  + acc) t
        | StartsWith "SETE"         t -> conv          (7  + acc) t
        | StartsWith "OITO"         t -> conv          (8  + acc) t
        | StartsWith "NOVE"         t -> conv          (9  + acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "pt-BR")
    conv 0 canonicalized
