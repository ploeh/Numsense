module internal Ploeh.Numsense.BrazilianPortuguese

open Ploeh.Numsense.InternalDsl

let rec internal toBrazilianImp x =
    let formatPrefix prefix factor x =
        let remainder = x % factor

        let rec hundreds x =
            if x > 1000
            then hundreds (x / 1000)
            else x

        match remainder, hundreds remainder with
        | 0, _ -> prefix
        | _, r when r > 100 && r % 100 <> 0 ->
               sprintf "%s, %s"  prefix <| toBrazilianImp remainder
        | _ -> sprintf "%s e %s" prefix <| toBrazilianImp remainder

    let formatSuffix suffix factor x =
        let prefix = sprintf "%s%s" (toBrazilianImp (x / factor)) suffix
        formatPrefix prefix factor x

    match x with
    |  x when x < 0 -> sprintf "menos %s" <| toBrazilianImp -x
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
    | Between 1000000 2000000 x -> formatPrefix "um milhão" 1000000 x
    | Between 2000000 1000000000 x -> formatSuffix " milhões" 1000000 x
    | Between 1000000000 2000000000 x -> formatPrefix "um bilhão" 1000000000 x
    | _ -> formatSuffix " bilhões" 1000000000 x

let internal tryParseBrazilianImp (x : string) =
    let rec conv acc candidate =
        let conv' remainderFactor placeFactor acc' candidate =
            if (acc % remainderFactor) / placeFactor <> 0
            then None
            else conv acc' candidate

        let convBillions acc' candidate = 
            if acc / 1000000000 <> 0
            then None
            else conv acc' candidate

        let convMillions  = conv'    1000000000    1000000
        let convThousands = conv'       1000000       1000
        let convHundreds  = conv'          1000        100
        let convTens      = conv'           100         10
        let convUnits     = conv'            10          1

        match candidate with
        | ""                          -> Some acc
        | StartsWith " "            t
        | StartsWith ","            t
        | StartsWith "E"            t -> conv acc t
        | "ZERO"                      -> Some          (0  + acc)
        | StartsWith "BILHÃO"       t 
        | StartsWith "BILHÕES"      t -> convBillions (1000000000 %* acc) t
        | StartsWith "MILHÃO"       t
        | StartsWith "MILHÕES"      t -> convMillions    (1000000 %* acc) t
        | StartsWith "MIL"          t -> convThousands      (1000 %* acc) t
        | StartsWith "CEM"          t -> convHundreds        (100  + acc) t
        | StartsWith "CENTOS"       t -> convHundreds        (100 %* acc) t
        | StartsWith "CENTO"        t -> convHundreds        (100  + acc) t
        | StartsWith "DUZENTOS"     t -> convHundreds        (200  + acc) t
        | StartsWith "TREZENTOS"    t -> convHundreds        (300  + acc) t
        | StartsWith "QUINHENTOS"   t -> convHundreds        (500  + acc) t
        | StartsWith "VINTE"        t -> convTens             (20  + acc) t
        | StartsWith "TRINTA"       t -> convTens             (30  + acc) t
        | StartsWith "QUARENTA"     t -> convTens             (40  + acc) t
        | StartsWith "CINQUENTA"    t -> convTens             (50  + acc) t
        | StartsWith "CINQÜENTA"    t -> convTens             (50  + acc) t
        | StartsWith "SESSENTA"     t -> convTens             (60  + acc) t
        | StartsWith "SETENTA"      t -> convTens             (70  + acc) t
        | StartsWith "OITENTA"      t -> convTens             (80  + acc) t
        | StartsWith "NOVENTA"      t -> convTens             (90  + acc) t
        | StartsWith "ONZE"         t -> convTens             (11  + acc) t
        | StartsWith "DOZE"         t -> convTens             (12  + acc) t
        | StartsWith "TREZE"        t -> convTens             (13  + acc) t
        | StartsWith "CATORZE"      t -> convTens             (14  + acc) t
        | StartsWith "QUATORZE"     t -> convTens             (14  + acc) t
        | StartsWith "QUINZE"       t -> convTens             (15  + acc) t
        | StartsWith "DEZESSEIS"    t -> convTens             (16  + acc) t
        | StartsWith "DEZESSETE"    t -> convTens             (17  + acc) t
        | StartsWith "DEZOITO"      t -> convTens             (18  + acc) t
        | StartsWith "DEZENOVE"     t -> convTens             (19  + acc) t
        | StartsWith "DEZ"          t -> convTens             (10  + acc) t
        | StartsWith "UM"           t -> convUnits             (1  + acc) t
        | StartsWith "DOIS"         t -> convUnits             (2  + acc) t
        | StartsWith "TRÊS"         t -> convUnits             (3  + acc) t
        | StartsWith "QUATRO"       t -> convUnits             (4  + acc) t
        | StartsWith "CINCO"        t -> convUnits             (5  + acc) t
        | StartsWith "SEIS"         t -> convUnits             (6  + acc) t
        | StartsWith "SETE"         t -> convUnits             (7  + acc) t
        | StartsWith "OITO"         t -> convUnits             (8  + acc) t
        | StartsWith "NOVE"         t -> convUnits             (9  + acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "pt-BR")
    match canonicalized with
    | StartsWith "MENOS" t -> conv 0 t |> Option.map (~-)
    | _ -> conv 0 canonicalized
