module internal Ploeh.Numsense.BrazilianPortuguese

let internal tryParseBrazilianImp (x : string) =
    match x.Trim().ToUpper() with
    | "ZERO"      -> Some 0
    | "DEZ"       -> Some 10
    | "ONZE"      -> Some 11
    | "DOZE"      -> Some 12
    | "TREZE"     -> Some 13
    | "CATORZE"   -> Some 14
    | "QUATORZE"  -> Some 14
    | "QUINZE"    -> Some 15
    | "DEZESSEIS" -> Some 16
    | "DEZESSETE" -> Some 17
    | "DEZOITO"   -> Some 18
    | "DEZENOVE"  -> Some 19
    | "UM"        -> Some 1
    | "DOIS"      -> Some 2
    | "TRÊS"      -> Some 3
    | "QUATRO"    -> Some 4
    | "CINCO"     -> Some 5
    | "SEIS"      -> Some 6
    | "SETE"      -> Some 7
    | "OITO"      -> Some 8
    | "NOVE"      -> Some 9
    | _        -> None
