module internal Ploeh.Numsense.BrazilianPortuguese

let internal tryParseBrazilianImp (x : string) =
    match x.Trim().ToUpper() with
    | "ZERO"   -> Some 0
    | "UM"     -> Some 1
    | "DOIS"   -> Some 2
    | "TRÊS"   -> Some 3
    | "QUATRO" -> Some 4
    | "CINCO"  -> Some 5
    | "SEIS"   -> Some 6
    | "SETE"   -> Some 7
    | "OITO"   -> Some 8
    | "NOVE"   -> Some 9
    | _        -> None
