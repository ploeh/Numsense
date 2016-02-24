module internal Ploeh.Numsense.BrazilianPortuguese

let internal tryParseBrazilianImp (x : string) =
    match x with
    | "zero"   -> Some 0
    | "um"     -> Some 1
    | "dois"   -> Some 2
    | "três"   -> Some 3
    | "quatro" -> Some 4
    | "cinco"  -> Some 5
    | "seis"   -> Some 6
    | "sete"   -> Some 7
    | "oito"   -> Some 8
    | "nove"   -> Some 9
    | _        -> None
