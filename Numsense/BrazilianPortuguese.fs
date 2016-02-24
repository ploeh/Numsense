module internal Ploeh.Numsense.BrazilianPortuguese

let internal tryParseBrazilianImp (x : string) =
    match x with
    | "zero" -> Some 0
    | _ -> None
