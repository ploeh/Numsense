module internal Ploeh.Numsense.Lithuanian

open Ploeh.Numsense.InternalDsl

let rec internal toLithuanianImp x =

    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s-%s" prefix (toLithuanianImp (remainder))

    let formatNumeral suffix factor x =
        let prefix = sprintf "%s%s" (toLithuanianImp (x / factor)) suffix
        simplify prefix factor x

    let formatNoun suffix factor x =
        let factored = x /factor
        let prefix =
            match factored with
            | 1 -> sprintf "%s" (suffix factored)
            | _ -> sprintf "%s-%s" (toLithuanianImp factored) (suffix factored)
        simplify prefix factor x

    let (|Singular|Paucal|Plural|) x =
        let units = x % 10
        let tens = x / 10 % 10
        match tens, units with
        | 1, _
        | _, 0 -> Plural
        | _, 1 -> Singular
        | _ -> Paucal

    let thousand =
        function
        | Singular -> "tūkstantis"
        | Paucal   -> "tūkstančiai"
        | Plural   -> "tūkstančių"

    let million =
        function
        | Singular -> "milijonas"
        | Paucal   -> "milijonai"
        | Plural   -> "milijonų"

    let billion =
        function
        | Singular -> "milijardas"
        | Paucal   -> "milijardai"
        | Plural   -> "milijardų"

    match x with
    |  x when x < 0 -> sprintf "minus %s" (toLithuanianImp -x)
    |  0 -> "nulis"
    |  1 -> "vienas"
    |  2 -> "du"
    |  3 -> "trys"
    |  4 -> "keturi"
    |  5 -> "penki"
    |  6 -> "šeši"
    |  7 -> "septyni"
    |  8 -> "aštuoni"
    |  9 -> "devyni"
    | 10 -> "dešimt"
    | 11 -> "vienuolika"
    | 12 -> "dvylika"
    | 13 -> "trylika"
    | 14 -> "keturiolika"
    | 15 -> "penkiolika"
    | 16 -> "šešiolika"
    | 17 -> "septyniolika"
    | 18 -> "aštuoniolika"
    | 19 -> "devyniolika"
    | Between 20 30 x -> simplify "dvidešimt" 10 x
    | Between 30 40 x -> simplify "trisdešimt" 10 x
    | Between 40 100 x -> formatNumeral "asdešimt" 10 x
    | Between 100 200 x -> simplify "šimtas" 100 x
    | Between 200 1000 x -> formatNumeral "-šimtai" 100 x
    | Between 1000 1000000 x -> formatNoun thousand 1000 x
    | Between 1000000 1000000000 x -> formatNoun million 1000000 x
    | _ -> formatNoun billion 1000000000 x

let internal tryParseLithuanianImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | "" -> Some acc
        | StartsWith "-" t
        | StartsWith "IR" t -> conv acc t
        | StartsWith "NULIS" t -> conv (0 + acc) t
        | StartsWith "VIENAS" t -> conv (1 + acc) t
        | StartsWith "DU" t -> conv (2 + acc) t
        | StartsWith "TRYS" t -> conv (3 + acc) t
        | StartsWith "KETURI" t -> conv (4 + acc) t
        | StartsWith "PENKI" t -> conv (5 + acc) t
        | StartsWith "ŠEŠI" t -> conv (6 + acc) t
        | StartsWith "SEPTYNI" t -> conv (7 + acc) t
        | StartsWith "AŠTUONI" t -> conv (8 + acc) t
        | StartsWith "DEVYNI" t -> conv (9 + acc) t
        | StartsWith "DEŠIMT" t -> conv (10 + acc) t
        | StartsWith "VIENUOLIKA" t -> conv (11 + acc) t
        | StartsWith "DVYLIKA" t -> conv (12 + acc) t
        | StartsWith "TRYLIKA" t -> conv (13 + acc) t
        | StartsWith "OLIKA" t -> conv (10 + acc) t
        | StartsWith "DVIDEŠIMT" t -> conv (20 + acc) t
        | StartsWith "TRISDEŠIMT" t -> conv (30 + acc) t
        | StartsWith "ASDEŠIMT" t -> conv (10 %* acc) t
        | StartsWith "ŠIMTAS" t -> 
            conv (if acc % 10 = 0 then 100 + acc else 100 %* acc) t
        | StartsWith "ŠIMTAI" t -> conv (100 %* acc) t
        | StartsWith "TŪKSTANTIS" t -> 
            conv (if acc % 10 = 0 then 1000 + acc else 1000 %* acc) t
        | StartsWith "TŪKSTANČIAI" t
        | StartsWith "TŪKSTANČIŲ" t -> conv (1000 %* acc) t
        | StartsWith "MILIJONAS" t -> 
            conv (if acc % 10 = 0 then 1000000 + acc else 1000000 %* acc) t
        | StartsWith "MILIJONAI" t
        | StartsWith "MILIJONŲ" t -> conv (1000000 %* acc) t
        | StartsWith "MILIJARDAS" t -> conv 1000000000 t
        | StartsWith "MILIJARDAI" t
        | StartsWith "MILIJARDŲ" t -> conv (1000000000 %* acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "lt")
    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized