module internal Ploeh.Numsense.Slovak

open Ploeh.Numsense.InternalDsl

type internal Gender =
    | MasculineAnimate
    | MasculineInanimate
    | Feminine
    | Neuter

let rec internal toSlovakImp gender x =
    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s %s" prefix (toSlovakImp MasculineInanimate remainder)

    let format suffixGender suffix factor x =
        let factored = x / factor
        let prefix =
            if factored = 1
            then sprintf "%s" (suffix factored)
            else sprintf "%s %s" (toSlovakImp suffixGender factored) (suffix factored)
        simplify prefix factor x

    let (|Singular|Paucal|Plural|) =
        function
        | 1 -> Singular
        | 2
        | 3
        | 4 -> Paucal
        | _ -> Plural

    let hundred _ = "sto"

    let thousand _ = "tisíc" 

    let million =
        function
        | Singular -> "milión"
        | Paucal   -> "milióny"
        | Plural   -> "miliónov"

    let billion =
        function
        | Singular -> "miliarda"
        | Paucal   -> "miliardy"
        | Plural   -> "miliárd"

    // Numbers 1-4 are gender-specific on their own, but they take
    // the masculine inanimate form in 24, 501, 1002, etc.
    match x with
    | x when x < 0 -> sprintf "mínus %s" (toSlovakImp gender -x)
    | 0                            -> "nula"
    | 1 ->
        match gender with
        | MasculineAnimate
        | MasculineInanimate       -> "jeden"
        | Feminine                 -> "jedna"
        | Neuter                   -> "jedno"
    | 2 ->
        match gender with
        | MasculineInanimate       -> "dva"
        | MasculineAnimate         -> "dvaja"
        | Feminine
        | Neuter                   -> "dve"
    | 3 ->
        match gender with
        | MasculineInanimate
        | Feminine
        | Neuter                   -> "tri"
        | MasculineAnimate         -> "traja"
    | 4 ->
        match gender with
        | MasculineInanimate
        | Feminine
        | Neuter                   -> "štyri"
        | MasculineAnimate         -> "štyria"
    |  5                           -> "päť"
    |  6                           -> "šesť"
    |  7                           -> "sedem"
    |  8                           -> "osem"
    |  9                           -> "deväť"
    | 10                           -> "desať"
    | 11                           -> "jedenásť"
    | 12                           -> "dvanásť"
    | 13                           -> "trinásť"
    | 14                           -> "štrnásť"
    | 15                           -> "pätnásť"
    | 16                           -> "šestnásť"
    | 17                           -> "sedemnásť"
    | 18                           -> "osemnásť"
    | 19                           -> "devätnásť"
    | Between              20 30 x -> simplify "dvadsať" 10 x
    | Between              30 40 x -> simplify "tridsať" 10 x
    | Between              40 50 x -> simplify "štyridsať" 10 x
    | Between              50 60 x -> simplify "päťdesiat" 10 x
    | Between              60 70 x -> simplify "šesťdesiat" 10 x
    | Between              70 80 x -> simplify "sedemdesiat" 10 x
    | Between              80 90 x -> simplify "osemdesiat" 10 x
    | Between             90 100 x -> simplify "deväťdesiat" 10 x
    | Between           100 1000 x -> format Feminine hundred 100 x
    | Between       1000 1000000 x -> format Feminine thousand 1000 x
    | Between 1000000 1000000000 x -> format MasculineInanimate million 1000000 x
    | _                            -> format Feminine billion 1000000000 x

let internal tryParseSlovakImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                            -> Some acc
        | StartsWith " "              t
        | StartsWith "NULA"           t -> conv           (0  + acc) t
        | StartsWith "JEDENÁSŤ"       t -> conv           (11 + acc) t
        | StartsWith "JEDEN"          t
        | StartsWith "JEDNA"          t
        | StartsWith "JEDNO"          t -> conv           (1  + acc) t
        | StartsWith "DVAJA"          t
        | StartsWith "DVA"            t
        | StartsWith "DVE"            t -> conv           (2  + acc) t
        | StartsWith "TRI"            t
        | StartsWith "TRAJA"          t -> conv           (3  + acc) t
        | StartsWith "ŠTYRIA"         t
        | StartsWith "ŠTYRI"          t -> conv           (4  + acc) t
        | StartsWith "PÄŤ"            t -> conv           (5  + acc) t
        | StartsWith "ŠESŤ"           t -> conv           (6  + acc) t
        | StartsWith "SEDEM"          t -> conv           (7  + acc) t
        | StartsWith "OSEM"           t -> conv           (8  + acc) t
        | StartsWith "DEVÄŤ"          t -> conv           (9  + acc) t
        | StartsWith "DESAŤ"          t -> conv          (10  + acc) t
        | StartsWith "ŠTRNÁSŤ"        t -> conv          (14  + acc) t
        | StartsWith "PÄTNÁSŤ"        t -> conv          (15  + acc) t
        | StartsWith "ŠESTNÁSŤ"       t -> conv          (16  + acc) t
        | StartsWith "DEVÄTNÁSŤ"      t -> conv          (19  + acc) t
        | StartsWith "NÁSŤ"           t -> conv          (10  + acc) t
        | StartsWith "DSAŤ"           t
        | StartsWith "DESIAT"         t -> conv          (10 %* acc) t
        | StartsWith "STO"            t ->
            conv (if acc % 100 <= 1 then 100 + acc else  100 %* acc) t
        | StartsWith "TISÍC"          t ->
            conv (if acc = 0 then       1000 else       1000 %* acc) t
        | StartsWith "MILIÓNY"        t -> conv     (1000000 %* acc) t
        | StartsWith "MILIÓNOV"       t -> conv     (1000000 %* acc) t
        | StartsWith "MILIÓN"         t -> conv     (1000000  + acc) t
        | StartsWith "MILIÁRD"        t -> conv  (1000000000 %* acc) t
        | StartsWith "MILIARDY"       t -> conv  (1000000000 %* acc) t
        | StartsWith "MILIARDA"       t -> conv  (1000000000  + acc) t
        | _ -> None
    
    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "sk-SK")
    match canonicalized with
    | StartsWith "MÍNUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized