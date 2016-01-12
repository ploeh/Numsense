module internal Ploeh.Numsense.Russian

open Ploeh.Numsense.InternalDsl

type internal Gender =
    | Masculine
    | Feminine
    | Neuter

let rec internal toRussianImp gender x =

    let simplify gender prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s %s" prefix (toRussianImp gender (remainder))

    let impUnderThousand gender x =
        let simplify = simplify gender
        match x with
        |  1 ->
            match gender with
            | Feminine -> "одна"
            | Neuter -> "одно"
            | _ -> "один"
        |  2 ->
            match gender with
            | Feminine -> "две"
            | _ -> "два"
        |  3 -> "три"
        |  4 -> "четыре"
        |  5 -> "пять"
        |  6 -> "шесть"
        |  7 -> "семь"
        |  8 -> "восемь"
        |  9 -> "девять"
        | 10 -> "десять"
        | 11 -> "одиннадцать"
        | 12 -> "двенадцать"
        | 13 -> "тринадцать"
        | 14 -> "четырнадцать"
        | 15 -> "пятнадцать"
        | 16 -> "шестнадцать"
        | 17 -> "семнадцать"
        | 18 -> "восемнадцать"
        | 19 -> "девятнадцать"
        | Between 20 30 x -> simplify "двадцать" 10 x
        | Between 30 40 x -> simplify "тридцать" 10 x
        | Between 40 50 x -> simplify "сорок" 10 x
        | Between 50 60 x -> simplify "пятьдесят" 10 x
        | Between 60 70 x -> simplify "шестьдесят" 10 x
        | Between 70 80 x -> simplify "семьдесят" 10 x
        | Between 80 90 x -> simplify "восемьдесят" 10 x
        | Between 90 100 x -> simplify "девяносто" 10 x
        | Between 100 200 x -> simplify "сто" 100 x
        | Between 200 300 x -> simplify "двести" 100 x
        | Between 300 400 x -> simplify "триста" 100 x
        | Between 400 500 x -> simplify "четыреста" 100 x
        | Between 500 600 x -> simplify "пятьсот" 100 x
        | Between 600 700 x -> simplify "шестьсот" 100 x
        | Between 700 800 x -> simplify "семьсот" 100 x
        | Between 800 900 x -> simplify "восемьсот" 100 x
        | Between 900 1000 x -> simplify "девятьсот" 100 x

    let format suffixGender suffix factor x =
        let factored = x / factor
        let prefix = sprintf "%s %s" (impUnderThousand suffixGender factored) (suffix factored)
        simplify gender prefix factor x

    let (|Singular|Paucal|Plural|) x =
        let tensDigit = x / 10 % 10
        let unitDigit = x % 10
        match tensDigit, unitDigit with
        | 1, _ -> Plural
        | _, 1 -> Singular
        | _, 2
        | _, 3
        | _, 4 -> Paucal
        | _, _ -> Plural

    let thousand =
        function
        | Singular -> "тысяча"
        | Paucal   -> "тысячи"
        | Plural   -> "тысячь"

    let million =
        function
        | Singular -> "миллион"
        | Paucal   -> "миллиона"
        | Plural   -> "миллионов"

    let billion =
        function
        | Singular -> "миллиард"
        | Paucal   -> "миллиарда"
        | Plural   -> "миллиардов"

    match x with
    |  x when x < 0 -> sprintf "минус %s" (toRussianImp gender -x)
    |  0 -> "ноль"
    | Between 1 1000 x -> impUnderThousand gender x
    | Between 1000 1000000 x -> format Feminine thousand 1000 x
    | Between 1000000 1000000000 x -> format Masculine million 1000000 x
    | _ -> format Masculine billion 1000000000 x

let internal tryParseRussianImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                        -> Some acc
        | StartsWith " "          t -> conv                     acc  t
        | StartsWith "ноль"       t -> conv               (0  + acc) t
        | StartsWith "один"       t
        | StartsWith "одно"       t
        | StartsWith "одна"       t -> conv               (1  + acc) t
        | StartsWith "два"        t
        | StartsWith "две"        t -> conv               (2  + acc) t
        | StartsWith "три"        t -> conv               (3  + acc) t
        | StartsWith "четыре"     t
        | StartsWith "четыр"      t -> conv               (4  + acc) t
        | StartsWith "пять"       t
        | StartsWith "пят"        t -> conv               (5  + acc) t
        | StartsWith "шесть"      t
        | StartsWith "шест"       t -> conv               (6  + acc) t
        | StartsWith "семь"       t
        | StartsWith "сем"        t -> conv               (7  + acc) t
        | StartsWith "восемь"     t
        | StartsWith "восем"      t -> conv               (8  + acc) t
        | StartsWith "девять"     t
        | StartsWith "девят"      t -> conv               (9  + acc) t
        | StartsWith "десять"     t -> conv              (10  + acc) t
        | StartsWith "надцать"    t -> conv              (10  + acc) t
        | StartsWith "сорок"      t -> conv              (40  + acc) t
        | StartsWith "девяносто"  t -> conv              (90  + acc) t
        | StartsWith "дцать"      t
        | StartsWith "десят"      t -> conv              (10 %* acc) t
        | StartsWith "сто"        t -> conv             (100  + acc) t
        | StartsWith "ста"        t
        | StartsWith "сти"        t
        | StartsWith "сот"        t -> conv             (100 %* acc) t
        | StartsWith "тысячь"     t
        | StartsWith "тысяча"     t
        | StartsWith "тысячи"     t ->
            conv (if acc = 0 then 1000 else             1000 %* acc) t
        | StartsWith "миллионов"  t
        | StartsWith "миллиона"   t
        | StartsWith "миллион"    t ->
            conv (if acc = 0 then 1000000 else       1000000 %* acc) t
        | StartsWith "миллиардов" t
        | StartsWith "миллиарда"  t
        | StartsWith "миллиард"   t ->
            conv (if acc = 0 then 1000000000 else 1000000000 %* acc) t
        | _ -> None

    let canonicalized = x.Trim().ToLower(System.Globalization.CultureInfo "ru")
    match canonicalized with
    | StartsWith "минус" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized