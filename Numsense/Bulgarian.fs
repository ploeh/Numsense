module internal Ploeh.Numsense.Bulgarian

open Ploeh.Numsense.InternalDsl

let rec internal toBulgarianImp x =

    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s-%s" prefix (toBulgarianImp remainder)

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toBulgarianImp (x / factor)) suffix
        simplify prefix factor x

    let formatTens x =
        let tens = x / 10
        match x with
        | 10 -> "десет"
        | 11 -> "единадесет"
        | 12 -> "дванадесет"
        | 13 -> "тринадесет"
        | 14 -> "четиринадесет"
        | 15 -> "петнадесет"
        | 16 -> "шестнадесет"
        | 17 -> "седемнадесет"
        | 18 -> "осемнадесет"
        | 19 -> "деветнадесет"
        | x when x < 20 -> format "надесет" 1 tens
        | _ -> match tens with
               | 2 -> simplify "двадесет" 10 x
               | 4 -> simplify "четиресет" 10 x
               | _ -> simplify (format "десет" 1 tens) 10 x

    let formatHundreds x =
        let hundreds = x / 100
        match hundreds with
        | 1 -> simplify "сто" 100 x
        | 2 -> simplify "двеста" 100 x
        | 3 -> simplify "триста" 100 x
        | _ -> format "стотин" 100 x

    let formatNumerals single plural1 plural2 factor x =
        let thousands = x / factor
        let remainder = thousands % 10
        match thousands with
        | 1 -> simplify single factor x
        | thousands when thousands > 9 && thousands < 20 -> format plural1 factor x
        | _ -> match remainder with
               | remainder when remainder > 1 && remainder < 5 -> format plural2 factor x
               | _ -> format plural1 factor x

    match x with
    |  x when x < 0 -> sprintf "минус %s" (toBulgarianImp -x)
    |  0 -> "нула"
    |  1 -> "едно"
    |  2 -> "две"
    |  3 -> "три"
    |  4 -> "четири"
    |  5 -> "пет"
    |  6 -> "шест"
    |  7 -> "седем"
    |  8 -> "осем"
    |  9 -> "девет"
    | Between 10 100 x -> formatTens x
    | Between 100 1000 x -> formatHundreds x
    | Between 1000 1000000 x -> formatNumerals "tysiąc" "-tysięcy" "-tysiące" 1000 x
    | Between 1000000 1000000000 x -> formatNumerals "milion" "-milionów" "-miliony" 1000000 x
    | _ -> formatNumerals "miliard" "-miliardów" "-miliardy" 1000000000 x

let internal tryParseBulgarianImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                            -> Some acc
        | StartsWith "-"              t
        | StartsWith "И"              t -> conv                 acc  t
        | StartsWith "НУЛА"           t -> conv           (0  + acc) t
        | StartsWith "ЕДИНАДЕСЕТ"     t -> conv          (11  + acc) t
        | StartsWith "ЕДНА"           t // Matches една in двеста и една хиляди
        | StartsWith "ЕДИН"           t // Matches един in един милион/милиард
        | StartsWith "ЕДНО"           t -> conv           (1  + acc) t
        | StartsWith "ДВА"            t // Macthes два in дванадесет, двадесет and два милиона/милиарда
        | StartsWith "ДВЕ"            t -> conv           (2  + acc) t
        | StartsWith "ТРИ"            t -> conv           (3  + acc) t
        | StartsWith "ЧЕТИРИ"         t -> conv           (4  + acc) t
        | StartsWith "ПЕТ"            t -> conv           (5  + acc) t
        | StartsWith "ШЕСТ"           t -> conv           (6  + acc) t
        | StartsWith "СЕДЕМ"          t -> conv           (7  + acc) t
        | StartsWith "ОСЕМ"           t -> conv           (8  + acc) t
        | StartsWith "ДЕВЕТ"          t -> conv           (9  + acc) t
        | StartsWith "НАДЕСЕТ"        t -> conv           (10 + acc) t
        | StartsWith "ДЕСЕТ"          t -> 
           conv (if acc % 10 = 0
                       then 10 + acc       else          10  %* acc) t
        | StartsWith "ЧЕТИРЕСЕТ"      t -> conv          (40  + acc) t
        | StartsWith "СТА"            t // Matches ста in двеста and триста
        | StartsWith "СТОТИН"         t -> conv         (100 %* acc) t
        | StartsWith "СТО"            t -> conv          (100 + acc) t
        | StartsWith "ХИЛЯДА"         t -> conv                 1000 t
        | StartsWith "ХИЛЯДИ"         t -> conv        (1000 %* acc) t
        | StartsWith "МИЛИОНА"        t -> conv     (1000000 %* acc) t
        | StartsWith "МИЛИОН"         t -> conv              1000000 t
        | StartsWith "МИЛИАРДА"       t -> conv  (1000000000 %* acc) t
        | StartsWith "МИЛИАРД"        t -> conv           1000000000 t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "bg")
    match canonicalized with
    | StartsWith "МИНУС" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized