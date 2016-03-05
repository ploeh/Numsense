module internal Ploeh.Numsense.Bulgarian

open Ploeh.Numsense.InternalDsl

// The magnitude parameter is only used in order to determine the
// right gender in some situations ("1" and "2" change gender)
let rec internal toBulgarianImp magnitude x =

    // Determines if a number should be bound by "и" ("and").
    // Number 1-19 should be bound (f.x., две хиляди И ЕДНО).
    // Same is valid for round numbers 20, ..., 90, 100, 200,..., 900,
    // 1000, 2000, ..., 10000, 11000, ..., 19000, 20000, 30000, ...,
    // 100000, ...
    let rec bindable x =
        match x with
        | x when x < 20    -> true
        | x when x < 100   -> (x % 10 = 0)
        | x when x < 1000  -> (x % 100 = 0)
        | _                -> let remainder = x % 1000
                              match remainder with
                              | 0 -> bindable (x / 1000)
                              | _ -> false

    // Gets the binding between two parts of the number, based on the
    // remainder after the division
    let binding remainder =
        if bindable remainder
        then "-и-"
        else "-"

    let formatTens x =
        let tens = x / 10
        let remainder = x % 10
        let mag = max magnitude 10
        match x with
        | 10 -> "десет"
        | 11 -> "единадесет"
        | 12 -> "дванадесет"
        | x when x < 20
             -> sprintf "%s%s" (toBulgarianImp 1 remainder) "надесет"
        | x when remainder = 0
             -> match tens with
                | 2 -> "двадесет"
                | _ -> sprintf "%s%s" (toBulgarianImp 1 tens) "десет"
        | _  -> sprintf "%s-и-%s" 
                          (toBulgarianImp 1 (tens * 10)) 
                          (toBulgarianImp mag remainder)

    let formatHundreds x =
        let remainder = x % 100
        let hundreds = x / 100
        let mag = max magnitude 100
        match remainder with
        | 0 -> match hundreds with
               | 1 -> "сто"
               | 2
               | 3 -> sprintf "%s%s" (toBulgarianImp 1 hundreds) "ста"
               | _ -> sprintf "%s%s" (toBulgarianImp 1 hundreds) "стотин"
        | _ -> sprintf "%s%s%s" 
                         (toBulgarianImp mag (hundreds * 100)) 
                         (binding remainder) 
                         (toBulgarianImp mag remainder)

    let formatNumerals single plural factor x =
        let remainder = x % factor
        let numerals = x / factor
        let mag = max magnitude factor
        match remainder with
        | 0 -> match numerals with
               | 1 -> single
               | _ -> sprintf "%s-%s" (toBulgarianImp mag numerals) plural
        | _ -> sprintf "%s%s%s" 
                         (toBulgarianImp mag (numerals * factor)) 
                         (binding remainder) 
                         (toBulgarianImp 1 remainder)

    match x with
    | x when x < 0 -> sprintf "минус %s" (toBulgarianImp magnitude -x)
    | 0 -> "нула"
    | 1 -> match magnitude with
           | 1000 -> "една" // Matches една, f.x., in двеста и една хиляди
           | x when x > 1000 
                  -> "един" // Matches една, f.x., in един милион/милиард
           | _    -> "едно"
    | 2 -> match magnitude with
           | x when x > 1000
                  -> "два" // Matches два, f.x., in дванадесет, двадесет and два милиона
           | _    -> "две"
    | 3 -> "три"
    | 4 -> "четири"
    | 5 -> "пет"
    | 6 -> "шест"
    | 7 -> "седем"
    | 8 -> "осем"
    | 9 -> "девет"
    | Between      10        100 x -> formatTens x
    | Between     100       1000 x -> formatHundreds x
    | Between    1000    1000000 x -> formatNumerals "хиляда" "хиляди" 1000 x
    | Between 1000000 1000000000 x -> formatNumerals "един-милион" "милиона" 1000000 x
    | _                            -> formatNumerals "един-милиард" "милиарда" 1000000000 x

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
        | StartsWith "ДВА"            t // Macthes два in дванадесет, двадесет and два милиарда
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