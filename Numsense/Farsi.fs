module internal Ploeh.Numsense.Farsi

open Ploeh.Numsense.InternalDsl

let rec internal toFarsiImp x =
    
    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s و %s" prefix (toFarsiImp (remainder))

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toFarsiImp (x / factor)) suffix
        simplify prefix factor x

    match x with
    |  x when x < 0 -> sprintf "منفی %s" (toFarsiImp -x)
    |  0 -> "صفر"
    |  1 -> "یک"
    |  2 -> "دو"
    |  3 -> "سه"
    |  4 -> "چهار"
    |  5 -> "پنج"
    |  6 -> "شش"
    |  7 -> "هفت"
    |  8 -> "هشت"
    |  9 -> "نه"
    | 10 -> "ده"
    | 11 -> "یازده"
    | 12 -> "دوازده"
    | 13 -> "سیزده"
    | 14 -> "چهارده"
    | 15 -> "پانزده"
    | 16 -> "شانزده"
    | 17 -> "هفده"
    | 18 -> "هجده"
    | 19 -> "نوزده"
    | Between 20 30 x -> simplify "بیست" 10 x
    | Between 30 40 x -> simplify "سی" 10 x
    | Between 40 50 x -> simplify "چهل" 10 x
    | Between 50 60 x -> simplify "پنجاه" 10 x
    | Between 60 70 x -> simplify "شصت" 10 x
    | Between 70 80 x -> simplify "هفتاد" 10 x
    | Between 80 90 x -> simplify "هشتاد" 10 x
    | Between 90 100 x -> simplify "نود" 10 x
    | Between 100 200 x -> simplify "صد" 100 x
    | Between 200 300 x -> simplify "دویست" 100 x
    | Between 300 400 x -> simplify "سیصد" 100 x
    | Between 400 500 x -> simplify "چهارصد" 100 x
    | Between 500 600 x -> simplify "پانصد" 100 x
    | Between 600 700 x -> simplify "ششصد" 100 x
    | Between 700 800 x -> simplify "هفتصد" 100 x
    | Between 800 900 x -> simplify "هشتصد" 100 x
    | Between 900 1000 x -> simplify "نهصد" 100 x
    | Between 1000 2000 x -> simplify "هزار" 1000 x
    | Between 2000 1000000 x -> format " هزار" 1000 x
    | Between 1000000 1000000000 x -> format " میلیون" 1000000 x
    | _ -> format " میلیارد" 1000000000 x


let internal tryParseFarsiImp (x : string) =
    let rec conv acc (candidate : string) = 
        let trimmedCandidate = candidate.Trim()
        match trimmedCandidate with
        | ""                        -> Some acc
        | StartsWith " "          t
        | StartsWith "و"          t -> conv acc                                     t
        | StartsWith "میلیون"     t -> conv (1000000 %* acc)                        t
        | StartsWith "میلیارد"    t -> conv (1000000000 %* acc)                     t
        | StartsWith "هزار"       t -> conv (if acc = 0 then 1000 else 1000 %* acc) t
        | StartsWith "صد"         t -> conv (if acc = 0 then 100 else 100 %* acc)   t

        | StartsWith "نهصد"       t -> conv (900 + acc) t
        | StartsWith "هشتصد"      t -> conv (800 + acc) t
        | StartsWith "هفتصد"      t -> conv (700 + acc) t
        | StartsWith "ششصد"       t -> conv (600 + acc) t
        | StartsWith "پانصد"      t -> conv (500 + acc) t
        | StartsWith "چهارصد"     t -> conv (400 + acc) t
        | StartsWith "سیصد"       t -> conv (300 + acc) t
        | StartsWith "دویست"      t -> conv (200 + acc) t
        | StartsWith "نوزده"      t -> conv (19 + acc)  t
        | StartsWith "هجده"       t -> conv (18 + acc)  t
        | StartsWith "هفده"       t -> conv (17 + acc)  t
        | StartsWith "شانزده"     t -> conv (16 + acc)  t
        | StartsWith "پانزده"     t -> conv (15 + acc)  t
        | StartsWith "چهارده"     t -> conv (14 + acc)  t
        | StartsWith "سیزده"      t -> conv (13 + acc)  t
        | StartsWith "دوازده"     t -> conv (12 + acc)  t
        | StartsWith "یازده"      t -> conv (11 + acc)  t
        | StartsWith "نود"        t -> conv (90 + acc)  t
        | StartsWith "هشتاد"      t -> conv (80 + acc)  t
        | StartsWith "هفتاد"      t -> conv (70 + acc)  t
        | StartsWith "شصت"        t -> conv (60 + acc)  t
        | StartsWith "پنجاه"      t -> conv (50 + acc)  t
        | StartsWith "چهل"        t -> conv (40 + acc)  t
        | StartsWith "سی"         t -> conv (30 + acc)  t
        | StartsWith "بیست"       t -> conv (20 + acc)  t
        | StartsWith "ده"         t -> conv (10 + acc)  t
        | StartsWith "نه"         t -> conv (9  + acc)  t
        | StartsWith "هشت"        t -> conv (8  + acc)  t
        | StartsWith "هفت"        t -> conv (7  + acc)  t
        | StartsWith "شش"         t -> conv (6  + acc)  t
        | StartsWith "پنج"        t -> conv (5  + acc)  t
        | StartsWith "چهار"       t -> conv (4  + acc)  t
        | StartsWith "سه"         t -> conv (3  + acc)  t
        | StartsWith "دو"         t -> conv (2  + acc)  t
        | StartsWith "یک"         t -> conv (1  + acc)  t
        | StartsWith "صفر"        t -> conv (0  + acc)  t
        | _ -> None
 

    match x with
    | StartsWith "منفی" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 x



