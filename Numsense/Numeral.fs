module Ploeh.Numsense.Numeral

let rec toEnglish x =
    let (|Between|_|) lower upper candidate =
        if lower < candidate && candidate < upper
        then Some candidate
        else None

    match x with
    | x' when x' < 0 -> sprintf "minus %s" (toEnglish -x)
    |  0 -> "zero"
    |  1 -> "one"
    |  2 -> "two"
    |  3 -> "three"
    |  4 -> "four"
    |  5 -> "five"
    |  6 -> "six"
    |  7 -> "seven"
    |  8 -> "eight"
    |  9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | 20 -> "twenty"
    | Between 20 30 x' -> sprintf "twenty-%s" (toEnglish (x' % 10))
    | 30 -> "thirty"
    | Between 30 40 x' -> sprintf "thirty-%s" (toEnglish (x' % 10))
    | 40 -> "forty"
    | Between 40 50 x' -> sprintf "forty-%s" (toEnglish (x' % 10))
    | 50 -> "fifty"
    | Between 50 60 x' -> sprintf "fifty-%s" (toEnglish (x' % 10))
    | 60 -> "sixty"
    | Between 60 70 x' -> sprintf "sixty-%s" (toEnglish (x' % 10))
    | 70 -> "seventy"
    | Between 70 80 x' -> sprintf "seventy-%s" (toEnglish (x' % 10))
    | 80 -> "eighty"
    | Between 80 90 x' -> sprintf "eighty-%s" (toEnglish (x' % 10))
    | 90 -> "ninety"
    | Between 90 100 x' -> sprintf "ninety-%s" (toEnglish (x' % 10))
    | x' when 99 < x' && x' < 1000 && x' % 100 = 0 ->
        sprintf "%s-hundred" (toEnglish (x' / 100))
    | Between 99 1000 x' ->
        sprintf "%s-hundred-%s" (toEnglish (x' / 100)) (toEnglish (x' % 100))
    | x' when 999 < x' && x' < 1000000 && x' % 1000 = 0 ->
        sprintf "%s-thousand" (toEnglish (x' / 1000))
    | Between 999 1000000 x' ->
        sprintf "%s-thousand-%s" (toEnglish (x' / 1000)) (toEnglish (x' % 1000))
    | x' when 999999 < x' && x' < 1000000000 && x' % 1000000 = 0 ->
        sprintf "%s-million" (toEnglish (x' / 1000000))
    | Between 999999 1000000000 x' ->
        sprintf "%s-million-%s" (toEnglish (x' / 1000000)) (toEnglish (x' % 1000000))
    | x' when 999999999 < x' && x' % 1000000000 = 0 ->
        sprintf "%s-billion" (toEnglish (x' / 1000000000))
    | x' when 999999999 < x' ->
        sprintf "%s-billion-%s" (toEnglish (x' / 1000000000)) (toEnglish (x' % 1000000000))
    | _ ->
        // This case shouldn't be reached, but due to the the use of 'when'
        // clauses in the above pattern match, the compiler can't verify that
        // all cases are covered.
        failwith (sprintf "Unhandled number: %i" x)

let tryOfEnglish (x : string) =
    let (%*) factor x =
        let multiplicand = x % factor
        x + (factor * multiplicand) - multiplicand

    let (|StartsWith|_|) prefix (candidate : string) =
        if candidate.StartsWith prefix
        then Some (candidate.Substring prefix.Length)
        else None

    let rec conv acc xs =        
        match xs with
        | ""                      -> Some acc
        | StartsWith "-"        t
        | StartsWith "AND"      t -> conv                acc  t
        | StartsWith "ZERO"     t -> conv          (0  + acc) t
        | StartsWith "ONE"      t -> conv          (1  + acc) t
        | StartsWith "TWO"      t -> conv          (2  + acc) t
        | StartsWith "THREE"    t -> conv          (3  + acc) t
        | StartsWith "FOUR"     t -> conv          (4  + acc) t
        | StartsWith "FIVE"     t -> conv          (5  + acc) t
        | StartsWith "SIX"      t -> conv          (6  + acc) t
        | StartsWith "SEVEN"    t -> conv          (7  + acc) t
        | StartsWith "EIGHT"    t -> conv          (8  + acc) t
        | StartsWith "NINE"     t -> conv          (9  + acc) t
        | StartsWith "TEN"      t -> conv         (10  + acc) t
        | StartsWith "ELEVEN"   t -> conv         (11  + acc) t
        | StartsWith "TWELVE"   t -> conv         (12  + acc) t
        | StartsWith "THIRTEEN" t -> conv         (13  + acc) t
        | StartsWith "FIFTEEN"  t -> conv         (15  + acc) t
        | StartsWith "EEN"      t // matches 'een' in 'eighteen'         
        | StartsWith "TEEN"     t -> conv         (10  + acc) t
        | StartsWith "TWENTY"   t -> conv         (20  + acc) t
        | StartsWith "THIRTY"   t -> conv         (30  + acc) t
        | StartsWith "FORTY"    t -> conv         (40  + acc) t
        | StartsWith "FIFTY"    t -> conv         (50  + acc) t
        | StartsWith "Y"        t // matches 'y' in 'eighty'
        | StartsWith "TY"       t -> conv         (10 %* acc) t
        | StartsWith "HUNDRED"  t ->
            conv (if acc = 0 then  100 else       100 %* acc) t
        | StartsWith "THOUSAND" t ->
            conv (if acc = 0 then 1000 else      1000 %* acc) t
        | StartsWith "MILLION"  t -> conv    (1000000 %* acc) t
        | StartsWith "BILLION"  t -> conv (1000000000  * acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "en")
    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized