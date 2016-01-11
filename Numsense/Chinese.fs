module internal Ploeh.Numsense.Chinese

open Ploeh.Numsense.InternalDsl

type CharacterSet =
    {
        Negative:        string;
        Zero:            string;
        One:             string;
        Two:             string;
        AlternativeTwo:  string;
        Three:           string;
        Four:            string;
        Five:            string;
        Six:             string;
        Seven:           string;
        Eight:           string;
        Nine:            string;
        Ten:             string;
        Hundred:         string;
        Thousand:        string;
        TenThousands:    string;
        HundredMillions: string
    }

let internal traditionalCharacterSet =
    {
        Negative        = "負";
        Zero            = "零";
        One             = "一";
        Two             = "二";
        AlternativeTwo  = "兩";
        Three           = "三";
        Four            = "四";
        Five            = "五";
        Six             = "六";
        Seven           = "七";
        Eight           = "八";
        Nine            = "九";
        Ten             = "十";
        Hundred         = "百";
        Thousand        = "千";
        TenThousands    = "萬";
        HundredMillions = "億"
    }

let internal traditionalFinancialCharacterSet =
    {
        Negative        = "負";
        Zero            = "零";
        One             = "壹";
        Two             = "貳";
        AlternativeTwo  = "兩";
        Three           = "叄";
        Four            = "肆";
        Five            = "伍";
        Six             = "陸";
        Seven           = "柒";
        Eight           = "捌";
        Nine            = "玖";
        Ten             = "拾";
        Hundred         = "佰";
        Thousand        = "仟";
        TenThousands    = "萬";
        HundredMillions = "億"
    }

let internal simplifiedCharacterSet =
    {
        Negative        = "负";
        Zero            = "零";
        One             = "一";
        Two             = "二";
        AlternativeTwo  = "两";
        Three           = "三";
        Four            = "四";
        Five            = "五";
        Six             = "六";
        Seven           = "七";
        Eight           = "八";
        Nine            = "九";
        Ten             = "十";
        Hundred         = "百";
        Thousand        = "千";
        TenThousands    = "万";
        HundredMillions = "亿"
    }

let internal simplifiedFinancialCharacterSet =
    {
        Negative        = "负";
        Zero            = "零";
        One             = "壹";
        Two             = "贰";
        AlternativeTwo  = "两";
        Three           = "叁";
        Four            = "肆";
        Five            = "伍";
        Six             = "陆";
        Seven           = "柒";
        Eight           = "捌";
        Nine            = "玖";
        Ten             = "拾";
        Hundred         = "佰";
        Thousand        = "仟";
        TenThousands    = "万";
        HundredMillions = "亿"
    }

let rec internal toChineseImp characterSet x =

    let getCharacter useAlternativeTwo x =
        match x with
        | 0 -> characterSet.Zero
        | 1 -> characterSet.One
        | 2 -> if useAlternativeTwo then characterSet.AlternativeTwo else characterSet.Two
        | 3 -> characterSet.Three
        | 4 -> characterSet.Four
        | 5 -> characterSet.Five
        | 6 -> characterSet.Six
        | 7 -> characterSet.Seven
        | 8 -> characterSet.Eight
        | 9 -> characterSet.Nine
        | _ -> ""

    let formatTenThousand suffix previousValue x =
        let formatInternal suffix mayOutputZero isQuantity x =
            match x, mayOutputZero with
            | 0, false -> ""
            | 0, true  -> sprintf "%s" (getCharacter isQuantity x)
            | _, _     -> sprintf "%s%s" (getCharacter isQuantity x) suffix

        let thousands = (x / 1000) % 10
        let hundreds = (x / 100) % 10
        let tens = (x / 10) % 10
        let units = x % 10

        sprintf
            "%s%s%s%s%s"
            (formatInternal characterSet.Thousand (previousValue <> 0 && hundreds <> 0) true thousands)
            (formatInternal characterSet.Hundred ((previousValue <> 0 || thousands <> 0) && tens <> 0) true hundreds)
            (formatInternal characterSet.Ten ((previousValue <> 0 || hundreds <> 0 || thousands <> 0) && units <> 0) false tens)
            (formatInternal "" false false units)
            (if thousands <> 0 || hundreds <> 0 || tens <> 0 || units <> 0 then suffix else "")

    let format x =
        let hundredMillions = (x / 100000000) % 10000
        let tenThousands = (x / 10000) % 10000
        let units = x % 10000

        sprintf
            "%s%s%s"
            (formatTenThousand characterSet.HundredMillions 0 hundredMillions)
            (formatTenThousand characterSet.TenThousands (x / 100000000) tenThousands)
            (formatTenThousand "" (x / 10000) units)

    match x with
    | x when x < 0 -> sprintf "%s%s" characterSet.Negative (toChineseImp characterSet -x)
    | Between 0 10 x -> getCharacter true x
    | 10 -> characterSet.Ten
    | Between 11 20 x -> sprintf "%s%s" characterSet.Ten (getCharacter false (x % 10))
    | _ -> format x

let internal tryParseChineseImp characterSet (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                                           -> Some acc
        | StartsWith characterSet.Zero               t ->
            if t.StartsWith characterSet.Ten
            then conv (1 + acc) t
            else conv (0 + acc) t
        | StartsWith characterSet.One                t -> conv (1 + acc) t
        | StartsWith characterSet.Two                t
        | StartsWith characterSet.AlternativeTwo     t -> conv (2 + acc) t
        | StartsWith characterSet.Three              t -> conv (3 + acc) t
        | StartsWith characterSet.Four               t -> conv (4 + acc) t
        | StartsWith characterSet.Five               t -> conv (5 + acc) t
        | StartsWith characterSet.Six                t -> conv (6 + acc) t
        | StartsWith characterSet.Seven              t -> conv (7 + acc) t
        | StartsWith characterSet.Eight              t -> conv (8 + acc) t
        | StartsWith characterSet.Nine               t -> conv (9 + acc) t
        | StartsWith characterSet.Ten                t ->
            conv (if acc = 0 then 10 else 10 %* acc) t
        | StartsWith characterSet.Hundred            t -> conv (100 %* acc) t
        | StartsWith characterSet.Thousand           t -> conv (1000 %* acc) t
        | StartsWith characterSet.TenThousands       t -> conv (10000 %* acc) t
        | StartsWith characterSet.HundredMillions    t -> conv (100000000 %* acc) t
        | _                                            -> None

    let canonicalized = x.Trim()
    match canonicalized with
    | StartsWith characterSet.Negative t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized