module internal Ploeh.Numsense.InternalDsl

let internal (|StartsWith|_|) prefix (candidate : string) =
    if candidate.StartsWith prefix
    then Some (candidate.Substring prefix.Length)
    else None

let internal (|Between|_|) lower upper candidate =
    if lower <= candidate && candidate < upper
    then Some candidate
    else None

let internal (%*) factor x =
    let multiplicand = x % factor
    if multiplicand = 0 
    then x + factor
    else x + (factor * multiplicand) - multiplicand
