module Ploeh.Numsense.NumeralProperties

open FsCheck.Xunit
open Swensen.Unquote

[<Property(QuietOnSuccess = true)>]
let ``tryOfEnglish is the inverse of toEnglish`` (x : int) =
    test <@ Some x = (x |> Numeral.toEnglish |> Numeral.tryOfEnglish) @>

