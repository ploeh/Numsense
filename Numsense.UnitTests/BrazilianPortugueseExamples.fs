module Ploeh.Numsense.BrazilianPortugueseExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("zero", 0)>]
let ``tryParseBrazilian returns correct result`` (portuguese, expected) =
    let actual = Numeral.tryParseBrazilian portuguese
    Some expected =! actual
