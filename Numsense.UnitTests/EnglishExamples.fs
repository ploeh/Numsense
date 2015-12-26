module Ploeh.Numsense.EnglishExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("zero", 0)>]
let ``ofEnglish returns correct result`` (english : string, expected : int) =
    let actual = Numeral.ofEnglish english
    expected =! actual