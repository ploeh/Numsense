module Ploeh.Numsense.DanishExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData(                                              "nul",          0)>]
[<InlineData(                                             " nul",          0)>]
[<InlineData(                                             "nul ",          0)>]
[<InlineData(                                          "  nul  ",          0)>]
[<InlineData(                                              "Nul",          0)>]
[<InlineData(                                              "NUL",          0)>]
[<InlineData(                                            "nUl\t",          0)>]
[<InlineData(                                               "et",          1)>]
[<InlineData(                                               "ET",          1)>]
[<InlineData(                                               "to",          2)>]
[<InlineData(                                              " to",          2)>]
[<InlineData(                                              "tre",          3)>]
[<InlineData(                                            "tre  ",          3)>]
[<InlineData(                                             "fire",          4)>]
[<InlineData(                                          "  fire ",          4)>]
[<InlineData(                                              "fem",          5)>]
[<InlineData(                                              "FeM",          5)>]
[<InlineData(                                             "seks",          6)>]
[<InlineData(                                         "  SEKS  ",          6)>]
[<InlineData(                                              "syv",          7)>]
[<InlineData(                                          "    sYv",          7)>]
[<InlineData(                                             "otte",          8)>]
[<InlineData(                                             "oTTE",          8)>]
[<InlineData(                                               "ni",          9)>]
[<InlineData(                                             "Ni  ",          9)>]
[<InlineData(                                               "ti",         10)>]
[<InlineData(                                           "elleve",         11)>]
[<InlineData(                                             "tolv",         12)>]
[<InlineData(                                          "tretten",         13)>]
[<InlineData(                                          "fjorten",         14)>]
[<InlineData(                                           "femten",         15)>]
[<InlineData(                                          "seksten",         16)>]
[<InlineData(                                           "sytten",         17)>]
[<InlineData(                                            "atten",         18)>]
[<InlineData(                                           "nitten",         19)>]
let ``tryOfDanish returns correct result`` (danish : string, expected : int) =
    let actual = Numeral.tryOfDanish danish
    Some expected =! actual

