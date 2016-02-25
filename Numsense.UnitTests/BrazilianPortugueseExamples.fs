﻿module Ploeh.Numsense.BrazilianPortugueseExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData(                "zero",   0)>]
[<InlineData(               " zero",   0)>]
[<InlineData(               "zero ",   0)>]
[<InlineData(            "  zero  ",   0)>]
[<InlineData(                "Zero",   0)>]
[<InlineData(                "ZERO",   0)>]
[<InlineData(             " zERo\t",   0)>]
[<InlineData(                  "um",   1)>]
[<InlineData(                  "UM",   1)>]
[<InlineData(                "dois",   2)>]
[<InlineData(               " dois",   2)>]
[<InlineData(                "três",   3)>]
[<InlineData(              "três  ",   3)>]
[<InlineData(              "quatro",   4)>]
[<InlineData(           "  quatro ",   4)>]
[<InlineData(               "cinco",   5)>]
[<InlineData(               "CincO",   5)>]
[<InlineData(                "seis",   6)>]
[<InlineData(            "  SEIS  ",   6)>]
[<InlineData(                "sete",   7)>]
[<InlineData(            "    seTe",   7)>]
[<InlineData(                "oito",   8)>]
[<InlineData(                "oITO",   8)>]
[<InlineData(                "nove",   9)>]
[<InlineData(              "NoVe  ",   9)>]
[<InlineData(                "dez",   10)>]
[<InlineData(               "onze",   11)>]
[<InlineData(               "doze",   12)>]
[<InlineData(              "treze",   13)>]
[<InlineData(            "catorze",   14)>]
[<InlineData(            "quatorze",  14)>]
[<InlineData(              "quinze",  15)>]
[<InlineData(           "dezesseis",  16)>]
[<InlineData(           "dezessete",  17)>]
[<InlineData(             "dezoito",  18)>]
[<InlineData(            "dezenove",  19)>]
[<InlineData(               "vinte",  20)>]
[<InlineData(          "vinte e um",  21)>]
[<InlineData(        "vinte e dois",  22)>]
[<InlineData(        "vinte e três",  23)>]
[<InlineData(      "vinte e quatro",  24)>]
[<InlineData(       "vinte e cinco",  25)>]
[<InlineData(        "vinte e seis",  26)>]
[<InlineData(        "vinte e sete",  27)>]
[<InlineData(        "vinte e oito",  28)>]
[<InlineData(        "vinte e nove",  29)>]
[<InlineData(              "trinta",  30)>]
[<InlineData(     "trinta e quatro",  34)>]
[<InlineData(            "quarenta",  40)>]
[<InlineData(       "quarenta e um",  41)>]
[<InlineData(           "cinquenta",  50)>]
[<InlineData(           "cinqüenta",  50)>]
[<InlineData(    "cinquenta e dois",  52)>]
[<InlineData(    "cinqüenta e sete",  57)>]
[<InlineData(            "sessenta",  60)>]
[<InlineData(     "sessenta e dois",  62)>]
[<InlineData(             "setenta",  70)>]
[<InlineData(    "setenta e quatro",  74)>]
[<InlineData(             "oitenta",  80)>]
[<InlineData(     "oitenta e cinco",  85)>]
[<InlineData(             "noventa",  90)>]
[<InlineData(      "noventa e seis",  96)>]
[<InlineData(                 "cem", 100)>]
[<InlineData(          "cento e um", 101)>]
[<InlineData("cento e vinte e seis", 126)>]
let ``tryParseBrazilian returns correct result`` (portuguese, expected) =
    let actual = Numeral.tryParseBrazilian portuguese
    Some expected =! actual
