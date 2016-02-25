﻿module Ploeh.Numsense.BrazilianPortugueseExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData(                                                              "zero",          0)>]
[<InlineData(                                                             " zero",          0)>]
[<InlineData(                                                             "zero ",          0)>]
[<InlineData(                                                          "  zero  ",          0)>]
[<InlineData(                                                              "Zero",          0)>]
[<InlineData(                                                              "ZERO",          0)>]
[<InlineData(                                                           " zERo\t",          0)>]
[<InlineData(                                                                "um",          1)>]
[<InlineData(                                                                "UM",          1)>]
[<InlineData(                                                              "dois",          2)>]
[<InlineData(                                                             " dois",          2)>]
[<InlineData(                                                              "três",          3)>]
[<InlineData(                                                            "três  ",          3)>]
[<InlineData(                                                            "quatro",          4)>]
[<InlineData(                                                         "  quatro ",          4)>]
[<InlineData(                                                             "cinco",          5)>]
[<InlineData(                                                             "CincO",          5)>]
[<InlineData(                                                              "seis",          6)>]
[<InlineData(                                                          "  SEIS  ",          6)>]
[<InlineData(                                                              "sete",          7)>]
[<InlineData(                                                          "    seTe",          7)>]
[<InlineData(                                                              "oito",          8)>]
[<InlineData(                                                              "oITO",          8)>]
[<InlineData(                                                              "nove",          9)>]
[<InlineData(                                                            "NoVe  ",          9)>]
[<InlineData(                                                              "dez",          10)>]
[<InlineData(                                                             "onze",          11)>]
[<InlineData(                                                             "doze",          12)>]
[<InlineData(                                                            "treze",          13)>]
[<InlineData(                                                          "catorze",          14)>]
[<InlineData(                                                          "quatorze",         14)>]
[<InlineData(                                                            "quinze",         15)>]
[<InlineData(                                                         "dezesseis",         16)>]
[<InlineData(                                                         "dezessete",         17)>]
[<InlineData(                                                           "dezoito",         18)>]
[<InlineData(                                                          "dezenove",         19)>]
[<InlineData(                                                             "vinte",         20)>]
[<InlineData(                                                        "vinte e um",         21)>]
[<InlineData(                                                      "vinte e dois",         22)>]
[<InlineData(                                                      "vinte e três",         23)>]
[<InlineData(                                                    "vinte e quatro",         24)>]
[<InlineData(                                                     "vinte e cinco",         25)>]
[<InlineData(                                                      "vinte e seis",         26)>]
[<InlineData(                                                      "vinte e sete",         27)>]
[<InlineData(                                                      "vinte e oito",         28)>]
[<InlineData(                                                      "vinte e nove",         29)>]
[<InlineData(                                                            "trinta",         30)>]
[<InlineData(                                                   "trinta e quatro",         34)>]
[<InlineData(                                                          "quarenta",         40)>]
[<InlineData(                                                     "quarenta e um",         41)>]
[<InlineData(                                                         "cinquenta",         50)>]
[<InlineData(                                                         "cinqüenta",         50)>]
[<InlineData(                                                  "cinquenta e dois",         52)>]
[<InlineData(                                                  "cinqüenta e sete",         57)>]
[<InlineData(                                                          "sessenta",         60)>]
[<InlineData(                                                   "sessenta e dois",         62)>]
[<InlineData(                                                           "setenta",         70)>]
[<InlineData(                                                  "setenta e quatro",         74)>]
[<InlineData(                                                           "oitenta",         80)>]
[<InlineData(                                                   "oitenta e cinco",         85)>]
[<InlineData(                                                           "noventa",         90)>]
[<InlineData(                                                    "noventa e seis",         96)>]
[<InlineData(                                                               "cem",        100)>]
[<InlineData(                                                        "cento e um",        101)>]
[<InlineData(                                              "cento e vinte e seis",        126)>]
[<InlineData(                                                          "duzentos",        200)>]
[<InlineData(                                          "duzentos e trinta e sete",        237)>]
[<InlineData(                                                         "trezentos",        300)>]
[<InlineData(                                       "trezentos e setenta e cinco",        375)>]
[<InlineData(                                                      "quatrocentos",        400)>]
[<InlineData(                                            "quatrocentos e noventa",        490)>]
[<InlineData(                                                        "quinhentos",        500)>]
[<InlineData(                                      "quinhentos e sessenta e sete",        567)>]
[<InlineData(                                                        "seiscentos",        600)>]
[<InlineData(                                      "seiscentos e trinta e quatro",        634)>]
[<InlineData(                                                        "setecentos",        700)>]
[<InlineData(                                      "setecentos e quarenta e oito",        748)>]
[<InlineData(                                                        "oitocentos",        800)>]
[<InlineData(                                              "oitocentos e catorze",        814)>]
[<InlineData(                                                        "novecentos",        900)>]
[<InlineData(                                         "novecentos e setenta e um",        971)>]
[<InlineData(                                                               "mil",       1000)>]
[<InlineData(                                                          "mil e um",       1001)>]
[<InlineData(                                                        "mil e dois",       1002)>]
[<InlineData(                                           "mil e sessenta e quatro",       1064)>]
[<InlineData(                                                    "mil e duzentos",       1200)>]
[<InlineData(                                      "mil, duzentos e oitenta e um",       1281)>]
[<InlineData(                                                            "um mil",       1000)>]
[<InlineData(                                                          "dois mil",       2000)>]
[<InlineData(                                                          "três mil",       3000)>]
[<InlineData(                                                        "quatro mil",       4000)>]
[<InlineData(                                                         "cinco mil",       5000)>]
[<InlineData(                                                          "seis mil",       6000)>]
[<InlineData(                                                          "sete mil",       7000)>]
[<InlineData(                                 "oito mil, setecentos e vinte e um",       8721)>]
[<InlineData(                                    "nove mil, cento e vinte e três",       9123)>]
[<InlineData(                                                           "dez mil",      10000)>]
[<InlineData(                                               "dez mil, cento e um",      10101)>]
[<InlineData(                                             "dez mil, cento e dois",      10102)>]
[<InlineData(                                                          "onze mil",      11000)>]
[<InlineData(                                                          "doze mil",      12000)>]
[<InlineData(                                                         "treze mil",      13000)>]
[<InlineData(                                                      "quatorze mil",      14000)>]
[<InlineData(                                                       "catorze mil",      14000)>]
[<InlineData(                                                        "quinze mil",      15000)>]
[<InlineData(                                                     "dezesseis mil",      16000)>]
[<InlineData(                                                     "dezessete mil",      17000)>]
[<InlineData(                                                       "dezoito mil",      18000)>]
[<InlineData(                                                      "dezenove mil",      19000)>]
[<InlineData(                  "vinte e três mil, quinhentos e sessenta e quatro",      23564)>]
[<InlineData(                                                 "oitenta mil e dez",      80010)>]
[<InlineData(                                                           "cem mil",     100000)>]
[<InlineData(                                                      "cem mil e um",     100001)>]
[<InlineData(             "trezentos e dezenove mil, trezentos e quarenta e nove",     319349)>]
[<InlineData(                                                         "um milhão",    1000000)>]
[<InlineData(                                                    "um milhão e um",    1000001)>]
[<InlineData(                                                   "um milhão e cem",    1000100)>]
[<InlineData(                            "um milhão, trezentos e vinte mil e cem",    1320100)>]
[<InlineData(                 "um milhão, trezentos e vinte mil, trezentos e dez",    1320310)>]
[<InlineData(                                                "dois milhões e dez",    2000010)>]
[<InlineData(                                         "seis milhões e trinta mil",    6030000)>]
[<InlineData(             "nove milhões, duzentos e um mil, seiscentos e oitenta",    9201680)>]
[<InlineData(                                                       "dez milhões",   10000000)>]
[<InlineData(                                                "dez milhões e nove",   10000009)>]
[<InlineData(                                                "vinte e um milhões",   21000000)>]
[<InlineData("quarenta e sete milhões, seiscentos e sessenta e um mil e sessenta",   47661060)>]
[<InlineData(                                                       "cem milhões",  100000000)>]
[<InlineData(                                 "cem milhões, dois mil e trezentos",  100002300)>]
[<InlineData(                                          "duzentos e cinco milhões",  205000000)>]
[<InlineData(                                          "trezentos e sete milhões",  307000000)>]
[<InlineData(   "quatrocentos e sessenta e um milhões, sessenta mil e seiscentos",  461060600)>]
[<InlineData(                                                         "um bilhão", 1000000000)>]
[<InlineData(                                                      "dois bilhões", 2000000000)>]
[<InlineData( "dois bilhões, quarenta e nove milhões, seiscentos e cinquenta mil", 2049650000)>]
[<InlineData(
    "dois bilhões, cento e quarenta e sete milhões, quatrocentos e oitenta e três mil, seiscentos e quarenta e sete",
    System.Int32.MaxValue)>]
let ``tryParseBrazilian returns correct result`` (portuguese, expected) =
    let actual = Numeral.tryParseBrazilian portuguese
    Some expected =! actual

[<Theory>]
[<InlineData(         0, "zero")>]
[<InlineData(         1, "um")>]
[<InlineData(         2, "dois")>]
[<InlineData(         3, "três")>]
[<InlineData(         4, "quatro")>]
[<InlineData(         5, "cinco")>]
[<InlineData(         6, "seis")>]
[<InlineData(         7, "sete")>]
[<InlineData(         8, "oito")>]
[<InlineData(         9, "nove")>]
[<InlineData(        10, "dez")>]
[<InlineData(        11, "onze")>]
[<InlineData(        12, "doze")>]
[<InlineData(        13, "treze")>]
[<InlineData(        14, "quatorze")>]
[<InlineData(        15, "quinze")>]
[<InlineData(        16, "dezesseis")>]
[<InlineData(        17, "dezessete")>]
[<InlineData(        18, "dezoito")>]
[<InlineData(        19, "dezenove")>]
[<InlineData(        20, "vinte")>]
[<InlineData(        21, "vinte e um")>]
[<InlineData(        27, "vinte e sete")>]
[<InlineData(        30, "trinta")>]
[<InlineData(        34, "trinta e quatro")>]
[<InlineData(        40, "quarenta")>]
[<InlineData(        42, "quarenta e dois")>]
[<InlineData(        50, "cinquenta")>]
[<InlineData(        58, "cinquenta e oito")>]
[<InlineData(        60, "sessenta")>]
[<InlineData(        65, "sessenta e cinco")>]
[<InlineData(        70, "setenta")>]
[<InlineData(        79, "setenta e nove")>]
[<InlineData(        80, "oitenta")>]
[<InlineData(        86, "oitenta e seis")>]
[<InlineData(        90, "noventa")>]
[<InlineData(        93, "noventa e três")>]
[<InlineData(       100, "cem")>]
[<InlineData(       101, "cento e um")>]
[<InlineData(       110, "cento e dez")>]
[<InlineData(       114, "cento e quatorze")>]
[<InlineData(       135, "cento e trinta e cinco")>]
[<InlineData(       200, "duzentos")>]
[<InlineData(       282, "duzentos e oitenta e dois")>]
[<InlineData(       331, "trezentos e trinta e um")>]
[<InlineData(       407, "quatrocentos e sete")>]
[<InlineData(       520, "quinhentos e vinte")>]
[<InlineData(       666, "seiscentos e sessenta e seis")>]
[<InlineData(       798, "setecentos e noventa e oito")>]
[<InlineData(       857, "oitocentos e cinquenta e sete")>]
[<InlineData(       999, "novecentos e noventa e nove")>]
[<InlineData(      1000, "mil")>]
[<InlineData(      1001, "mil e um")>]
[<InlineData(      1010, "mil e dez")>]
[<InlineData(      1066, "mil e sessenta e seis")>]
[<InlineData(      1337, "mil, trezentos e trinta e sete")>]
[<InlineData(      1500, "mil e quinhentos")>]
[<InlineData(      1984, "mil, novecentos e oitenta e quatro")>]
[<InlineData(      7441, "sete mil, quatrocentos e quarenta e um")>]
[<InlineData(      8513, "oito mil, quinhentos e treze")>]
[<InlineData(    100000, "cem mil")>]
[<InlineData(    100001, "cem mil e um")>]
[<InlineData(    100100, "cem mil e cem")>]
[<InlineData(    100101, "cem mil, cento e um")>]
[<InlineData(    948077, "novecentos e quarenta e oito mil e setenta e sete")>]
[<InlineData(    948200, "novecentos e quarenta e oito mil e duzentos")>]
[<InlineData(   1000000, "um milhão")>]
[<InlineData(   2000002, "dois milhões e dois")>]
[<InlineData(   3040506, "três milhões e quarenta mil, quinhentos e seis")>]
[<InlineData(   4025800, "quatro milhões e vinte e cinco mil e oitocentos")>]
[<InlineData(   4321000, "quatro milhões, trezentos e vinte e um mil")>]
[<InlineData(   4600819, "quatro milhões e seiscentos mil, oitocentos e dezenove")>]
[<InlineData(   5004621, "cinco milhões e quatro mil, seiscentos e vinte e um")>]
[<InlineData(   6982001, "seis milhões, novecentos e oitenta e dois mil e um")>]
[<InlineData(   7000000, "sete milhões")>]
[<InlineData(   8000220, "oito milhões, duzentos e vinte")>]
[<InlineData(   9099000, "nove milhões e noventa e nove mil")>]
[<InlineData(  10000000, "dez milhões")>]
[<InlineData(  10100000, "dez milhões e cem mil")>]
[<InlineData(  24000000, "vinte e quatro milhões")>]
[<InlineData(  39020011, "trinta e nove milhões e vinte mil e onze")>]
[<InlineData(  40606100, "quarenta milhões, seiscentos e seis mil e cem")>]
[<InlineData(  53000000, "cinquenta e três milhões")>]
[<InlineData(  64000098, "sessenta e quatro milhões e noventa e oito")>]
[<InlineData(  70003190, "setenta milhões e três mil, cento e noventa")>]
[<InlineData(  80000000, "oitenta milhões")>]
[<InlineData(  99000099, "noventa e nove milhões e noventa e nove")>]
[<InlineData( 100000000, "cem milhões")>]
[<InlineData( 209000000, "duzentos e nove milhões")>]
[<InlineData( 398000000, "trezentos e noventa e oito milhões")>]
[<InlineData( 439011000, "quatrocentos e trinta e nove milhões e onze mil")>]
[<InlineData( 560400000, "quinhentos e sessenta milhões e quatrocentos mil")>]
[<InlineData( 600010900, "seiscentos milhões e dez mil e novecentos")>]
[<InlineData( 700000000, "setecentos milhões")>]
[<InlineData( 800116000, "oitocentos milhões, cento e dezesseis mil")>]
[<InlineData( 900800007, "novecentos milhões e oitocentos mil e sete")>]
[<InlineData(1000000000, "um bilhão")>]
[<InlineData(1000000100, "um bilhão e cem")>]
[<InlineData(1000001000, "um bilhão e mil")>]
[<InlineData(1001000000, "um bilhão e um milhão")>]
[<InlineData(1500000000, "um bilhão e quinhentos milhões")>]
[<InlineData(1501000000, "um bilhão, quinhentos e um milhões")>]
[<InlineData(2121000000, "dois bilhões, cento e vinte e um milhões")>]
[<InlineData(2100028412, "dois bilhões e cem milhões e vinte e oito mil, quatrocentos e doze")>]
[<InlineData(
    System.Int32.MaxValue,
    "dois bilhões, cento e quarenta e sete milhões, quatrocentos e oitenta e três mil, seiscentos e quarenta e sete")>]
let ``toBrazilian returns correct result`` (i, expected) =
    let actual = Numeral.toBrazilian i
    expected =! actual
