module Tests exposing (..)

import Test exposing (..)
import Expect
import String exposing (toList)
import EditDistance
    exposing
        ( EditStep(..)
        , edits
        , editsWithCostFunc
        , editsFromStrings
        , levenshtein
        , levenshteinFromStrings
        )


all : Test
all =
    describe "The EditDistance module"
        [ describe "when getting edits between lists"
            [ test "kitten -> sitting edit steps" <|
                \() ->
                    Expect.equal (edits kitten sitting)
                        [ Substitute 's' 0
                        , Substitute 'i' 4
                        , Insert 'g' 6
                        ]
            , test "sitting -> kitten edit steps" <|
                \() ->
                    Expect.equal (edits sitting kitten)
                        [ Substitute 'k' 0
                        , Substitute 'e' 4
                        , Delete 'g' 6
                        ]
            , test "garvey -> avery edit steps" <|
                \() ->
                    Expect.equal (edits garvey avery)
                        [ Delete 'g' 0
                        , Move 'r' 2 3
                        ]
            , test "avery -> garvey edit steps" <|
                \() ->
                    Expect.equal (edits avery garvey)
                        [ Insert 'g' 0
                        , Move 'r' 3 2
                        ]
            , test "preterit -> zeitgeist edit steps" <|
                \() ->
                    Expect.equal (edits preterit zeitgeist)
                        [ Substitute 'z' 0
                        , Delete 'r' 1
                        , Insert 'i' 2
                        , Insert 'g' 4
                        , Delete 'r' 5
                        , Insert 's' 7
                        ]
            , test "zeitgeist -> preterit edit steps" <|
                \() ->
                    Expect.equal (edits zeitgeist preterit)
                        [ Substitute 'p' 0
                        , Insert 'r' 1
                        , Delete 'i' 2
                        , Delete 'g' 4
                        , Insert 'r' 5
                        , Delete 's' 7
                        ]
            , test "kitten -> empty string edit steps" <|
                \() ->
                    Expect.equal (edits kitten [])
                        [ Delete 'k' 0
                        , Delete 'i' 1
                        , Delete 't' 2
                        , Delete 't' 3
                        , Delete 'e' 4
                        , Delete 'n' 5
                        ]
            , test "empty string -> kitten edit steps" <|
                \() ->
                    Expect.equal (edits [] kitten)
                        [ Insert 'k' 0
                        , Insert 'i' 1
                        , Insert 't' 2
                        , Insert 't' 3
                        , Insert 'e' 4
                        , Insert 'n' 5
                        ]
            ]
        , describe "when getting edits with a cost function"
            [ test "abc -> adc (without cost function) edit steps" <|
                \() ->
                    Expect.equal (edits abc adc)
                        [ Substitute 'd' 1
                        ]
            , test "abc -> adc (without cost function) edit steps" <|
                \() ->
                    let
                        costFunc editStep =
                            case editStep of
                                Substitute _ _ ->
                                    3

                                _ ->
                                    1
                    in
                        Expect.equal (editsWithCostFunc costFunc abc adc)
                            [ Insert 'd' 1
                            , Delete 'b' 1
                            ]
            ]
        , describe "when getting edits between strings"
            [ test "preterit -> zeitgeist edit steps" <|
                \() ->
                    Expect.equal (editsFromStrings "preterit" "zeitgeist")
                        [ Substitute 'z' 0
                        , Delete 'r' 1
                        , Insert 'i' 2
                        , Insert 'g' 4
                        , Delete 'r' 5
                        , Insert 's' 7
                        ]
            , test "zeitgeist -> preterit edit steps" <|
                \() ->
                    Expect.equal (editsFromStrings "zeitgeist" "preterit")
                        [ Substitute 'p' 0
                        , Insert 'r' 1
                        , Delete 'i' 2
                        , Delete 'g' 4
                        , Insert 'r' 5
                        , Delete 's' 7
                        ]
            , test "kitten -> empty string edit steps" <|
                \() ->
                    Expect.equal (editsFromStrings "kitten" "")
                        [ Delete 'k' 0
                        , Delete 'i' 1
                        , Delete 't' 2
                        , Delete 't' 3
                        , Delete 'e' 4
                        , Delete 'n' 5
                        ]
            , test "empty string -> kitten edit steps" <|
                \() ->
                    Expect.equal (editsFromStrings "" "kitten")
                        [ Insert 'k' 0
                        , Insert 'i' 1
                        , Insert 't' 2
                        , Insert 't' 3
                        , Insert 'e' 4
                        , Insert 'n' 5
                        ]
            ]
        , describe "when getting Levenshtein distance between lists"
            [ test "kitten <-> sitting Levenshtein distance" <|
                \() -> Expect.equal (levenshtein kitten sitting) 3
            , test "sitting <-> kitten Levenshtein distance" <|
                \() -> Expect.equal (levenshtein sitting kitten) 3
            , test "preterit <-> zeitgeist Levenshtein distance" <|
                \() -> Expect.equal (levenshtein preterit zeitgeist) 6
            , test "zeitgeist <-> preterit Levenshtein distance" <|
                \() -> Expect.equal (levenshtein zeitgeist preterit) 6
            , test "garvey <-> avery Levenshtein distance" <|
                \() -> Expect.equal (levenshtein garvey avery) 3
            , test "avery <-> garvey Levenshtein distance" <|
                \() -> Expect.equal (levenshtein avery garvey) 3
            , test "kitten <-> empty string Levenshtein distance" <|
                \() -> Expect.equal (levenshtein kitten []) 6
            , test "empty string <-> kitten Levenshtein distance" <|
                \() -> Expect.equal (levenshtein [] kitten) 6
            ]
        , describe "when getting Levenshtein distance between strings"
            [ test "kitten <-> sitting Levenshtein distance" <|
                \() -> Expect.equal (levenshteinFromStrings "kitten" "sitting") 3
            , test "sitting <-> kitten Levenshtein distance" <|
                \() -> Expect.equal (levenshteinFromStrings "sitting" "kitten") 3
            , test "kitten <-> empty string Levenshtein distance" <|
                \() -> Expect.equal (levenshteinFromStrings "kitten" "") 6
            , test "empty string <-> kitten Levenshtein distance" <|
                \() -> Expect.equal (levenshteinFromStrings "" "kitten") 6
            ]
        ]



-- List helpers.


kitten : List Char
kitten =
    (toList "kitten")


sitting : List Char
sitting =
    (toList "sitting")


garvey : List Char
garvey =
    (toList "garvey")


avery : List Char
avery =
    (toList "avery")


preterit : List Char
preterit =
    (toList "preterit")


zeitgeist : List Char
zeitgeist =
    (toList "zeitgeist")


abc : List Char
abc =
    (toList "abc")


adc : List Char
adc =
    (toList "adc")
