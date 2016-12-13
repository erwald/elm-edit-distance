module Tests exposing (..)

import String exposing (toList)
import ElmTest exposing (..)
import EditDistance
    exposing
        ( EditStep(..)
        , edits
        , editsWithCostFunc
        , editsFromStrings
        , levenshtein
        , levenshteinFromStrings
        )


-- Setup


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



-- Tests for the edits functions.


editsTests : List Test
editsTests =
    [ test "kitten -> sitting edit steps"
        (assertEqual (edits kitten sitting)
            ([ Substitute 's' 0
             , Substitute 'i' 4
             , Insert 'g' 6
             ]
            )
        )
    , test "sitting -> kitten edit steps"
        (assertEqual (edits sitting kitten)
            ([ Substitute 'k' 0
             , Substitute 'e' 4
             , Delete 'g' 6
             ]
            )
        )
    , test "garvey -> avery edit steps"
        (assertEqual (edits garvey avery)
            ([ Delete 'g' 0
             , Move 'r' 2 3
             ]
            )
        )
    , test "avery -> garvey edit steps"
        (assertEqual (edits avery garvey)
            ([ Insert 'g' 0
             , Move 'r' 3 2
             ]
            )
        )
    , test "preterit -> zeitgeist edit steps"
        (assertEqual (edits preterit zeitgeist)
            ([ Substitute 'z' 0
             , Delete 'r' 1
             , Insert 'i' 2
             , Insert 'g' 4
             , Delete 'r' 5
             , Insert 's' 7
             ]
            )
        )
    , test "zeitgeist -> preterit edit steps"
        (assertEqual (edits zeitgeist preterit)
            ([ Substitute 'p' 0
             , Insert 'r' 1
             , Delete 'i' 2
             , Delete 'g' 4
             , Insert 'r' 5
             , Delete 's' 7
             ]
            )
        )
    , test "kitten -> empty string edit steps"
        (assertEqual (edits kitten [])
            ([ Delete 'k' 0
             , Delete 'i' 1
             , Delete 't' 2
             , Delete 't' 3
             , Delete 'e' 4
             , Delete 'n' 5
             ]
            )
        )
    , test "empty string -> kitten edit steps"
        (assertEqual (edits [] kitten)
            ([ Insert 'k' 0
             , Insert 'i' 1
             , Insert 't' 2
             , Insert 't' 3
             , Insert 'e' 4
             , Insert 'n' 5
             ]
            )
        )
    ]


editsWithCostFuncTests : List Test
editsWithCostFuncTests =
    let
        costFunc editStep =
            case editStep of
                Substitute _ _ ->
                    3

                _ ->
                    1
    in
        [ test "abc -> adc (without cost function) edit steps"
            (assertEqual (edits abc adc)
                ([ Substitute 'd' 1
                 ]
                )
            )
        , test "abc ~> adc cost function edit steps"
            (assertEqual (editsWithCostFunc costFunc abc adc)
                ([ Insert 'd' 1
                 , Delete 'b' 1
                 ]
                )
            )
        ]


editsFromStringsTests : List Test
editsFromStringsTests =
    [ test "preterit -> zeitgeist (as String values) edit steps"
        (assertEqual (editsFromStrings "preterit" "zeitgeist")
            ([ Substitute 'z' 0
             , Delete 'r' 1
             , Insert 'i' 2
             , Insert 'g' 4
             , Delete 'r' 5
             , Insert 's' 7
             ]
            )
        )
    , test "zeitgeist -> preterit (as String values) edit steps"
        (assertEqual (editsFromStrings "zeitgeist" "preterit")
            ([ Substitute 'p' 0
             , Insert 'r' 1
             , Delete 'i' 2
             , Delete 'g' 4
             , Insert 'r' 5
             , Delete 's' 7
             ]
            )
        )
    , test "kitten -> empty string (as String values) edit steps"
        (assertEqual (editsFromStrings "kitten" "")
            ([ Delete 'k' 0
             , Delete 'i' 1
             , Delete 't' 2
             , Delete 't' 3
             , Delete 'e' 4
             , Delete 'n' 5
             ]
            )
        )
    , test "empty string -> kitten (as String values) edit steps"
        (assertEqual (editsFromStrings "" "kitten")
            ([ Insert 'k' 0
             , Insert 'i' 1
             , Insert 't' 2
             , Insert 't' 3
             , Insert 'e' 4
             , Insert 'n' 5
             ]
            )
        )
    ]



-- Tests for the levenshtein functions.


levenshteinTests : List Test
levenshteinTests =
    [ test "kitten <-> sitting levenshtein distance" (assertEqual (levenshtein kitten sitting) 3)
    , test "sitting <-> kitten levenshtein distance" (assertEqual (levenshtein sitting kitten) 3)
    , test "preterit <-> zeitgeist levenshtein distance" (assertEqual (levenshtein preterit zeitgeist) 6)
    , test "zeitgeist <-> preterit levenshtein distance" (assertEqual (levenshtein zeitgeist preterit) 6)
    , test "garvey <-> avery levenshtein distance" (assertEqual (levenshtein garvey avery) 3)
    , test "avery <-> garvey levenshtein distance" (assertEqual (levenshtein avery garvey) 3)
    , test "kitten <-> empty string levenshtein distance" (assertEqual (levenshtein kitten []) 6)
    , test "empty string <-> kitten levenshtein distance" (assertEqual (levenshtein [] kitten) 6)
    ]


levenshteinFromStringsTests : List Test
levenshteinFromStringsTests =
    [ test "kitten <-> sitting (as String values) levenshtein distance" (assertEqual (levenshteinFromStrings "kitten" "sitting") 3)
    , test "sitting <-> kitten (as String values) levenshtein distance" (assertEqual (levenshteinFromStrings "sitting" "kitten") 3)
    , test "kitten <-> empty string (as String values) levenshtein distance" (assertEqual (levenshteinFromStrings "kitten" "") 6)
    , test "empty string <-> kitten (as String values) levenshtein distance" (assertEqual (levenshteinFromStrings "" "kitten") 6)
    ]



-- Test execution.


allTests : Test
allTests =
    let
        tests =
            List.concat
                [ editsTests
                , editsWithCostFuncTests
                , editsFromStringsTests
                , levenshteinTests
                , levenshteinFromStringsTests
                ]
    in
        suite "Edit Distance Tests" tests


main : Program Never
main =
    runSuite allTests
