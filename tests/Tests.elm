module Tests exposing (..)

import String exposing (toList)
import ElmTest exposing (..)

import EditDistance exposing ( EditStep (..), edits, levenshtein )

-- Setup

kitten : List Char
kitten = (toList "kitten")

sitting : List Char
sitting = (toList "sitting")

garvey : List Char
garvey = (toList "garvey")

avery : List Char
avery = (toList "avery")

preterit : List Char
preterit = (toList "preterit")

zeitgeist : List Char
zeitgeist = (toList "zeitgeist")

-- Tests for the edits function.

edits_tests : List Test
edits_tests =
  [ test "kitten -> sitting edit steps" (
      assertEqual (edits kitten sitting) (
        [ Substitute 's' 0
        , Substitute 'i' 4
        , Insert 'g' 6
        ]
      )
    )
  , test "sitting -> kitten edit steps" (
      assertEqual (edits sitting kitten) (
        [ Substitute 'k' 0
        , Substitute 'e' 4
        , Delete 'g' 6
        ]
      )
    )
  , test "garvey -> avery edit steps" (
      assertEqual (edits garvey avery) (
        [ Delete 'g' 0
        , Move 'r' 2 3
        ]
      )
    )
  , test "avery -> garvey edit steps" (
      assertEqual (edits avery garvey) (
        [ Insert 'g' 0
        , Move 'r' 3 2
        ]
      )
    )
  , test "preterit -> zeitgeist edit steps" (
      assertEqual (edits preterit zeitgeist) (
        [ Substitute 'z' 0
        , Delete 'r' 1
        , Insert 'i' 2
        , Insert 'g' 4
        , Delete 'r' 5
        , Insert 's' 7
        ]
      )
    )
  , test "zeitgeist -> preterit edit steps" (
      assertEqual (edits zeitgeist preterit) (
        [ Substitute 'p' 0
        , Insert 'r' 1
        , Delete 'i' 2
        , Delete 'g' 4
        , Insert 'r' 5
        , Delete 's' 7
        ]
      )
    )
  , test "kitten -> empty string edit steps" (
      assertEqual (edits kitten []) (
        [ Delete 'k' 0
        , Delete 'i' 1
        , Delete 't' 2
        , Delete 't' 3
        , Delete 'e' 4
        , Delete 'n' 5
        ]
      )
    )
  , test "empty string -> kitten edit steps" (
      assertEqual (edits [] kitten) (
        [ Insert 'k' 0
        , Insert 'i' 1
        , Insert 't' 2
        , Insert 't' 3
        , Insert 'e' 4
        , Insert 'n' 5
        ]
      )
    )
  ]

-- Tests for the levenshtein function.

levenshtein_tests : List Test
levenshtein_tests =
    [ test "kitten -> sitting levenshtein distance" (assertEqual (levenshtein kitten sitting) 3)
    , test "sitting -> kitten levenshtein distance" (assertEqual (levenshtein sitting kitten) 3)
    , test "preterit -> zeitgeist levenshtein distance" (assertEqual (levenshtein preterit zeitgeist) 6)
    , test "zeitgeist -> preterit levenshtein distance" (assertEqual (levenshtein zeitgeist preterit) 6)
    , test "garvey -> avery levenshtein distance" (assertEqual (levenshtein garvey avery) 3)
    , test "avery -> garvey levenshtein distance" (assertEqual (levenshtein avery garvey) 3)
    ]

-- Test execution.

all_tests : Test
all_tests =
  let
    tests =
      List.concat
        [ edits_tests
        , levenshtein_tests
        ]
  in
    suite "Edit Distance Tests" tests


main : Program Never
main =
  runSuite all_tests
