module Tests exposing (..)

import String exposing (toList)
import ElmTest exposing (..)

import EditDistance exposing ( Change (..), edits, levenshtein )

-- Setup

kitten : List Char
kitten = (toList "kitten")

sitting : List Char
sitting = (toList "sitting")

-- Tests

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
  ]

levenshtein_tests : List Test
levenshtein_tests =
    [ test "kitten -> sitting levenshtein distance" (assertEqual (levenshtein kitten sitting) 3)
    , test "sitting -> kitten levenshtein distance" (assertEqual (levenshtein sitting kitten) 3)
    , test "preterit -> zeitgeist levenshtein distance" (assertEqual (levenshtein (toList "preterit") (toList "zeitgeist")) 6)
    , test "zeitgeist -> preterit levenshtein distance" (assertEqual (levenshtein (toList "zeitgeist") (toList "preterit")) 6)
    , test "garvey -> avery levenshtein distance" (assertEqual (levenshtein (toList "garvey") (toList "avery")) 3)
    , test "avery -> garvey levenshtein distance" (assertEqual (levenshtein (toList "avery") (toList "garvey")) 3)
    ]

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
