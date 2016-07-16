module Tests exposing (..)

import String exposing (toList)
import ElmTest exposing (..)

import EditDistance exposing ( levenshtein )

levenshtein_tests : List Test
levenshtein_tests =
    [ test "kitten -> sitting levenshtein distance" (assertEqual (levenshtein (toList "kitten") (toList "sitting")) 3)
    , test "sitting -> kitten levenshtein distance" (assertEqual (levenshtein (toList "sitting") (toList "kitten")) 3)
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
        [ levenshtein_tests
        ]
  in
    suite "Edit Distance tests" tests


main : Program Never
main =
  runSuite all_tests
