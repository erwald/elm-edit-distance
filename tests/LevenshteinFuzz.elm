module LevenshteinFuzz exposing (all)

import EditDistance exposing (levenshtein)
import Test exposing (Test, describe, fuzz2)
import Fuzz exposing (list, char)
import Expect


all : Test
all =
    describe "The optimized levenshtein implementation"
        [ fuzz2 (list char) (list char) "should match the old implementation" <|
            \source target ->
                Expect.equal (levenshtein source target)
                    (safeLevenshtein source target)
        ]


min3 : comparable -> comparable -> comparable -> comparable
min3 a b c =
    min a b |> min c


safeLevenshtein : List comparable -> List comparable -> Int
safeLevenshtein source target =
    case ( source, target ) of
        ( source, [] ) ->
            List.length source

        ( [], target ) ->
            List.length target

        ( src_hd :: src_tail, tgt_hd :: tgt_tail ) ->
            if src_hd == tgt_hd then
                levenshtein src_tail tgt_tail
            else
                min3
                    (levenshtein src_tail target)
                    (levenshtein source tgt_tail)
                    (levenshtein src_tail tgt_tail)
                    + 1
