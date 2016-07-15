module EditDistance exposing ( levenshtein )

{-| A package for calculating between-list edit distances.
-}

{-| Calculate the Levenshtein distance between two lists, i.e. how many
insertions, deletions or substitutions are required to turn one given list into
another.
-}
levenshtein : List comparable -> List comparable -> Int
levenshtein source target =
  case (source, target) of
    (source, []) -> List.length source
    ([], target) -> List.length target
    (src_hd::src_rest, tgt_hd::tgt_rest) ->
      if src_hd == tgt_hd then
        levenshtein src_rest tgt_rest
      else
        Maybe.withDefault 0 (List.minimum
          [ (levenshtein src_rest target) + 1
          , (levenshtein source tgt_rest) + 1
          , (levenshtein src_rest tgt_rest) + 1
          ])
