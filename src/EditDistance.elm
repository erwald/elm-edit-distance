module EditDistance exposing ( EditStep (..), edits, levenshtein )

{-|  The EditDistance module allows for calculating the Levenshtein distance
between two lists.

# Edit Distance
@docs EditStep, edits, levenshtein
-}

{-| Describes an edit step used to go from one list to another.
-}
type EditStep a
  = Insert a Int
  | Delete a Int
  | Substitute a Int
  | Move a Int Int

type alias EditSteps a = List (EditStep a)

{-| Calculate the minimal steps (insertions, deletions, moves and substitutions)
required to turn one given list into another.

    edits (String.toList "kitten") (String.toList "sitting") ==
      [ Substitute 's' 0
      , Substitute 'i' 4
      , Insert 'g' 6
      ]
    edits (String.toList "garvey") (String.toList "avery") ==
      [ Delete 'g' 0
      , Move 'r' 2 3
      ]
-}
edits : List a -> List a -> EditSteps a
edits source target =
  let
    (result, _) = doEdits (List.reverse source) (List.reverse target)
  in
    List.reverse result
      |> reduceMoves

{-| Helper for edits function. -}
doEdits : List a -> List a -> (EditSteps a, Int)
doEdits source target =
  case (source, target) of
    ([], []) ->
      ([], 0)

    (src_hd::src_tail, []) ->
      let
        edit = Delete src_hd (List.length src_tail)
        (result, cost) = doEdits src_tail []
      in
        (edit :: result, cost + 1)

    ([], tgt_hd::tgt_tail) ->
      let
        edit = Insert tgt_hd (List.length tgt_tail)
        (result, cost) = doEdits [] tgt_tail
      in
        (edit :: result, cost + 1)

    (src_hd::src_tail, tgt_hd::tgt_tail) ->
      if src_hd == tgt_hd then
        doEdits src_tail tgt_tail
      else
        let
          results =
            [ doEdits src_tail (tgt_hd :: tgt_tail)
            , doEdits (src_hd :: src_tail) tgt_tail
            , doEdits src_tail tgt_tail
            ]
          edits =
            [ Delete src_hd (List.length src_tail)
            , Insert tgt_hd (List.length tgt_tail)
            , Substitute tgt_hd (List.length tgt_tail)
            ]
          combineResultsWithEdits (res, cost) edit =
            (edit :: res, cost + 1)
          getCost (_, cost) =
            cost
        in
          List.map2 combineResultsWithEdits results edits
            |> List.sortBy getCost
            |> List.head
            |> Maybe.withDefault ([], 0)

{- Reduces a list of edit steps to combine insertions and deletions of the same
value into a single Move action with that value. (These are equivalent anyway,
as a deletion and insertion elsewhere of a certain value is nothing more than a
movement of that value.)
-}
reduceMoves : EditSteps a -> EditSteps a
reduceMoves editSteps =
  let
    findMove step acc =
      case (moveFromSteps editSteps step) of
        Just move -> acc ++ [move]
        Nothing -> acc ++ [step]

    {- Takes an element and a list and returns the list with the element added
    if it wasn't already in the list.
    -}
    addElementIfUnique elem list =
      if (List.member elem list) then
        list
      else
        list ++ [elem]

    {- Takes a list and returns it with any duplicate elements removed.
    -}
    dropDuplicates list =
      List.foldl addElementIfUnique [] list
  in
    editSteps
      |> List.foldl findMove []
      |> dropDuplicates

{-| Takes an edit step and a list of edit steps and returns either a move step
if there is one to be found for that edit step, or Nothing if not.
-}
moveFromSteps : EditSteps a -> EditStep a -> Maybe (EditStep a)
moveFromSteps editSteps step =
  case step of
    Insert value index ->
      let
        -- Find the corresponding deletion, if there is one.
        isCorrespondingDelete step =
          case step of
            Delete otherValue _ -> value == otherValue
            _ -> False
        correspondingDelete =
          editSteps
            |> List.filter isCorrespondingDelete
            |> List.head
      in
        case correspondingDelete of
          Just (Delete _ deleteIndex) -> Just (Move value deleteIndex index)
          _ -> Nothing

    Delete value index ->
      let
        -- Find the corresponding insertion, if there is one.
        isCorrespondingInsert step =
          case step of
            Insert otherValue _ -> value == otherValue
            _ -> False
        correspondingInsert =
          editSteps
            |> List.filter isCorrespondingInsert
            |> List.head
      in
        case correspondingInsert of
          Just (Insert _ insertIndex) -> Just (Move value index insertIndex)
          _ -> Nothing

    _ ->
      Nothing

{-| Calculate the Levenshtein distance between two lists, i.e. how many
insertions, deletions or substitutions are required to turn one given list into
another.

    levenshtein (String.toList "kitten") (String.toList "sitting") == 3
    levenshtein (String.toList "preterit") (String.toList "zeitgeist") == 6
    levenshtein (String.toList "garvey") (String.toList "avery") == 3
-}
levenshtein : List a -> List a -> Int
levenshtein source target =
  case (source, target) of
    (source, []) ->
      List.length source

    ([], target) ->
      List.length target

    (src_hd::src_tail, tgt_hd::tgt_tail) ->
      if src_hd == tgt_hd then
        levenshtein src_tail tgt_tail
      else
        Maybe.withDefault 0 (List.minimum
          [ (levenshtein src_tail target) + 1
          , (levenshtein source tgt_tail) + 1
          , (levenshtein src_tail tgt_tail) + 1
          ])
