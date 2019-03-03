module EditDistance
    exposing
        ( EditStep(..)
        , edits
        , editsWithCostFunc
        , editsFromStrings
        , editsWithCostFuncFromStrings
        , levenshtein
        , levenshteinFromStrings
        )

{-| The EditDistance module allows for calculating the Levenshtein distance
between two lists, or the actual edit steps required to go from one to the
other.

# Edit Steps
@docs EditStep, edits, editsFromStrings, editsWithCostFunc, editsWithCostFuncFromStrings

# Levenshtein
@docs levenshtein, levenshteinFromStrings

-}

import String exposing (toList)


{-| Describes an edit step used to go from one list to another.
-}
type EditStep a
    = Insert a Int
    | Delete a Int
    | Substitute a Int
    | Move a Int Int


{-| Calculate the minimum steps (insertions, deletions, moves and substitutions)
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

The resulting indices reflect edits where _deletions are made first_, before
insertions and substitutions. That is, indices for deletions refer to the source
list, whereas indices for insertions and substitutions refer to the latter,
intermediate lists.
-}
edits : List comparable -> List comparable -> List (EditStep comparable)
edits source target =
    editsWithCostFunc (\_ -> 1) source target


{-| Same as `edits`, but also takes a custom cost function, which takes an
EditStep (of type Insert, Delete or Substitute) and returns a cost (i.e. an
Int).

    edits (String.toList "abc") (String.toList "adc") ==
      [ Substitute 'd' 1
      ]

    -- Make substitutions more costly.
    costFunc editStep =
      case editStep of
        Substitute _ _ -> 3
        _ -> 1

    -- Substitutions are replaced by insertions and deletions.
    editsWithCostFunc costFunc (String.toList "abc") (String.toList "adc") ==
      [ Insert 'd' 1
      , Delete 'b' 1
      ]

(Note that the cost function is applied _before_ insertions and deletions are
converted into moves, meaning it will never receive an EditStep of type Move as
an argument.)
-}
editsWithCostFunc : (EditStep comparable -> Int) -> List comparable -> List comparable -> List (EditStep comparable)
editsWithCostFunc costFunc source target =
    let
        ( result, _ ) =
            doEdits costFunc (List.reverse source) (List.reverse target)
    in
        List.reverse result
            |> reduceMoves


{-| Helper for edits function.
-}
doEdits : (EditStep comparable -> Int) -> List comparable -> List comparable -> ( List (EditStep comparable), Int )
doEdits costFunc source target =
    case ( source, target ) of
        ( [], [] ) ->
            ( [], 0 )

        ( src_hd :: src_tail, [] ) ->
            let
                edit =
                    Delete src_hd (List.length src_tail)

                ( result, cost ) =
                    doEdits costFunc src_tail []
            in
                ( edit :: result, cost + (costFunc edit) )

        ( [], tgt_hd :: tgt_tail ) ->
            let
                edit =
                    Insert tgt_hd (List.length tgt_tail)

                ( result, cost ) =
                    doEdits costFunc [] tgt_tail
            in
                ( edit :: result, cost + (costFunc edit) )

        ( src_hd :: src_tail, tgt_hd :: tgt_tail ) ->
            if src_hd == tgt_hd then
                doEdits costFunc src_tail tgt_tail
            else
                let
                    results =
                        [ doEdits costFunc src_tail (tgt_hd :: tgt_tail)
                        , doEdits costFunc (src_hd :: src_tail) tgt_tail
                        , doEdits costFunc src_tail tgt_tail
                        ]

                    editsList =
                        [ Delete src_hd (List.length src_tail)
                        , Insert tgt_hd (List.length tgt_tail)
                        , Substitute tgt_hd (List.length tgt_tail)
                        ]

                    combineResultsWithEdits ( res, cost ) edit =
                        ( edit :: res, cost + (costFunc edit) )

                    getCost ( _, cost ) =
                        cost
                in
                    List.map2 combineResultsWithEdits results editsList
                        |> List.sortBy getCost
                        |> List.head
                        |> Maybe.withDefault ( [], 0 )



{- Reduces a list of edit steps to combine insertions and deletions of the same
   value into a single Move action with that value. (These are equivalent anyway,
   as a deletion and insertion elsewhere of a certain value is nothing more than a
   movement of that value.)
-}


reduceMoves : List (EditStep comparable) -> List (EditStep comparable)
reduceMoves editSteps =
    let
        findMove step acc =
            case (moveFromSteps editSteps step) of
                Just move ->
                    acc ++ [ move ]

                Nothing ->
                    acc ++ [ step ]

        {- Takes an element and a list and returns the list with the element added
           if it wasn't already in the list.
        -}
        addElementIfUnique elem list =
            if (List.member elem list) then
                list
            else
                list ++ [ elem ]

        {- Takes a list and returns it with any duplicate elements removed. -}
        dropDuplicates list =
            List.foldl addElementIfUnique [] list
    in
        editSteps
            |> List.foldl findMove []
            |> dropDuplicates


{-| Takes an edit step and a list of edit steps and returns either a move step
if there is one to be found for that edit step, or Nothing if not.
-}
moveFromSteps : List (EditStep comparable) -> EditStep comparable -> Maybe (EditStep comparable)
moveFromSteps editSteps step =
    case step of
        Insert value index ->
            let
                -- Find the corresponding deletion, if there is one.
                isCorrespondingDelete s =
                    case s of
                        Delete otherValue _ ->
                            value == otherValue

                        _ ->
                            False

                correspondingDelete =
                    editSteps
                        |> List.filter isCorrespondingDelete
                        |> List.head
            in
                case correspondingDelete of
                    Just (Delete _ deleteIndex) ->
                        Just (Move value deleteIndex index)

                    _ ->
                        Nothing

        Delete value index ->
            let
                -- Find the corresponding insertion, if there is one.
                isCorrespondingInsert s =
                    case s of
                        Insert otherValue _ ->
                            value == otherValue

                        _ ->
                            False

                correspondingInsert =
                    editSteps
                        |> List.filter isCorrespondingInsert
                        |> List.head
            in
                case correspondingInsert of
                    Just (Insert _ insertIndex) ->
                        Just (Move value index insertIndex)

                    _ ->
                        Nothing

        _ ->
            Nothing


{-| Same as the `edits` function, but for String values.

    editsFromStrings "kitten" "sitting" ==
      [ Substitute 's' 0
      , Substitute 'i' 4
      , Insert 'g' 6
      ]

    editsFromStrings "garvey" "avery" ==
      [ Delete 'g' 0
      , Move 'r' 2 3
      ]
-}
editsFromStrings : String -> String -> List (EditStep Char)
editsFromStrings source target =
    edits (String.toList source) (String.toList target)


{-| Same as the `editsWithCostFunc` function, but for String values.

    editsFromStrings "abc" "adc" ==
      [ Substitute 'd' 1
      ]

    -- Make substitutions more costly.
    costFunc editStep =
      case editStep of
        Substitute _ -> 3
        _ -> 1

    -- Substitutions are replaced by insertions and deletions.
    editsWithCostFuncFromStrings costFunc "abc" "adc" ==
      [ Insert 'd' 1
      , Delete 'b' 1
      ]
-}
editsWithCostFuncFromStrings : (EditStep Char -> Int) -> String -> String -> List (EditStep Char)
editsWithCostFuncFromStrings costFunc source target =
    editsWithCostFunc costFunc (String.toList source) (String.toList target)


{-| Calculate the Levenshtein distance between two lists, i.e. how many
insertions, deletions or substitutions are required to turn one given list
into another.

    levenshtein (String.toList "kitten") (String.toList "sitting")
      == 3

    levenshtein (String.toList "preterit") (String.toList "zeitgeist")
      == 6

    levenshtein (String.toList "garvey") (String.toList "avery")
      == 3
-}
levenshtein : List comparable -> List comparable -> Int
levenshtein source target =
    case ( source, target ) of
        ( s, [] ) ->
            List.length source

        ( [], t ) ->
            List.length t

        ( src_hd :: src_tail, tgt_hd :: tgt_tail ) ->
            if src_hd == tgt_hd then
                levenshtein src_tail tgt_tail
            else
                Maybe.withDefault 0
                    (List.minimum
                        [ (levenshtein src_tail target) + 1
                        , (levenshtein source tgt_tail) + 1
                        , (levenshtein src_tail tgt_tail) + 1
                        ]
                    )


{-| Same as the `levenshtein` function, but for String values.

    levenshtein "kitten" "sitting" == 3

    levenshtein "preterit" "zeitgeist" == 6

    levenshtein "garvey" "avery" == 3
-}
levenshteinFromStrings : String -> String -> Int
levenshteinFromStrings source target =
    levenshtein (String.toList source) (String.toList target)
