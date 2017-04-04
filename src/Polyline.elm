module Polyline exposing (encode, decode)

{-| A library for handling polylines

# Functions
@docs encode, decode

-}

import Bitwise exposing (shiftLeftBy, shiftRightBy, complement, and, or)
import Char exposing (fromCode, toCode)
import List.Extra exposing (foldl1)
import Round
import String exposing (fromChar, dropLeft)


{-| Encode a list of coordinates into a polyline string

    encode [ ( 43.525524, -80.288602 ), ( 44.285024, -81.29482 ) ] == "oathGvj`iN{isCzocE"
-}
encode : List ( Float, Float ) -> String
encode points =
    points
        |> List.foldl
            (\( lat_, lng_ ) acc ->
                let
                    lat =
                        (lat_ * 1.0e5 |> round |> toFloat) / 1.0e5

                    lng =
                        (lng_ * 1.0e5 |> round |> toFloat) / 1.0e5
                in
                    case acc of
                        [] ->
                            [ ( lat, lng ) ]

                        diffs ->
                            let
                                ( currentLat, currentLng ) =
                                    diffs
                                        |> foldl1
                                            (\( pos_lat, pos_lng ) ( diff_lat, diff_lng ) ->
                                                ( pos_lat + diff_lat, pos_lng + diff_lng )
                                            )
                                        -- Empty list handled above, this case should not happen
                                        |>
                                            Maybe.withDefault ( 0.0, 0.0 )
                            in
                                ( lat - currentLat, lng - currentLng ) :: diffs
            )
            []
        |> List.reverse
        |> List.map
            (\( latDiff, lngDiff ) ->
                String.concat [ encodeDiff (latDiff), encodeDiff (lngDiff) ]
            )
        |> String.concat


encodeDiff : Float -> String
encodeDiff diff =
    let
        shifted =
            shiftLeftBy 1 <| round <| diff * 100000

        toEncode =
            if diff < 0 then
                complement shifted
            else
                shifted
    in
        if (diff == 0) then
            String.concat [ encodeFiveBit toEncode "", "?" ]
        else
            encodeFiveBit toEncode ""


encodeFiveBit : Int -> String -> String
encodeFiveBit value str =
    if value /= 0 then
        let
            fiveBit =
                if value >= 32 then
                    ((value |> and 31) |> or 32) + 63
                else
                    (value |> and 31) + 63
        in
            encodeFiveBit
                (shiftRightBy 5 value)
                (str ++ (fromChar <| fromCode <| fiveBit))
    else
        str


{-| Decode a polyline string into a list of coordinates

    decode "oathGvj`iN{isCzocE" == [ ( 43.52552, -80.28860 ), ( 44.28502, -81.29482 ) ]
-}
decode : String -> List ( Float, Float )
decode polyline =
    decodeDiffs polyline []
        |> List.foldr
            (\( diff_lat, diff_lng ) acc ->
                case acc of
                    [] ->
                        [ ( diff_lat, diff_lng ) ]

                    coords ->
                        let
                            ( lat, lng ) =
                                List.head coords
                                    -- This case should be covered above
                                    |>
                                        Maybe.withDefault ( 0.0, 0.0 )
                        in
                            ( lat + diff_lat, lng + diff_lng ) :: coords
            )
            []
        |> List.reverse
        |> List.map (\( lat, lng ) -> ( roundFloat 5 lat, roundFloat 5 lng ))


{-| This is ugly and needs to be fixed at some point
-}
roundFloat : Int -> Float -> Float
roundFloat digits number =
    number
        |> Round.round digits
        |> String.toFloat
        |> Result.withDefault number


decodeDiffs : String -> List ( Float, Float ) -> List ( Float, Float )
decodeDiffs polyline diffs =
    if String.length polyline > 0 then
        let
            ( latDiff_, pl1 ) =
                decodeDiff polyline 0 0

            ( lngDiff_, pl2 ) =
                decodeDiff pl1 0 0

            latDiff =
                (toFloat latDiff_) / 1.0e5

            lngDiff =
                (toFloat lngDiff_) / 1.0e5
        in
            decodeDiffs pl2 (( latDiff, lngDiff ) :: diffs)
    else
        diffs


decodeDiff : String -> Int -> Int -> ( Int, String )
decodeDiff polyline shift result =
    let
        byte =
            polyline
                |> String.toList
                |> List.head
                |> Maybe.withDefault ' '
                |> (\c -> (toCode c) - 63)

        newResult =
            result |> or ((byte |> and 31) |> shiftLeftBy shift)
    in
        if byte >= 32 then
            decodeDiff (dropLeft 1 polyline) (shift + 5) newResult
        else
            let
                endResult =
                    if (newResult |> and 1) == 1 then
                        complement (newResult |> shiftRightBy 1)
                    else
                        newResult |> shiftRightBy 1
            in
                ( endResult, dropLeft 1 polyline )
