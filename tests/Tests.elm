module Tests exposing (..)

import Test exposing (..)
import Expect
import Polyline exposing (encode, decode)


coordinates1 : List ( Float, Float )
coordinates1 =
    [ ( 38.5, -120.2 )
    , ( 40.7, -120.95 )
    , ( 43.252, -126.453 )
    ]


polyline1 : String
polyline1 =
    "_p~iF~ps|U_ulLnnqC_mqNvxq`@"


coordinates2 : List ( Float, Float )
coordinates2 =
    [ ( 43.525524, -80.288602 )
    , ( 44.285024, -81.29482 )
    ]


polyline2 : String
polyline2 =
    "oathGvj`iN{isCzocE"


all : Test
all =
    describe "Simple Test Suite"
        [ describe "Bare Minimum Unit Tests"
            [ test "Encode a list of coordinates to polyline string (#1)" <|
                \() ->
                    Expect.equal (encode coordinates1) polyline1
            , test "Encode a list of coordinates to polyline string (#2)" <|
                \() ->
                    Expect.equal (encode coordinates2) polyline2
            , test "Decode a polyline string to a list of coordinates (#1)" <|
                \() ->
                    Expect.equal (decode polyline1) coordinates1
            , test "Decode a polyline string to a list of coordinates (#2)" <|
                \() ->
                    Expect.equal (decode polyline2)
                        [ ( 43.52552, -80.2886 )
                        , ( 44.28502, -81.29482 )
                        ]
            ]
        ]
