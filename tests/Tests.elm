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


coordinates3 : List ( Float, Float )
coordinates3 =
    [ ( 43.44659, -80.5496 )
    , ( 43.44715, -80.55008 )
    , ( 43.44734, -80.55029 )
    , ( 43.44734, -80.55029 )
    , ( 43.44782, -80.55087 )
    , ( 43.4479, -80.55097 )
    , ( 43.4479, -80.55097 )
    , ( 43.44808, -80.55068 )
    , ( 43.44835, -80.55009 )
    , ( 43.44835, -80.55009 )
    , ( 43.44841, -80.54993 )
    , ( 43.44841, -80.54993 )
    , ( 43.44992, -80.55118 )
    , ( 43.44992, -80.55118 )
    , ( 43.45082, -80.55192 )
    , ( 43.45082, -80.55192 )
    , ( 43.45196, -80.55291 )
    , ( 43.45196, -80.55291 )
    , ( 43.45251, -80.55339 )
    , ( 43.45251, -80.55339 )
    , ( 43.45361, -80.55441 )
    , ( 43.45368, -80.55448 )
    , ( 43.45368, -80.55448 )
    , ( 43.45412, -80.55488 )
    , ( 43.45457, -80.55523 )
    , ( 43.45457, -80.55523 )
    , ( 43.45513, -80.5556 )
    , ( 43.45513, -80.5556 )
    , ( 43.45541, -80.55579 )
    , ( 43.45541, -80.55579 )
    , ( 43.45545, -80.55567 )
    , ( 43.45557, -80.55555 )
    , ( 43.45571, -80.55545 )
    , ( 43.45605, -80.55524 )
    , ( 43.45605, -80.55524 )
    , ( 43.45604, -80.5552 )
    , ( 43.45604, -80.55511 )
    , ( 43.45606, -80.555 )
    , ( 43.45651, -80.55371 )
    , ( 43.45659, -80.55358 )
    , ( 43.45669, -80.55347 )
    , ( 43.45681, -80.55339 )
    , ( 43.45711, -80.55328 )
    ]


polyline3 : String
polyline3 =
    "etdhG~isjNoB~Ae@h@??_BrBOR??c@y@u@uB??K_@??mHxF??sDrC??cFdE??mB~A??{EjEML??wAnAyAdA??oBhA??w@d@??GWWW[ScAi@??@G?QCUyAaGOYSUWO{@U"


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
            , test "Encode a list of coordinates to a polyline string containing '?'s (#3)" <|
                \() ->
                    Expect.equal (encode coordinates3) polyline3
            , test "Decode a polyline string to a list of coordinates (#1)" <|
                \() ->
                    Expect.equal (decode polyline1) coordinates1
            , test "Decode a polyline string to a list of coordinates (#2)" <|
                \() ->
                    Expect.equal (decode polyline2)
                        [ ( 43.52552, -80.2886 )
                        , ( 44.28502, -81.29482 )
                        ]
            , test "Decode a polyline string containing '?'s to a list of rounded coordinates (#3)" <|
                \() ->
                    Expect.equal (decode polyline3) coordinates3
            ]
        ]
