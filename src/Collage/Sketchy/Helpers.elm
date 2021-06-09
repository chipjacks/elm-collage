module Collage.Sketchy.Helpers exposing (rotateList, segments, random)

import Collage exposing (Point)


segments : Bool -> List Point -> List ( Point, Point )
segments closed ps =
    List.map2 Tuple.pair ps (rotateList ps)
        |> (if closed then
                identity

            else
                List.take (List.length ps - 1)
           )


rotateList : List a -> List a
rotateList list =
    case list of
        head :: tail ->
            tail ++ [ head ]

        _ ->
            list


{-| Faster and easier way to shift points randomly.

Generated manually:

    $ ruby -e 'puts 100.times.map{|i| "#{i} -> #{Random.new.rand(-1.0..1.0).round(2)}" }.join("\n").to_s'

-}
random : Int -> Float
random i =
    let
        index =
            modBy 100 i
    in
    case index of
        0 ->
            -0.95

        1 ->
            0.32

        2 ->
            0.5

        3 ->
            0.68

        4 ->
            0.27

        5 ->
            0.67

        6 ->
            0.79

        7 ->
            0.85

        8 ->
            0.84

        9 ->
            -0.45

        10 ->
            -0.73

        11 ->
            -0.58

        12 ->
            1.0

        13 ->
            -0.06

        14 ->
            -0.0

        15 ->
            -0.84

        16 ->
            -0.63

        17 ->
            0.71

        18 ->
            -0.61

        19 ->
            0.2

        20 ->
            -0.56

        21 ->
            -0.26

        22 ->
            -0.81

        23 ->
            -0.07

        24 ->
            -0.53

        25 ->
            0.68

        26 ->
            -0.2

        27 ->
            -0.66

        28 ->
            -0.51

        29 ->
            0.18

        30 ->
            0.63

        31 ->
            -0.38

        32 ->
            -0.5

        33 ->
            0.09

        34 ->
            -0.85

        35 ->
            0.06

        36 ->
            -0.92

        37 ->
            -0.73

        38 ->
            -0.42

        39 ->
            -0.63

        40 ->
            -0.99

        41 ->
            0.5

        42 ->
            0.46

        43 ->
            -0.47

        44 ->
            0.65

        45 ->
            0.41

        46 ->
            0.28

        47 ->
            0.31

        48 ->
            0.72

        49 ->
            0.6

        50 ->
            -0.73

        51 ->
            0.58

        52 ->
            0.86

        53 ->
            -0.41

        54 ->
            0.38

        55 ->
            -1.0

        56 ->
            0.05

        57 ->
            -0.99

        58 ->
            -0.27

        59 ->
            0.62

        60 ->
            -0.77

        61 ->
            -0.89

        62 ->
            -0.47

        63 ->
            0.76

        64 ->
            0.9

        65 ->
            -0.32

        66 ->
            -0.75

        67 ->
            0.2

        68 ->
            0.85

        69 ->
            0.95

        70 ->
            -0.73

        71 ->
            0.66

        72 ->
            0.12

        73 ->
            -0.32

        74 ->
            -0.94

        75 ->
            0.8

        76 ->
            -0.94

        77 ->
            0.81

        78 ->
            -0.29

        79 ->
            -0.48

        80 ->
            -0.73

        81 ->
            0.6

        82 ->
            -0.23

        83 ->
            0.16

        84 ->
            -0.21

        85 ->
            0.8

        86 ->
            -0.24

        87 ->
            -0.35

        88 ->
            0.35

        89 ->
            -0.11

        90 ->
            0.55

        91 ->
            -0.01

        92 ->
            0.82

        93 ->
            0.33

        94 ->
            -0.76

        95 ->
            -0.2

        96 ->
            0.99

        97 ->
            -0.2

        98 ->
            -0.33

        99 ->
            0.42

        _ ->
            0.5
