module Collage.Sketchy exposing (Config, defaultConfig, sketchy, nextSeed)

{-| Transform a collage so it looks rough and hand drawn.

@docs Config, defaultConfig, sketchy, nextSeed

-}

import Array
import Collage exposing (Collage, Point)
import Collage.Core as Core
import Collage.Sketchy.Fill as Fill
import Collage.Sketchy.Helpers exposing (rotateList, segments, random)


{-| Configure how rough results should look.

  - `roughness` controls how far points will be shifted from original locations.
  - `bowing` controls depth of curvature between two points on a line. Currently only responds to 0 or 1 values.
  - `seed` controls random number generator. Reuse the same seed to reproduce an identical sketched collage.

-}
type alias Config =
    { roughness : Float
    , bowing : Float
    , seed : Int
    }


{-| Default configuration values.

    { roughness = 2, bowing = 1, seed = 0 }

-}
defaultConfig : Config
defaultConfig =
    { roughness = 2, bowing = 1, seed = 0 }


{-| Helper for incrementing the seed value to generate a new randomized Sketchy collage.

    sketchy (nextSeed config) collage

-}
nextSeed : Config -> Config
nextSeed config =
    { config | seed = config.seed + 1 }


{-| Generate a sketched version of a collage.

    sketchy defaultConfig collage
        |> Collage.Render.svg

-}
sketchy : Config -> Collage msg -> Collage msg
sketchy config collage =
    case collage.basic of
        Core.Path style path ->
            let
                sketchPolyline ps =
                    sketchSegments False config ps
                        |> List.map (\segment -> Collage.curve segment |> Collage.traced style)

                sketchCurve ps =
                    [ sketchPoints config ps
                    , sketchPoints (nextSeed config) ps
                    ]
                        |> List.map (\c -> Collage.curve c |> Collage.traced style)
            in
            case path of
                Core.Polyline ps ->
                    { collage | basic = Core.Group (sketchPolyline ps) }

                Core.Curve ps ->
                    { collage | basic = Core.Group (sketchCurve ps) }

        Core.Shape ( fill, line ) path ->
            let
                sketchPolygon ps =
                    sketchSegments True config ps
                        |> List.map (\segment -> Collage.curve segment |> Collage.traced line)

                hachureThickness =
                    max 1 (line.thickness - 1)

                sketchFill ps =
                    Fill.hachureLines hachureThickness ps
                        |> List.concat
                        |> sketchPoints { config | roughness = 1 }
                        |> Collage.curve
                        |> Collage.traced (Collage.solid hachureThickness fill)
                        |> List.singleton

                sketchEllipse ps =
                    sketchPoints { config | bowing = 0 } (ps ++ rotateList ps)
                        |> Collage.curve
                        |> Collage.traced line
            in
            case path of
                Core.Polygon ps ->
                    { collage | basic = Core.Group <| sketchPolygon ps ++ sketchFill ps }

                Core.Rectangle w h r ->
                    let
                        ps =
                            [ ( -w / 2, -h / 2 )
                            , ( w / 2, -h / 2 )
                            , ( w / 2, h / 2 )
                            , ( -w / 2, h / 2 )
                            ]
                    in
                    { collage | basic = Core.Group <| sketchPolygon ps ++ sketchFill ps }

                Core.Circle r ->
                    let
                        ps =
                            ellipsePoints 8 r r

                        fillPs =
                            ellipsePoints 16 r r
                    in
                    { collage | basic = Core.Group <| [ sketchEllipse ps ] ++ sketchFill fillPs }

                Core.Ellipse rx ry ->
                    let
                        ps =
                            ellipsePoints 8 rx ry

                        fillPs =
                            ellipsePoints 16 rx ry
                    in
                    { collage | basic = Core.Group <| [ sketchEllipse ps ] ++ sketchFill fillPs }

                _ ->
                    collage

        Core.Group collages ->
            List.map (sketchy (nextSeed config)) collages
                |> (\group -> { collage | basic = Core.Group group })

        Core.Subcollage fore back ->
            (\sketchedFore sketchedBack -> { collage | basic = Core.Subcollage sketchedFore sketchedBack })
                (sketchy (nextSeed config) fore)
                (sketchy (nextSeed config) back)

        _ ->
            collage



-- INTERNAL


sketchSegments : Bool -> Config -> List Point -> List (List Point)
sketchSegments closed config ps =
    segments closed ps
        |> List.concatMap (\( a, b ) -> [ sketchPoints config [ a, b ], sketchPoints (nextSeed config) [ a, b ] ])


sketchPoints : Config -> List Point -> List Point
sketchPoints config ps =
    let
        bowedPs =
            if config.bowing == 0 then
                ps

            else
                segments True ps
                    |> List.map
                        (\( ( x1, y1 ), ( x2, y2 ) ) ->
                            [ ( x1, y1 )
                            , ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 )
                            ]
                        )
                    |> List.concat
                    |> List.take ((List.length ps * 2) - 1)

        lineLength =
            segments True ps
                |> List.map
                    (\( ( x1, y1 ), ( x2, y2 ) ) -> (x2 - x1) ^ 2 + (y2 - y1) ^ 2 |> sqrt)
                |> List.take (List.length ps)
                |> List.sum

        roughness =
            (if lineLength > 200 then
                1

             else if lineLength > 500 then
                0.4

             else
                lineLength / 100
            )
                * config.roughness

        randomOffset i =
            ( random (config.seed + i) * roughness, random (config.seed + i + 1) * roughness )
    in
    List.indexedMap
        (\i ( x, y ) ->
            let
                ( shiftX, shiftY ) =
                    randomOffset (i * 2)
            in
            ( x + shiftX, y + shiftY )
        )
        bowedPs


ellipsePoints : Int -> Float -> Float -> List Point
ellipsePoints count rx ry =
    List.range 0 count
        |> List.map
            (\i ->
                toFloat i
                    |> (*) (360 / toFloat count)
                    |> (-) 90
                    |> degrees
                    |> (\angle -> ( rx * cos angle, ry * sin angle ))
            )
