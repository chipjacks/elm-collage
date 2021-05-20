module Sierpinski exposing (main, sierpinski)

import Example
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Color
import Html exposing (Html)


sierpinski : Int -> Float -> Collage msg
sierpinski n side =
  case n of
    0 ->
      triangle side
        |> filled (uniform Color.blue)
    _ ->
      let
        smaller = sierpinski (n - 1) side
      in
      vertical
        [ smaller
        , horizontal [ smaller, smaller ] |> center
        ]


collage =
  sierpinski 5 10


main : Platform.Program () (Example.Model () (Collage ())) (Example.Msg ())
main =
    Example.example
        { init = collage
        , update = (\_ _ -> collage)
        , render = (\_ -> collage)
        , view = svg
        }




{- Brent Yorgey's Original Code:

   sierpinski 1 = triangle 1
   sierpinski n =     s
                    ===
                 (s ||| s) # centerX
     where s = sierpinski (n-1)
-}
