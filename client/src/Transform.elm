module Transform exposing (rotate, translate)

import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Vector exposing (Vector)


type alias Transformation msg =
    Svg msg -> Svg msg


translate : Vector -> Transformation msg
translate { x, y } node =
    Svg.g [ SvgA.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")") ]
        [ node ]


rotate : Float -> Transformation msg
rotate deg node =
    Svg.g
        [ SvgA.transform ("rotate(" ++ String.fromFloat deg ++ ", 0, 0)") ]
        [ node ]
