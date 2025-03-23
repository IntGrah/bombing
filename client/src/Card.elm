module Card exposing (Card(..), Joker(..), Rank(..), Suit(..), compare, decode, decodeCompact, decodeRank, encode, faceDown, faceUp, height, spacing, width)

import Json.Decode as D
import Json.Encode as E
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Transform



---- Model ----
{-
   A card is either a Black or Red Joker,
   or a card with Rank and Suit.
-}


type Card
    = R Rank Suit
    | J Joker


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type Suit
    = Diamonds
    | Clubs
    | Hearts
    | Spades


type Joker
    = Black
    | Red


compare : Card -> Card -> Order
compare c1 c2 =
    case ( c1, c2 ) of
        ( R r1 _, R r2 _ ) ->
            Basics.compare (rankToInt r1) (rankToInt r2)

        ( J _, R _ _ ) ->
            GT

        ( R _ _, J _ ) ->
            LT

        ( J Black, J Red ) ->
            LT

        ( J Red, J Black ) ->
            GT

        _ ->
            EQ



---- View ----


faceUp : (Float -> Float) -> Card -> List (Svg.Attribute msg) -> Svg msg
faceUp scale card attributes =
    Svg.g attributes
        [ Svg.rect
            [ SvgA.fill "#f7f7f7"
            , SvgA.width (String.fromFloat <| scale width)
            , SvgA.height (String.fromFloat <| scale height)
            , SvgA.rx (String.fromFloat <| 0.0625 * scale height)
            , SvgA.cursor "pointer"
            , SvgA.filter "url(#cardShadow)"
            ]
            []
            |> Transform.translate { x = scale width / -2, y = scale height / -2 }
        , let
            pips =
                case card of
                    R rank suit ->
                        Svg.g []
                            [ Svg.text_
                                [ SvgA.x (String.fromFloat <| 0.12 * scale width)
                                , SvgA.y (String.fromFloat <| 0.15 * scale height)
                                , SvgA.fontSize (String.fromFloat <| 0.13 * scale height)
                                ]
                                [ Svg.text (rankToString rank) ]
                            , Svg.text_
                                [ SvgA.x (String.fromFloat <| 0.12 * scale width)
                                , SvgA.y (String.fromFloat <| 0.275 * scale height)
                                , SvgA.fontSize (String.fromFloat <| 0.13 * scale height)
                                ]
                                [ Svg.text (suitToString suit) ]
                            ]

                    J _ ->
                        Svg.text_
                            [ SvgA.x (String.fromFloat <| 0.1 * scale height)
                            , SvgA.y (String.fromFloat <| 0.15 * scale height)
                            , SvgA.fontSize (String.fromFloat <| 0.13 * scale height)
                            ]
                            [ Svg.text "❂" ]
          in
          Svg.g
            [ SvgA.fontFamily "sans"
            , SvgA.fontSize (String.fromFloat <| scale 0.02)
            , SvgA.fontStyle "bold"
            , SvgA.fill (colourOf card)
            , SvgA.textAnchor "middle"
            , SvgA.style "user-select: none"
            , SvgA.cursor "pointer"
            ]
            [ pips |> Transform.translate { x = scale width / -2, y = scale height / -2 }
            , pips |> Transform.translate { x = scale width / -2, y = scale height / -2 } |> Transform.rotate 180
            ]
        ]


faceDown : (Float -> Float) -> List (Svg.Attribute msg) -> Svg msg
faceDown scale attributes =
    Svg.g attributes
        [ Svg.rect
            [ SvgA.fill "#3d5c8a"
            , SvgA.width (String.fromFloat <| scale width)
            , SvgA.height (String.fromFloat <| scale height)
            , SvgA.rx (String.fromFloat <| 0.0625 * scale height)
            , SvgA.cursor "pointer"
            , SvgA.filter "url(#cardShadow)"
            ]
            []
        , Svg.image
            [ SvgA.xlinkHref "/optiver-logo.svg"
            , SvgA.width (String.fromFloat <| 0.5 * scale width)
            , SvgA.x (String.fromFloat <| 0.25 * scale width)
            , SvgA.y (String.fromFloat <| 0.3 * scale height)
            ]
            []
        , Svg.image
            [ SvgA.xlinkHref "/optiver-circle.png"
            , SvgA.width (String.fromFloat <| 0.4 * scale width)
            , SvgA.x (String.fromFloat <| 0.3 * scale width)
            , SvgA.y (String.fromFloat <| 0.5 * scale height)
            ]
            []
        ]
        |> Transform.translate { x = scale width / -2, y = scale height / -2 }



{-
   How tall is a card? How wide is it? The normal ratio is 4:3.
   Note that these numbers also depends on the window size.
   Scale them by the window height!
-}


height : Float
height =
    0.1


width : Float
width =
    0.75 * height


spacing : Float
spacing =
    0.18 * height



---- Utils ----


colourOf : Card -> String
colourOf card =
    case card of
        J Red ->
            "#e05050"

        R _ Hearts ->
            "#e05050"

        R _ Diamonds ->
            "#e05050"

        J Black ->
            "#404040"

        R _ Spades ->
            "#404040"

        R _ Clubs ->
            "#404040"


rankToString : Rank -> String
rankToString rank =
    case rank of
        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"

        Ace ->
            "A"


suitToString : Suit -> String
suitToString suit =
    case suit of
        Diamonds ->
            "♦"

        Clubs ->
            "♣"

        Hearts ->
            "♥"

        Spades ->
            "♠"


rankToInt : Rank -> Int
rankToInt rank =
    case rank of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13

        Ace ->
            14



---- JSON encoders ----


encode : Card -> E.Value
encode card =
    case card of
        R r s ->
            E.list identity [ E.string "R", encodeRank r, encodeSuit s ]

        J j ->
            E.list identity [ E.string "J", encodeJoker j ]


encodeRank : Rank -> E.Value
encodeRank rank =
    case rank of
        Two ->
            E.string "Two"

        Three ->
            E.string "Three"

        Four ->
            E.string "Four"

        Five ->
            E.string "Five"

        Six ->
            E.string "Six"

        Seven ->
            E.string "Seven"

        Eight ->
            E.string "Eight"

        Nine ->
            E.string "Nine"

        Ten ->
            E.string "Ten"

        Jack ->
            E.string "Jack"

        Queen ->
            E.string "Queen"

        King ->
            E.string "King"

        Ace ->
            E.string "Ace"


encodeSuit : Suit -> E.Value
encodeSuit suit =
    case suit of
        Diamonds ->
            E.string "Diamonds"

        Clubs ->
            E.string "Clubs"

        Hearts ->
            E.string "Hearts"

        Spades ->
            E.string "Spades"


encodeJoker : Joker -> E.Value
encodeJoker joker =
    case joker of
        Black ->
            E.string "Black"

        Red ->
            E.string "Red"



---- JSON decoders ----


decode : D.Decoder Card
decode =
    D.index 0 D.string
        |> D.andThen
            (\variant ->
                case variant of
                    "R" ->
                        D.map2 R (D.index 1 decodeRank) (D.index 2 decodeSuit)

                    "J" ->
                        D.map J (D.index 1 decodeJoker)

                    -- D.map decodeJoker <| D.index 1 decodeJoker
                    _ ->
                        D.fail "Not a valid card!"
            )


decodeRank : D.Decoder Rank
decodeRank =
    D.index 0 <|
        D.andThen
            (\variant ->
                case variant of
                    "Two" ->
                        D.succeed Two

                    "Three" ->
                        D.succeed Three

                    "Four" ->
                        D.succeed Four

                    "Five" ->
                        D.succeed Five

                    "Six" ->
                        D.succeed Six

                    "Seven" ->
                        D.succeed Seven

                    "Eight" ->
                        D.succeed Eight

                    "Nine" ->
                        D.succeed Nine

                    "Ten" ->
                        D.succeed Ten

                    "Jack" ->
                        D.succeed Jack

                    "Queen" ->
                        D.succeed Queen

                    "King" ->
                        D.succeed King

                    "Ace" ->
                        D.succeed Ace

                    _ ->
                        D.fail "Not a valid Rank!"
            )
            D.string


decodeSuit : D.Decoder Suit
decodeSuit =
    D.index 0 <|
        D.andThen
            (\variant ->
                case variant of
                    "Diamonds" ->
                        D.succeed Diamonds

                    "Clubs" ->
                        D.succeed Clubs

                    "Hearts" ->
                        D.succeed Hearts

                    "Spades" ->
                        D.succeed Spades

                    _ ->
                        D.fail "Not a valid Suit!"
            )
            D.string


decodeJoker : D.Decoder Joker
decodeJoker =
    D.index 0 <|
        D.andThen
            (\variant ->
                case variant of
                    "Black" ->
                        D.succeed Black

                    "Red" ->
                        D.succeed Red

                    _ ->
                        D.fail "Not a valid Joker type!"
            )
            D.string



---- Compact JSON encoders ----
{-
   Since the JSON representation can be verbose,
   here are more compact representations.
-}


encodeCompact : Card -> E.Value
encodeCompact card =
    case card of
        R r s ->
            E.string (String.fromList [ encodeRankCompact r, encodeSuitCompact s ])

        J Black ->
            E.string "xx"

        J Red ->
            E.string "XX"


encodeRankCompact : Rank -> Char
encodeRankCompact rank =
    case rank of
        Two ->
            '2'

        Three ->
            '3'

        Four ->
            '4'

        Five ->
            '5'

        Six ->
            '6'

        Seven ->
            '7'

        Eight ->
            '8'

        Nine ->
            '9'

        Ten ->
            'T'

        Jack ->
            'J'

        Queen ->
            'Q'

        King ->
            'K'

        Ace ->
            'A'


encodeSuitCompact : Suit -> Char
encodeSuitCompact suit =
    case suit of
        Diamonds ->
            'd'

        Clubs ->
            'c'

        Hearts ->
            'h'

        Spades ->
            's'



---- JSON decoders ----


decodeCompact : ( Char, Char ) -> Maybe Card
decodeCompact ( a, b ) =
    case ( a, b ) of
        ( 'x', 'x' ) ->
            Just (J Black)

        ( 'X', 'X' ) ->
            Just (J Red)

        ( r, s ) ->
            Maybe.map2 R (decodeRankCompact r) (decodeSuitCompact s)


decodeRankCompact : Char -> Maybe Rank
decodeRankCompact char =
    case char of
        '2' ->
            Just Two

        '3' ->
            Just Three

        '4' ->
            Just Four

        '5' ->
            Just Five

        '6' ->
            Just Six

        '7' ->
            Just Seven

        '8' ->
            Just Eight

        '9' ->
            Just Nine

        'T' ->
            Just Ten

        'J' ->
            Just Jack

        'Q' ->
            Just Queen

        'K' ->
            Just King

        'A' ->
            Just Ace

        _ ->
            Nothing


decodeSuitCompact : Char -> Maybe Suit
decodeSuitCompact char =
    case char of
        'd' ->
            Just Diamonds

        'c' ->
            Just Clubs

        'h' ->
            Just Hearts

        's' ->
            Just Spades

        _ ->
            Nothing
