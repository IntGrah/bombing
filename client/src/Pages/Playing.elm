module Pages.Playing exposing (Model, Msg(..), init, update, view)

import Card exposing (Card)
import Discord
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events.Extra.Mouse as HtmlE
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Time
import Transform
import Tween exposing (Tween)
import Vector exposing (Vector)



---- Model ----


type alias Model =
    { auth : Discord.Auth
    , width : Int
    , height : Int
    , handSelf : List (CardData Card)
    , handTeammate : List (CardData (Maybe Card))
    , handLeftPlayer : List (CardData ())
    , handRightPlayer : List (CardData ())
    , table : List (CardData Card)
    , selected : List (CardData Card)
    , mouse : Vector
    , selectionPos : Vector
    }


type alias CardData card =
    { pos : Tween, z : Int, card : card }



---- Util ----


scale : Model -> Float -> Float
scale model k =
    k * toFloat model.height


yHandSelf : Float
yHandSelf =
    0.85


yTable : Float
yTable =
    0.5


type Region
    = Table
    | HandSelf
    | HandTeammate
    | HandLeftPlayer
    | HandRightPlayer


region : Model -> Region
region model =
    if model.mouse.y < scale model 0.25 then
        HandTeammate

    else if model.mouse.y < scale model 0.6 then
        if model.mouse.x < toFloat model.width / 2 - scale model 0.3 then
            HandLeftPlayer

        else if model.mouse.x < toFloat model.width / 2 + scale model 0.3 then
            Table

        else
            HandRightPlayer

    else
        HandSelf



---- Init ----


init : Discord.Auth -> Int -> Int -> Model
init auth width height =
    recalculateContainers True
        { auth = auth
        , width = width
        , height = height
        , handSelf =
            List.map initCardData
                [ Card.R Card.Two Card.Spades
                , Card.J Card.Red
                , Card.R Card.Five Card.Clubs
                , Card.R Card.Ace Card.Diamonds
                , Card.R Card.Three Card.Spades
                , Card.R Card.Jack Card.Hearts
                , Card.R Card.Ace Card.Clubs
                , Card.R Card.Three Card.Diamonds
                , Card.R Card.Four Card.Clubs
                , Card.R Card.Two Card.Hearts
                , Card.R Card.Five Card.Spades
                , Card.R Card.Three Card.Clubs
                , Card.R Card.Seven Card.Clubs
                , Card.R Card.Ten Card.Diamonds
                , Card.R Card.Ten Card.Hearts
                , Card.R Card.Five Card.Spades
                , Card.R Card.Eight Card.Hearts
                , Card.R Card.Queen Card.Hearts
                , Card.R Card.Four Card.Hearts
                , Card.R Card.Three Card.Clubs
                , Card.R Card.Queen Card.Hearts
                , Card.R Card.Queen Card.Diamonds
                , Card.J Card.Black
                , Card.R Card.Nine Card.Hearts
                , Card.R Card.Ace Card.Spades
                , Card.R Card.King Card.Spades
                , Card.R Card.Four Card.Hearts
                ]
        , handTeammate = List.repeat 27 (initCardData Nothing)
        , handLeftPlayer = List.repeat 27 (initCardData ())
        , handRightPlayer = List.repeat 27 (initCardData ())
        , table =
            List.map initCardData
                [ Card.R Card.Ace Card.Spades
                , Card.R Card.Four Card.Hearts
                , Card.R Card.Seven Card.Clubs
                , Card.R Card.Ten Card.Diamonds
                , Card.J Card.Black
                , Card.J Card.Red
                ]
        , selected = []
        , mouse = { x = 0, y = 0 }
        , selectionPos = { x = 0, y = 0 }
        }


initCardData : card -> CardData card
initCardData card =
    { z = 0, pos = Tween.init { x = 0, y = 0 }, card = card }



---- Update ----


type Msg
    = Tick Time.Posix
    | WindowResized Int Int
    | MouseDownHand Int
    | MouseDownTable Int
    | Mouse MouseAction ( Float, Float )
    | SortHand


type MouseAction
    = Move
    | Up


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SortHand ->
            ( { model | handSelf = List.sortWith (\cd1 cd2 -> Card.compare cd1.card cd2.card) model.handSelf } |> recalculateContainers False, Cmd.none )

        Tick _ ->
            let
                tickContainer : List (CardData card) -> List (CardData card)
                tickContainer =
                    List.map (\cd -> { cd | pos = Tween.tick cd.pos })
            in
            ( { model
                | handSelf = tickContainer model.handSelf
                , handTeammate = tickContainer model.handTeammate
                , handLeftPlayer = tickContainer model.handLeftPlayer
                , handRightPlayer = tickContainer model.handRightPlayer
                , table = tickContainer model.table

                -- , selected = List.map (\cd -> { cd | pos = Tween.init cd.pos.target }) model.selected
              }
            , Cmd.none
            )

        WindowResized w h ->
            ( { model | width = w, height = h } |> recalculateContainers True, Cmd.none )

        MouseDownHand z ->
            let
                ( selected2, hand2 ) =
                    List.partition (\cd -> cd.z == z) model.handSelf
            in
            ( { model
                | handSelf = hand2
                , selectionPos = List.head selected2 |> Maybe.withDefault { pos = Tween.init { x = 0, y = 0 }, z = 0, card = Card.R Card.Ace Card.Spades } |> (\a -> a.pos.current)
                , selected = List.map (\cd -> { cd | pos = Tween.init cd.pos.current }) selected2
              }
                |> recalculateContainers False
            , Cmd.none
            )

        MouseDownTable z ->
            let
                ( selected2, table2 ) =
                    List.partition (\cd -> cd.z == z) model.table
            in
            ( { model
                | table = table2
                , selectionPos = List.head selected2 |> Maybe.withDefault { pos = Tween.init { x = 0, y = 0 }, z = 0, card = Card.R Card.Ace Card.Spades } |> (\a -> a.pos.current)
                , selected = List.map (\{ pos, card } -> { pos = Tween.init pos.current, z = z, card = card }) selected2
              }
                |> recalculateContainers False
            , Cmd.none
            )

        Mouse action ( x2, y2 ) ->
            let
                mouse2 =
                    { x = x2, y = y2 }

                pos2 =
                    Vector.add model.selectionPos (Vector.sub mouse2 model.mouse)

                model2 =
                    { model | mouse = mouse2, selectionPos = pos2 }
            in
            (case action of
                Move ->
                    model2

                Up ->
                    case region model2 of
                        HandSelf ->
                            let
                                iInsertIntoHand =
                                    iInsert model (List.length model.handSelf)

                                handSelf2 =
                                    List.take iInsertIntoHand model.handSelf ++ model.selected ++ List.drop iInsertIntoHand model.handSelf
                            in
                            { model | handSelf = handSelf2, selected = [] }

                        Table ->
                            let
                                iInsertIntoTable =
                                    iInsert model (List.length model.table)

                                table2 =
                                    List.take iInsertIntoTable model.table ++ model.selected ++ List.drop iInsertIntoTable model.table
                            in
                            { model | table = table2, selected = [] }

                        _ ->
                            model
            )
                |> recalculateContainers False
                |> (\model3 -> ( model3, Cmd.none ))


recalculateContainers : Bool -> Model -> Model
recalculateContainers instantly model =
    let
        withGap : Int -> Int -> List (CardData card) -> (Float -> Vector) -> List (CardData card)
        withGap iInsertion nInsertion container posFromOffset =
            List.indexedMap
                (\i cd ->
                    let
                        iAdjusted =
                            if i < iInsertion then
                                i

                            else
                                i + nInsertion

                        offset =
                            toFloat iAdjusted - toFloat (List.length container - 1) / 2
                    in
                    { pos = Tween.set instantly (posFromOffset offset) cd.pos
                    , z = iAdjusted
                    , card = cd.card
                    }
                )
                container

        normal =
            withGap 0 0

        handTeammate2 =
            normal model.handTeammate (\offset -> { x = toFloat model.width / 2 + offset * scale model Card.spacing, y = scale model 0.15 })

        handLeftPlayer2 =
            normal model.handLeftPlayer (\offset -> { x = toFloat model.width / 2 - scale model 0.35, y = scale model 0.5 - offset * scale model Card.spacing })

        handRightPlayer2 =
            normal model.handRightPlayer (\offset -> { x = toFloat model.width / 2 + scale model 0.35, y = scale model 0.5 + offset * scale model Card.spacing })
    in
    case region model of
        HandSelf ->
            let
                iInsertIntoHand =
                    iInsert model (List.length model.handSelf)

                hand2 =
                    withGap iInsertIntoHand
                        (List.length model.selected)
                        model.handSelf
                        (\offset -> { x = toFloat model.width / 2 + offset * scale model Card.spacing, y = scale model yHandSelf })

                table2 =
                    normal model.table (\offset -> { x = toFloat model.width / 2 + offset * scale model Card.spacing, y = scale model yTable })

                selected2 =
                    normal model.selected (\offset -> { x = model.selectionPos.x + offset * scale model Card.spacing, y = model.selectionPos.y })
                        |> List.map (\cd -> { cd | pos = Tween.init cd.pos.target, z = cd.z + iInsertIntoHand })
            in
            { model
                | handSelf = hand2
                , handTeammate = handTeammate2
                , handLeftPlayer = handLeftPlayer2
                , handRightPlayer = handRightPlayer2
                , table = table2
                , selected = selected2
            }

        Table ->
            let
                iInsertIntoTable =
                    iInsert model (List.length model.table)

                handSelf2 =
                    normal model.handSelf (\offset -> { x = toFloat model.width / 2 + offset * scale model Card.spacing, y = scale model yHandSelf })

                table2 =
                    withGap iInsertIntoTable
                        (List.length model.selected)
                        model.table
                        (\offset -> { x = toFloat model.width / 2 + offset * scale model Card.spacing, y = scale model yTable })

                selected2 =
                    normal model.selected (\offset -> { x = model.selectionPos.x + offset * scale model Card.spacing, y = model.selectionPos.y })
                        |> List.map (\cd -> { cd | pos = Tween.init cd.pos.target, z = cd.z + iInsertIntoTable })
            in
            { model
                | handSelf = handSelf2
                , handTeammate = handTeammate2
                , handLeftPlayer = handLeftPlayer2
                , handRightPlayer = handRightPlayer2
                , table = table2
                , selected = selected2
            }

        _ ->
            let
                handSelf2 =
                    normal model.handSelf (\offset -> { x = toFloat model.width / 2 + offset * scale model Card.spacing, y = scale model yHandSelf })

                table2 =
                    normal model.table (\offset -> { x = toFloat model.width / 2 + offset * scale model Card.spacing, y = scale model yTable })

                selected2 =
                    normal model.selected (\offset -> { x = model.selectionPos.x + offset * scale model Card.spacing, y = model.selectionPos.y })
                        |> List.map (\cd -> { cd | pos = Tween.init cd.pos.target })
            in
            { model
                | handSelf = handSelf2
                , handTeammate = handTeammate2
                , handLeftPlayer = handLeftPlayer2
                , handRightPlayer = handRightPlayer2
                , table = table2
                , selected = selected2
            }


iInsert : Model -> Int -> Int
iInsert model n =
    ((model.selectionPos.x - toFloat model.width / 2)
        / scale model Card.spacing
        + toFloat (n + List.length model.selected)
        / 2
    )
        |> floor
        |> clamp 0 n



---- View ----


view : Model -> Html Msg
view model =
    Svg.svg
        [ HtmlA.width model.width
        , HtmlA.height model.height
        , HtmlE.onMove (.offsetPos >> Mouse Move)
        , HtmlE.onUp (.offsetPos >> Mouse Up)
        , HtmlE.onLeave (.offsetPos >> Mouse Up)
        ]
        [ Svg.defs []
            [ Svg.filter
                [ SvgA.id "cardShadow"
                , SvgA.x "-50%"
                , SvgA.width "150%"
                ]
                [ Svg.node "feDropShadow"
                    [ SvgA.dx (String.fromFloat <| scale model 0.001)
                    , SvgA.dy "0"
                    , SvgA.stdDeviation (String.fromFloat <| scale model 0.004)
                    , SvgA.floodColor "rgba(0,0,0,0.1)"
                    , SvgA.filterUnits "userSpaceOnUse"
                    ]
                    []
                ]
            ]

        -- , Svg.text_ [ SvgA.x "100", SvgA.y "100" ]
        --     [ Svg.text (region model |> Debug.toString) ]
        , Svg.rect
            [ SvgA.fill "#1a528a"
            , SvgA.x (String.fromFloat <| toFloat model.width / 2 - scale model 0.25)
            , SvgA.y (String.fromFloat <| scale model 0.92)
            , SvgA.width (String.fromFloat <| scale model 0.5)
            , SvgA.height (String.fromFloat <| scale model 0.06)
            , SvgA.rx (String.fromFloat <| scale model 0.02)
            , SvgA.cursor "pointer"
            ]
            []
        , Svg.text_ [ SvgA.x "100", SvgA.y "100" ]
            [ Svg.text (region model |> Debug.toString) ]
        , Svg.rect
            [ SvgA.fill "#a1662e"
            , SvgA.x (String.fromFloat <| toFloat model.width / 2 - scale model 0.2)
            , SvgA.y (String.fromFloat <| scale model 0.42)
            , SvgA.width (String.fromFloat <| scale model 0.4)
            , SvgA.height (String.fromFloat <| scale model 0.16)
            , SvgA.rx (String.fromFloat <| scale model 0.01)
            , SvgA.cursor "pointer"
            ]
            []
        , Svg.g [ SvgE.onClick SortHand ]
            (model.handTeammate
                |> List.sortBy .z
                |> List.map
                    (\cd ->
                        Card.faceDown (scale model) []
                            |> Transform.translate cd.pos.current
                    )
            )
        , Svg.g [ SvgE.onClick SortHand ]
            (model.handLeftPlayer
                |> List.sortBy .z
                |> List.map
                    (\cd ->
                        Card.faceDown (scale model) []
                            |> Transform.rotate 270
                            |> Transform.translate cd.pos.current
                    )
            )
        , Svg.g [ SvgE.onClick SortHand ]
            (model.handRightPlayer
                |> List.sortBy .z
                |> List.map
                    (\cd ->
                        Card.faceDown (scale model) []
                            |> Transform.rotate 90
                            |> Transform.translate cd.pos.current
                    )
            )
        , Svg.g []
            (let
                mouseDownHand =
                    List.map (\cd -> ( cd, [ SvgE.onMouseDown (MouseDownHand cd.z) ] )) model.handSelf

                mouseDownTable =
                    List.map (\cd -> ( cd, [ SvgE.onMouseDown (MouseDownTable cd.z) ] )) model.table

                noAttributeSelected =
                    List.map (\cd -> ( cd, [] )) model.selected
             in
             (mouseDownHand ++ mouseDownTable ++ noAttributeSelected)
                |> List.sortBy (Tuple.first >> .z)
                |> List.map
                    (\( cd, attributes ) ->
                        Card.faceUp (scale model) cd.card attributes
                            |> Transform.translate cd.pos.current
                    )
            )
        ]
