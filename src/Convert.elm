module Convert exposing (convert, CandsStats, HarnStats)


type alias CandsStats =
    { strength : Int
    , constitution : Int
    , dexterity : Int
    , wisdom : Int
    , intelligence : Int
    , bardic_voice : Int
    , appearance : Int
    , fortitude : Int
    , piety : Int
    }


type alias HarnStats =
    { comeliness : Float
    , strength : Float
    , stamina : Float
    , dexterity : Float
    , agility : Float
    , smell : Float
    , hearing : Float
    , eyesight : Float
    , voice : Float
    , aura : Float
    , intelligence : Float
    , will : Float
    , morality : Float
    }


convert : Bool -> CandsStats -> HarnStats
convert aspect candsrec =
    let
        stats =
            [ candsrec.strength
            , candsrec.constitution
            , candsrec.dexterity
            , candsrec.wisdom
            , candsrec.intelligence
            , candsrec.bardic_voice
            , candsrec.appearance
            , candsrec.fortitude
            , candsrec.piety
            ]

        avg =
            toFloat (List.sum stats) / 9

        cprs =
            calcOriginalCPRS stats aspect

        str =
            calcStat cprs candsrec.strength avg

        sta =
            calcStat cprs candsrec.constitution avg

        dex =
            calcStat cprs candsrec.dexterity avg

        aur =
            calcStat cprs candsrec.wisdom avg

        int =
            calcStat cprs candsrec.intelligence avg

        voi =
            calcStat cprs candsrec.bardic_voice avg

        cml =
            calcStat cprs candsrec.appearance avg

        wil =
            calcStat cprs candsrec.fortitude avg

        harn_stats =
            HarnStats cml str sta dex 0 0 0 0 voi aur int wil 0
    in
    harn_stats


calcStat : Float -> Int -> Float -> Float
calcStat cprs stat avg =
    cprs * calcCSR stat avg


calcOriginalCPRS : List Int -> Bool -> Float
calcOriginalCPRS stats aspect =
    List.map calcCost stats
        |> List.sum
        |> applyAspect aspect


applyAspect : Bool -> Int -> Float
applyAspect aspect total =
    if aspect == True then
        (toFloat total - (toFloat total * 0.05) - 50) / 10

    else
        toFloat (total - 50) / 10


calcCost : Int -> Int
calcCost stat =
    if stat >= 17 then
        stat + (stat - 16)

    else
        stat


calcCSR : Int -> Float -> Float
calcCSR stat average =
    toFloat (calcCost stat) / average
