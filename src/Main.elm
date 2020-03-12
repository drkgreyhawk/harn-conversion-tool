module Main exposing (..)

import Browser
import Convert exposing (CandsStats, HarnStats, convert)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, on, onClick, onInput, targetValue)
import Json.Decode exposing (map, succeed)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , aspect : Bool
    , level : String
    , dexterity : String
    , constitution : String
    , strength : String
    , wisdom : String
    , intelligence : String
    , bardic_voice : String
    , appearance : String
    , fortitude : String
    , piety : String
    , hearing : String
    , eyesight : String
    , eyesight_description : String
    , base_roll_or_add : String
    , primitive_talent : Bool
    , use_generator : Bool
    , harn_comeliness : String
    , harn_strength : String
    , harn_stamina : String
    , harn_dexterity : String
    , harn_agility : String
    , harn_smell : String
    , harn_hearing : String
    , harn_eyesight : String
    , harn_voice : String
    , harn_aura : String
    , harn_intelligence : String
    , harn_will : String
    , harn_morality : String
    , harn_veteran_points : String
    }


init : Model
init =
    { name = ""
    , aspect = False
    , level = ""
    , dexterity = ""
    , constitution = ""
    , strength = ""
    , wisdom = ""
    , intelligence = ""
    , bardic_voice = ""
    , appearance = ""
    , fortitude = ""
    , piety = ""
    , hearing = ""
    , eyesight = ""
    , eyesight_description = ""
    , base_roll_or_add = ""
    , primitive_talent = False
    , use_generator = False
    , harn_comeliness = ""
    , harn_strength = ""
    , harn_stamina = ""
    , harn_dexterity = ""
    , harn_agility = ""
    , harn_smell = ""
    , harn_hearing = ""
    , harn_eyesight = ""
    , harn_voice = ""
    , harn_aura = ""
    , harn_intelligence = ""
    , harn_will = ""
    , harn_morality = ""
    , harn_veteran_points = ""
    }



-- UPDATE


type Msg
    = NameChanged String
    | AspectToggled
    | LevelChanged String
    | DexterityChanged String
    | ConstitutionChanged String
    | StrengthChanged String
    | WisdomChanged String
    | IntelligenceChanged String
    | BardicVoiceChanged String
    | AppearanceChanged String
    | FortitudeChanged String
    | PietyChanged String
    | HearingChanged String
    | EyesightChanged String
    | BaseRollOrAddSelected String
    | PrimitiveTalentToggled
    | UseGeneratorToggled
    | ConvertStatsClicked


update : Msg -> Model -> Model
update msg state_model =
    case msg of
        NameChanged updatedName ->
            { state_model | name = updatedName }

        AspectToggled ->
            { state_model | aspect = not state_model.aspect }

        LevelChanged updatedLevel ->
            { state_model | level = updatedLevel }

        DexterityChanged updatedDex ->
            { state_model | dexterity = updatedDex }

        ConstitutionChanged updatedCon ->
            { state_model | constitution = updatedCon }

        StrengthChanged updatedStr ->
            { state_model | strength = updatedStr }

        WisdomChanged updatedWis ->
            { state_model | wisdom = updatedWis }

        IntelligenceChanged updatedInt ->
            { state_model | intelligence = updatedInt }

        BardicVoiceChanged updated_BV ->
            { state_model | bardic_voice = updated_BV }

        AppearanceChanged updatedApp ->
            { state_model | appearance = updatedApp }

        FortitudeChanged updatedFor ->
            { state_model | fortitude = updatedFor }

        PietyChanged updatedPie ->
            { state_model | piety = updatedPie }

        HearingChanged updatedHearing ->
            { state_model | hearing = updatedHearing }

        EyesightChanged updatedEyesight ->
            { state_model | eyesight = updatedEyesight }

        BaseRollOrAddSelected updated_broa ->
            case updated_broa of
                "Roll" ->
                    { state_model | base_roll_or_add = "Roll" }

                "Add" ->
                    { state_model | base_roll_or_add = "Add" }

                _ ->
                    { state_model | base_roll_or_add = "" }

        PrimitiveTalentToggled ->
            { state_model | primitive_talent = not state_model.primitive_talent }

        UseGeneratorToggled ->
            { state_model | use_generator = not state_model.use_generator }

        ConvertStatsClicked ->
            let
                converted_stats =
                    assignCandsStats state_model
                        |> convert state_model.aspect
                        |> applyHearing state_model.hearing
                        |> applyEyesight state_model.eyesight
                        |> checkPrimitiveTalent state_model.primitive_talent
                        |> checkAndGenStats state_model.use_generator
                        |> calcBROA state_model.base_roll_or_add

                veteran_points =
                    assignVeteranPoints state_model.level
            in
            { state_model
                | harn_comeliness = convertFloatToStringAndCeil converted_stats.comeliness
                , harn_strength = convertFloatToStringAndCeil converted_stats.strength
                , harn_stamina = convertFloatToStringAndCeil converted_stats.stamina
                , harn_dexterity = convertFloatToStringAndCeil converted_stats.dexterity
                , harn_agility = convertFloatToStringAndCeil converted_stats.agility
                , harn_smell = convertFloatToStringAndCeil converted_stats.smell
                , harn_hearing = convertFloatToStringAndCeil converted_stats.hearing
                , harn_eyesight = convertFloatToStringAndCeil converted_stats.eyesight
                , eyesight_description = checkAndApplyEyesightDescription state_model.eyesight
                , harn_voice = convertFloatToStringAndCeil converted_stats.voice
                , harn_aura = convertFloatToStringAndCeil converted_stats.aura
                , harn_intelligence = convertFloatToStringAndCeil converted_stats.intelligence
                , harn_will = convertFloatToStringAndCeil converted_stats.will
                , harn_morality = convertFloatToStringAndCeil converted_stats.morality
                , harn_veteran_points = veteran_points
            }


addToEachStat : HarnStats -> HarnStats
addToEachStat stats =
    let
        x = 3.5
        newStats =
            { stats
                | comeliness = stats.comeliness + x
                , strength = stats.strength + x
                , stamina = stats.stamina + x
                , dexterity = stats.dexterity + x
                , voice = stats.voice + x
                , aura = stats.aura + x
                , intelligence = stats.intelligence + x
                , will = stats.will + x
            }
    in
    newStats
    

assignCandsStats : Model -> CandsStats
assignCandsStats stats_model =
    let
        str =
            convertStringToInt stats_model.strength

        con =
            convertStringToInt stats_model.constitution

        dex =
            convertStringToInt stats_model.dexterity

        wis =
            convertStringToInt stats_model.wisdom

        int =
            convertStringToInt stats_model.intelligence

        bvo =
            convertStringToInt stats_model.bardic_voice

        app =
            convertStringToInt stats_model.appearance

        fort =
            convertStringToInt stats_model.fortitude

        pie =
            convertStringToInt stats_model.piety

        stats =
            CandsStats str con dex wis int bvo app fort pie
    in
    stats


applyEyesight : String -> HarnStats -> HarnStats
applyEyesight eyesight stats =
    case eyesight of
        "Farsighted" ->
            { stats | eyesight = stats.eyesight + 1.0 }

        "Nearsighted" ->
            { stats | eyesight = stats.eyesight - 1.0 }

        "Very Nearsighted" ->
            { stats | eyesight = stats.eyesight - 3.0 }

        "Myopic" ->
            { stats | eyesight = stats.eyesight - 5.0 }

        "Nightvision" ->
            { stats | eyesight = stats.eyesight + 8.0 }

        _ ->
            stats


applyHearing : String -> HarnStats -> HarnStats
applyHearing hearing stats =
    case hearing of
        "Very Poor" ->
            { stats | hearing = stats.hearing - 2.0 }

        "Poor" ->
            { stats | hearing = stats.hearing - 1.0 }

        "Acute" ->
            { stats | hearing = stats.hearing + 1.0 }

        "Perfect" ->
            { stats | hearing = stats.hearing + 2.0 }

        _ ->
            stats


assignVeteranPoints : String -> String
assignVeteranPoints level =
    let
        x =
            calcVeteranPoints (String.toInt level)
    in
    x


calcBROA : String -> HarnStats -> HarnStats
calcBROA broa stats =
    case broa of
        "Add" ->
            addToEachStat stats
        
        "Roll" ->
            rollThenAddToEachStat stats

        _ ->
            stats


calcVeteranPoints : Maybe Int -> String
calcVeteranPoints level =
    if Maybe.withDefault 0 level >= 9 then
        String.fromInt (Maybe.withDefault 0 level + (Maybe.withDefault 0 level - 8))

    else
        String.fromInt (Maybe.withDefault 0 level)


checkAndApplyEyesightDescription : String -> String
checkAndApplyEyesightDescription selected =
    case selected of
        "Color Blind, Blue Yellow" ->
            "Keep color blindness to blue and yellow and add two to any other physical attribute."

        "Color Blind, Red Green" ->
            "Keep color blindness to red and green and add two to any other physical attribute."

        "Color Blind" ->
            "Keep total color blindness and add two to any other physical attribute."

        _ ->
            ""


checkAndGenStats : Bool -> HarnStats -> HarnStats
checkAndGenStats gt stats =
    if gt == True then
        let
            new_stats =
                stats
        in
        new_stats

    else
        stats


checkPrimitiveTalent : Bool -> HarnStats -> HarnStats
checkPrimitiveTalent pt stats =
    if pt == True then
        let
            new_stats =
                { stats | aura = stats.aura + 3.0 }
        in
        new_stats

    else
        stats


convertFloatToStringAndCeil : Float -> String
convertFloatToStringAndCeil float =
    String.fromInt (ceiling float)


convertStringToInt : String -> Int
convertStringToInt string =
    let
        result =
            String.toInt string
    in
    Maybe.withDefault 0 result


rollThenAddToEachStat : HarnStats -> HarnStats
rollThenAddToEachStat stats =
    stats


-- VIEW


view : Model -> Html Msg
view state_model =
    div []
        [ Html.form formStyle
            [ div inputStyle
                [ label ([ for "name" ] ++ labelStyle) [ text "Name" ]
                , viewInput "text" "name" "Bob" state_model.name NameChanged
                ]
            , div []
                [ label [ for "aspect" ] [ text "Well Auspicious?" ]
                , input [ type_ "checkbox", checked state_model.aspect, onClick AspectToggled ] []
                ]
            , div []
                [ label ([ for "level" ] ++ labelStyle) [ text "Level" ]
                , viewInput "number" "level" "0" state_model.level LevelChanged
                ]
            , div []
                [ label ([ for "strength" ] ++ labelStyle) [ text "STR" ]
                , viewInput "number" "strength" "0" state_model.strength StrengthChanged
                ]
            , div []
                [ label ([ for "constitution" ] ++ labelStyle) [ text "CON" ]
                , viewInput "number" "constitution" "0" state_model.constitution ConstitutionChanged
                ]
            , div []
                [ label ([ for "dexterity" ] ++ labelStyle) [ text "DEX" ]
                , viewInput "number" "dexterity" "0" state_model.dexterity DexterityChanged
                ]
            , div []
                [ label ([ for "wisdom" ] ++ labelStyle) [ text "WIS" ]
                , viewInput "number" "wisdom" "0" state_model.wisdom WisdomChanged
                ]
            , div []
                [ label ([ for "intelligence" ] ++ labelStyle) [ text "INT" ]
                , viewInput "number" "intelligence" "0" state_model.intelligence IntelligenceChanged
                ]
            , div []
                [ label ([ for "bardic_voice" ] ++ labelStyle) [ text "BV" ]
                , viewInput "number" "bardic_voice" "0" state_model.bardic_voice BardicVoiceChanged
                ]
            , div []
                [ label ([ for "appearance" ] ++ labelStyle) [ text "APP" ]
                , viewInput "number" "appearance" "0" state_model.appearance AppearanceChanged
                ]
            , div []
                [ label ([ for "fortitude" ] ++ labelStyle) [ text "FOR" ]
                , viewInput "number" "fortitude" "0" state_model.fortitude FortitudeChanged
                ]
            , div []
                [ label ([ for "piety" ] ++ labelStyle) [ text "PIETY" ]
                , viewInput "number" "piety" "0" state_model.piety PietyChanged
                ]
            , div []
                [ label ([ for "hearing" ] ++ labelStyle) [ text "Hearing" ]
                , select [ name "hearing", on "change" (Json.Decode.map HearingChanged targetValue) ]
                    [ option [ value "Very Poor" ] [ text "Very Poor" ]
                    , option [ value "Poor" ] [ text "Poor" ]
                    , option [ value "Normal", selected True ] [ text "Normal" ]
                    , option [ value "Acute" ] [ text "Acute" ]
                    , option [ value "Perfect" ] [ text "Perfect" ]
                    ]
                ]
            , div []
                [ label ([ for "eyesight" ] ++ labelStyle) [ text "Eyesight" ]
                , select [ name "eyesight", on "change" (Json.Decode.map EyesightChanged targetValue) ]
                    [ option [ value "Farsighted" ] [ text "Farsighted" ]
                    , option [ value "Perfect", selected True ] [ text "Perfect" ]
                    , option [ value "Nearsighted" ] [ text "Nearsighted" ]
                    , option [ value "Very Nearsighted" ] [ text "Very Nearsighted" ]
                    , option [ value "Myopic" ] [ text "Myopic" ]
                    , option [ value "Color Blind, Blue Yellow" ] [ text "Color Blind to Blue and Yellow" ]
                    , option [ value "Color Blind, Red Green" ] [ text "Color Blind to Red and Green" ]
                    , option [ value "Color Blind" ] [ text "Totally Color Blind" ]
                    , option [ value "Nightvision" ] [ text "Nightvision" ]
                    ]
                ]
            , div []
                [ h4 []
                    [ text "Options" ]
                , input [ name "finalStatsDecision", type_ "radio", value "None", on "change" (Json.Decode.map BaseRollOrAddSelected targetValue) ]
                    []
                , text "Base Stats"
                , input [ name "finalStatsDecision", type_ "radio", value "Roll", on "change" (Json.Decode.map BaseRollOrAddSelected targetValue) ]
                    []
                , text "Add 1d6"
                , input [ name "finalStatsDecision", type_ "radio", value "Add", on "change" (Json.Decode.map BaseRollOrAddSelected targetValue) ]
                    []
                , text "Add 3.5"
                ]
            , div []
                [ label [ for "primitiveTalent" ] [ text "Primitive Talent?" ]
                , input [ type_ "checkbox", name "primitiveTalent", checked state_model.primitive_talent, onClick PrimitiveTalentToggled ] []
                ]
            , div []
                [ label [ for "useGenerator" ] [ text "Generate Non-Converted Stats?" ]
                , input [ type_ "checkbox", name "useGenerator", checked state_model.use_generator, onClick UseGeneratorToggled ] []
                ]
            , button [ onClickNoDefault ConvertStatsClicked ] [ text "Convert" ]
            ]
        , div resultStyle
            [ div resultLabelStyle [ text state_model.name ]
            , div resultLabelStyle [ text "Comeliness:" ]
            , div resultNumberStyle [ text state_model.harn_comeliness ]
            , div resultLabelStyle [ text "Strength:" ]
            , div resultNumberStyle [ text state_model.harn_strength ]
            , div resultLabelStyle [ text "Stamina:" ]
            , div resultNumberStyle [ text state_model.harn_stamina ]
            , div resultLabelStyle [ text "Dexterity:" ]
            , div resultNumberStyle [ text state_model.harn_dexterity ]
            , div resultLabelStyle [ text "Agility:" ]
            , div resultNumberStyle [ text state_model.harn_agility ]
            , div resultLabelStyle [ text "Smell:" ]
            , div resultNumberStyle [ text state_model.harn_smell ]
            , div resultLabelStyle [ text "Hearing:" ]
            , div resultNumberStyle [ text state_model.harn_hearing ]
            , div resultLabelStyle [ text "Eyesight:" ]
            , div resultNumberStyle [ text state_model.harn_eyesight ]
            , div resultNumberStyle [ text state_model.eyesight_description ]
            , div resultLabelStyle [ text "Voice:" ]
            , div resultNumberStyle [ text state_model.harn_voice ]
            , div resultLabelStyle [ text "Aura:" ]
            , div resultNumberStyle [ text state_model.harn_aura ]
            , div resultLabelStyle [ text "Intelligence:" ]
            , div resultNumberStyle [ text state_model.harn_intelligence ]
            , div resultLabelStyle [ text "Will:" ]
            , div resultNumberStyle [ text state_model.harn_will ]
            , div resultLabelStyle [ text "Morality:" ]
            , div resultNumberStyle [ text state_model.harn_morality ]
            , div resultLabelStyle [ text "Veteran Points:" ]
            , div resultNumberStyle [ text state_model.harn_veteran_points ]
            ]
        ]


viewInput : String -> String -> String -> String -> (String -> msg) -> Html msg
viewInput t n p v toMsg =
    input ([ type_ t, name n, placeholder p, value v, onInput toMsg ] ++ textBoxStyle) []


onClickNoDefault : msg -> Attribute msg
onClickNoDefault message =
    let
        config =
            { message = message
            , stopPropagation = True
            , preventDefault = True
            }
    in
    custom "click" (Json.Decode.succeed config)



-- CSS


formStyle : List (Attribute msg)
formStyle =
    [ style "border-radius" "5px"
    , style "background-color" "#f2f2f2"
    , style "padding" "20px"
    , style "width" "300px"
    , style "height" "100%"
    , style "font-family" "Arial"
    , style "position" "fixed"
    ]


inputStyle : List (Attribute msg)
inputStyle =
    [ style "padding" "2px 0"
    ]


labelStyle : List (Attribute msg)
labelStyle =
    [ style "display" "block"
    ]


resultStyle : List (Attribute msg)
resultStyle =
    [ style "padding" "10px 10px 10px 350px"
    , style "font-family" "Arial"
    ]


resultLabelStyle : List (Attribute msg)
resultLabelStyle =
    [ style "font-weight" "bold"
    ]


resultNumberStyle : List (Attribute msg)
resultNumberStyle =
    [ style "font-family" "\"Courier New\""
    ]


textBoxStyle : List (Attribute msg)
textBoxStyle =
    [ style "margin" "0 2px"
    ]
