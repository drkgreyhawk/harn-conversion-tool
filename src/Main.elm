module Main exposing (..)

import Browser
import Convert exposing (CandsStats, convert, HarnStats)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, onClick, onInput)
import Json.Decode exposing (succeed)



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
            { state_model | base_roll_or_add = updated_broa }

        PrimitiveTalentToggled ->
            { state_model | primitive_talent = not state_model.primitive_talent }

        UseGeneratorToggled ->
            { state_model | use_generator = not state_model.use_generator }

        ConvertStatsClicked ->
            let
                converted_stats =
                    assignCandsStats state_model
                        |> convert state_model.aspect
            in
            { state_model | harn_comeliness = convertFloatToString converted_stats.comeliness
            , harn_strength = convertFloatToString converted_stats.strength }


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



-- TEST FUNCTIONS


convertStringToInt : String -> Int
convertStringToInt string =
    let
        result =
            String.toInt string
    in
    Maybe.withDefault 0 result


convertFloatToString : Float -> String
convertFloatToString float =
    String.fromFloat float



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
                , select [ name "hearing" ]
                    [ option [] [ text "Very Poor" ]
                    , option [] [ text "Poor" ]
                    , option [ selected True ] [ text "Normal" ]
                    , option [] [ text "Acute" ]
                    , option [] [ text "Perfect" ]
                    ]
                ]
            , div []
                [ label ([ for "eyesight" ] ++ labelStyle) [ text "Eyesight" ]
                , select [ name "eyesight" ]
                    [ option [] [ text "Farsighted" ]
                    , option [ selected True ] [ text "Perfect" ]
                    , option [] [ text "Nearsighted" ]
                    , option [] [ text "Very Nearsighted" ]
                    , option [] [ text "Myopic" ]
                    , option [] [ text "Colorblind to Blue and Yellow" ]
                    , option [] [ text "Colorblind to Red and Green" ]
                    , option [] [ text "Totally Colorblind" ]
                    , option [] [ text "Nightvision" ]
                    ]
                ]
            , div []
                [ h4 []
                    [ text "Options" ]
                , input [ checked True, name "finalStatsDecision", type_ "radio", value "none" ]
                    []
                , text "Base Stats"
                , input [ name "finalStatsDecision", type_ "radio", value "roll" ]
                    []
                , text "Add 1d6"
                , input [ name "finalStatsDecision", type_ "radio", value "add" ]
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
            [ div [] [ text state_model.name ]
            , div [] [ text "Comeliness:" ]
            , div [] [ text state_model.harn_comeliness ]
            , div [] [ text "Strength:" ]
            , div [] [ text state_model.harn_strength ]
            , div [] [ text "Stamina:" ]
            , div [] [ text state_model.harn_stamina ]
            , div [] [ text "Dexterity:" ]
            , div [] [ text state_model.harn_dexterity ]
            , div [] [ text "Agility:" ]
            , div [] [ text state_model.harn_agility ]
            , div [] [ text "Smell:" ]
            , div [] [ text state_model.harn_smell ]
            , div [] [ text "Hearing:" ]
            , div [] [ text state_model.harn_hearing ]
            , div [] [ text "Eyesight:" ]
            , div [] [ text state_model.harn_eyesight ]
            , div [] [ text "Voice:" ]
            , div [] [ text state_model.harn_voice ]
            , div [] [ text "Aura:" ]
            , div [] [ text state_model.harn_aura ]
            , div [] [ text "Intelligence:" ]
            , div [] [ text state_model.harn_intelligence ]
            , div [] [ text "Will:" ]
            , div [] [ text state_model.harn_will ]
            , div [] [ text "Morality:" ]
            , div [] [ text state_model.harn_morality ]
            , div [] [ text "Veteran Points:" ]
            , div [] [ text state_model.harn_veteran_points ]
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
    ]


textBoxStyle : List (Attribute msg)
textBoxStyle =
    [ style "margin" "0 2px"
    ]
