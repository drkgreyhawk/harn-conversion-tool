module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Convert exposing (CandsStats, HarnStats, convert)
import Element as Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Random as R
import Round exposing (ceiling)
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { attribute_model : AttributeModel
    , harn_model : HarnModel
    , m_seed : Maybe R.Seed
    , vp_width : Int
    , vp_height : Int
    }


type alias AttributeModel =
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
    }


type alias HarnModel =
    { harn_name : String
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
    , eyesight_description : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { attribute_model = defaultAttributeModel
            , harn_model = defaultHarnModel
            , m_seed = Nothing
            , vp_width = 0
            , vp_height = 0
            }
    in
    ( model
    , Task.perform (\vp -> SizeChanged (round vp.scene.width) (round vp.scene.height)) Browser.Dom.getViewport
    )


defaultAttributeModel : AttributeModel
defaultAttributeModel =
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
    }


defaultHarnModel : HarnModel
defaultHarnModel =
    { harn_name = ""
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
    , eyesight_description = ""
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize SizeChanged



-- UPDATE


type Msg
    = NameChanged String
    | AspectToggled Bool
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
    | PrimitiveTalentToggled Bool
    | UseGeneratorToggled Bool
    | ConvertStatsClicked
    | InitialIntegerThenConvertStats Int
    | SizeChanged Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SizeChanged w h ->
            ( { model
                | vp_width = w
                , vp_height = h
              }
            , Cmd.none
            )

        NameChanged updated_name ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | name = updated_name }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        AspectToggled _ ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | aspect = not attribute_model.aspect }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        LevelChanged updated_level ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | level = updated_level }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        DexterityChanged updated_dexterity ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | dexterity = updated_dexterity }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        ConstitutionChanged updated_constitution ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | constitution = updated_constitution }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        StrengthChanged updated_strength ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | strength = updated_strength }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        WisdomChanged updated_wisdom ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | wisdom = updated_wisdom }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        IntelligenceChanged updated_intelligence ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | intelligence = updated_intelligence }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        BardicVoiceChanged updated_bardic_voice ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | bardic_voice = updated_bardic_voice }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        AppearanceChanged updated_appearance ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | appearance = updated_appearance }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        FortitudeChanged updated_fortitude ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | fortitude = updated_fortitude }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        PietyChanged updated_piety ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | piety = updated_piety }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        HearingChanged updated_hearing ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | hearing = updated_hearing }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        EyesightChanged updated_eyesight ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | eyesight = updated_eyesight }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        BaseRollOrAddSelected updated_broa ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    case updated_broa of
                        "Roll" ->
                            { attribute_model | base_roll_or_add = "Roll" }

                        "Add" ->
                            { attribute_model | base_roll_or_add = "Add" }

                        _ ->
                            { attribute_model | base_roll_or_add = "Base" }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        PrimitiveTalentToggled _ ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | primitive_talent = not attribute_model.primitive_talent }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        UseGeneratorToggled _ ->
            let
                attribute_model =
                    model.attribute_model

                new_model =
                    { attribute_model | use_generator = not attribute_model.use_generator }
            in
            ( { model | attribute_model = new_model }, Cmd.none )

        ConvertStatsClicked ->
            case model.m_seed of
                Nothing ->
                    ( model, R.generate InitialIntegerThenConvertStats (R.int 1 23000) )

                Just _ ->
                    ( getModelWithUpdatedHarn model, Cmd.none )

        InitialIntegerThenConvertStats the_int ->
            let
                seed =
                    R.initialSeed the_int

                new_model =
                    { model | m_seed = Just seed }
            in
            ( getModelWithUpdatedHarn new_model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "DrkGreyHawk | C&S to HM3 Conversion Tool"
    , body = [ mainView model ]
    }


mainView : Model -> Html Msg
mainView model =
    let
        attribute_model =
            model.attribute_model

        harn_model =
            model.harn_model
    in
    Element.layout [] <|
        Element.row [ Element.height Element.fill, Element.width Element.fill ]
            [ inputPanel model attribute_model
            , labelPanel
            , outputPanel harn_model
            ]


inputPanel : Model -> AttributeModel -> Element.Element Msg
inputPanel model attribute_model =
    Element.column
        [ Element.height (Element.fill |> Element.maximum model.vp_height)
        , Element.width <| Element.fillPortion 1
        , Background.color <| Element.rgb255 21 21 21
        , Font.color <| Element.rgb255 200 200 200
        , Font.size 13
        , Element.padding 15
        , Element.spacing 10
        , Element.scrollbarY
        ]
        [ textInput "Name" NameChanged "" attribute_model.name
        , Element.row [ Element.spacing 5 ]
            [ textInput "Level" LevelChanged "" attribute_model.level
            , textInput "Dexterity" DexterityChanged "" attribute_model.dexterity
            ]
        , Element.row [ Element.spacing 5 ]
            [ textInput "Constitution" ConstitutionChanged "" attribute_model.constitution
            , textInput "Strength" StrengthChanged "" attribute_model.strength
            ]
        , Element.row [ Element.spacing 5 ]
            [ textInput "Wisdom" WisdomChanged "" attribute_model.wisdom
            , textInput "Intelligence" IntelligenceChanged "" attribute_model.intelligence
            ]
        , Element.row [ Element.spacing 5 ]
            [ textInput "Bardic Voice" BardicVoiceChanged "" attribute_model.bardic_voice
            , textInput "Appearance" AppearanceChanged "" attribute_model.appearance
            ]
        , Element.row [ Element.spacing 5 ]
            [ textInput "Fortitude" FortitudeChanged "" attribute_model.fortitude
            , textInput "Piety" PietyChanged "" attribute_model.piety
            ]
        , toggleInput "Well Auspicious" AspectToggled attribute_model.aspect
        , toggleInput "Primitive Talent" PrimitiveTalentToggled attribute_model.primitive_talent
        , toggleInput "Generate Stats" UseGeneratorToggled attribute_model.use_generator
        , statBonusRadio BaseRollOrAddSelected attribute_model.base_roll_or_add
        , hearingRadio HearingChanged attribute_model.hearing
        , eyesightRadio EyesightChanged attribute_model.eyesight
        , convertButton
        ]


labelPanel : Element.Element Msg
labelPanel =
    Element.column
        [ Element.height Element.fill
        , Element.width <| Element.fillPortion 1
        , Background.color <| Element.rgb255 56 56 56
        , Font.color <| Element.rgb255 200 200 200
        , Element.padding 5
        ]
        [ Element.text "Name:"
        , Element.text "Comeliness:"
        , Element.text "Strength:"
        , Element.text "Stamina:"
        , Element.text "Dexterity:"
        , Element.text "Agility:"
        , Element.text "Smell:"
        , Element.text "Hearing:"
        , Element.text "Eyesight:"
        , Element.text "Voice:"
        , Element.text "Aura:"
        , Element.text "Intelligence:"
        , Element.text "Will:"
        , Element.text "Morality:"
        , Element.text "Veteran Points:"
        ]


outputPanel : HarnModel -> Element.Element Msg
outputPanel harn_model =
    Element.column
        [ Element.height Element.fill
        , Element.width <| Element.fillPortion 3
        , Background.color <| Element.rgb255 56 56 56
        , Font.color <| Element.rgb255 200 200 200
        , Font.family
            [ Font.typeface "Courier New"
            , Font.serif
            ]
        , Element.padding 5
        ]
        [ Element.text harn_model.harn_name
        , Element.text harn_model.harn_comeliness
        , Element.text harn_model.harn_strength
        , Element.text harn_model.harn_stamina
        , Element.text harn_model.harn_dexterity
        , Element.text harn_model.harn_agility
        , Element.text harn_model.harn_smell
        , Element.text harn_model.harn_hearing
        , Element.text harn_model.harn_eyesight
        , Element.text harn_model.harn_voice
        , Element.text harn_model.harn_aura
        , Element.text harn_model.harn_intelligence
        , Element.text harn_model.harn_will
        , Element.text harn_model.harn_morality
        , Element.text harn_model.harn_veteran_points
        , Element.el [ Font.size 11 ] (Element.text harn_model.eyesight_description)
        ]


convertButton : Element.Element Msg
convertButton =
    Input.button
        [ Background.color <| Element.rgb255 52 52 52
        , Border.rounded 5
        , Element.padding 5
        ]
        { onPress = Just ConvertStatsClicked
        , label = Element.text "Convert Stats"
        }


textInput : String -> (String -> Msg) -> String -> String -> Element.Element Msg
textInput label_text update_func placeholder_text model_var =
    Input.text
        [ Background.color <| Element.rgb255 32 32 32
        , Border.color <| Element.rgb255 21 21 21
        ]
        { label = Input.labelAbove [] (Element.text label_text)
        , onChange = update_func
        , placeholder = Just (Input.placeholder [] (Element.text placeholder_text))
        , text = model_var
        }


toggleInput : String -> (Bool -> Msg) -> Bool -> Element.Element Msg
toggleInput label_text update_func model_var =
    Input.checkbox
        []
        { label = Input.labelRight [] (Element.text label_text)
        , onChange = update_func
        , icon = Input.defaultCheckbox
        , checked = model_var
        }


statBonusRadio : (String -> Msg) -> String -> Element.Element Msg
statBonusRadio update_func model_var =
    Input.radioRow
        [ Element.spacing 5
        , Element.paddingXY 0 5
        ]
        { label = Input.labelAbove [] (Element.text "Converted Stat Options")
        , onChange = update_func
        , selected = Just model_var
        , options =
            [ Input.option "Base" (Element.text "Base")
            , Input.option "Roll" (Element.text "Roll")
            , Input.option "Add" (Element.text "Add 3.5")
            ]
        }


hearingRadio : (String -> Msg) -> String -> Element.Element Msg
hearingRadio update_func model_var =
    Input.radio
        [ Element.spacing 5
        , Element.paddingXY 0 5
        ]
        { label = Input.labelAbove [] (Element.text "Hearing:")
        , onChange = update_func
        , selected = Just model_var
        , options =
            [ Input.option "Very Poor" (Element.text "Very Poor")
            , Input.option "Poor" (Element.text "Poor")
            , Input.option "Normal" (Element.text "Normal")
            , Input.option "Acute" (Element.text "Acute")
            , Input.option "Perfect" (Element.text "Perfect")
            ]
        }


eyesightRadio : (String -> Msg) -> String -> Element.Element Msg
eyesightRadio update_func model_var =
    Input.radio
        [ Element.spacing 5
        , Element.paddingXY 0 5
        ]
        { label = Input.labelAbove [] (Element.text "Eyesight:")
        , onChange = update_func
        , selected = Just model_var
        , options =
            [ Input.option "Farsighted" (Element.text "Farsighted")
            , Input.option "Perfect" (Element.text "Perfect")
            , Input.option "Nearsighted" (Element.text "Nearsighted")
            , Input.option "Very Nearsighted" (Element.text "Very Nearsighted")
            , Input.option "Myopic" (Element.text "Myopic")
            , Input.option "Color Blind, Blue Yellow" (Element.text "Color Blind to Blue and Yellow")
            , Input.option "Color Blind, Red Green" (Element.text "Color Blind to Red and Green")
            , Input.option "Color Blind" (Element.text "Totally Color Blind")
            , Input.option "Nightvision" (Element.text "Nightvision")
            ]
        }



-- HELPERS


getModelWithUpdatedHarn : Model -> Model
getModelWithUpdatedHarn the_model =
    let
        attribute_model =
            the_model.attribute_model

        harn_model =
            the_model.harn_model

        seed =
            the_model.m_seed

        converted_stats =
            assignCandsStats attribute_model
                |> convert attribute_model.aspect
                |> applyHearing attribute_model.hearing
                |> applyEyesight attribute_model.eyesight
                |> checkPrimitiveTalent attribute_model.primitive_talent
                |> checkAndGenStats attribute_model.use_generator seed
                |> calcBROA attribute_model.base_roll_or_add seed

        veteran_points =
            assignVeteranPoints attribute_model.level

        ceilingLength =
            0

        new_harn_model =
            { harn_model
                | harn_name = checkForName attribute_model.name
                , harn_comeliness = Round.ceiling ceilingLength converted_stats.comeliness
                , harn_strength = Round.ceiling ceilingLength converted_stats.strength
                , harn_stamina = Round.ceiling ceilingLength converted_stats.stamina
                , harn_dexterity = Round.ceiling ceilingLength converted_stats.dexterity
                , harn_agility = Round.ceiling ceilingLength converted_stats.agility
                , harn_smell = Round.ceiling ceilingLength converted_stats.smell
                , harn_hearing = Round.ceiling ceilingLength converted_stats.hearing
                , harn_eyesight = Round.ceiling ceilingLength converted_stats.eyesight
                , eyesight_description = checkAndApplyEyesightDescription attribute_model.eyesight
                , harn_voice = Round.ceiling ceilingLength converted_stats.voice
                , harn_aura = Round.ceiling ceilingLength converted_stats.aura
                , harn_intelligence = Round.ceiling ceilingLength converted_stats.intelligence
                , harn_will = Round.ceiling ceilingLength converted_stats.will
                , harn_morality = Round.ceiling ceilingLength converted_stats.morality
                , harn_veteran_points = veteran_points
            }
    in
    { the_model | harn_model = new_harn_model }


addToEachStat : HarnStats -> HarnStats
addToEachStat stats =
    let
        x =
            3.5

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


assignCandsStats : AttributeModel -> CandsStats
assignCandsStats model =
    let
        str =
            convertStringToInt model.strength

        con =
            convertStringToInt model.constitution

        dex =
            convertStringToInt model.dexterity

        wis =
            convertStringToInt model.wisdom

        int =
            convertStringToInt model.intelligence

        bvo =
            convertStringToInt model.bardic_voice

        app =
            convertStringToInt model.appearance

        fort =
            convertStringToInt model.fortitude

        pie =
            convertStringToInt model.piety

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


calcBROA : String -> Maybe R.Seed -> HarnStats -> HarnStats
calcBROA broa seed stats =
    case broa of
        "Add" ->
            addToEachStat stats

        "Roll" ->
            rollThenAddToEachStat stats seed

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


checkAndGenStats : Bool -> Maybe R.Seed -> HarnStats -> HarnStats
checkAndGenStats gt seed stats =
    if gt == True then
        genStats stats seed

    else
        stats


checkForName : String -> String
checkForName name =
    case name of
        "" ->
            "Your Harn Character"

        _ ->
            name


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


convertStringToInt : String -> Int
convertStringToInt string =
    let
        result =
            String.toInt string
    in
    Maybe.withDefault 0 result


genStats : HarnStats -> Maybe R.Seed -> HarnStats
genStats stats seed =
    case seed of
        Nothing ->
            stats

        Just seed0 ->
            let
                ( agility_rolls, seed1 ) =
                    R.step (R.list 4 (R.int 1 6)) seed0

                ( smell_rolls, seed2 ) =
                    R.step (R.list 4 (R.int 1 6)) seed1

                ( hearing_rolls, seed3 ) =
                    R.step (R.list 4 (R.int 1 6)) seed2

                ( eyesight_rolls, seed4 ) =
                    R.step (R.list 4 (R.int 1 6)) seed3

                ( morality_rolls, _ ) =
                    R.step (R.list 4 (R.int 1 6)) seed4

                new_stats =
                    { stats
                        | agility = stats.agility + toFloat (dropLowestThenSum agility_rolls)
                        , smell = stats.smell + toFloat (dropLowestThenSum smell_rolls)
                        , hearing = stats.hearing + toFloat (dropLowestThenSum hearing_rolls)
                        , eyesight = stats.eyesight + toFloat (dropLowestThenSum eyesight_rolls)
                        , morality = stats.morality + toFloat (dropLowestThenSum morality_rolls)
                    }
            in
            new_stats


dropLowestThenSum : List Int -> Int
dropLowestThenSum rolls =
    List.sort rolls
        |> List.drop 1
        |> List.sum


rollThenAddToEachStat : HarnStats -> Maybe R.Seed -> HarnStats
rollThenAddToEachStat stats seed =
    case seed of
        Nothing ->
            stats

        Just seed0 ->
            let
                ( comeliness_roll, seed1 ) =
                    R.step (R.int 1 6) seed0

                ( strength_roll, seed2 ) =
                    R.step (R.int 1 6) seed1

                ( stamina_roll, seed3 ) =
                    R.step (R.int 1 6) seed2

                ( dexterity_roll, seed4 ) =
                    R.step (R.int 1 6) seed3

                ( voice_roll, seed5 ) =
                    R.step (R.int 1 6) seed4

                ( aura_roll, seed6 ) =
                    R.step (R.int 1 6) seed5

                ( intelligence_roll, seed7 ) =
                    R.step (R.int 1 6) seed6

                ( will_roll, _ ) =
                    R.step (R.int 1 6) seed7

                new_stats =
                    { stats
                        | comeliness = toFloat comeliness_roll + stats.comeliness
                        , strength = toFloat strength_roll + stats.strength
                        , stamina = toFloat stamina_roll + stats.stamina
                        , dexterity = toFloat dexterity_roll + stats.dexterity
                        , voice = toFloat voice_roll + stats.voice
                        , aura = toFloat aura_roll + stats.aura
                        , intelligence = toFloat intelligence_roll + stats.intelligence
                        , will = toFloat will_roll + stats.will
                    }
            in
            new_stats
