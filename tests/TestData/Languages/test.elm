-- Elm Test File for UAST-Grep
-- Tests: modules, types, functions, HTML, JSON


module Test exposing
    ( Model
    , Msg(..)
    , Person
    , Status(..)
    , init
    , main
    , update
    , view
    )

import Browser
import Html exposing (Html, button, div, h1, input, li, p, span, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- CONSTANTS


maxItems : Int
maxItems =
    100


defaultName : String
defaultName =
    "UAST-Grep"



-- TYPES


type Status
    = OK
    | NotFound
    | ServerError


type Role
    = Admin
    | User
    | Guest
    | Moderator


type alias Person =
    { name : String
    , age : Int
    , email : Maybe String
    , active : Bool
    }


type alias Language =
    { id : Int
    , name : String
    , extension : String
    , tier : Int
    , features : List String
    , active : Bool
    }


type alias Config =
    { name : String
    , version : String
    , maxItems : Int
    , enabled : Bool
    , features : Features
    , thresholds : Thresholds
    }


type alias Features =
    { parsing : Bool
    , validation : Bool
    , formatting : Bool
    }


type alias Thresholds =
    { warning : Float
    , error : Float
    , critical : Float
    }



-- MODEL


type alias Model =
    { persons : List Person
    , languages : List Language
    , searchTerm : String
    , loading : Bool
    , error : Maybe String
    , counter : Int
    , config : Config
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { persons = []
      , languages = sampleLanguages
      , searchTerm = ""
      , loading = False
      , error = Nothing
      , counter = 0
      , config = defaultConfig
      }
    , Cmd.none
    )


defaultConfig : Config
defaultConfig =
    { name = defaultName
    , version = "1.0.0"
    , maxItems = maxItems
    , enabled = True
    , features =
        { parsing = True
        , validation = True
        , formatting = False
        }
    , thresholds =
        { warning = 0.75
        , error = 0.9
        , critical = 0.99
        }
    }


sampleLanguages : List Language
sampleLanguages =
    [ { id = 1
      , name = "TypeScript"
      , extension = ".ts"
      , tier = 1
      , features = [ "classes", "interfaces", "generics" ]
      , active = True
      }
    , { id = 2
      , name = "Python"
      , extension = ".py"
      , tier = 1
      , features = [ "classes", "decorators", "async" ]
      , active = True
      }
    , { id = 3
      , name = "Rust"
      , extension = ".rs"
      , tier = 1
      , features = [ "structs", "traits", "lifetimes" ]
      , active = True
      }
    ]



-- MSG


type Msg
    = Increment
    | Decrement
    | Reset
    | UpdateSearch String
    | FetchPersons
    | GotPersons (Result Http.Error (List Person))
    | AddPerson Person
    | RemovePerson Int
    | ToggleFeature String Bool



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        Reset ->
            ( { model | counter = 0 }, Cmd.none )

        UpdateSearch term ->
            ( { model | searchTerm = term }, Cmd.none )

        FetchPersons ->
            ( { model | loading = True }
            , fetchPersons
            )

        GotPersons result ->
            case result of
                Ok persons ->
                    ( { model | persons = persons, loading = False, error = Nothing }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to fetch persons", loading = False }
                    , Cmd.none
                    )

        AddPerson person ->
            ( { model | persons = person :: model.persons }, Cmd.none )

        RemovePerson index ->
            ( { model | persons = removeAt index model.persons }, Cmd.none )

        ToggleFeature feature enabled ->
            let
                features =
                    model.config.features

                newFeatures =
                    case feature of
                        "parsing" ->
                            { features | parsing = enabled }

                        "validation" ->
                            { features | validation = enabled }

                        "formatting" ->
                            { features | formatting = enabled }

                        _ ->
                            features

                config =
                    model.config
            in
            ( { model | config = { config | features = newFeatures } }, Cmd.none )



-- HELPER FUNCTIONS


removeAt : Int -> List a -> List a
removeAt index list =
    List.take index list ++ List.drop (index + 1) list


isAdult : Person -> Bool
isAdult person =
    person.age >= 18


calculateSum : Int -> Int -> Int
calculateSum a b =
    a + b


transform : Int -> Int
transform value =
    if value > 0 then
        value * 2

    else if value < 0 then
        negate value

    else
        0


filterActive : List { a | active : Bool } -> List { a | active : Bool }
filterActive items =
    List.filter .active items


mapItems : (a -> b) -> List a -> List b
mapItems fn items =
    List.map fn items


findPerson : String -> List Person -> Maybe Person
findPerson name persons =
    List.filter (\p -> p.name == name) persons
        |> List.head


getStatusMessage : Status -> String
getStatusMessage status =
    case status of
        OK ->
            "OK"

        NotFound ->
            "Not Found"

        ServerError ->
            "Server Error"



-- HTTP


fetchPersons : Cmd Msg
fetchPersons =
    Http.get
        { url = "https://api.example.com/persons"
        , expect = Http.expectJson GotPersons personsDecoder
        }



-- JSON DECODERS


personDecoder : Decoder Person
personDecoder =
    Decode.map4 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)
        (Decode.field "email" (Decode.nullable Decode.string))
        (Decode.field "active" Decode.bool)


personsDecoder : Decoder (List Person)
personsDecoder =
    Decode.list personDecoder



-- JSON ENCODERS


encodePerson : Person -> Encode.Value
encodePerson person =
    Encode.object
        [ ( "name", Encode.string person.name )
        , ( "age", Encode.int person.age )
        , ( "email"
          , case person.email of
                Just email ->
                    Encode.string email

                Nothing ->
                    Encode.null
          )
        , ( "active", Encode.bool person.active )
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text defaultName ]
        , viewCounter model.counter
        , viewSearch model.searchTerm
        , viewPersonList model
        , viewLanguages model.languages
        , viewError model.error
        ]


viewCounter : Int -> Html Msg
viewCounter count =
    div [ class "counter" ]
        [ button [ onClick Decrement ] [ text "-" ]
        , span [] [ text (String.fromInt count) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Reset, disabled (count == 0) ] [ text "Reset" ]
        ]


viewSearch : String -> Html Msg
viewSearch term =
    div [ class "search" ]
        [ input
            [ type_ "text"
            , placeholder "Search..."
            , value term
            , onInput UpdateSearch
            ]
            []
        ]


viewPersonList : Model -> Html Msg
viewPersonList model =
    div [ class "persons" ]
        [ if model.loading then
            p [] [ text "Loading..." ]

          else
            ul []
                (List.indexedMap viewPerson model.persons)
        , button [ onClick FetchPersons ] [ text "Fetch Persons" ]
        ]


viewPerson : Int -> Person -> Html Msg
viewPerson index person =
    li []
        [ text (person.name ++ " (" ++ String.fromInt person.age ++ ")")
        , if isAdult person then
            span [ class "adult" ] [ text " [Adult]" ]

          else
            text ""
        , button [ onClick (RemovePerson index) ] [ text "Remove" ]
        ]


viewLanguages : List Language -> Html Msg
viewLanguages languages =
    div [ class "languages" ]
        [ h1 [] [ text "Languages" ]
        , ul []
            (List.map viewLanguage languages)
        ]


viewLanguage : Language -> Html Msg
viewLanguage lang =
    li []
        [ text (lang.name ++ " (" ++ lang.extension ++ ")")
        , text (" - Tier " ++ String.fromInt lang.tier)
        ]


viewError : Maybe String -> Html Msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ class "error" ] [ text error ]

        Nothing ->
            text ""



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
