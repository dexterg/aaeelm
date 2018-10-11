module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Html.Events exposing (..)
import Http
-- import Utils

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table
import Bootstrap.Badge as Badge
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Button as Button
import Bootstrap.Navbar as Navbar

main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }

type alias Model =
    { page : Page
    , navState : Navbar.State
    -- , modalVisibility : Modal.Visibility
    }

type UidStatus
  = EmptyUid
  | ValidUid
  | ToShortUid
  | ToLongtUid
  | DefinedUid
  | UndefinedUid
  | InvalidUid

type Page
    = Home
    | CreateUser
    | CopyHabilities
    | AddHabilities
    | Statistics
--    | GettingStarted
--    | Modules
    | NotFound


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            -- urlUpdate location { navState = navState, page = Home, modalVisibility= Modal.hidden }
            urlUpdate location { navState = navState, page = Home }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )



type Msg
    = UrlChange Location
    | NavMsg Navbar.State
--    | CloseModal
--    | ShowModal

subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )
{-	
        CloseModal ->
            ( { model | modalVisibility = Modal.hidden } 
            , Cmd.none 
            )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown } 
            , Cmd.none 
            )
-}

urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map CreateUser (UrlParser.s "create-user")
        , UrlParser.map CopyHabilities (UrlParser.s "copy-habilities")
        , UrlParser.map AddHabilities (UrlParser.s "add-habilities")
        , UrlParser.map Statistics (UrlParser.s "statistics")
        -- , UrlParser.map GettingStarted (UrlParser.s "getting-started")
        -- , UrlParser.map Modules (UrlParser.s "modules")
        ]


view : Model -> Html Msg
view model =
    div []
        [ menu model
        , mainContent model
        -- , modal model
        ]


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "LDAP/PKI" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#create-user" ] [ text "Création d'utilisateurs" ]
            , Navbar.itemLink [ href "#copy-habilities" ] [ text "Copie d'habilitations" ]
            , Navbar.itemLink [ href "#add-habilities" ] [ text "Ajout d'habilitations" ]
            , Navbar.itemLink [ href "#statistics" ] [ text "Statistiques" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            CreateUser ->
                pageCreateUser model

            CopyHabilities ->
                pageCopyHabilities model

            AddHabilities ->
                pageAddHabilities model

            Statistics ->
                pageStatistics model

            -- GettingStarted ->
            --    pageGettingStarted model

            -- Modules ->
            --     pageModules model

            NotFound ->
                pageNotFound

{-

mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            GettingStarted ->
                pageGettingStarted model

            NotFound ->
                pageNotFound

pageHome : Model -> List (Html Msg)
pageHome model =
    [ h3 [] [ text "Gestion users" ]
      , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Getting started" ]
                |> Card.block []
                    [ Block.text [] [ text "Getting started is real easy. Just click the start button." ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#getting-started" ] ]
                            [ text "Start" ]
                    ]
                |> Card.view
            ]
        , Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.headerH4 [] [ text "Modules" ]
                |> Card.block []
                    [ Block.text [] [ text "Check out the modules overview" ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#modules" ] ]
                            [ text "Module" ]
                    ]
                |> Card.view
            ]
        ]
    ]

pageGettingStarted : Model -> List (Html Msg)
pageGettingStarted model =
    [ h2 [] [ text "Getting started" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick ShowModal ]
        ]
        [ text "Click me" ]
    ]


pageModules : Model -> List (Html Msg)
pageModules model =
    [ h1 [] [ text "Modules" ]
    , Listgroup.ul
        [ Listgroup.li [] [ text "Alert" ]
-}


-- main =
pageStatistics : Model -> List (Html Msg)
pageStatistics model =
  [ br [] []
    , h1 [] [ text "Statistique" ]
    , br [] []
    , Grid.container []
      [ Table.table
        { options = [ Table.striped, Table.hover ]
          , thead = Table.simpleThead
            [ Table.th [] [ text "Demande" ]
            , Table.th [] [ text "Statut" ]
            , Table.th [] [ text "Operation" ]
            , Table.th [] [ text "Context" ]
            , Table.th [] [ text "uid source" ]
            , Table.th [] [ text "uid cible" ]
            , Table.th [] [ text "Demandeur" ]
            , Table.th [] [ text "Date" ]
            ]
          , tbody = Table.tbody []
            [ Table.tr []
              [ Table.td [] [ Badge.badgeDark [ ] [ text "1" ] ]
              , Table.td [] [ Badge.badgePrimary [ ] [ text "processing" ] ]
              , Table.td [] [ text "create user" ]
              , Table.td [] [ text "aaeqvr" ]
              , Table.td [] [ text "" ]
              , Table.td [] [ text "ifre8200" ]
              , Table.td [] [ text "ipga5470" ]
              , Table.td [] [ text "12/04/2018 16:25:41" ]
              ]
              , Table.tr []
                [ Table.td [] [ Badge.badgeDark [ ] [ text "2" ] ]
                , Table.td [] [ Badge.badgeSuccess [ ] [ text "success" ] ]
                , Table.td [] [ text "create user" ]
                , Table.td [] [ text "aaeqv" ]
                , Table.td [] [ text "" ]
                , Table.td [] [ text "esla2500" ]
                , Table.td [] [ text "ipga5470" ]
                , Table.td [] [ text "12/04/2018 16:23:54" ]
                ]
              , Table.tr []
                [ Table.td [] [ Badge.badgeDark [ ] [ text "3" ] ]
                , Table.td [] [ Badge.badgeDanger [ ] [ text "error" ] ]
                , Table.td [] [ text "copie habilitations" ]
                , Table.td [] [ text "aaedev" ]
                , Table.td [] [ text "emon2300" ]
                , Table.td [] [ text "esla2500" ]
                , Table.td [] [ text "ipga5470" ]
                , Table.td [] [ text "12/04/2018 14:58:23" ]
                -- , Table.th [] [ Badge.badgeDanger [ Spacing.ml1 ] [ text "error" ] ]
                ]
            ]
        }
      ]
  ]

pageAddHabilities : Model -> List (Html Msg)
pageAddHabilities model =
  [ br [] []
    , h1 [] [ text "Ajout d'habilitations" ]
    , br [] []
    , Grid.container []
      [ Form.form []
        [ Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Ldap" ]
          , Form.col [ Col.sm6 ]
            [ Fieldset.config
              |> Fieldset.children
                ( Radio.radioList "radioldap"
                  [ Radio.create [ Radio.id "radio-aae", Radio.inline ] "aae"
                  , Radio.create [ Radio.id "radio-spe", Radio.inline, Radio.disabled False ] "spe"
                  ]
                )
              |> Fieldset.view
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Environnement" ]
          , Form.col [ Col.sm6 ]
            [ Fieldset.config
              |> Fieldset.children
                ( Radio.radioList "radioenv"
                  [ Radio.create [ Radio.id "radio-qvr", Radio.inline ] "qvr"
                  , Radio.create [ Radio.id "radio-dev", Radio.inline, Radio.disabled False ] "dev"
                  , Radio.create [ Radio.id "radio-bench", Radio.inline, Radio.disabled False ] "bench"
                  ]
                )
              |> Fieldset.view
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Uid" ]
          , Form.col [ Col.sm6 ]
            [ Input.text [ Input.danger ]
            , Form.validFeedback [] [ text "Saisie correcte" ]
            , Form.invalidFeedback [] [ text "Saisie incorrecte" ]
            , Form.help [] [ text "Cet uid doit respecter la forme [ie][a-zA-Z]{3}[0-9]{4}" ]
            , Form.help [] [ text "Cet uid doit exister dans aae prod" ]
            , Form.help [] [ text "Cet uid doit exister dans le ldap/environnement source choisi" ]
            , Form.help [] [ text "10 uid maximum" ]
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "webApplication" ]
          , Form.col [ Col.sm6 ]
            [ Textarea.textarea [ Textarea.danger ]
            , Form.validFeedback [] [ text "Saisie correcte" ]
            , Form.invalidFeedback [] [ text "Saisie incorrecte" ]
            , Form.help [] [ text "Ces habilitations webApplications doivent exister dans l'annuaire" ]
            , Form.help [] [ text "10 habilitations webApplications maximum" ]
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "webProfil" ]
          , Form.col [ Col.sm6 ]
            [ Textarea.textarea [ Textarea.danger ]
            , Form.validFeedback [] [ text "Saisie correcte" ]
            , Form.invalidFeedback [] [ text "Saisie incorrecte" ]
            , Form.help [] [ text "Ces habilitations webProfil doivent exister dans l'annuaire" ]
            , Form.help [] [ text "10 habilitations webProfil maximum" ]
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "mediaPortaildossier" ]
          , Form.col [ Col.sm6 ]
            [ Textarea.textarea [ Textarea.danger ]
            , Form.validFeedback [] [ text "Saisie correcte" ]
            , Form.invalidFeedback [] [ text "Saisie incorrecte" ]
            , Form.help [] [ text "Ces habilitations mediaPortaildossier doivent exister dans l'annuaire" ]
            , Form.help [] [ text "10 habilitations mediaPortaildossier maximum" ]
            ]
          ]
        , br [] []
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "" ]
          , Form.col [ Col.sm6 ]
            [ Button.button
              [ Button.primary, Button.attrs [ class "float-left"] ]
              [ text "Soumettre" ]
            ]
          ]
        ]
      ]
  ]

pageCopyHabilities : Model -> List (Html Msg)
pageCopyHabilities model =
  [ br [] []
    , h1 [] [ text "Copie d'habilitations" ]
    , br [] []
    , Grid.container []
      [ Form.form []
        [ Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Ldap" ]
          , Form.col [ Col.sm6 ]
            [ Fieldset.config
              |> Fieldset.children
                ( Radio.radioList "radioldap"
                  [ Radio.create [ Radio.id "radio-aae", Radio.inline ] "aae"
                  , Radio.create [ Radio.id "radio-spe", Radio.inline, Radio.disabled False ] "spe"
                  ]
                )
              |> Fieldset.view
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Environnement" ]
          , Form.col [ Col.sm6 ]
            [ Fieldset.config
              |> Fieldset.children
                ( Radio.radioList "radioenv"
                  [ Radio.create [ Radio.id "radio-qvr", Radio.inline ] "qvr"
                  , Radio.create [ Radio.id "radio-dev", Radio.inline, Radio.disabled False ] "dev"
                  , Radio.create [ Radio.id "radio-bench", Radio.inline, Radio.disabled False ] "bench"
                  ]
                )
              |> Fieldset.view
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Uid source" ]
          , Form.col [ Col.sm6 ]
            [ Input.text [ Input.danger ]
            , Form.validFeedback [] [ text "Saisie correcte" ]
            , Form.invalidFeedback [] [ text "Saisie incorrecte" ]
            , Form.help [] [ text "Cet uid doit respecter la forme [ie][a-zA-Z]{3}[0-9]{4}" ]
            , Form.help [] [ text "Cet uid doit exister dans aae prod" ]
            , Form.help [] [ text "Cet uid doit exister dans le ldap/environnement source choisi" ]
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Uid destination" ]
          , Form.col [ Col.sm6 ]
            [ Input.text [ Input.danger ]
            , Form.validFeedback [] [ text "Saisie correcte" ]
            , Form.invalidFeedback [] [ text "Saisie incorrecte" ]
            , Form.help [] [ text "Cet uid doit respecter la forme [ie][a-zA-Z]{3}[0-9]{4}" ]
            , Form.help [] [ text "Cet uid doit exister dans aae prod" ]
            , Form.help [] [ text "Cet uid doit exister dans le ldap/environnement de destination choisi" ]
            ]
          ]
        , br [] []
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "" ]
          , Form.col [ Col.sm6 ]
            [ Button.button
              [ Button.primary, Button.attrs [ class "float-left"] ]
              [ text "Soumettre" ]
            ]
          ]
        ]
      ]
  ]

pageHome : Model -> List (Html Msg)
pageHome model =
  [ h1 [] [ text "Bienvenue" ]
  ]

pageCreateUser : Model -> List (Html Msg)
pageCreateUser model =
  [ br [] []
    , h1 [] [ text "Creation d'un utilisateur" ]
    , br [] []
    , Grid.container []
      [ Form.form []
        [ Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Ldap" ]
          , Form.col [ Col.sm6 ]
            [ Fieldset.config
              |> Fieldset.children
                ( Radio.radioList "radioldap"
                  [ Radio.create [ Radio.id "radio-aae", Radio.inline ] "aae"
                  , Radio.create [ Radio.id "radio-spe", Radio.inline, Radio.disabled False ] "spe"
                  ]
                )
              |> Fieldset.view
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Environnement" ]
          , Form.col [ Col.sm6 ]
            [ Fieldset.config
              |> Fieldset.children
                ( Radio.radioList "radioenv"
                  [ Radio.create [ Radio.id "radio-qvr", Radio.inline ] "qvr"
                  , Radio.create [ Radio.id "radio-dev", Radio.inline, Radio.disabled False ] "dev"
                  , Radio.create [ Radio.id "radio-bench", Radio.inline, Radio.disabled False ] "bench"
                  ]
                )
              |> Fieldset.view
            ]
          ]
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "Uid" ]
          , Form.col [ Col.sm6 ]
            [ Input.text [ Input.danger ]
            , Form.validFeedback [] [ text "Saisie correcte" ]
            , Form.invalidFeedback [] [ text "Saisie incorrecte" ]
            , Form.help [] [ text "Cet uid doit exister en aae prod" ]
            , Form.help [] [ text "Cet uid doit respecter la forme [ie][a-zA-Z]{3}[0-9]{4}" ]
            , Form.help [] [ text "Cet uid ne doit pas déjà exister dans le ldap/environnement cible choisi" ]
            ]
          ]
        , br [] []
        , Form.row []
          [ Form.colLabel [ Col.sm2 ] [ text "" ]
          , Form.col [ Col.sm6 ]
            [ Button.button
              [ Button.primary, Button.attrs [ class "float-left"] ]
              [ text "Soumettre" ]
            ]
          ]
        ]
      ]
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "SOrry couldn't find that page"
    ]

