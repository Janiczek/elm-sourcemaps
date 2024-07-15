module Index exposing (main)


import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, href, src, style, title)
import Json.Decode as D

import Elm.License as License
import Elm.Package as Package
import Elm.Project as Project
import Elm.Version as Version

import Index.Icon as Icon
import Index.Navigator as Navigator
import Index.Skeleton as Skeleton



-- MAIN


main : Program D.Value Model Never
main =
  Browser.document
    { init = \flags -> (D.decodeValue decoder flags, Cmd.none)
    , update = \_ model -> (model, Cmd.none)
    , subscriptions = \_ -> Sub.none
    , view = view
    }



-- FLAGS


type alias Flags =
  { root : String
  , pwd : List String
  , dirs : List String
  , files : List File
  , readme : Maybe String
  , project : Maybe Project.Project
  , exactDeps : Dict.Dict String Version.Version
  }


type alias File =
  { name : String
  , runnable : Bool
  }



-- DECODER


decoder : D.Decoder Flags
decoder =
  D.map7 Flags
    (D.field "root" D.string)
    (D.field "pwd" (D.list D.string))
    (D.field "dirs" (D.list D.string))
    (D.field "files" (D.list fileDecoder))
    (D.field "readme" (D.nullable D.string))
    (D.field "outline" (D.nullable Project.decoder))
    (D.field "exactDeps" (D.dict Version.decoder))


fileDecoder : D.Decoder File
fileDecoder =
  D.map2 File
    (D.field "name" D.string)
    (D.field "runnable" D.bool)



-- MODEL


type alias Model =
  Result D.Error Flags



-- VIEW


view : Model -> Browser.Document msg
view model =
  case model of
    Err error ->
      { title = "???"
      , body =
          [ text (D.errorToString error)
          ]
      }

    Ok { root, pwd, dirs, files, readme, project, exactDeps } ->
      { title = String.join "/" ("~" :: pwd)
      , body =
          [ header [ class "header" ] []
          , div [ class "content" ]
              [ Navigator.view root pwd
              , viewLeftColumn dirs files readme
              , viewRightColumn exactDeps project
              , div [ style "clear" "both" ] []
              ]
          ]
      }


viewLeftColumn : List String -> List File -> Maybe String -> Html msg
viewLeftColumn dirs files readme =
  section [ class "left-column" ]
    [ viewFiles dirs files
    , viewReadme readme
    ]


viewRightColumn : ExactDeps -> Maybe Project.Project -> Html msg
viewRightColumn exactDeps maybeProject =
  section [ class "right-column" ] <|
    case maybeProject of
      Nothing ->
        []

      Just project ->
        [ viewProjectSummary project
        , viewDeps exactDeps project
        , viewTestDeps exactDeps project
        ]


-- VIEW README


viewReadme : Maybe String -> Html msg
viewReadme readme =
  case readme of
    Nothing ->
      text ""

    Just markdown ->
      Skeleton.readmeBox markdown



-- VIEW FILES


viewFiles : List String -> List File -> Html msg
viewFiles dirs files =
  Skeleton.box
    { title = "File Navigation"
    , items =
        List.filterMap viewDir (List.sort dirs)
        ++
        List.filterMap viewFile (List.sortBy .name files)
    , footer = Nothing
    }


viewDir : String -> Maybe (List (Html msg))
viewDir dir =
  if String.startsWith "." dir || dir == "elm-stuff" then
    Nothing
  else
    Just [ a [ href dir ] [ Icon.folder, text dir ] ]


viewFile : File -> Maybe (List (Html msg))
viewFile {name} =
  if String.startsWith "." name then
    Nothing
  else
    Just [ a [ href name ] [ Icon.lookup name, text name ] ]



-- VIEW PAGE SUMMARY


viewProjectSummary : Project.Project -> Html msg
viewProjectSummary project =
  case project of
    Project.Application info ->
      Skeleton.box
        { title = "Source Directories"
        , items = List.map (\dir -> [text dir]) info.dirs
        , footer = Nothing
        }
        -- TODO show estimated bundle size here

    Project.Package info ->
      Skeleton.box
        { title = "Package Info"
        , items =
            [ [ text ("Name: " ++ Package.toString info.name) ]
            , [ text ("Version: " ++ Version.toString info.version) ]
            , [ text ("License: " ++ License.toString info.license) ]
            ]
        , footer = Nothing
        }



-- VIEW DEPENDENCIES


type alias ExactDeps =
  Dict.Dict String Version.Version


viewDeps : ExactDeps -> Project.Project -> Html msg
viewDeps exactDeps project =
  let
    dependencies =
      case project of
        Project.Application info ->
          List.map viewVersion info.depsDirect

        Project.Package info ->
          List.map (viewConstraint exactDeps) info.deps
  in
  Skeleton.box
    { title = "Dependencies"
    , items = dependencies
    , footer = Nothing -- TODO Just ("/_elm/dependencies", "Add more dependencies?")
    }


viewTestDeps : ExactDeps -> Project.Project -> Html msg
viewTestDeps exactDeps project =
  let
    dependencies =
      case project of
        Project.Application info ->
          List.map viewVersion info.testDepsDirect

        Project.Package info ->
          List.map (viewConstraint exactDeps) info.testDeps
  in
  Skeleton.box
    { title = "Test Dependencies"
    , items = dependencies
    , footer = Nothing -- TODO Just ("/_elm/test-dependencies", "Add more test dependencies?")
    }


viewVersion : (Package.Name, Version.Version) -> List (Html msg)
viewVersion (pkg, version) =
  [ div [ style "float" "left" ]
      [ Icon.package
      , a [ href (toPackageUrl pkg version) ] [ text (Package.toString pkg) ]
      ]
  , div [ style "float" "right" ] [ text (Version.toString version) ]
  ]


viewConstraint : ExactDeps -> (Package.Name, constraint) -> List (Html msg)
viewConstraint exactDeps (pkg, _) =
  case Dict.get (Package.toString pkg) exactDeps of
    Just vsn ->
      viewVersion (pkg, vsn)

    Nothing ->
      [ div [ style "float" "left" ]
          [ Icon.package
          , text (Package.toString pkg)
          ]
      , div [ style "float" "right" ] [ text "???" ]
      ]


toPackageUrl : Package.Name -> Version.Version -> String
toPackageUrl name version =
  "https://package.elm-lang.org/packages/"
  ++ Package.toString name ++ "/" ++ Version.toString version
