module Getto.Html.Table exposing
  ( Border
  , BorderStyle(..)
  , Column
  , Config
  , ColumnModel
  , UnionModel
  , render
  , column
  , group
  , union
  , parts
  , rows
  , none
  , single
  , double
  , empty
  , th
  , td
  , map
  )

{-| construct table structure for html

    import Html as H
    import Html.Attributes as A

    config =
      { attr =
        { table = \data ->
          [ [ "div-auto-size: none"
            , "rows: " ++ (data.thead |> String.fromInt)
            ]
            |> String.join "; "
            |> A.attribute "_fixedhead"
          ]
        , summary = [ "border-top-single" |> A.class ]
        , border = \(left,right) ->
          [ case left of
            Table.None -> []
            Table.Single -> ["border-left"]
            Table.Double -> ["border-left-double"]
          , case right of
            Table.None -> []
            Table.Single -> ["border-right"]
            Table.Double -> ["border-right-double"]
          ]
          |> List.concat |> List.map A.class
        }
      , emptyContent = Table.td []
        [ H.p [ "alert" |> A.class ]
          [ "empty!" |> H.text ]
        ]
      }

    data =
      { sum = 18
      , roleLength = 2
      , genders =
        [ { value = "male"
          , sum = 2
          }
        , { value = "female"
          , sum = 3
          }
        , { value = "other"
          , sum = 1
          }
        ]
      }

    rows =
      [ { id     = 2
        , name   = "John"
        , gender = "male"
        , roles = [ "admin" ]
        , comments =
          [ { user = "guest"
            , text = "looks goot to me"
            , likes =
              [ { user = "master"
                , text = "looks great to me!"
                }
              ]
            }
          ]
        }
      ]

    rows |> Table.render config
      [ Table.column ( Table.none, Table.none )
        { header  = Table.th [] [ "id" |> H.text ]
        , summary = Table.empty
        , content = \row -> Table.td [ "is-center" |> A.class ]
          [ H.p [] [ row.id |> String.fromInt |> H.text ] ]
        }
      , Table.group ( Table.th [ "is-center" |> A.class ] [ "info" |> H.text ] )
        [ Table.column ( Table.none, Table.none )
          { header  = Table.th [] [ "name" |> H.text ]
          , summary = Table.th [ "is-right" |> A.class ] [ "sum" |> H.text ]
          , content = \row -> Table.td []
            [ H.p [] [ row.name |> H.text ] ]
          }
        , Table.column ( Table.none, Table.single )
          { header  = Table.th [] [ "gender" |> H.text ]
          , summary = Table.td [] [ H.p [] [ data.sum |> String.fromInt |> H.text ] ]
          , content = \row -> Table.td []
            [ H.p [] [ row.gender |> H.text ] ]
          }
        , Table.union ( Table.none, Table.none )
          { header  = Table.th [] [ "roles" |> H.text ]
          , summary = Table.empty
          , colspan = data.roleLength
          , data    = \row -> row.roles |> List.map (\role -> ( row, role ))
          , content = \(row,role) -> Table.td []
            [ H.p [] [ role |> H.text ] ]
          }
        , Table.parts data.genders
          (\gender ->
            [ Table.column ( Table.none, Table.none )
              { header  = Table.th [] [ gender.value |> H.text ]
              , summary = Table.td [ "is-center" |> A.class ]
                [ H.p [] [ gender.sum |> String.fromInt |> H.text ] ]
              , content = \row -> Table.td [ "is-center" |> A.class ]
                [ H.p []
                  [ if row.gender == gender.value
                    then "v" |> H.text
                    else ""  |> H.text
                  ]
                ]
              }
            , Table.column ( Table.none, Table.none )
              { header  = Table.th [] [ gender.value |> H.text ]
              , summary = Table.td [ "is-center" |> A.class ]
                [ H.p [] [ gender.sum |> String.fromInt |> H.text ] ]
              , content = \row -> Table.td [ "is-center" |> A.class ]
                [ H.p []
                  [ if row.gender /= gender.value
                    then "o" |> H.text
                    else ""  |> H.text
                  ]
                ]
              }
            ]
          )
        , Table.rows ( \row -> row.roles |> List.map (\role -> ( row, role )) )
          [ Table.column ( Table.none, Table.none )
            { header  = Table.th [] [ "roles" |> H.text ]
            , summary = Table.empty
            , content = \(row,role) -> Table.td []
              [ H.p [] [ role |> H.text ] ]
            }
          , Table.column ( Table.none, Table.none )
            { header  = Table.th [] [ "roles" |> H.text ]
            , summary = Table.empty
            , content = \(row,role) -> Table.td []
              [ H.p [] [ role |> H.text ] ]
            }
          ]
        , Table.group ( Table.th [ "is-center" |> A.class ] [ "comment" |> H.text ] )
          [ Table.rows ( \row -> row.comments |> List.map (\comment -> ( row, comment )) )
            [ Table.column ( Table.single, Table.none )
              { header  = Table.th [] [ "user" |> H.text ]
              , summary = Table.empty
              , content = \(row,comment) -> Table.td []
                [ H.p [] [ comment.user |> H.text ] ]
              }
            , Table.column ( Table.none, Table.none )
              { header  = Table.th [] [ "text" |> H.text ]
              , summary = Table.empty
              , content = \(row,comment) -> Table.td []
                [ H.p [] [ comment.text |> H.text ] ]
              }
            , Table.group ( Table.th [ "is-center" |> A.class ] [ "like" |> H.text ] )
              [ Table.rows
                ( \(row,comment) -> comment.likes |> List.map (\like -> ( row, comment, like )) )
                [ Table.column ( Table.single, Table.none )
                  { header  = Table.th [] [ "user" |> H.text ]
                  , summary = Table.empty
                  , content = \(row,comment,like) -> Table.td []
                    [ H.p [] [ like.user |> H.text ] ]
                  }
                , Table.column ( Table.none, Table.none )
                  { header  = Table.th [] [ "text" |> H.text ]
                  , summary = Table.empty
                  , content = \(row,comment,like) -> Table.td []
                    [ H.p [] [ like.text |> H.text ] ]
                  }
                ]
              ]
            ]
          ]
        ]
      ]

# Definition
@docs Config, Border, BorderStyle, Column, ColumnModel, UnionModel

# Render
@docs render

# Column Construction
@docs column, group, union, parts, rows

# Border Construction
@docs none, single, double

# Cell Construction
@docs empty, th, td

# Helper
@docs map
 -}


import Getto.Html.Table.Struct as Struct

import Html as H exposing ( Html )
import Html.Attributes as A


{-| left border and right border

    ( BorderStyle, BorderStyle ) -- left, right
 -}
type alias Border = Struct.Border


{-| border style
 -}
type BorderStyle
  = None
  | Single
  | Double


{-| column definition
 -}
type alias Column row msg = Struct.Column row (Cell msg)

{-| config for `column`

    { header  : Cell msg        -- header cell
    , summary : Cell msg        -- summary cell
    , content : row -> Cell msg -- content cell
    }
 -}
type alias ColumnModel row msg = Struct.ColumnModel row (Cell msg)

{-| config for `union`

    { header  : Cell msg         -- header cell
    , summary : Cell msg         -- summary cell
    , colspan : Int              -- colspan
    , data    : row -> List data -- list of sub data
    , content : data -> Cell msg -- content cell
    }
 -}
type alias UnionModel row data msg = Struct.UnionModel row data (Cell msg)


{-| configuration for rendering

      { attr =
          -- attribute of <table> tag
        { table = \data ->
          [ [ "div-auto-size: none"
            , "rows: " ++ (data.thead |> String.fromInt)
            ]
            |> String.join "; "
            |> A.attribute "_fixedhead"
          ]
          -- attribute of summary <tr> tag
        , summary = [ "border-top-single" |> A.class ]
          -- border attribute of <td> or <th> tag
        , border = \(left,right) ->
          [ case left of
            Table.None -> []
            Table.Single -> ["border-left"]
            Table.Double -> ["border-left-double"]
          , case right of
            Table.None -> []
            Table.Single -> ["border-right"]
            Table.Double -> ["border-right-double"]
          ]
          |> List.concat |> List.map A.class
        }
        -- <td> content when empty rows
      , emptyContent = Table.td []
        [ H.p [ "alert" |> A.class ]
          [ "empty!" |> H.text ]
        ]
      }

 -}
type alias Config msg =
  { attr :
    { table   : { thead : Int } -> List (H.Attribute msg)
    , summary : List (H.Attribute msg)
    , border  : BorderAttribute msg
    }
  , emptyContent : Struct.Cell (Cell msg)
  }
type alias BorderAttribute msg = ( BorderStyle, BorderStyle ) -> List (H.Attribute msg)

type alias Cell msg =
  { tag  : Tag
  , attr : List (H.Attribute msg)
  , body : List (Html msg)
  }

type Tag
  = Th
  | Td


{-| define `column`

    Table.column
      ( Table.none, Table.none )       -- border ( left, right )
      { header  =
        Table.th [] [ "id" |> H.text ] -- header cell
      , summary = Table.empty          -- summary cell
      , content =                      -- content cell
        \row ->
          Table.td [ "is-center" |> A.class ]
            [ H.p [] [ row.id |> String.fromInt |> H.text ] ]
      }
 -}
column : Border -> ColumnModel row msg -> Column row msg
column = Struct.column


{-| define `group`

    Table.group
      ( Table.th [ "is-center" |> A.class ] -- header cell
        [ "info" |> H.text ]
      )
      [ Table.column                        -- group columns
        ( Table.none, Table.none )
        { header  = Table.th [] [ "name" |> H.text ]
        , summary = Table.th [ "is-right" |> A.class ] [ "sum" |> H.text ]
        , content = \row -> Table.td []
          [ H.p [] [ row.name |> H.text ] ]
        }
      ]
 -}
group : Struct.Cell (Cell msg) -> List (Column row msg) -> Column row msg
group = Struct.group


{-| define `union`

    Table.union
      ( Table.none, Table.none )          -- border ( left, right )
      { header  =
        Table.th [] [ "roles" |> H.text ] -- header cell
      , summary = Table.empty             -- summary cell
      , colspan = data.roleLength         -- colspan
      , data    =                         -- list of sub data
        \row -> row.roles |> List.map (\role -> ( row, role ))
      , content =                         -- content cell
        \(row,role) ->
          Table.td []
            [ H.p [] [ role |> H.text ] ]
      }
 -}
union : Border -> UnionModel row data msg -> Column row msg
union = Struct.union


{-| define `parts`

    Table.parts
      data.genders -- list of sub data
      (\gender ->  -- sub data columns
        [ Table.column ( Table.none, Table.none )
          { header  = Table.th [] [ gender.value |> H.text ]
          , summary = Table.td [ "is-center" |> A.class ]
            [ H.p [] [ gender.sum |> String.fromInt |> H.text ] ]
          , content = \row -> Table.td [ "is-center" |> A.class ]
            [ H.p []
              [ if row.gender == gender.value
                then "v" |> H.text
                else ""  |> H.text
              ]
            ]
          }
        ]
      )
 -}
parts : List data -> (data -> List (Column row msg)) -> Column row msg
parts = Struct.parts


{-| define `rows`

    Table.rows
      ( \row -> -- list of sub data
        row.roles |> List.map (\role -> ( row, role ))
      )
      [ Table.column -- sub data columns
        ( Table.none, Table.none )
        { header  = Table.th [] [ "roles" |> H.text ]
        , summary = Table.empty
        , content = \(row,role) -> Table.td []
          [ H.p [] [ role |> H.text ] ]
        }
      ]
 -}
rows : (row -> List data) -> List (Column data msg) -> Column row msg
rows = Struct.rows


{-| border style `None`
 -}
none : Struct.BorderStyle
none = Struct.None


{-| border style `Single`
 -}
single : Struct.BorderStyle
single = Struct.Single


{-| border style `Double`
 -}
double : Struct.BorderStyle
double = Struct.Double


{-| render html by Config, Columns, data
 -}
render : Config msg -> List (Column row msg) -> List row -> Html msg
render config columns list =
  let
    data = list |> Struct.render columns
      { emptyContent = config.emptyContent
      , render       = cell config.attr.border
      }

    thead = List.concat
      [ data.header  |> List.map (H.tr [])
      , data.summary |> List.map (H.tr config.attr.summary)
      ]

    tbody = data.content |> List.map (H.tr [])
  in
    H.table ( { thead = thead |> List.length } |> config.attr.table )
      [ H.thead [] thead
      , H.tbody [] tbody
      ]

cell : BorderAttribute msg -> Struct.Render (Cell msg) (Html msg)
cell attr border info data =
  let
    base = List.concat
      [ [ info.rowspan |> A.rowspan
        , info.colspan |> A.colspan
        ]
      , border |> Tuple.mapBoth mapBorderStyle mapBorderStyle |> attr
      ]
  in
    case data of
      Struct.Empty -> H.td base []
      Struct.Cell html -> tag html.tag (base ++ html.attr) html.body

mapBorderStyle : Struct.BorderStyle -> BorderStyle
mapBorderStyle style =
  case style of
    Struct.None -> None
    Struct.Single -> Single
    Struct.Double -> Double


{-| empty Cell

    summary = Table.empty -- nothing for summary
 -}
empty : Struct.Cell (Cell msg)
empty = Struct.Empty


{-| th Cell

    Table.th []          -- attributes
      [ "id" |> H.text ] -- contents
 -}
th : List (H.Attribute msg) -> List (Html msg) -> Struct.Cell (Cell msg)
th attr body = Struct.Cell
  { tag  = Th
  , attr = attr
  , body = body
  }


{-| td Cell

    Table.td [] -- attributes
      [ H.p []  -- content
        [ "content" |> H.text ]
      ]
 -}
td : List (H.Attribute msg) -> List (Html msg) -> Struct.Cell (Cell msg)
td attr body = Struct.Cell
  { tag  = Td
  , attr = attr
  , body = body
  }

tag : Tag -> List (H.Attribute msg) -> List (Html msg) -> Html msg
tag tagType =
  case tagType of
    Th -> H.th
    Td -> H.td


{-| map Column

    type Msg
      = Sub SubMsg

    column : model -> Table.Column row SubMsg

    model |> column |> Table.map Sub
 -}
map : (msg -> super) -> Column row msg -> Column row super
map msg = Struct.map (mapCell msg)

mapCell : (msg -> super) -> Cell msg -> Cell super
mapCell msg data =
  { tag  = data.tag
  , attr = data.attr |> List.map (A.map msg)
  , body = data.body |> List.map (H.map msg)
  }
