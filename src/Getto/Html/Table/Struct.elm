module Getto.Html.Table.Struct exposing
  ( Column
  , BorderStyle(..)
  , Cell(..)
  , Render
  , Border
  , ColumnModel
  , UnionModel
  , render
  , column
  , group
  , union
  , parts
  , rows
  , map
  )

type Column row a
  = Column
    { header  : Header a
    , summary : Summary a
    , content : List row -> Content a
    }
  | Group
    { header    : Header a
    , summaries : List (Summary a)
    , contents  : List row -> List (Content a)
    }
  | Union
    { header  : Header a
    , summary : Summary a
    , content : List row -> Content a
    }
  | Parts
    { headers   : List (Header a)
    , summaries : List (Summary a)
    , contents  : List row -> List (Content a)
    }
  | Rows
    { headers   : List (Header a)
    , summaries : List (Summary a)
    , content   : List row -> Content a
    }

type Header a
  = Header      { border : Border, cell : Cell a }
  | GroupHeader { border : Border, cell : Cell a, children : List (Header a) }
  | UnionHeader { border : Border, cell : Cell a, colspan : Int }

type Summary a
  = Summary      { border : Border, cell : Cell a }
  | UnionSummary { border : Border, cell : Cell a, colspan : Int }

type Content a
  = Content      { border : Border, cells        : List (Cell a) }
  | UnionContent { border : Border, cellLists    : List (List (Cell a)),    colspan : Int }
  | RowsContent  { border : Border, contentLists : List (List (Content a)), colspan : Int }

type Build a
  = Build      { border : Border, cell       : Cell a,                colspan : Int }
  | UnionBuild { border : Border, cells      : List (Cell a),         colspan : Int }
  | RowsBuild  { border : Border, buildLists : List (List (Build a)), colspan : Int }

type alias Border = ( BorderStyle, BorderStyle )

type BorderStyle
  = None
  | Single
  | Double

type Cell a
  = Empty
  | Cell a

type alias Table a =
  { header  : List (List a)
  , summary : List (List a)
  , content : List (List a)
  }

type alias Struct a =
  { header  : HeaderStruct a
  , summary : SummaryStruct a
  , content : ContentStruct a
  }
type alias HeaderStruct a =
  { headers : List (Header a)
  , rowspan : Int
  }
type alias SummaryStruct a = Maybe (List (Summary a))
type alias ContentStruct a = List (List (Build a))

type alias Render a content = Border -> RenderInfo -> Cell a -> content
type alias RenderInfo =
  { colspan : Int
  , rowspan : Int
  }

type alias Config a content =
  { emptyContent : Cell a
  , render : Render a content
  }

render : List (Column row a) -> Config a content -> List row -> Table content
render columns config list =
  let
    struct = list |> build columns config.emptyContent
  in
    { header  = struct.header  |> toHeaderRows  config.render
    , summary = struct.summary |> toSummaryRows config.render
    , content = struct.content |> toContentRows config.render
    }

build : List (Column row a) -> Cell a -> List row -> Struct a
build columns emptyContent list =
  let
    struct = columns |> List.foldl
      (\col acc ->
        case col of
          Column data ->
            { acc
            | header  = acc.header  ++ [ data.header ]
            , summary = acc.summary ++ [ data.summary ]
            , content = acc.content ++ [ list |> data.content ]
            }
          Group data ->
            { acc
            | header  = acc.header  ++ [ data.header ]
            , summary = acc.summary ++ ( data.summaries )
            , content = acc.content ++ ( list |> data.contents )
            }
          Union data ->
            { acc
            | header  = acc.header  ++ [ data.header ]
            , summary = acc.summary ++ [ data.summary ]
            , content = acc.content ++ [ list |> data.content ]
            }
          Parts data ->
            { acc
            | header  = acc.header  ++ ( data.headers )
            , summary = acc.summary ++ ( data.summaries )
            , content = acc.content ++ ( list |> data.contents )
            }
          Rows data ->
            { acc
            | header  = acc.header  ++ ( data.headers )
            , summary = acc.summary ++ ( data.summaries )
            , content = acc.content ++ [ list |> data.content ]
            }
      )
      { header  = []
      , summary = []
      , content = []
      }
  in
    { header  = struct.header  |> buildHeader
    , summary = struct.summary |> buildSummary
    , content = struct.content |> buildContent |> withEmptyContent emptyContent struct.summary
    }

buildHeader : List (Header a) -> HeaderStruct a
buildHeader headers =
  let
    depth header =
      case header of
        GroupHeader data -> 1 + (data.children |> maxDepth depth)
        _ -> 1
  in
    { headers = headers
    , rowspan = headers |> maxDepth depth
    }

buildSummary : List (Summary a) -> SummaryStruct a
buildSummary summaries =
  let
    summaryCell summary =
      case summary of
        Summary      data -> data.cell
        UnionSummary data -> data.cell
  in
    if summaries |> List.all (summaryCell >> (==) Empty)
      then Nothing
      else Just summaries

buildContent : List (Content a) -> ContentStruct a
buildContent = List.map
  (\content ->
    case content of
      Content data ->
        data.cells |> List.map
          (\cellValue -> Build
            { border  = data.border
            , cell    = cellValue
            , colspan = 1
            }
          )
      UnionContent data ->
        data.cellLists |> List.map
          (\cells -> UnionBuild
            { border  = data.border
            , cells   = cells
            , colspan = data.colspan
            }
          )
      RowsContent data ->
        data.contentLists |> List.map
          (\contents -> RowsBuild
            { border     = data.border
            , buildLists = contents |> buildContent
            , colspan    = data.colspan
            }
          )
  )
  >> transpose

transpose : List (List a) -> List (List a)
transpose listOfLists =
  let
    rowsLength =
      case listOfLists of
        [] -> 0
        head :: _ -> head |> List.length
  in
    List.foldr (List.map2 (::)) (List.repeat rowsLength []) listOfLists

withEmptyContent : Cell a -> List (Summary a) -> ContentStruct a -> ContentStruct a
withEmptyContent emptyContent summaries builds =
  if builds |> List.isEmpty |> not
    then builds
    else
      let
        border =
          ( summaries |> List.head                 |> summaryBorder Tuple.first
          , summaries |> List.reverse |> List.head |> summaryBorder Tuple.second
          )
      in
        [ [ Build
            { border  = border
            , cell    = emptyContent
            , colspan = summaries |> List.length
            }
          ]
        ]


toHeaderRows : Render a content -> HeaderStruct a -> List (List content)
toHeaderRows f struct =
  let
    width header =
      case header of
        UnionHeader data -> data.colspan
        GroupHeader data -> data.children |> List.map width |> List.sum
        _ -> 1

    fullWidth = List.map width >> List.sum
  in
    struct.headers
    |> List.foldl
      (\cellValue -> fill <|
        case cellValue of
          Header data ->
            [ data.cell |> f data.border
              { colspan = 1
              , rowspan = struct.rowspan
              }
            ] :: []
          UnionHeader data ->
            if data.colspan == 0
              then []
              else
                [ data.cell |> f data.border
                  { colspan = data.colspan
                  , rowspan = struct.rowspan
                  }
                ] :: []
          GroupHeader data ->
            [ data.cell |> f data.border
              { colspan = data.children |> fullWidth
              , rowspan = 1
              }
            ] :: ( { headers = data.children, rowspan = struct.rowspan - 1 } |> toHeaderRows f )
      )
      []

toSummaryRows : Render a content -> SummaryStruct a -> List (List content)
toSummaryRows f =
  Maybe.map
    (\summaries ->
      [ summaries |> List.map
        (\summary ->
          case summary of
            Summary data -> data.cell |> f data.border
              { colspan = 1
              , rowspan = 1
              }
            UnionSummary data -> data.cell |> f data.border
              { colspan = data.colspan
              , rowspan = 1
              }
        )
      ]
    )
  >> Maybe.withDefault []

toContentRows : Render a content -> ContentStruct a -> List (List content)
toContentRows f = List.concatMap <|
  \row ->
    let
      depth content =
        case content of
          RowsBuild data -> data.buildLists |> List.map (maxDepth depth) |> List.sum
          _ -> 1

      fullDepth = List.map (maxDepth depth) >> List.sum

      rowspan = row |> maxDepth depth
    in
      row |> List.foldl
        (\col -> fill <|
          case col of
            Build data ->
              [ [ data.cell |> f data.border
                  { colspan = data.colspan
                  , rowspan = rowspan
                  }
                ]
              ]
            UnionBuild data ->
              [ ( data.cells ++
                  ( Empty |> List.repeat ( data.colspan - (data.cells |> List.length)) )
                )
                |> List.map
                  (f data.border
                    { colspan = 1
                    , rowspan = rowspan
                    }
                  )
              ]
            RowsBuild data ->
              let
                paddingLength = rowspan - (data.buildLists |> fullDepth)
              in
                ( data.buildLists |> toContentRows f ) ++
                ( if paddingLength > 0
                    then
                      [ [ Empty |> f data.border
                          { colspan = data.colspan
                          , rowspan = paddingLength
                          }
                        ]
                      ]
                    else []
                )
        )
        []

fill : List (List a) -> List (List a) -> List (List a)
fill list acc =
  let
    rowLength = list |> List.length
    accLength = acc  |> List.length

    length =
      if rowLength > accLength
        then rowLength
        else accLength

    expand targetLength target =
      target ++
      ( [] |> List.repeat (length - targetLength) )
  in
    List.map2 List.append
      (acc  |> expand accLength)
      (list |> expand rowLength)


maxDepth : (row -> Int) -> List row -> Int
maxDepth depth =
  List.map depth
  >> List.maximum
  >> Maybe.withDefault 1


type alias ColumnModel row a =
  { header  : Cell a
  , summary : Cell a
  , content : row -> Cell a
  }

column : Border -> ColumnModel row a -> Column row a
column border model = Column
  { header  = Header  { border = border, cell = model.header }
  , summary = Summary { border = border, cell = model.summary }
  , content = \list -> Content { border = border, cells = list |> List.map model.content }
  }

group : Cell a -> List (Column row a) -> Column row a
group header columns =
  Group
    { header = GroupHeader
      { border   = columns |> columnBorder
      , cell     = header
      , children = columns |> columnHeaders
      }
    , summaries = columns |> columnSummaries
    , contents  = \list -> columns |> columnContents list
    }

type alias UnionModel row data a =
  { header  : Cell a
  , summary : Cell a
  , colspan : Int
  , data    : row -> List data
  , content : data -> Cell a
  }

union : Border -> UnionModel row data a -> Column row a
union border model = Union
  { header  = UnionHeader  { border = border, cell = model.header,  colspan = model.colspan }
  , summary = UnionSummary { border = border, cell = model.summary, colspan = model.colspan }
  , content = \list -> UnionContent
    { border    = border
    , cellLists = list |> List.map ( model.data >> List.map model.content )
    , colspan   = model.colspan
    }
  }

parts : List data -> (data -> List (Column row a)) -> Column row a
parts data f =
  let
    columns = data |> List.concatMap f
  in
    Parts
      { headers   = columns |> columnHeaders
      , summaries = columns |> columnSummaries
      , contents  = \list -> columns |> columnContents list
      }

rows : (row -> List data) -> List (Column data a) -> Column row a
rows data columns =
  let
    summaries = columns |> columnSummaries
  in
    Rows
      { headers   = columns |> columnHeaders
      , summaries = summaries
      , content   = \list -> RowsContent
        { border       = columns |> columnBorder
        , contentLists = list |> List.map (\row -> columns |> columnContents (row |> data))
        , colspan      = summaries |> List.length
        }
      }

columnHeaders : List (Column row a) -> List (Header a)
columnHeaders = List.concatMap <|
  \col ->
    case col of
      Column data -> [ data.header ]
      Group  data -> [ data.header ]
      Union  data -> [ data.header ]
      Parts  data -> ( data.headers )
      Rows   data -> ( data.headers )

columnSummaries : List (Column row a) -> List (Summary a)
columnSummaries = List.concatMap <|
  \col ->
    case col of
      Column data -> [ data.summary ]
      Group  data -> ( data.summaries )
      Union  data -> [ data.summary ]
      Parts  data -> ( data.summaries )
      Rows   data -> ( data.summaries )

columnContents : List row -> List (Column row a) -> List (Content a)
columnContents list = List.concatMap <|
  \col ->
    case col of
      Column data -> [ list |> data.content ]
      Group  data -> ( list |> data.contents )
      Union  data -> [ list |> data.content ]
      Parts  data -> ( list |> data.contents )
      Rows   data -> [ list |> data.content ]


columnBorder : List (Column row a) -> Border
columnBorder columns =
  let
    summary f col =
      case col of
        Column data -> Just data.summary
        Group  data -> data.summaries |> f |> List.head
        Union  data -> Just data.summary
        Parts  data -> data.summaries |> f |> List.head
        Rows   data -> data.summaries |> f |> List.head
  in
    ( columns |> List.head                 |> Maybe.andThen (summary identity)     |> summaryBorder Tuple.first
    , columns |> List.reverse |> List.head |> Maybe.andThen (summary List.reverse) |> summaryBorder Tuple.second
    )

summaryBorder : (Border -> BorderStyle) -> Maybe (Summary a) -> BorderStyle
summaryBorder f = Maybe.map
  (\summary ->
    case summary of
      Summary      data -> data.border |> f
      UnionSummary data -> data.border |> f
  )
  >> Maybe.withDefault None

map : (a -> b) -> Column row a -> Column row b
map f col =
  case col of
    Column data -> Column
      { header  = data.header  |> mapHeader f
      , summary = data.summary |> mapSummary f
      , content = data.content >> mapContent f
      }
    Group data -> Group
      { header    = data.header    |> mapHeader f
      , summaries = data.summaries |> List.map (mapSummary f)
      , contents  = data.contents  >> List.map (mapContent f)
      }
    Union data -> Union
      { header  = data.header  |> mapHeader f
      , summary = data.summary |> mapSummary f
      , content = data.content >> mapContent f
      }
    Parts data -> Parts
      { headers   = data.headers   |> List.map (mapHeader f)
      , summaries = data.summaries |> List.map (mapSummary f)
      , contents  = data.contents  >> List.map (mapContent f)
      }
    Rows data -> Rows
      { headers   = data.headers   |> List.map (mapHeader f)
      , summaries = data.summaries |> List.map (mapSummary f)
      , content   = data.content   >> mapContent f
      }

mapHeader : (a -> b) -> Header a -> Header b
mapHeader f header =
  case header of
    Header data -> Header
      { border = data.border
      , cell   = data.cell |> mapCell f
      }
    GroupHeader data -> GroupHeader
      { border   = data.border
      , cell     = data.cell |> mapCell f
      , children = data.children |> List.map (mapHeader f)
      }
    UnionHeader data -> UnionHeader
      { border  = data.border
      , cell    = data.cell |> mapCell f
      , colspan = data.colspan
      }

mapSummary : (a -> b) -> Summary a -> Summary b
mapSummary f summary =
  case summary of
    Summary data -> Summary
      { border = data.border
      , cell   = data.cell |> mapCell f
      }
    UnionSummary data -> UnionSummary
      { border  = data.border
      , cell    = data.cell |> mapCell f
      , colspan = data.colspan
      }

mapContent : (a -> b) -> Content a -> Content b
mapContent f content =
  case content of
    Content data -> Content
      { border = data.border
      , cells  = data.cells |> mapCells f
      }
    UnionContent data -> UnionContent
      { border    = data.border
      , cellLists = data.cellLists |> List.map (mapCells f)
      , colspan   = data.colspan
      }
    RowsContent data -> RowsContent
      { border       = data.border
      , contentLists = data.contentLists |> List.map (List.map (mapContent f))
      , colspan      = data.colspan
      }

mapCell : (a -> b) -> Cell a -> Cell b
mapCell f cell =
  case cell of
    Empty -> Empty
    Cell content -> content |> f |> Cell

mapCells : (a -> b) -> List (Cell a) -> List (Cell b)
mapCells f = List.map (mapCell f)
