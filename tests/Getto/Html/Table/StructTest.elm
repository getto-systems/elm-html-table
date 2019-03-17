module Getto.Html.Table.StructTest exposing (..)
import Getto.Html.Table.Struct as Struct

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

type alias Struct =
  { header  : List (List String)
  , summary : List (List String)
  , content : List (List String)
  }

render : List (Struct.Column row String) -> List row -> Struct
render columns = Struct.render columns
  { emptyContent = Struct.Cell "##"
  , render = \border info data ->
    let
      space = " " |> String.repeat 4 |> String.repeat (info.colspan - 1)
      rowspan = info.rowspan |> String.fromInt

      toBorder style =
        case style of
          Struct.None   -> ""
          Struct.Single -> "|"
          Struct.Double -> "||"

      (left,right) = border |> Tuple.mapBoth toBorder toBorder
    in
      case data of
        Struct.Empty     -> left ++ "  " ++ "-" ++ rowspan ++ space ++ right
        Struct.Cell text -> left ++ text ++ "-" ++ rowspan ++ space ++ right
  }

suite : Test
suite =
  describe "Struct"
    [ describe "render"
      [ test "should render empty cell if empty data" <|
        \_ ->
          let
            data = []
          in
            data |> render
              [ Struct.column ( Struct.None, Struct.None )
                { header  = Struct.Cell "A"
                , summary = Struct.Empty
                , content = \row -> Struct.Cell (row.id ++ row.colA)
                }
              , Struct.column ( Struct.None, Struct.None )
                { header  = Struct.Cell "B"
                , summary = Struct.Empty
                , content = \row -> Struct.Cell (row.id ++ row.colB)
                }
              , Struct.column ( Struct.None, Struct.None )
                { header  = Struct.Cell "C"
                , summary = Struct.Empty
                , content = \row -> Struct.Cell (row.id ++ row.colC)
                }
              ]
            |> Expect.equal
              { header  =
                [ [  "A-1",  "B-1",  "C-1" ]
                ]
              , summary =
                []
              , content =
                [ [ "##-1"++"    "++"    " ]
                ]
              }

      , describe "column"
        [ test "should render simple column" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", colB = "b", colC = "c" }
                , { id = "2", colA = "a", colB = "b", colC = "c" }
                , { id = "3", colA = "a", colB = "b", colC = "c" }
                ]
            in
              data |> render
                [ Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Empty
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "B"
                  , summary = Struct.Empty
                  , content = \row -> Struct.Cell (row.id ++ row.colB)
                  }
                , Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "C"
                  , summary = Struct.Empty
                  , content = \row -> Struct.Cell (row.id ++ row.colC)
                  }
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1",  "B-1",  "C-1" ]
                  ]
                , summary =
                  []
                , content =
                  [ [ "1a-1", "1b-1", "1c-1" ]
                  , [ "2a-1", "2b-1", "2c-1" ]
                  , [ "3a-1", "3b-1", "3c-1" ]
                  ]
                }

        , test "should render simple column with summary row" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", colB = "b", colC = "c" }
                , { id = "2", colA = "a", colB = "b", colC = "c" }
                , { id = "3", colA = "a", colB = "b", colC = "c" }
                ]
            in
              data |> render
                [ Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Cell "S"
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "B"
                  , summary = Struct.Cell "U"
                  , content = \row -> Struct.Cell (row.id ++ row.colB)
                  }
                , Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "C"
                  , summary = Struct.Cell "M"
                  , content = \row -> Struct.Cell (row.id ++ row.colC)
                  }
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1",  "B-1",  "C-1" ]
                  ]
                , summary =
                  [ [  "S-1",  "U-1",  "M-1" ]
                  ]
                , content =
                  [ [ "1a-1", "1b-1", "1c-1" ]
                  , [ "2a-1", "2b-1", "2c-1" ]
                  , [ "3a-1", "3b-1", "3c-1" ]
                  ]
                }

        , test "should render simple column with border" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", colB = "b", colC = "c" }
                , { id = "2", colA = "a", colB = "b", colC = "c" }
                , { id = "3", colA = "a", colB = "b", colC = "c" }
                ]
            in
              data |> render
                [ Struct.column ( Struct.Single, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Cell "S"
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "B"
                  , summary = Struct.Cell "U"
                  , content = \row -> Struct.Cell (row.id ++ row.colB)
                  }
                , Struct.column ( Struct.None, Struct.Double )
                  { header  = Struct.Cell "C"
                  , summary = Struct.Cell "M"
                  , content = \row -> Struct.Cell (row.id ++ row.colC)
                  }
                ]
              |> Expect.equal
                { header  =
                  [ [  "|A-1",  "B-1",  "C-1||" ]
                  ]
                , summary =
                  [ [  "|S-1",  "U-1",  "M-1||" ]
                  ]
                , content =
                  [ [ "|1a-1", "1b-1", "1c-1||" ]
                  , [ "|2a-1", "2b-1", "2c-1||" ]
                  , [ "|3a-1", "3b-1", "3c-1||" ]
                  ]
                }
        ]

      , describe "group"
        [ test "should render group column" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", colB = "b", colC = "c" }
                , { id = "2", colA = "a", colB = "b", colC = "c" }
                , { id = "3", colA = "a", colB = "b", colC = "c" }
                ]
            in
              data |> render
                [ Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Empty
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.group (Struct.Cell "G")
                  [ Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "B"
                    , summary = Struct.Empty
                    , content = \row -> Struct.Cell (row.id ++ row.colB)
                    }
                  , Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "C"
                    , summary = Struct.Empty
                    , content = \row -> Struct.Cell (row.id ++ row.colC)
                    }
                  ]
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-2",  "G-1"++"    " ]
                  , [          "B-1",  "C-1" ]
                  ]
                , summary =
                  []
                , content =
                  [ [ "1a-1", "1b-1", "1c-1" ]
                  , [ "2a-1", "2b-1", "2c-1" ]
                  , [ "3a-1", "3b-1", "3c-1" ]
                  ]
                }

        , test "should render nested group column" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", colB = "b", colC = "c", colD = "d", colE = "e" }
                , { id = "2", colA = "a", colB = "b", colC = "c", colD = "d", colE = "e" }
                , { id = "3", colA = "a", colB = "b", colC = "c", colD = "d", colE = "e" }
                ]
            in
              data |> render
                [ Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Empty
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.group (Struct.Cell "G")
                  [ Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "B"
                    , summary = Struct.Empty
                    , content = \row -> Struct.Cell (row.id ++ row.colB)
                    }
                  , Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "C"
                    , summary = Struct.Empty
                    , content = \row -> Struct.Cell (row.id ++ row.colC)
                    }
                  , Struct.group (Struct.Cell "H")
                    [ Struct.column ( Struct.None, Struct.None )
                      { header  = Struct.Cell "D"
                      , summary = Struct.Empty
                      , content = \row -> Struct.Cell (row.id ++ row.colD)
                      }
                    , Struct.column ( Struct.None, Struct.None )
                      { header  = Struct.Cell "E"
                      , summary = Struct.Empty
                      , content = \row -> Struct.Cell (row.id ++ row.colE)
                      }
                    ]
                  ]
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-3",  "G-1"++"    "++"    "++"    " ]
                  , [          "B-2",  "C-2",  "H-1"++"    " ]
                  , [                          "D-1",  "E-1" ]
                  ]
                , summary =
                  []
                , content =
                  [ [ "1a-1", "1b-1", "1c-1", "1d-1", "1e-1" ]
                  , [ "2a-1", "2b-1", "2c-1", "2d-1", "2e-1" ]
                  , [ "3a-1", "3b-1", "3c-1", "3d-1", "3e-1" ]
                  ]
                }

        , test "should render group column with summary row" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", colB = "b", colC = "c" }
                , { id = "2", colA = "a", colB = "b", colC = "c" }
                , { id = "3", colA = "a", colB = "b", colC = "c" }
                ]
            in
              data |> render
                [ Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Cell "S"
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.group (Struct.Cell "G")
                  [ Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "B"
                    , summary = Struct.Cell "U"
                    , content = \row -> Struct.Cell (row.id ++ row.colB)
                    }
                  , Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "C"
                    , summary = Struct.Cell "M"
                    , content = \row -> Struct.Cell (row.id ++ row.colC)
                    }
                  ]
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-2",  "G-1"++"    " ]
                  , [          "B-1",  "C-1" ]
                  ]
                , summary =
                  [ [  "S-1",  "U-1",  "M-1" ]
                  ]
                , content =
                  [ [ "1a-1", "1b-1", "1c-1" ]
                  , [ "2a-1", "2b-1", "2c-1" ]
                  , [ "3a-1", "3b-1", "3c-1" ]
                  ]
                }
        ]
      , describe "union"
        [ test "should render union column" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = ["a"] }
                , { id = "2", colA = [] }
                , { id = "3", colA = ["a", "b"] }
                ]
            in
              data |> render
                [ Struct.union ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Empty
                  , colspan = 2
                  , data    = \row -> row.colA |> List.map (\a -> (row,a))
                  , content = \(row,a) -> Struct.Cell (row.id ++ a)
                  }
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1"++"    " ]
                  ]
                , summary =
                  []
                , content =
                  [ [ "1a-1", "  -1" ]
                  , [ "  -1", "  -1" ]
                  , [ "3a-1", "3b-1" ]
                  ]
                }

        , test "should render union column with summary row" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = ["a"] }
                , { id = "2", colA = [] }
                , { id = "3", colA = ["a", "b"] }
                ]
            in
              data |> render
                [ Struct.union ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Cell "S"
                  , colspan = 2
                  , data    = \row -> row.colA |> List.map (\a -> (row,a))
                  , content = \(row,a) -> Struct.Cell (row.id ++ a)
                  }
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1"++"    " ]
                  ]
                , summary =
                  [ [  "S-1"++"    " ]
                  ]
                , content =
                  [ [ "1a-1", "  -1" ]
                  , [ "  -1", "  -1" ]
                  , [ "3a-1", "3b-1" ]
                  ]
                }

        , test "should render union column with border" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = ["a"] }
                , { id = "2", colA = [] }
                , { id = "3", colA = ["a", "b"] }
                ]
            in
              data |> render
                [ Struct.union ( Struct.Single, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Cell "S"
                  , colspan = 2
                  , data    = \row -> row.colA |> List.map (\a -> (row,a))
                  , content = \(row,a) -> Struct.Cell (row.id ++ a)
                  }
                ]
              |> Expect.equal
                { header  =
                  [ [  "|A-1"++"    " ]
                  ]
                , summary =
                  [ [  "|S-1"++"    " ]
                  ]
                , content =
                  [ [ "|1a-1", "|  -1" ]
                  , [ "|  -1", "|  -1" ]
                  , [ "|3a-1", "|3b-1" ]
                  ]
                }
        ]
      , describe "parts"
        [ test "should render parts column" <|
          \_ ->
            let
              parts =
                [ { val = "a" }
                , { val = "b" }
                , { val = "c" }
                ]
              data =
                [ { id = "1", colA = "a" }
                , { id = "2", colA = "b" }
                , { id = "3", colA = "c" }
                ]
            in
              data |> render
                [ Struct.parts parts
                  (\part ->
                    [ Struct.column ( Struct.None, Struct.None )
                      { header  = Struct.Cell (part.val |> String.toUpper)
                      , summary = Struct.Empty
                      , content = \row -> Struct.Cell (row.id ++ (if part.val == row.colA then part.val else " "))
                      }
                    , Struct.column ( Struct.None, Struct.None )
                      { header  = Struct.Cell part.val
                      , summary = Struct.Empty
                      , content = \row -> Struct.Cell (row.id ++ (if part.val /= row.colA then part.val else " "))
                      }
                    ]
                  )
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1",  "a-1",  "B-1",  "b-1",  "C-1",  "c-1" ]
                  ]
                , summary =
                  []
                , content =
                  [ [ "1a-1", "1 -1", "1 -1", "1b-1", "1 -1", "1c-1" ]
                  , [ "2 -1", "2a-1", "2b-1", "2 -1", "2 -1", "2c-1" ]
                  , [ "3 -1", "3a-1", "3 -1", "3b-1", "3c-1", "3 -1" ]
                  ]
                }

        , test "should render parts column with summary row" <|
          \_ ->
            let
              parts =
                [ { val = "a", sum = "x" }
                , { val = "b", sum = "y" }
                , { val = "c", sum = "z" }
                ]
              data =
                [ { id = "1", colA = "a" }
                , { id = "2", colA = "b" }
                , { id = "3", colA = "c" }
                ]
            in
              data |> render
                [ Struct.parts parts
                  (\part ->
                    [ Struct.column ( Struct.None, Struct.None )
                      { header  = Struct.Cell (part.val |> String.toUpper)
                      , summary = Struct.Cell (part.sum |> String.toUpper)
                      , content = \row -> Struct.Cell (row.id ++ (if part.val == row.colA then part.val else " "))
                      }
                    , Struct.column ( Struct.None, Struct.None )
                      { header  = Struct.Cell part.val
                      , summary = Struct.Cell part.sum
                      , content = \row -> Struct.Cell (row.id ++ (if part.val /= row.colA then part.val else " "))
                      }
                    ]
                  )
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1",  "a-1",  "B-1",  "b-1",  "C-1",  "c-1" ]
                  ]
                , summary =
                  [ [  "X-1",  "x-1",  "Y-1",  "y-1",  "Z-1",  "z-1" ]
                  ]
                , content =
                  [ [ "1a-1", "1 -1", "1 -1", "1b-1", "1 -1", "1c-1" ]
                  , [ "2 -1", "2a-1", "2b-1", "2 -1", "2 -1", "2c-1" ]
                  , [ "3 -1", "3a-1", "3 -1", "3b-1", "3c-1", "3 -1" ]
                  ]
                }

        , test "should render parts column with border" <|
          \_ ->
            let
              parts =
                [ { val = "a", sum = "x" }
                , { val = "b", sum = "y" }
                , { val = "c", sum = "z" }
                ]
              data =
                [ { id = "1", colA = "a" }
                , { id = "2", colA = "b" }
                , { id = "3", colA = "c" }
                ]
            in
              data |> render
                [ Struct.parts parts
                  (\part ->
                    [ Struct.column ( Struct.Single, Struct.None )
                      { header  = Struct.Cell (part.val |> String.toUpper)
                      , summary = Struct.Cell (part.sum |> String.toUpper)
                      , content = \row -> Struct.Cell (row.id ++ (if part.val == row.colA then part.val else " "))
                      }
                    , Struct.column ( Struct.None, Struct.Double )
                      { header  = Struct.Cell part.val
                      , summary = Struct.Cell part.sum
                      , content = \row -> Struct.Cell (row.id ++ (if part.val /= row.colA then part.val else " "))
                      }
                    ]
                  )
                ]
              |> Expect.equal
                { header  =
                  [ [  "|A-1",  "a-1||",  "|B-1",  "b-1||",  "|C-1",  "c-1||" ]
                  ]
                , summary =
                  [ [  "|X-1",  "x-1||",  "|Y-1",  "y-1||",  "|Z-1",  "z-1||" ]
                  ]
                , content =
                  [ [ "|1a-1", "1 -1||", "|1 -1", "1b-1||", "|1 -1", "1c-1||" ]
                  , [ "|2 -1", "2a-1||", "|2b-1", "2 -1||", "|2 -1", "2c-1||" ]
                  , [ "|3 -1", "3a-1||", "|3 -1", "3b-1||", "|3c-1", "3 -1||" ]
                  ]
                }
        ]
      , describe "rows"
        [ test "should render rows column" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", items = [ { id = "7", colB = "x" } ] }
                , { id = "2", colA = "b", items = [] }
                , { id = "3", colA = "c", items = [ { id = "8", colB = "y" }
                                                  , { id = "9", colB = "z" } ] }
                ]
            in
              data |> render
                [ Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Empty
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.rows ( \row -> row.items |> List.map (\item -> ( row, item )) )
                  [ Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "B"
                    , summary = Struct.Empty
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.id)
                    }
                  , Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "C"
                    , summary = Struct.Empty
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.colB)
                    }
                  ]
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1",  "B-1",  "C-1" ]
                  ]
                , summary =
                  []
                , content =
                  [ [ "1a-1", "17-1", "1x-1" ]
                  , [ "2b-1", "  -1"++"    " ]
                  , [ "3c-2", "38-1", "3y-1" ]
                  , [         "39-1", "3z-1" ]
                  ]
                }

        , test "should render side by side rows column" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", items  = [ { id = "7", colB = "x" } ]
                                        , things = [ { id = "4", colC = "m" }
                                                   , { id = "5", colC = "n" } ] }
                , { id = "2", colA = "b", items  = []
                                        , things = [] }
                , { id = "3", colA = "c", items  = []
                                        , things = [ { id = "6", colC = "o" } ] }
                , { id = "4", colA = "d", items  = [ { id = "8", colB = "y" }
                                                   , { id = "9", colB = "z" } ]
                                        , things = [ { id = "7", colC = "p" } ] }
                ]
            in
              data |> render
                [ Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Empty
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.rows ( \row -> row.items |> List.map (\item -> ( row, item )) )
                  [ Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "B"
                    , summary = Struct.Empty
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.id)
                    }
                  , Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "C"
                    , summary = Struct.Empty
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.colB)
                    }
                  ]
                , Struct.rows ( \row -> row.things |> List.map (\thing -> ( row, thing )) )
                  [ Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "D"
                    , summary = Struct.Empty
                    , content = \(row,thing) -> Struct.Cell (row.id ++ thing.id)
                    }
                  , Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "E"
                    , summary = Struct.Empty
                    , content = \(row,thing) -> Struct.Cell (row.id ++ thing.colC)
                    }
                  ]
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1",  "B-1",  "C-1",  "D-1",  "E-1" ]
                  ]
                , summary =
                  []
                , content =
                  [ [ "1a-2", "17-1", "1x-1", "14-1", "1m-1" ]
                  , [         "  -1"++"    ", "15-1", "1n-1" ]
                  , [ "2b-1", "  -1"++"    ", "  -1"++"    " ]
                  , [ "3c-1", "  -1"++"    ", "36-1", "3o-1" ]
                  , [ "4d-2", "48-1", "4y-1", "47-1", "4p-1" ]
                  , [         "49-1", "4z-1", "  -1"++"    " ]
                  ]
                }

        , test "should render nested rows column" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", items = [ { id = "7", colB = "x", things = [ { id = "4", colC = "m" }
                                                                                     , { id = "5", colC = "n" } ] } ] }
                , { id = "2", colA = "b", items = [] }
                , { id = "3", colA = "c", items = [ { id = "8", colB = "y", things = [ { id = "6", colC = "o" } ] }
                                                  , { id = "9", colB = "z", things = [] } ] }
                ]
            in
              data |> render
                [ Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Empty
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.rows ( \row -> row.items |> List.map (\item -> ( row, item )) )
                  [ Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "B"
                    , summary = Struct.Empty
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.id)
                    }
                  , Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "C"
                    , summary = Struct.Empty
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.colB)
                    }
                  , Struct.rows ( \(row,item) -> item.things |> List.map (\thing -> ( row, item, thing )) )
                    [ Struct.column ( Struct.None, Struct.None )
                      { header  = Struct.Cell "D"
                      , summary = Struct.Empty
                      , content = \(row,item,thing) -> Struct.Cell (row.id ++ thing.id)
                      }
                    , Struct.column ( Struct.None, Struct.None )
                      { header  = Struct.Cell "E"
                      , summary = Struct.Empty
                      , content = \(row,item,thing) -> Struct.Cell (row.id ++ thing.colC)
                      }
                    ]
                  ]
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1",  "B-1",  "C-1",  "D-1",  "E-1" ]
                  ]
                , summary =
                  []
                , content =
                  [ [ "1a-2", "17-2", "1x-2", "14-1", "1m-1" ]
                  , [                         "15-1", "1n-1" ]
                  , [ "2b-1", "  -1"++"    "++"    "++"    " ]
                  , [ "3c-2", "38-1", "3y-1", "36-1", "3o-1" ]
                  , [         "39-1", "3z-1", "  -1"++"    " ]
                  ]
                }

        , test "should render rows column with summary row" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", items = [ { id = "7", colB = "x" } ] }
                , { id = "2", colA = "b", items = [] }
                , { id = "3", colA = "c", items = [ { id = "8", colB = "y" }
                                                  , { id = "9", colB = "z" } ] }
                ]
            in
              data |> render
                [ Struct.column ( Struct.None, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Cell "S"
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.rows ( \row -> row.items |> List.map (\item -> ( row, item )) )
                  [ Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "B"
                    , summary = Struct.Cell "U"
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.id)
                    }
                  , Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "C"
                    , summary = Struct.Cell "M"
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.colB)
                    }
                  ]
                ]
              |> Expect.equal
                { header  =
                  [ [  "A-1",  "B-1",  "C-1" ]
                  ]
                , summary =
                  [ [  "S-1",  "U-1",  "M-1" ]
                  ]
                , content =
                  [ [ "1a-1", "17-1", "1x-1" ]
                  , [ "2b-1", "  -1"++"    " ]
                  , [ "3c-2", "38-1", "3y-1" ]
                  , [         "39-1", "3z-1" ]
                  ]
                }

        , test "should render rows column with border" <|
          \_ ->
            let
              data =
                [ { id = "1", colA = "a", items = [ { id = "7", colB = "x" } ] }
                , { id = "2", colA = "b", items = [] }
                , { id = "3", colA = "c", items = [ { id = "8", colB = "y" }
                                                  , { id = "9", colB = "z" } ] }
                ]
            in
              data |> render
                [ Struct.column ( Struct.Single, Struct.None )
                  { header  = Struct.Cell "A"
                  , summary = Struct.Cell "S"
                  , content = \row -> Struct.Cell (row.id ++ row.colA)
                  }
                , Struct.rows ( \row -> row.items |> List.map (\item -> ( row, item )) )
                  [ Struct.column ( Struct.None, Struct.None )
                    { header  = Struct.Cell "B"
                    , summary = Struct.Cell "U"
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.id)
                    }
                  , Struct.column ( Struct.None, Struct.Double )
                    { header  = Struct.Cell "C"
                    , summary = Struct.Cell "M"
                    , content = \(row,item) -> Struct.Cell (row.id ++ item.colB)
                    }
                  ]
                ]
              |> Expect.equal
                { header  =
                  [ [  "|A-1",  "B-1",  "C-1||" ]
                  ]
                , summary =
                  [ [  "|S-1",  "U-1",  "M-1||" ]
                  ]
                , content =
                  [ [ "|1a-1", "17-1", "1x-1||" ]
                  , [ "|2b-1", "  -1"++"    ||" ]
                  , [ "|3c-2", "38-1", "3y-1||" ]
                  , [          "39-1", "3z-1||" ]
                  ]
                }
        ]
      ]
    ]
