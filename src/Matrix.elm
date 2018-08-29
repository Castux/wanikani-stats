port module Matrix exposing (makepProblemMatrix, solution, solve)

import Dict exposing (Dict)


type alias Matrix =
    List (List Float)



-- Transpose and -1 on the diagonals!


makepProblemMatrix : Int -> Dict ( Int, Int ) Float -> Matrix
makepProblemMatrix size values =
    List.range 1 size
        |> List.map
            (\row ->
                List.range 1 size
                    |> List.map
                        (\col ->
                            Maybe.withDefault 0.0 (Dict.get ( col, row ) values)
                                + (if row == col then
                                    -1.0

                                   else
                                    0.0
                                  )
                        )
            )


port solve : { a : Matrix, b : List Float } -> Cmd msg


port solution : (List Float -> msg) -> Sub msg
