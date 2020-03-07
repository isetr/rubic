namespace AIStuff

    module Moves =
        open Model
        open Print 

        let Rotate (rot: Rotation) (cube: Cube) : Cube =
            let o, b, r, g, y, w = cube
            match rot with
            | Front ->
                let ny =
                    changeRow 2 (col 2 g false) y
                let nb =
                    changeCol 0 (row 2 y false) b
                let nw =
                    changeRow 0 (col 0 b false) w
                let ng =
                    changeCol 2 (row 0 w false) g
                let no = rotateSide o
                no, nb, r, ng, ny, nw
            | Back ->
                let ny =
                    changeRow 0 (col 2 b false) y
                let nb =
                    changeCol 2 (row 2 w false) b
                let nw =
                    changeRow 2 (col 0 g false) w
                let ng =
                    changeCol 0 (row 0 y false) g
                let nr = rotateSide r
                o, nb, nr, ng, ny, nw
            | Down ->
                let ng =
                    changeRow 2 (row 2 r false) g
                let no =
                    changeRow 2 (row 2 g false) o
                let nb =
                    changeRow 2 (row 2 o false) b
                let nr =
                    changeRow 2 (row 2 b false) r
                let nw = rotateSide w
                no, nb, nr, ng, y, nw
            | Up ->
                let ng =
                    changeRow 0 (row 0 o false) g
                let no =
                    changeRow 0 (row 0 b false) o
                let nb =
                    changeRow 0 (row 0 r false) b
                let nr =
                    changeRow 0 (row 0 g false) r
                let ny = rotateSide y
                no, nb, nr, ng, ny, w
            | Right ->
                let ny =
                    changeCol 2 (col 2 o false) y
                let no = 
                    changeCol 2 (col 2 w false) o
                let nw =
                    changeCol 2 (col 0 r false) w
                let nr =
                    changeCol 0 (col 2 y false) r
                let nb = rotateSide b
                no, nb, nr, g, ny, nw
            | Left ->
                let ny =
                    changeCol 0 (col 2 r false) y
                let no = 
                    changeCol 0 (col 0 y false) o
                let nw =
                    changeCol 0 (col 0 o false) w
                let nr =
                    changeCol 2 (col 0 w true) r
                let ng = rotateSide g
                no, b, nr, ng, ny, nw
            | FrontBackwards ->
                let ny =
                    changeRow 2 (col 0 b true) y
                let nb =
                    changeCol 0 (row 0 w true) b
                let nw =
                    changeRow 0 (col 0 g true) w
                let ng =
                    changeCol 2 (row 0 y true) g
                let no = counterRotateSide o
                no, nb, r, ng, ny, nw
            | BackBackwards ->
                let ny =
                    changeRow 0 (col 0 g true) y
                let nb =
                    changeCol 2 (row 0 y true) b
                let nw =
                    changeRow 2 (col 2 b true) w
                let ng =
                    changeCol 0 (row 2 w true) g
                let nr = counterRotateSide r
                o, nb, nr, ng, ny, nw
            | DownBackwards ->
                let ng =
                    changeRow 2 (row 2 o true) g
                let no =
                    changeRow 2 (row 2 b true) o
                let nb =
                    changeRow 2 (row 2 r true) b
                let nr =
                    changeRow 2 (row 2 g true) r
                let nw = counterRotateSide w
                no, nb, nr, ng, y, nw
            | UpBackwards ->
                let ng =
                    changeRow 0 (row 0 r true) g
                let no =
                    changeRow 0 (row 0 g true) o
                let nb =
                    changeRow 0 (row 0 o true) b
                let nr =
                    changeRow 0 (row 0 b true) r
                let ny = counterRotateSide y
                no, nb, nr, ng, ny, w
            | RightBackwards ->
                let ny =
                    changeCol 2 (col 0 r true) y
                let no = 
                    changeCol 2 (col 2 y true) o
                let nw =
                    changeCol 2 (col 2 o true) w
                let nr =
                    changeCol 0 (col 2 w true) r
                let nb = counterRotateSide b
                no, nb, nr, g, ny, nw
            | LeftBackwards ->
                let ny =
                    changeCol 0 (col 0 o true) y
                let no = 
                    changeCol 0 (col 0 w true) o
                let nw =
                    changeCol 0 (col 2 r true) w
                let nr =
                    changeCol 2 (col 0 y true) r
                let ng = counterRotateSide g
                no, b, nr, ng, ny, nw

        let scramble (steps: int) (actions: Rotation array) (cube: Cube) ((printSteps, printCube): bool * bool) =
            let rand = System.Random()
            let actionCount = actions.Length

            let sequence = 
                Array.init steps (fun _ ->
                    Array.item (rand.Next(0, actionCount - 1)) actions
                )

            sequence
            |> Array.fold (fun state action ->
                if printSteps then 
                    printfn "step: %A" action
                let s = Rotate action state
                if printCube then 
                    Print s
                s
            ) cube

        let scrambleWithSteps (actions: Rotation array) (cube: Cube) ((printSteps, printCube): bool * bool) =
            actions
            |> Array.fold (fun state action ->
                if printSteps then 
                    printfn "step: %A" action
                let s = Rotate action state
                if printCube then 
                    Print s
                s
            ) cube


