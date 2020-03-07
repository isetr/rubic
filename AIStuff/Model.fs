namespace AIStuff

    module Model =
        
        type Rotation =
            | Front
            | Back
            | Up
            | Down
            | Left
            | Right
            | FrontBackwards
            | BackBackwards
            | UpBackwards
            | DownBackwards
            | LeftBackwards
            | RightBackwards

        let Actions =
            [|
                Front
                Back
                Up
                Down
                Left
                Right
                FrontBackwards
                BackBackwards
                UpBackwards
                DownBackwards
                LeftBackwards
                RightBackwards
            |]

        type Color =
            | White
            | Yellow
            | Orange
            | Blue
            | Red
            | Green

        //
        //   Y
        //  GOBR
        //   W
        //

        let face (c: Color) =
            Array.init 9 (fun _ -> c)

        type Cube = 
            // O             B             R             G             Y             W
            (Color array * Color array * Color array * Color array * Color array * Color array)

        let CompleteCube : Cube =
            (
                face Orange,
                face Blue,
                face Red,
                face Green,
                face Yellow,
                face White
            )

        let col (n: int) (face: Color array) (reverse: bool) =
            face
            |> Array.mapi (fun i color ->
                if i % 3 = n then
                    Some color
                else
                    None
            )
            |> Array.choose id
            |> fun x -> if reverse then Array.rev x else x
        let row (n: int) (face: Color array) (reverse: bool) =
            face
            |> Array.mapi (fun i color ->
                if i >= n * 3 && i < n*3 + 3 then
                    Some color
                else
                    None
            )
            |> Array.choose id
            |> fun x -> if reverse then Array.rev x else x
        let changeCol (n: int) (newColors: Color array) (toFace: Color array) =
            toFace
            |> Array.mapi (fun i color ->
                if i % 3 = n then
                    Array.item (i / 3) newColors
                else
                    color
            )
        let changeRow (n: int) (newColors: Color array) (toFace: Color array) =
            toFace
            |> Array.mapi (fun i color ->
                if i >= n * 3 && i < n * 3 + 3 then
                    Array.item (i - n * 3) newColors
                else
                    color
            )
        let rotateSide (side: Color array) =
            let side1 = changeCol 0 (row 2 side false) side
            let side2 = changeCol 2 (row 0 side false) side1
            let side3 = changeRow 0 (col 0 side false) side2
            changeRow 2 (col 2 side true) side3
        let counterRotateSide (side: Color array) =
            let side1 = changeCol 0 (row 0 side true) side
            let side2 = changeCol 2 (row 2 side true) side1
            let side3 = changeRow 0 (col 2 side true) side2
            changeRow 2 (col 0 side false) side3
