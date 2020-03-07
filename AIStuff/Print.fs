namespace AIStuff

    module Print =
        open Model
        open System

        let FromColor (c: Color) =
            match c with
            | Red -> ConsoleColor.Red
            | Yellow -> ConsoleColor.Yellow
            | Orange -> ConsoleColor.DarkRed
            | Blue -> ConsoleColor.Blue
            | Green -> ConsoleColor.Green
            | White -> ConsoleColor.White

        let cprint (c: Color) =
            Console.BackgroundColor <- ConsoleColor.Black
            printf " "
            Console.BackgroundColor <- FromColor c
            printf " "
            Console.BackgroundColor <- ConsoleColor.Black
            printf " "


        let Print (cube: Cube) =
            let o, b, r, g, y, w = cube

            printf "         "
            (row 0 y false) |> Array.iter cprint
            printfn ""
            printfn ""
            printf "         "
            (row 1 y false) |> Array.iter cprint
            printfn ""
            printfn ""
            printf "         "
            (row 2 y false) |> Array.iter cprint
            printfn ""
            printfn ""
            
            (row 0 g false) |> Array.iter cprint
            (row 0 o false) |> Array.iter cprint
            (row 0 b false) |> Array.iter cprint
            (row 0 r false) |> Array.iter cprint
            printfn ""
            printfn ""
            
            (row 1 g false) |> Array.iter cprint
            (row 1 o false) |> Array.iter cprint
            (row 1 b false) |> Array.iter cprint
            (row 1 r false) |> Array.iter cprint
            printfn ""
            printfn ""
            
            (row 2 g false) |> Array.iter cprint
            (row 2 o false) |> Array.iter cprint
            (row 2 b false) |> Array.iter cprint
            (row 2 r false) |> Array.iter cprint
            printfn ""
            printfn ""
            
            printf "         "
            (row 0 w false) |> Array.iter cprint
            printfn ""
            printfn ""
            printf "         "
            (row 1 w false) |> Array.iter cprint
            printfn ""
            printfn ""
            printf "         "
            (row 2 w false) |> Array.iter cprint
            printfn ""
            printfn ""
