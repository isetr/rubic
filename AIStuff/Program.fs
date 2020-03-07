namespace AIStuff

module Program =
    open Model
    open Moves
    open Print
    open AStar

    let steps =
        [|
            Front
            Left
            Up
            Right
            Back
        |]

    let stepNum = 3

    let randomSteps = false

    let h = epsilonHamming 20

    [<EntryPoint>]
    let main argv = 
        let startT = System.DateTime.Now

        printfn "Stat scrambling . . ."
        let start = scramble stepNum Actions CompleteCube (randomSteps, randomSteps)
        let startMySteps = scrambleWithSteps steps  CompleteCube (not randomSteps, not randomSteps)

        let scrambleT = System.DateTime.Now
        
        printfn "Stat solving . . ."
        let finish = 
            if randomSteps then
                solve start h
            else
                solve startMySteps h

        let solveT = System.DateTime.Now

        match finish with
        | None -> printfn "Nope D:"
        | Some path ->
            path
            |> Array.iter (fun (state, action) ->
                Print state
                printfn "action: %A" action
            )
            Print CompleteCube
            printfn "Scramble: %A\nAStar: %A" (scrambleT - startT) (solveT - scrambleT)
        0
