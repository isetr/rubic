namespace AIStuff

    module AStar =
        open AStarModel
        open Model
        open Moves
        open Print

        let hamming (state: HState) : float32 =
            let (o, b, r, g, y, w), _ = state
            let oH, bH, rH, gH, yH, wH = CompleteCube
            let faceDistance f fH = 
                (Array.zip f fH)
                |> Array.choose (fun (c, cH) ->
                    if c = cH then 
                        None
                    else
                        Some c
                )
                |> Array.length
            (faceDistance o oH) + (faceDistance b bH) + (faceDistance r rH) + (faceDistance g gH) + (faceDistance y yH) + (faceDistance w wH)
            |> float32

        let epsilonHamming (maxDepth: int) (state: HState) : float32 =
            let _, depth = state
            let h = 0.f //hamming state
            let epsilon = 5.f
            let w =
                if depth < maxDepth then
                    1.f - (float32 depth / float32 maxDepth)
                else
                    0.f
            h * (1.f + epsilon * w)

        let solvedPath (cameFrom: Map<State, State * Action>) (current: State) : (State * Action) array =
            let rec unfoldPath (current: State) (path: (State * Action) array) : (State * Action) array =
                match cameFrom.TryFind current with
                | None -> path
                | Some (s, a) -> 
                    unfoldPath s (Array.append [|(s, a)|] path)
            unfoldPath current [||]
        
        let solve (state: State) (h: HState -> float32) : (State * Action) array option =
            let closedSet = [||]
            let openSet = [|state|]

            let cameFrom = Map.empty

            let g = Map [|state, 0.f|]
            let f = Map [|state, h (state, 0)|]

            let rec solveRec (f: Map<State, float32>) (g: Map<State, float32>) (openSet: State array) (closedSet: State array) (cameFrom: Map<State, State * Action>) (n: int) =
                if Array.isEmpty openSet then
                    None
                else
                    let fScore = StateMap f
                    let current = 
                        openSet
                        |> Array.minBy fScore

                    //printfn "Step: %d" n
                    //Print current
                    //printfn ""

                    if current = CompleteCube then
                        Some (solvedPath cameFrom current)
                    else
                        let openSet =
                            openSet
                            |> Array.filter (fun s -> s <> current)
                        let closedSet =
                            Array.append [|current|] closedSet
                        let openSet, cameFrom, g, f =
                            Actions
                            |> Array.fold (fun s v ->
                                let (openSet: State array), (cameFrom : Map<State, State*Action>), (g: Map<State, float32>), (f: Map<State, float32>) = s
                                let newState = Rotate v current
                                let gScore = StateMap g

                                if Array.contains newState closedSet then
                                    s
                                else 
                                    let openSet =
                                        if not (Array.contains newState openSet) then
                                            Array.append [|newState|] openSet
                                        else
                                            openSet

                                    let newGScore = gScore current + 10.f
                                        
                                    let w (state: State) : float32 =
                                        if gScore current < gScore state then
                                            0.5f
                                        else
                                            2.f

                                    if newGScore >= gScore newState then
                                        openSet, cameFrom, g, f
                                    else
                                        let cameFrom = cameFrom.Add (newState, (current, v))
                                        let g = g.Add (newState, newGScore)
                                        let f = f.Add (newState, (1.f + w newState) * (newGScore + h (newState, int newGScore)))
                                        openSet, cameFrom, g, f

                            ) (openSet, cameFrom, g, f)
                        solveRec f g openSet closedSet cameFrom (n + 1)
            solveRec f g openSet closedSet cameFrom 0

