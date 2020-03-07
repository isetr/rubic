namespace AIStuff

    module AStarModel =
        open Model

        type State = Cube

        type Action = Rotation

        type HState = (State * int)

        let StateMap (map: Map<State, float32>) : State -> float32 =
            let stateMap (s: State) =
                match map.TryFind s with
                | None -> System.Single.PositiveInfinity
                | Some v -> v
            stateMap



