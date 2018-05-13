module Epic exposing (..)


type Rules id entity world scene
    = Rules (List (Rule id entity world scene))


type InteractionMatcher id entity
    = With id
    | WithAny (entity -> Bool)
    | WithAnything


type Condition world
    = Boolean (world -> Bool)
    | Quantitative (world -> Float) Bounds


type Bounds
    = Equals Float
    | Between Float Float
    | LessThan Float
    | GreaterThan Float
    | LessThanOrEquals Float
    | GreaterThanOrEquals Float


type Rule id entity world scene
    = Rule
        { interaction : InteractionMatcher id entity
        , scene : Maybe scene
        , conditions : List (Condition world)
        , onMatch : world -> world
        }


valuateRule : id -> entity -> Maybe scene -> world -> Rule id entity world scene -> ( Float, Float, Float, Float )
valuateRule id entity maybeCurrentScene world (Rule rule) =
    let
        sceneValue =
            if maybeCurrentScene /= Nothing && rule.scene == maybeCurrentScene then
                1

            else
                0

        interactionValue =
            case rule.interaction of
                With _ ->
                    2

                WithAny _ ->
                    1

                WithAnything ->
                    0

        ( conditionValue, quantitativeExtension ) =
            List.foldl
                (\condition ( cVal, qExt ) ->
                    case condition of
                        Boolean _ ->
                            ( cVal + 1, qExt )

                        Quantitative n bounds ->
                            ( cVal + 1
                            , qExt
                                + (case bounds of
                                    Equals _ ->
                                        3

                                    Between _ _ ->
                                        2

                                    LessThan _ ->
                                        1

                                    GreaterThan _ ->
                                        1

                                    LessThanOrEquals _ ->
                                        0

                                    GreaterThanOrEquals _ ->
                                        0
                                  )
                            )
                )
                ( 0, 0 )
                rule.conditions
    in
        ( sceneValue, interactionValue, conditionValue, quantitativeExtension )


filterMatchingRules : id -> Maybe scene -> world -> Rules id entity world scene -> entity -> Rules id entity world scene
filterMatchingRules id maybeCurrentScene world rules entity =
    let
        interactionFilter entity =
            \(Rule rule) ->
                case rule.interaction of
                    With id_ ->
                        id == id_

                    WithAny p ->
                        p entity

                    WithAnything ->
                        True

        sceneFilter =
            case maybeCurrentScene of
                Nothing ->
                    \(Rule rule) -> rule.scene == Nothing

                Just scene ->
                    \(Rule rule) -> rule.scene == Nothing || rule.scene == Just scene

        conditionFilter =
            \(Rule rule) ->
                rule.conditions
                    |> List.all
                        (\condition ->
                            case condition of
                                Boolean p ->
                                    p world

                                Quantitative toNum bounds ->
                                    case bounds of
                                        Equals n ->
                                            toNum world == n

                                        Between n1 n2 ->
                                            let
                                                n =
                                                    toNum world
                                            in
                                                n >= n1 && n <= n2

                                        LessThan n ->
                                            toNum world < n

                                        GreaterThan n ->
                                            toNum world > n

                                        LessThanOrEquals n ->
                                            toNum world <= n

                                        GreaterThanOrEquals n ->
                                            toNum world >= n
                        )

        (Rules rs) =
            rules
    in
        Rules (List.filter (interactionFilter entity <&> sceneFilter <&> conditionFilter) rs)


runRules : (id -> world -> Maybe entity) -> id -> Maybe scene -> world -> Rules id entity world scene -> world
runRules getEntity id maybeCurrentScene world rules =
    let
        maybeEntity =
            getEntity id world

        getBestRuleUpdate e (Rules rules) =
            List.map (\rule -> ( valuateRule id e maybeCurrentScene world rule, rule )) rules
                |> List.sortBy Tuple.first
                |> List.reverse
                |> List.head
                |> Maybe.map (Tuple.second >> (\(Rule rule) -> rule.onMatch))
    in
        case maybeEntity of
            Nothing ->
                world

            Just entity ->
                filterMatchingRules id maybeCurrentScene world rules entity
                    |> getBestRuleUpdate entity
                    |> Maybe.withDefault identity
                    |> (|>) world


(<&>) : (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&>) p1 p2 =
    \a ->
        p1 a && p2 a


infixl 3 <&>
