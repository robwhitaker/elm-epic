module Epic exposing
    ( Rules, fromList, concat
    , Rule, RuleDefinition, rule
    , InteractionMatcher, with, withAnyId, withAnyEntity, withAnything
    , InteractionHandler, HandlerM, handler, andHandler, runHandler
    , Condition, boolean, numeric, Bounds, equals, between, lessThan, greaterThan, lessThanOrEquals, greaterThanOrEquals
    , runRules
    )

{-| This module allows you to define a set of rules which will
match depending on the current state of your world. This is a
batteries-not-included module. It does not provide a premade way
to match locations or characters or anything, but it does give you
the tools to define conditions such as those yourself.


## Rules

@docs Rules, fromList, concat


## Rule

@docs Rule, RuleDefinition, rule


## Interaction matching

@docs InteractionMatcher, with, withAnyId, withAnyEntity, withAnything


## Interaction handling

@docs InteractionHandler, HandlerM, handler, andHandler, runHandler


## Conditions

@docs Condition, boolean, numeric, Bounds, equals, between, lessThan, greaterThan, lessThanOrEquals, greaterThanOrEquals


## Evaluating rules

@docs runRules

-}


{-| A set of `Rule`s.
-}
type Rules id entity world scene
    = Rules (List (Rule id entity world scene))


{-| Convert a list of `Rule`s to a `Rules`.
-}
fromList : List (Rule id entity world scene) -> Rules id entity world scene
fromList =
    Rules


{-| Concatenate two `Rules` together.
-}
concat : List (Rules id entity world scene) -> Rules id entity world scene
concat =
    List.concatMap (\(Rules rules) -> rules) >> Rules


{-| Used in the `interaction` field of a `Rule` to specify what the user interacted with.
-}
type InteractionMatcher id entity
    = With id
    | WithAnyId (id -> Bool)
    | WithAnyEntity (Maybe entity -> Bool)
    | WithAnything


{-| Matches if the user interacted with the specific id. Produces the strongest InteractionMatcher.
-}
with : id -> InteractionMatcher id entity
with =
    With


{-| Matches if the user interacted with any id that fits the predicate. Produces the second strongest InterationMatcher.
-}
withAnyId : (id -> Bool) -> InteractionMatcher id entity
withAnyId =
    WithAnyId


{-| Matches if the user interacted with any entity that fits the predicate. Produces the third strongest InteractionMatcher.
-}
withAnyEntity : (Maybe entity -> Bool) -> InteractionMatcher id entity
withAnyEntity =
    WithAnyEntity


{-| Matches if the user interacted with anything. Produces the weakest InteractionMatcher.
-}
withAnything : InteractionMatcher id entity
withAnything =
    WithAnything


{-| Used in the `conditions` field of a `Rule` to specify various conditions which must match.
-}
type Condition id entity world
    = Boolean (InteractionHandler id entity world Bool)
    | Numeric Bounds (InteractionHandler id entity world Float)


{-| Creates a condition that matches when the predicate returns True.

    type alias World =
        { playerLocation : String }

    playerIsInLivingRoom : Condition World
    playerIsInLivingRoom =
        boolean
            (\id maybeEntity world ->
                world.playerLocation == "Living Room"
            )

-}
boolean : InteractionHandler id entity world Bool -> Condition id entity world
boolean =
    Boolean


{-| Creates a condition that matches when the returned number is within the specified bounds.

    type alias World =
        { inventory : List Item }

    getNumApples : World -> Float
    getNumApples world =
        world.inventory
            |> List.filter (\item -> item == Apple)
            |> List.length
            |> toFloat

    playerHasSomeApples : Condition World
    playerHasSomeApples =
        numeric (between 3 10) (\_ _ -> getNumApples)

-}
numeric : Bounds -> InteractionHandler id entity world Float -> Condition id entity world
numeric =
    Numeric


{-| The bounds of a numeric condition. The more specific the bounds type, the stronger the match will be
(e.g. `between` produces a stronger match than `lessThan` but a weaker match than `equals`). This makes numeric
matches slightly stronger than boolean matches as they have additional influence based on the bounding specificity.
-}
type Bounds
    = Equals Float
    | Between Float Float
    | LessThan Float
    | GreaterThan Float
    | LessThanOrEquals Float
    | GreaterThanOrEquals Float


{-| Bounding method that checks for equality.
-}
equals : Float -> Bounds
equals =
    Equals


{-| Bounding method that checks that a number is between two values, inclusive.
-}
between : Float -> Float -> Bounds
between =
    Between


{-| Bounding method that checks that a number is less than a given value.
-}
lessThan : Float -> Bounds
lessThan =
    LessThan


{-| Bounding method that checks that a number is greater than a given value.
-}
greaterThan : Float -> Bounds
greaterThan =
    GreaterThan


{-| Bounding method that checks that a number is less than or equal to a given value.
-}
lessThanOrEquals : Float -> Bounds
lessThanOrEquals =
    LessThanOrEquals


{-| Bounding method that checks that a number is greater than or equal to a given value.
-}
greaterThanOrEquals : Float -> Bounds
greaterThanOrEquals =
    GreaterThanOrEquals


{-| A rule which may or may not match a specific world state. If it is the best match,
the onMatch function will be run over the world, producing a new world.
-}
type Rule id entity world scene
    = Rule (RuleDefinition id entity world scene)


{-| A function to make a `Rule`.
-}
rule : RuleDefinition id entity world scene -> Rule id entity world scene
rule =
    Rule


{-| A function that can return a result based on an interaction. Used for conditions and
rule match handling.
-}
type alias InteractionHandler id entity world a =
    id -> Maybe entity -> world -> a


{-| A wrapper type around InteractionHandler used for chaining handlers together in
onMatch functions.
-}
type HandlerM id entity world
    = HandlerM (InteractionHandler id entity world world)


{-| Construct a HandlerM from an InteractionHandler.
-}
handler : InteractionHandler id entity world world -> HandlerM id entity world
handler f =
    HandlerM f


{-| Chain another handler together with a HandlerM.
-}
andHandler : InteractionHandler id entity world world -> HandlerM id entity world -> HandlerM id entity world
andHandler f (HandlerM h) =
    HandlerM
        (\id maybeEntity world -> f id maybeEntity (h id maybeEntity world))


{-| Provide the id, entity, and world to actually run the HandlerM.
-}
runHandler : HandlerM id entity world -> InteractionHandler id entity world world
runHandler (HandlerM h) id maybeEntity world =
    h id maybeEntity world


{-| A record to pass into the `rule` function to define a rule.
-}
type alias RuleDefinition id entity world scene =
    { interaction : InteractionMatcher id entity
    , scene : Maybe scene
    , conditions : List (Condition id entity world)
    , onMatch : InteractionHandler id entity world world
    }


valuateRule : id -> Maybe scene -> world -> Rule id entity world scene -> List Float
valuateRule id maybeCurrentScene world (Rule r) =
    let
        sceneValue =
            if maybeCurrentScene /= Nothing && r.scene == maybeCurrentScene then
                1

            else
                0

        interactionValue =
            case r.interaction of
                With _ ->
                    3

                WithAnyId _ ->
                    2

                WithAnyEntity _ ->
                    1

                WithAnything ->
                    0

        ( conditionValue, numericExtension ) =
            List.foldl
                (\condition ( cVal, qExt ) ->
                    case condition of
                        Boolean _ ->
                            ( cVal + 1, qExt )

                        Numeric bounds n ->
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
                r.conditions
    in
    [ sceneValue, interactionValue, conditionValue, numericExtension ]


filterMatchingRules : id -> Maybe entity -> Maybe scene -> world -> Rules id entity world scene -> Rules id entity world scene
filterMatchingRules id maybeEntity maybeCurrentScene world (Rules rs) =
    let
        interactionFilter =
            \(Rule r) ->
                case r.interaction of
                    With id_ ->
                        id == id_

                    WithAnyId p ->
                        p id

                    WithAnyEntity p ->
                        p maybeEntity

                    WithAnything ->
                        True

        sceneFilter =
            case maybeCurrentScene of
                Nothing ->
                    \(Rule r) -> r.scene == Nothing

                Just scene ->
                    \(Rule r) -> r.scene == Nothing || r.scene == Just scene

        conditionFilter =
            \(Rule r) ->
                r.conditions
                    |> List.all
                        (\condition ->
                            case condition of
                                Boolean p ->
                                    p id maybeEntity world

                                Numeric bounds toNum ->
                                    case bounds of
                                        Equals n ->
                                            toNum id maybeEntity world == n

                                        Between n1 n2 ->
                                            let
                                                n =
                                                    toNum id maybeEntity world
                                            in
                                            n >= n1 && n <= n2

                                        LessThan n ->
                                            toNum id maybeEntity world < n

                                        GreaterThan n ->
                                            toNum id maybeEntity world > n

                                        LessThanOrEquals n ->
                                            toNum id maybeEntity world <= n

                                        GreaterThanOrEquals n ->
                                            toNum id maybeEntity world >= n
                        )
    in
    Rules (List.filter (interactionFilter |> and sceneFilter |> and conditionFilter) rs)


{-| Given an interaction, run a set of rules over the world, producing a new world.
-}
runRules : (id -> world -> Maybe entity) -> id -> Maybe scene -> world -> Rules id entity world scene -> world
runRules getEntity id maybeCurrentScene world rs =
    let
        maybeEntity =
            getEntity id world

        getBestRuleUpdate (Rules rs_) =
            List.map (\r -> ( valuateRule id maybeCurrentScene world r, r )) rs_
                |> List.sortBy Tuple.first
                |> List.reverse
                |> List.head
                |> Maybe.map (Tuple.second >> (\(Rule r) -> r.onMatch))
    in
    filterMatchingRules id maybeEntity maybeCurrentScene world rs
        |> getBestRuleUpdate
        |> Maybe.withDefault (\_ _ w -> w)
        |> (\matchedRule -> matchedRule id maybeEntity world)


and : (a -> Bool) -> (a -> Bool) -> (a -> Bool)
and p1 p2 =
    \a ->
        p1 a && p2 a
