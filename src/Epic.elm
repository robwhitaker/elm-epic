module Epic
    exposing
        ( Bounds
        , Condition
        , InteractionMatcher
        , Rule
        , Rules
        , between
        , boolean
        , concat
        , equals
        , fromList
        , greaterThan
        , greaterThanOrEquals
        , lessThan
        , lessThanOrEquals
        , numeric
        , rule
        , runRules
        , with
        , withAny
        , withAnything
        )

{-| This module allows you to define a set of rules which will
match depending on the current state of your world. This is a
batteries-not-included module. It does not provide a premade way
to match locations or characters or anything, but it does give you
the tools to define conditions such as those yourself.


## Rules

@docs Rules, fromList, concat


## Rule

@docs Rule, rule


## Interaction matching

@docs InteractionMatcher, with, withAny, withAnything


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
    | WithAny (Maybe entity -> Bool)
    | WithAnything


{-| Matches if the user interacted with the specific id. Produces the strongest InteractionMatcher.
-}
with : id -> InteractionMatcher id entity
with =
    With


{-| Matches if the user interacted with any entity that fits the predicate. Produces the second strongest InteractionMatcher.
-}
withAny : (Maybe entity -> Bool) -> InteractionMatcher id entity
withAny =
    WithAny


{-| Matches if the user interacted with anything. Produces the weakest InteractionMatcher.
-}
withAnything : InteractionMatcher id entity
withAnything =
    WithAnything


{-| Used in the `conditions` field of a `Rule` to specify various conditions which must match.
-}
type Condition world
    = Boolean (world -> Bool)
    | Numeric Bounds (world -> Float)


{-| Creates a condition that matches when the predicate returns True.

    type alias World = { playerLocation : String }

    playerIsInLivingRoom : Condition World
    playerIsInLivingRoom = boolean (\world -> world.playerLocation == "Living Room")

-}
boolean : (world -> Bool) -> Condition world
boolean =
    Boolean


{-| Creates a condition that matches when the returned number is within the specified bounds.

    type alias World = { inventory : List Item }

    getNumApples : World -> Float
    getNumApples world =
        world.inventory
            |> List.filter (\item -> item == Apple)
            |> List.length
            |> toFloat

    playerHasSomeApples : Condition World
    playerHasSomeApples = numeric (between 3 10) getNumApples

-}
numeric : Bounds -> (world -> Float) -> Condition world
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
    = Rule
        { interaction : InteractionMatcher id entity
        , scene : Maybe scene
        , conditions : List (Condition world)
        , onMatch : id -> world -> world
        }


{-| A function to make a `Rule`.
-}
rule :
    { interaction : InteractionMatcher id entity
    , scene : Maybe scene
    , conditions : List (Condition world)
    , onMatch : id -> world -> world
    }
    -> Rule id entity world scene
rule =
    Rule


valuateRule : id -> Maybe scene -> world -> Rule id entity world scene -> ( Float, Float, Float, Float )
valuateRule id maybeCurrentScene world (Rule rule) =
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
                rule.conditions
    in
        ( sceneValue, interactionValue, conditionValue, numericExtension )


filterMatchingRules : id -> Maybe scene -> world -> Rules id entity world scene -> Maybe entity -> Rules id entity world scene
filterMatchingRules id maybeCurrentScene world rules maybeEntity =
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

                                Numeric bounds toNum ->
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
        Rules (List.filter (interactionFilter maybeEntity <&> sceneFilter <&> conditionFilter) rs)


{-| Given an interaction, run a set of rules over the world, producing a new world.
-}
runRules : (id -> world -> Maybe entity) -> id -> Maybe scene -> world -> Rules id entity world scene -> world
runRules getEntity id maybeCurrentScene world rules =
    let
        maybeEntity =
            getEntity id world

        getBestRuleUpdate (Rules rules) =
            List.map (\rule -> ( valuateRule id maybeCurrentScene world rule, rule )) rules
                |> List.sortBy Tuple.first
                |> List.reverse
                |> List.head
                |> Maybe.map (Tuple.second >> (\(Rule rule) -> rule.onMatch))
    in
        filterMatchingRules id maybeCurrentScene world rules maybeEntity
            |> getBestRuleUpdate
            |> Maybe.withDefault (always identity)
            |> (\matchedRule -> matchedRule id world)


(<&>) : (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&>) p1 p2 =
    \a ->
        p1 a && p2 a


infixl 3 <&>
