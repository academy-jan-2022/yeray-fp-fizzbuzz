module FPFizzBuzz.Tests.FreeMonad
type Continuation<'output, 'next> = 'output -> 'next

module TerminalDsl =
    open Chessie.ErrorHandling

    type Terminal<'next> =
    | WriteLine    of string * Continuation<unit, 'next>
    | ReadLine     of unit   * Continuation<string, 'next>

    let mapTerminal (f: 'a -> 'b) (t: Terminal<'a>): Terminal<'b> =
        match t with
        | WriteLine (s, cont) -> WriteLine (s, cont >> f)
        | ReadLine (_, cont)  -> ReadLine ((), cont >> f)

module RocketDsl =
    open Chessie.ErrorHandling

    type Target = string
    type Result = string
    
    type Rocket<'next> =
    | LaunchRocket of Target * Continuation<Result, 'next>

    let mapRocket (f: 'a -> 'b) (r: Rocket<'a>): Rocket<'b> =
        match r with
        | LaunchRocket (t, cont) -> LaunchRocket (t, cont >> f)

module ProgramDsl =
    open TerminalDsl
    open RocketDsl

    type Dsl<'next> =
    | Terminal of Terminal<'next>
    | Rocket of Rocket<'next>

    let map (f: 'a -> 'b) (dsl : Dsl<'a>) : Dsl<'b> =
        match dsl with
        | Terminal t -> mapTerminal f t |> Terminal
        | Rocket r -> mapRocket f r |> Rocket

module Free =
    open ProgramDsl

    type Free<'a> =
    | Pure of 'a
    | FreeDsl of Dsl<Free<'a>>    

    [<RequireQualifiedAccess>]
    module FreeMonad =
        let rec bind (f: 'a -> Free<'b>) (dsl : Free<'a>) : Free<'b> =
            match dsl with
            | Pure value -> f value
            | FreeDsl t  -> map (bind f) t |> FreeDsl

        let liftF (dsl:Dsl<'a>) : Free<'a> =
            FreeDsl (map Pure dsl)

    type DslBuilder() =
        member x.Bind(dsl, f) = FreeMonad.bind f dsl
        member x.Return(value) = Pure value
        member x.Zero() = Pure ()
        member x.Delay(f) = f()

    let dsl = new DslBuilder()

module TerminalOperations =
    open TerminalDsl
    open ProgramDsl
    open Free

    let writeLine s: Free<unit> = FreeMonad.liftF (WriteLine (s, id) |> Terminal)
    let readLine: Free<string> = FreeMonad.liftF (ReadLine ((), id) |> Terminal)

module RocketOperations =
    open RocketDsl
    open ProgramDsl
    open Free

    let launch t : Free<Result> = FreeMonad.liftF (LaunchRocket (t, id) |> Rocket) 

module Program = 
    open Free
    open TerminalOperations
    open RocketOperations
    let rocketLauncher = dsl {
        let! _ = writeLine "Hi, what's your target?"
        let! target = readLine
        let! _ = writeLine (sprintf "Ok, the target is %s!" target)
        let! result = launch target
        do! writeLine result
        return ()
    }

module Effects =
    open Chessie.ErrorHandling

    type Error = string
    type Effect<'a> = AsyncResult<'a, Error>

    let ofResult r: Effect<'a> = r |> Async.singleton |> AR
    let singleton x: Effect<'a> = x |> ok |> ofResult

    let effects = asyncTrial
    let bind (f: 'a -> Effect<'b>) (x: Effect<'a>): Effect<'b> = 
        effects {
            let! value = x
            return! f value
        }

    let (>>=) x f = bind f x    

module Interpreters =
    open TerminalDsl
    open RocketDsl
    open Free
    open ProgramDsl
    open Effects

    let rec interpreter rIntr tIntr (free: Free<'a>): Effect<'a> =
        match free with
        | Pure value -> value |> singleton
        | FreeDsl dsl ->
            match dsl with
            | Terminal t -> tIntr t >>= interpreter rIntr tIntr
            | Rocket r -> rIntr r >>= interpreter rIntr tIntr

    let terminalInterpreter t =
        match t with
        | WriteLine (s, cont) ->
            stdout.WriteLine s |> cont |> singleton 
        | ReadLine (_, cont) ->
            stdin.ReadLine() |> cont |> singleton 

    let rocketInterpreter r =
        match r with
        | LaunchRocket (t, cont) -> 
            effects {
                do! Async.Sleep 3000
                return sprintf "%s destroyed" t |> cont
            }

open Chessie.ErrorHandling

let interpreter = Interpreters.interpreter Interpreters.rocketInterpreter Interpreters.terminalInterpreter

interpreter  Program.rocketLauncher
|> Async.ofAsyncResult 
|> Async.RunSynchronously
