module Impromptu

open Microsoft.FSharp.Core

type Permission =
  | ReadUser
  | ReadReport
  | ReadSecureReport

type DomainError =
  | Unauthorized of permissionMissing: Permission
  | NotFound of entityName: string * searchParameter: string
  | DependencyFailed of dependencyName: string * errorMessage: string
  | DependencyNotResponding of depencendyNamne: string

type User = { UserName: string; ID: int }
type Report = { Content: string; ID: int }

let findUser (email: string): User option =
  match email with
  | "jake.peralta@brooklynpd.com" -> Some { UserName= "Jake Peralta"; ID = 7 }
  | "charles.boyle@brooklynpd.com" -> Some { UserName= "Charles Boyle"; ID = 9 }
  | _ -> None

let findReport (userId: int) (reportId: int) =
  async {
    try
      return
        match userId with
        | 7 -> Error <| Unauthorized ReadReport
        | _ ->
            match reportId % 2 with
            | 0 -> Ok { Content = "Report content"; ID = reportId }
            | _ -> Error <| NotFound ("Report", string reportId)
    with
    | err when err.Message.Contains("timeout") -> return Error <| DependencyNotResponding "Report service"
    | err -> return Error <| DependencyFailed ("Report service", err.Message)
  }

let findSecureReport (userId: int) (reportId: int) =
  async {
    try
      return
        match userId with
        | 9 -> Error <| Unauthorized ReadSecureReport
        | _ ->
            match reportId % 2 with
            | 1 -> Ok { Content = "Secure report content"; ID = reportId }
            | _ -> Error <| NotFound ("Secure report", string reportId)
    with
    | err when err.Message.Contains("timeout") -> return Error <| DependencyNotResponding "Secure report service"
    | err -> return Error <| DependencyFailed ("Secure report", err.Message)
  }

open FsToolkit.ErrorHandling

let elevate (maybeUser: User option) : Result<User, DomainError> =
  match maybeUser with
  | Some user -> Ok user
  | None -> Error <| NotFound ("User not found", "")
  
type WebResponse =
  { Code: int
    Content: string }

let mapError =
  function
  | NotFound(entityName, searchParameter) ->
    { Code = 404; Content = $"Could not find {entityName} with {searchParameter}" }
  | DependencyFailed(dependencyName, errorMessage) ->
    { Code = 500; Content = $"Dependency {dependencyName} failed with error: {errorMessage}" }
  | DependencyNotResponding depencendyName ->
    { Code = 500; Content = $"Dependency {depencendyName}" }
  | Unauthorized permission ->
    match permission with
    | ReadReport -> { Code = 401; Content = "Can't read this report" }
    | ReadUser -> { Code = 401; Content = "Can't access this user data" }
    | ReadSecureReport -> { Code = 401; Content = "Missing permissions to read a secure report" }

let serialize a = { Code = 200; Content = a.ToString() }

let toWebResponse =
  Async.map (function | Ok v -> serialize v | Error e -> mapError e)

let reportsOrError =
  asyncResult {
    let! boyle = elevate <| findUser "charles.boyle@brooklynpd.com"
    let! peralta = elevate <| findUser "jake.peralta@brooklynpd.com"
    let! boyleFirstReport = findReport boyle.ID 2
    let! boyleSecondReport = findReport boyle.ID 4
    let! peraltaFirstReport = findReport peralta.ID 3
    let! peraltaSecondReport = findReport peralta.ID 5
    return
      [ boyleFirstReport
        boyleSecondReport
        peraltaFirstReport
        peraltaSecondReport ]
  }
  |> toWebResponse
  |> Async.RunSynchronously
