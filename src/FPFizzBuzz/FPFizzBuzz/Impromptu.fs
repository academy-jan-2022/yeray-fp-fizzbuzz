module Impromptu

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

let findUser (email: string): Result<User, DomainError> =
  match email with
  | "jake.peralta@brooklynpd.com" -> Ok { UserName= "Jake Peralta"; ID = 7 }
  | "charles.boyle@brooklynpd.com" -> Ok { UserName= "Charles Boyle"; ID = 9 }
  | "raymond.hold@brooklynpd.com" -> Error <| Unauthorized ReadUser
  | _ -> Error <| NotFound ("User", email)

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
            | 0 -> Ok { Content = "Secure report content"; ID = reportId }
            | _ -> Error <| NotFound ("Secure report", string reportId)
    with
    | err when err.Message.Contains("timeout") -> return Error <| DependencyNotResponding "Secure report service"
    | err -> return Error <| DependencyFailed ("Secure report", err.Message)
  }
