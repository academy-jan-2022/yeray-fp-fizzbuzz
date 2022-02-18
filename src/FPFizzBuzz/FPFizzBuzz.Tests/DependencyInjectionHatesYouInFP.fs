module DependencyInjectionHatesYouInFP

module Settings =
  type MySettings = { DatabaseConnectionString: string }

  let getSettings (settingsFileName: string) =
    { DatabaseConnectionString = "banana" }

module DatabaseAccess =
  let runQuery (settingsGetter: unit -> Settings.MySettings) (query: string) =
    0

module UserRepository =
  type User = { ID: int }

  let findUser (queryRunner: string -> int) (email: string) =
    let resultingId = queryRunner $"SELECT ID FROM Users WHERE Email = '{email}'"
    { ID = resultingId }

let user =
  UserRepository.findUser
    (DatabaseAccess.runQuery (fun () -> Settings.getSettings "settings.json"))
    "user@email.com"

module ProductionCompositionalRoot =
  module Settings =
    let getSettings () =
      Settings.getSettings "settings.json"

  module DatabaseAccess =
    let runQuery query =
      DatabaseAccess.runQuery Settings.getSettings query

  module UserRepository =
    let findUser email =
      UserRepository.findUser DatabaseAccess.runQuery email


#if ISTESTING
module CompositionalRoot = TestCompositionalRoot
#else
module CompositionalRoot = ProductionCompositionalRoot
#endif

let user2 = CompositionalRoot.UserRepository.findUser "user@email.com"

let maybeJuan = None
let maybeJohn = Some "John"

let maybeTwoNames =
  maybeJuan
  |> Option.bind (fun juan -> maybeJohn |> Option.map (fun john -> $"{juan}{john}"))

open FsToolkit.ErrorHandling

let easyMaybeTwoNames =
  option {
    let! juan = maybeJuan
    let! john = maybeJohn
    return $"{juan}{john}"
  }

type User = { ID: int }
type UserWithSSN = { ID: int; SocialSecurityNumber: int }

type QueryRunner = string -> int

type QueryDependency<'a> = QueryRunner -> 'a



let mapFreeRunner (mapper: 'a -> 'b) (input: QueryDependency<'a>): QueryDependency<'b> =
  (fun (queryRunner: QueryRunner) ->
    queryRunner
    |> input
    |> mapper
  )

let bindFreeRunner  (binder: 'a -> QueryDependency<'b>) (input: QueryDependency<'a>): QueryDependency<'b> =
  (fun (queryRunner: QueryRunner) ->
    mapFreeRunner binder input queryRunner queryRunner
  )

let findUserFree email : QueryDependency<User> =
  (fun (queryRunner: QueryRunner) ->
    { ID = queryRunner $"SELECT ID FROM Users WHERE Email = '{email}'" }
  )

let findUserWithSSNFree (user: User) =
  (fun (queryRunner: QueryRunner) ->
    { ID = user.ID
      SocialSecurityNumber = queryRunner $"SELECT ssn FROM SocialSecNums WHERE UserID = {user.ID}" }
  )



type QueryDependencyBuilder() =
  member x.Bind(comp, func) = bindFreeRunner func comp
  member x.Return(a) = (fun (_: QueryRunner) -> a)
  member x.ReturnFrom(comp) = comp

type AsyncQueryDep<'a> = Async<QueryDependency<'a>>

type AsyncQueryDepBuilder() =
  member x.Bind(comp: AsyncQueryDep<'a>, func) =
    async {
      let! queryDep = comp
      return bindFreeRunner func queryDep
    }

let queryDependency = QueryDependencyBuilder()
let freeUserWithSSn: QueryDependency<UserWithSSN> =
  findUserFree "john@email.com"
  |> bindFreeRunner findUserWithSSNFree

let freeUserWithSSnWithCE: QueryDependency<UserWithSSN> =
  queryDependency {
    let! user = findUserFree "john@email.com"
    let! userWithSSN = findUserWithSSNFree user
    return userWithSSN
  }

let runnerMock _ = 0

let userWithSSNfromCE: UserWithSSN = freeUserWithSSnWithCE runnerMock
let userWithSSN: UserWithSSN = freeUserWithSSn runnerMock
