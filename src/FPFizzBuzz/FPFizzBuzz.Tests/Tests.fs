module Tests

open Xunit

[<Fact>]
let ``convert number 1 to string`` () =
  let result = FizzBuzz.convert 1
  Assert.Equal("1", result)

let add a b = a + b

let addTwo = add 2

let addFour = addTwo >> addTwo

let addUgly(a, b) = a + b

let addTwoNotUglyAnymore b = addUgly(2, b)

let sixteen =
  2
  |> add 2
  |> addTwo
  |> add 2
  |> addFour
  |> addTwoNotUglyAnymore
  |> (fun n -> addUgly(2, n))

let eightAgain =
  let firstSumResult = add 2 2
  let secondSumResult = addTwo firstSumResult
  add 2 secondSumResult

let numbers = [ 1;2;3;4 ]
let isEven = fun n -> n % 2 = 0
let even =
  List.filter isEven numbers


let doubleIt = fun n -> n * 2
let evensDoubledThenAdded =
  numbers
  |> List.filter isEven
  |> List.map doubleIt
  |> List.fold (fun acc n -> acc + n) 0

let evensDoubledThenAddedV2 =
  numbers
  |> List.filter isEven
  |> List.map doubleIt
  |> List.fold add 0

type MyError =
  | NotFound of string
  | Unauthorized of string
  | InternalServerError of string
  | RequestTimeout of string

type User = { Name:string; Id:int }
type SensitiveData = { Content: string }

let findUser id =
  match id % 2 with
  | 0 -> Some { Name = "John Doe"; Id = id }
  | _ -> None

let getSensitiveData id =
  match id % 3 with
  | 0 -> Ok { Content = "some secret stuff" }
  | 1 -> Error <| NotFound $"Secret with id {id}"
  | 2 -> Error <| RequestTimeout $"Timed out trying to get secret with id {id}"
  | _ -> Error <| Unauthorized "Tried to get some secret stuff"

let elevate (getter: int -> User option) id =
  match getter id with
  | Some user -> Ok user
  | None -> Error <| NotFound $"User with id %i{id}"

let elevatedFindUser =
  elevate findUser

let someEffectulFailingthing id =
  async {
    return
      match id % 2 with
      | 0 -> Ok ()
      | _ -> Error <| InternalServerError "Effectful thing failed"
  }


open FsToolkit.ErrorHandling
let finalResult =
  asyncResult {
    let! userA = elevatedFindUser 2
    let! userB = elevatedFindUser 4
    let! userC = elevatedFindUser 6
    let! userD = elevatedFindUser 8
    let! userE = elevatedFindUser 10
    let! userF = elevatedFindUser 12 //64
    let! sensitiveDataA = getSensitiveData 3
    let! sensitiveDataB = getSensitiveData 6
    let! sensitiveDataC = getSensitiveData 8
    let! sensitiveDataD = getSensitiveData 12
    let! sensitiveDataE = getSensitiveData 15
    let! sensitiveDataF = getSensitiveData 18
    let! sensitiveDataG = getSensitiveData 21 // 16.384
    let! child = someEffectulFailingthing 3 |> Async.StartChild
    do! someEffectulFailingthing 2 // 4
    do! child
    return $"All of your sensitive data {sensitiveDataA.Content} {sensitiveDataB.Content}"
  }

[<Fact>]
let ``should be internal server error`` () =
  match finalResult |> Async.RunSynchronously with
  | Ok _ -> false
  | Error err ->
      match err with
      | NotFound _ -> false
      | Unauthorized _ -> false
      | InternalServerError _ -> false
      | RequestTimeout _ -> true
  |> Assert.True
