module Tests

open System
open FPFizzBuzz
open Xunit

[<Fact>]
let ``convert number 1 to string`` () =
  let result = FizzBuzz.convert 1
  Assert.Equal("1", result)
