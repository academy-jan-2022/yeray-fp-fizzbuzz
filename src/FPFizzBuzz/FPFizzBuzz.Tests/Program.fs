module Program

open System
open System.IO
open System.Threading

let mutable attempt = 0

let writeLineToFile line =
  async {
    do! File.AppendAllTextAsync("./file.txt", line, CancellationToken.None) |> Async.AwaitTask
  }

let readAllLinesFromFile () =
  async {
    attempt <- attempt + 1
    if attempt < 5
    then failwith "boom"
    return! File.ReadAllLinesAsync("./file.txt", CancellationToken.None) |> Async.AwaitTask
  }

module Async =
  let rec retry times (x: Async<'a>) : Async<'a> =
    async {
      try
        return! x
      with
      | fail ->
        match times with
        | t when t > 0 -> return! (retry (times - 1) x)
        | _ -> return raise fail
    }


let [<EntryPoint>] main _ =
  let linesAsync =
    async {
      do! writeLineToFile $"meh{Environment.NewLine}"
      return! readAllLinesFromFile ()
    }

  let lines ()=
    linesAsync
    |> Async.retry 7
    |> Async.RunSynchronously
    |> Seq.fold (fun acc str -> $"{acc}{Environment.NewLine}{str}") ""

  printfn $"{lines ()}"
  0
