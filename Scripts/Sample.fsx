#I __SOURCE_DIRECTORY__
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#load "../ISDParser.fs"

open ISD.Parser
open System.IO

let file = "..\sample\sample.ish"
let fullFile = Path.Combine(__SOURCE_DIRECTORY__,file)
let lines = getLinesOfFile(fullFile)
let data = lines |> Seq.map parseLine |> Seq.toList
printfn "Got %d items ..." data.Length
