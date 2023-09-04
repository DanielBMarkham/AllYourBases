open System
open System.Text
open FSharp.Collections
open System.Numerics
#r "bin/Debug/net7.0/AllYourBases.dll"
open BasesFunctions

printfn "Hello from F# scripting"
let VERBOSE=true

// OUTER ONION
let incomingSrcNumberToTranslate=try Environment.GetCommandLineArgs()[2] with |_ ->"0"
let incomingSrcNumberBase=try Environment.GetCommandLineArgs()[3] with |_ ->"0"
let incomingTargetNumberBase=try Environment.GetCommandLineArgs()[4] with |_ ->"0"


  // call into reusable
let res=main incomingSrcNumberToTranslate incomingSrcNumberBase incomingTargetNumberBase

printfn "%A " res
