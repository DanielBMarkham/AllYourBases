module BasesFunctions
open System
open System.Text
open System.Numerics
open FSharp.Collections

let foobar() = printfn "\nMODULE LOADED\n"

// DEFINE THE TYPE USED FOR INCOMING PARAMETERS
type NumberBase = BaseInBaseTenNotation of int | BaseAsACustomString of string
let parseCLIOptionsForNumberBase iArgIndex = // DISCRIMINATED UNION ARG HELPER NEVER FAIL
  try 
    let arg=Environment.GetCommandLineArgs()[iArgIndex]
    let tryParse = Int32.TryParse(arg)
    if fst tryParse
      then BaseInBaseTenNotation (snd tryParse)
      else BaseAsACustomString (arg)
   with |_ ->BaseInBaseTenNotation 10

let isScientificNotation numberString = System.Text.RegularExpressions.Regex.Match(numberString, "[e|E][\+\-]").Length=2
let splitByScientificNotation numberString = System.Text.RegularExpressions.Regex.Split(numberString, "[e|E][\+\-]")

let convertNumberToBaseNString srcNumber (srcBase:NumberBase) = 
  "8"
let convertBaseNStringToNumber srcNumber (srcBase:NumberBase) = 
  "2"

let convertFromBigIntegerToBaseX (bignumber:bigint) destBase =
  printfn "IM HERE"
  let numDigits= bigint.Log destBase
  let startingExponent = (int)numDigits+1
  let resultBuffer=new System.Text.StringBuilder(4096)
  printfn "numDigits %A" startingExponent
  [for x in 1..startingExponent do yield startingExponent-x] 
    |> Seq.fold(
      fun (acc:BigInteger*StringBuilder) x->
        let dec=(fst acc)/(destBase**x)
        printfn "zonk %A %A" x dec
        let remainder = (fst acc)%(destBase**x)
        resultBuffer.Append (string dec ) |> ignore
        (remainder,resultBuffer)
      ) (bignumber, resultBuffer)
    |>ignore
  printfn "THE ANSWER IS %A" resultBuffer
  resultBuffer
