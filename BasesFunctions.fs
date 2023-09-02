module BasesFunctions
open System
open System.Numerics


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



let convertFromBigIntegerToBaseX (bignumber:bigint) destBase =
  printfn "IM HERE"
  let numDigits= bigint.Log destBase
  let startingExponent = (int)numDigits+1
  printfn "numDigits %A" startingExponent
  [for x in 1..startingExponent do yield startingExponent-x] |> Seq.iteri(
    fun i x->printfn "zonk %A %A" i x
    
    )
  ""
