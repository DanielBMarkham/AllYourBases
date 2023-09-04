module BasesFunctions
open System
open System.Text
open System.Numerics
open FSharp.Collections

let foobar() = printfn "\nMODULE LOADED\n"

let characterLookup =
  [ (0,"0");(1,"1");(2,"2");(3,"3");(4,"4");(5,"5");(6,"6");(7,"7");(8,"8")
    ;(9,"9");(10,"a");(11,"b");(12,"c");(13,"d");(14,"e");(15,"f") ]


// DEFINE THE TYPE USED FOR INCOMING PARAMETERS
type NumberBase = BaseInBaseTenNotation of int | FunkyOddballBase of string
let isScientificNotation numberString = System.Text.RegularExpressions.Regex.Match(numberString, "[e|E][\+\-]").Length=2
let splitByScientificNotation numberString = System.Text.RegularExpressions.Regex.Split(numberString, "[e|E][\+\-]")

let convertValueToDigit (srcNumber:int) (destBase:NumberBase) = 
  let isNumberUnderTen=srcNumber<10
  if isNumberUnderTen then printfn "number under ten" else ()
  match isNumberUnderTen
    with 
      | true->(char)(srcNumber-1 + (int '1'))
      | false->
        let partUnderTen = srcNumber-10
        (char)((int 'a')+partUnderTen)

let convertDigitToValue (srcDigit:char) (srcBase:NumberBase) = 
  let isNormalDigitPresent=System.Text.RegularExpressions.Regex.Match(string srcDigit, "\d").Length>0
  if isNormalDigitPresent then printf "normal digit present" else printf "no normal digit present"
  match srcBase
    with 
      | BaseInBaseTenNotation(a)->
        if isNormalDigitPresent
          then int(string srcDigit)
          else 
            let valueChar (c:char) = (int c) - (int 'a')+10
            valueChar (System.Char.ToLower srcDigit)
      | FunkyOddballBase(a)->
        if isNormalDigitPresent
          then int(string srcDigit)
          else 
            let valueChar (c:char) = (int c) - (int 'a')+10
            valueChar (System.Char.ToLower srcDigit)

let convertFromBigIntegerToBaseX (bignumber:bigint) destBase =
  printfn "IM HERE"
  let numDigits= bigint.Log destBase
  let startingExponent = (int)numDigits+1
  let resultBuffer=new System.Text.StringBuilder(4096)
  printfn "numDigits %A" startingExponent
  [for x in 1..startingExponent do yield startingExponent-x] 
    |> Seq.fold(
      fun (acc:bigint*System.Text.StringBuilder) x->
        let dec=(fst acc)/(destBase**x)
        printfn "zonk %A %A" x dec
        let remainder = (fst acc)%(destBase**x)
        resultBuffer.Append (string dec ) |> ignore
        (remainder,resultBuffer)
      ) (bignumber, resultBuffer)
    |>ignore
  printfn "THE ANSWER IS %A" resultBuffer
  resultBuffer



let parseCLIOptionsForNumberBase iArgIndex = // DISCRIMINATED UNION ARG HELPER NEVER FAIL
  try 
    let arg=System.Environment.GetCommandLineArgs()[iArgIndex]
    let tryParse = System.Int32.TryParse(arg)
    if fst tryParse
      then BaseInBaseTenNotation (snd tryParse)
      else FunkyOddballBase (arg)
   with |_ ->BaseInBaseTenNotation 10
