open System
open System.Text
open FSharp.Collections
open System.Numerics
#r "bin/Debug/net7.0/AllYourBases.dll"

let VERBOSE=true
BasesFunctions.foobar()

let characterLookup =
  [ (0,"0");(1,"1");(2,"2");(3,"3");(4,"4");(5,"5");(6,"6");(7,"7");(8,"8")
    ;(9,"9");(10,"a");(11,"b");(12,"c");(13,"d");(14,"e");(15,"f") ]
// (all of these test sections go away when done, only needed during coding, not maintenance or execution)

type NumberBase = BaseInBaseTenNotation of int | BaseAsACustomString of string
let parseCLIOptionsForNumberBase iArgIndex = // DISCRIMINATED UNION ARG HELPER NEVER FAIL
  try 
    let arg=Environment.GetCommandLineArgs()[iArgIndex]
    let tryParse = Int32.TryParse(arg)
    if fst tryParse
      then BaseInBaseTenNotation (snd tryParse)
      else BaseAsACustomString (arg)
   with |_ ->BaseInBaseTenNotation 10

// OUTER ONION
let srcNumberToTranslate=try Environment.GetCommandLineArgs()[2] with |_ ->"0"
let srcNumberBase:NumberBase= parseCLIOptionsForNumberBase 4
let targetNumberBase = parseCLIOptionsForNumberBase 4

// OUTER ONION TEST/PRINT
if VERBOSE
  then
    printfn "OUTER ONION\n Number to translate: %A Number Base %A Base to translate to base: %A" srcNumberToTranslate srcNumberBase targetNumberBase    
  else ()
  
// BUSINESS CONSISTENCY - IS DATA IN FORMAT THAT MAKES SENSE TO PROBLEM
// USE TRIES TO HANDLE ANY ERRORS AS THE BIZ DESIRES - ERRORS ARE BIZ ISSUES, NOT CODING ONES
// IN A LARGE MICROSERVICES ENVIRONMENT, HERE IS WHERE YOU MIGHT FORK OR JOIN DATA STREAMS IN THE OUTER OS
// ONCE WE'RE DONE HERE, SHOULD HAVE INCOMING NUMBER IN A BIG N-LENGTH STRING IN BASE 10
let isScientificNotation numberString = System.Text.RegularExpressions.Regex.Match(numberString, "[e|E][\+\-]").Length=2
let splitByScientificNotation numberString = System.Text.RegularExpressions.Regex.Split(numberString, "[e|E][\+\-]")
let mainNumber = try (splitByScientificNotation srcNumberToTranslate)[0] with |_ -> "0"
let exponentNumber = try (splitByScientificNotation srcNumberToTranslate)[1] with |_ -> "0"
let exponentSign = if srcNumberToTranslate.Contains("-") then "-" else "+"
let makeSureStringHasDecimalOrAddAtEnd (myString:string) = 
  match System.Text.RegularExpressions.Regex.Split(myString, "\.").Length
    with 
      | 1->(myString + ".")
      | 2->myString
      | _->"0"
let numberWithDecimal=makeSureStringHasDecimalOrAddAtEnd mainNumber

type PaddingNeeded = AddLeadingZeros | MoveDecimalAroundInNumber  | AddTrailingZeros | Nothing

let moveDecimal (originalString:string) moveDirection expNum =
  let moveCount=try int(expNum) with |_ ->0
  let numSplit = try originalString.Split([|'.'|]) with |_ ->[|"0";"0"|]
  let decimalLocation = try originalString.IndexOf(".") with |_-> originalString.Length
  let moveRight = if moveDirection = "+" then true else false
  if VERBOSE
    then printfn "moveCount %A numSplit %A decimalLocation %A moveRight %A " moveCount numSplit decimalLocation moveRight
    else ()
  let newDecimal = 
    if exponentNumber = "0" then 0
    else if moveRight then decimalLocation + moveCount else decimalLocation - moveCount
  let whatToDo =
    match newDecimal
      with
        |_ when newDecimal =0 ->(Nothing,0)
        |_ when newDecimal <0 ->(AddLeadingZeros, -1*newDecimal)
        |_ when newDecimal >numSplit[1].Length+2 -> (AddTrailingZeros, 1+newDecimal-originalString.Length)
        |_ -> (MoveDecimalAroundInNumber,newDecimal)
  let explodedNumber =
    match whatToDo
      with
        | (Nothing,_) ->numSplit[0]
        | (AddLeadingZeros,n)->"." + String('0',snd whatToDo) + numSplit[0] + numSplit[1]
        | (AddTrailingZeros,n)->numSplit[0] + numSplit[1] + String('0',snd whatToDo)
        | (MoveDecimalAroundInNumber,n)->""

  if VERBOSE
    then
      printfn " BIZ CORE\n WhatToDo %A newDecimal %A Big Honking Incoming Number %A" whatToDo newDecimal explodedNumber
    else ()
  explodedNumber

// BUSINESS CONSISTENCY TESTS
if VERBOSE
  then
    printfn "BIZ CONSISTENCY\n IsScientificNotaion %A Main Number %A Exponent Number  %A Exponent Sign %A NumberWithDecimal %A " (isScientificNotation srcNumberToTranslate) mainNumber exponentNumber  exponentSign numberWithDecimal
  else ()



let superHugeNumberString = moveDecimal (makeSureStringHasDecimalOrAddAtEnd mainNumber) exponentSign exponentNumber

let exponentWalkingDirection = if superHugeNumberString.[0]='.' then -1 else 1
let inputBase = 
  match srcNumberBase with
    |BaseInBaseTenNotation(x) -> BigInteger(x)
    |BaseAsACustomString(x) -> BigInteger(10) // NOT IMPLEMENTED
let temporarySums =
  superHugeNumberString |> Seq.mapi(fun i x-> 
    let exponentMultiplier:bigint = 
      if exponentWalkingDirection = 1 
        then bigint (  superHugeNumberString.Length-1 - i * exponentWalkingDirection)
        else bigint ((i+1)*(-1))
    let digitValueMultiplier:bigint = BigInteger.Pow(inputBase,(int)exponentMultiplier)
    let digitValue = BigInteger.Parse(string x)
    let runningResult = digitValueMultiplier * digitValue
    runningResult
    )
  // WHO KNOWS WHAT KINDS OF BASE SYSTEMS WE'LL USE

printfn "Running result %A" temporarySums

let convertedToBaseTen:bigint = temporarySums |> Seq.sum
printfn "converted to base ten bigint %A" convertedToBaseTen


9


//    if exponentWalkingDirection = 1
//      then
//        printfn "UP exponentMultiplier %A" exponentMultiplier
//      else
//        printfn "DOWN exponentMultiplier %A" ((i+1)*(-1))
//    printfn "%A" ((i+1)*(-1))
//    printfn "This digit represents %A" digitValueMultiplier
//    printfn "Digit Value %A" digitValue
//    printfn "Running Result %A" runningResult

