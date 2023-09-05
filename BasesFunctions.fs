module BasesFunctions
open System
open System.Text
open System.Numerics
open FSharp.Collections

let VERBOSE=true
let logIt f = if VERBOSE then f else ()
let dumpIt debug = logIt (printfn debug)
let test="cat"
dumpIt "\nMODULE LOADED\n"

type NumberBase = BaseInBaseTenNotation of int | FunkyOddballBase of string
type PaddingNeeded = AddLeadingZeros | MoveDecimalAroundInNumber  | AddTrailingZeros | Nothing

let characterLookup =
  [ (0,"0");(1,"1");(2,"2");(3,"3");(4,"4");(5,"5");(6,"6");(7,"7");(8,"8")
    ;(9,"9");(10,"a");(11,"b");(12,"c");(13,"d");(14,"e");(15,"f") ]

let convertValueToDigit (srcNumber:int) (destBase:NumberBase) = 
  let isNumberUnderTen=srcNumber<10
  if isNumberUnderTen then dumpIt "number under ten" else ()
  match isNumberUnderTen
    with 
      | true->(char)(srcNumber-1 + (int '1'))
      | false->
        let partUnderTen = srcNumber-10
        (char)((int 'a')+partUnderTen)

let convertDigitToValue (srcDigit:char) (srcBase:NumberBase) = 
  let isNormalDigitPresent=System.Text.RegularExpressions.Regex.Match(string srcDigit, "\d").Length>0
  // if isNormalDigitPresent 
  //   then dumpIt "normal digit present" 
  //   else dumpIt "no normal digit present"
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

let isScientificNotation numberString = System.Text.RegularExpressions.Regex.Match(numberString, "[e|E][\+\-]").Length=2
let splitByScientificNotation numberString = System.Text.RegularExpressions.Regex.Split(numberString, "[e|E][\+\-]")

let convertFromBigIntegerToBaseX (bignumber:bigint) (destBase:int) =
  dumpIt $"bignumber {bignumber} destBase {destBase}"
  let numDigits= (bigint.Log bignumber)/(bigint.Log destBase)
  let startingExponent = (int)numDigits+1
  let resultBuffer=new System.Text.StringBuilder(4096)
  dumpIt $"numDigits {startingExponent}"
  [for x in 1..startingExponent do yield startingExponent-x] 
    |> Seq.fold(
      fun (acc:bigint*System.Text.StringBuilder) x->
        let dec=(fst acc)/((bigint destBase)**x)
        printf "current digit index %A power it represents %A number of times it fits %A  --  " x ((bigint destBase)**x) dec
        let remainder = (fst acc)%((bigint destBase)**x)
        resultBuffer.Append (string dec ) |> ignore
        (remainder,resultBuffer)
      ) (bignumber, resultBuffer)
    |>ignore
  dumpIt $"THE ANSWER IS {resultBuffer}"
  string resultBuffer

let parseCLIOptionsForNumberBase (arg:string) = // DISCRIMINATED UNION ARG HELPER NEVER FAIL
  try 
    let tryParse = System.Int32.TryParse(arg)
    if fst tryParse
      then BaseInBaseTenNotation (snd tryParse)
      else FunkyOddballBase (arg)
   with |_ ->BaseInBaseTenNotation 10

// COMMON ENTRY POINT FOR SCRIPT, MICROSERVICE, TEST, COMPOSITION, ETC
let main arg1 arg2 arg3 =
  let srcNumberToTranslate=arg1
  let srcNumberBase:NumberBase= parseCLIOptionsForNumberBase arg2
  let targetNumberBase = parseCLIOptionsForNumberBase arg3
  // OUTER ONION TEST/PRINT
  
  dumpIt $"OUTER ONION\n Number to translate: {srcNumberToTranslate} Number Base {srcNumberBase} Base to translate to base: {targetNumberBase}" 
      
  // BUSINESS CONSISTENCY - IS DATA IN FORMAT THAT MAKES SENSE TO PROBLEM
  // USE TRIES TO HANDLE ANY ERRORS AS THE BIZ DESIRES - ERRORS ARE BIZ ISSUES, NOT CODING ONES
  // IN A LARGE MICROSERVICES ENVIRONMENT, HERE IS WHERE YOU MIGHT FORK OR JOIN DATA STREAMS IN THE OUTER OS
  // ONCE WE'RE DONE HERE, SHOULD HAVE INCOMING NUMBER IN A BIG N-LENGTH STRING IN BASE 10
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

  let moveDecimal (originalString:string) moveDirection expNum =
    let moveCount=try int(expNum) with |_ ->0
    let numSplit = try originalString.Split([|'.'|]) with |_ ->[|"0";"0"|]
    let decimalLocation = try originalString.IndexOf(".") with |_-> originalString.Length
    let moveRight = if moveDirection = "+" then true else false
    dumpIt $"moveCount {moveCount} numSplit {numSplit} decimalLocation {decimalLocation} moveRight {moveRight} "

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

    dumpIt $" BIZ CORE\n WhatToDo {whatToDo} newDecimal {newDecimal} Big Honking Incoming Number {explodedNumber}"
    explodedNumber

  // BUSINESS CONSISTENCY TESTS
  dumpIt $"BIZ CONSISTENCY\n IsScientificNotaion {(isScientificNotation srcNumberToTranslate)} Main Number {mainNumber} Exponent Number  {exponentNumber} Exponent Sign{exponentSign} NumberWithDecimal {numberWithDecimal}"


  let superHugeNumberString = moveDecimal (makeSureStringHasDecimalOrAddAtEnd mainNumber) exponentSign exponentNumber

  let exponentWalkingDirection = if superHugeNumberString.[0]='.' then -1 else 1
  let inputBase = 
    match srcNumberBase with
      |BaseInBaseTenNotation(x) -> BigInteger(x)
      |FunkyOddballBase(x) -> BigInteger(10) // NOT IMPLEMENTED
  let temporarySums =
    superHugeNumberString |> Seq.mapi(fun i x-> 
      let exponentMultiplier:bigint = 
        if exponentWalkingDirection = 1 
          then bigint (  superHugeNumberString.Length-1 - i * exponentWalkingDirection)
          else bigint ((i+1)*(-1))
      let digitValueMultiplier:bigint = BigInteger.Pow(inputBase,(int)exponentMultiplier)
      //let digitValue = BigInteger.Parse(string x)
      let digitValue=convertDigitToValue x targetNumberBase |> bigint
      let runningResult = digitValueMultiplier * digitValue
      runningResult
      )
    // ODDBALL BASES NOT CODED

  dumpIt $"Running result {temporarySums}"

  let convertedToBaseTen:bigint = temporarySums |> Seq.sum
  dumpIt $"converted to base ten bigint {convertedToBaseTen}"

  let rur=
    match targetNumberBase
      with 
        | BaseInBaseTenNotation x->convertFromBigIntegerToBaseX convertedToBaseTen x
        |_->"FUNKY BASE TYPES NOT IMPLEMENTED"
    
  rur
