open BasesFunctions
printfn "Hello from F# microservice"

let incomingSrcNumberToTranslate=try System.Environment.GetCommandLineArgs()[1] with |_ ->"0"
let incomingSrcNumberBase=try System.Environment.GetCommandLineArgs()[2] with |_ ->"0"
let incomingTargetNumberBase=try System.Environment.GetCommandLineArgs()[3] with |_ ->"0"

let res=main incomingSrcNumberToTranslate incomingSrcNumberBase incomingTargetNumberBase

printfn "%A " res


