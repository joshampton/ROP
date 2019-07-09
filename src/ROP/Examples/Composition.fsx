let intToString x = x.ToString()

let stringToInt (x:string) =
    x |> int

let stringValue = intToString 1    
sprintf "%s" stringValue |> ignore

let intValue = stringToInt stringValue
sprintf "%i" intValue |> ignore

let thereAndBack = intToString >> stringToInt
let result = thereAndBack 1
sprintf "%i" result |> ignore
