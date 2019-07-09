module ROP

type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

let bind switch input =
    match input with 
    | Success s -> switch s
    | Failure f -> Failure f

// bind piping operator
let (>>=) input switch =
    bind switch input

// bind switch
let (>=>) switch1 switch2 =
    switch1 >> (bind switch2)

let switch f x =
    f x |> Success

let map f input =
    match input with
    | Success s -> Success (f s)
    | Failure f -> Failure f

let tee f x =
    f x |> ignore
    x

let tryCatch f handler x =
    try
        Success (f x)
    with 
    | ex -> Failure (handler ex)