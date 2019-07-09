#load "../ROP.fs"

open ROP

type Request = {name: string; email: string}

// Vocabulary

// two-track function: accepts or returns Result<'TSuccess, `TError>  
// Result<`TSuccess, `TError> r -> Success
//                              -> Error

// one-track / regular function: accepts one-track input and returns one-track output
// x -> y

// switch function: accepts one-track input and returns two-track output
// x -> Success 
//   -> Error

// single function example
let validateInput input =
    if input.name = "" then Failure "Name must not be blank"
    else if input.email = "" then Failure "Email must not be blank"
    else Success input

validateInput {name = ""; email = ""}

// combine switches
let validate1 input =
    if input.name = "" then Failure "Name must not be blank"
    else Success input

let validate2 input =
    if input.email = "" then Failure "Email must not be blank"
    else Success input

let validate3 input =
    if input.name.Length > 10 then Failure "Name must be less than 10 characters"
    else Success input

let combinedValidation =
    validate1
    >> bind validate2
    >> bind validate3

combinedValidation {name = "Josh"; email = ""}

let combinedValidation2 input =
    input |> validate1
    >>= validate2
    >>= validate3

combinedValidation2 {name = "Josh"; email = ""}

let combinedValidation3 =
    validate1
    >=> validate2
    >=> validate3

combinedValidation3 {name = "Josh"; email = ""}

// one-track to switch examples
let formatEmail input =
    { input with email = input.email.Trim().ToLower() }

let emailPipeline =
    validate1
    >=> validate2
    >=> validate3
    >=> switch formatEmail

emailPipeline {name = "Josh"; email="WHY ARE WE YELLING"}

// one-track to two-track examples (w/ regular composition)
let emailPipeline2 =
    validate1
    >=> validate2
    >=> validate3
    >> map formatEmail

// dead-end functions
let doNothing input =
    ()

let emailPipeline3 =
    validate1
    >> bind validate2
    >> bind validate3
    >> map (tee doNothing)
    >> map formatEmail

emailPipeline3 {name = "Josh"; email = "WHY ARE WE YELLING"}

// exception handling
exception Error of string
let throws input =
    raise (Error("Hello World"))

let emailPipeline4 =
    validate1
    >> bind validate2
    >> bind validate3
    >> map formatEmail
    >> map (tee doNothing)
    >> bind (tryCatch (tee throws)(fun ex -> ex.Message))

emailPipeline4 {name = "Josh"; email = "WHY ARE WE YELLING"}