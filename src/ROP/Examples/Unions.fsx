type IntOrBool =
    | I of int
    | B of bool

let input = B true

match input with 
| B _ -> "It's a bool"
| I _ -> "It's an integer"