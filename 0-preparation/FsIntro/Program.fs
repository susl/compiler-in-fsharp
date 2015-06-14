// The "let" keyword defines an (immutable) value
let myInt = 5
let myFloat = 3.14
let myString = "hello"   //note that no types needed

// ======== Lists ============
let oneToFive = [1;2;3;4;5]    
List.map ((+) 2) oneToFive // [3;4;5;6;7] 
List.reduce (+) oneToFive // 15

// ======== Functions ========
let square x = x * x 
square 3
// In F# there is no "return" keyword. A function always
// returns the value of the last expression used.

let add x y = 
    x + y         // F# is indentation-based language (like Python and Haskell)
add 2 3

let add1 = add 1
add1 10

square 3
3 |> square
3 |> square |> add 10

List.sum ( List.map square [1..100] ) // sumOfSquaresTo100

[1..100] |> List.map square |> List.sum 
[1..100] |> List.map (fun x-> x * x) |> List.sum


// ========= Complex Data Types =========

//tuples are quick 'n easy anonymous types
let twoTuple = (1, 2)
let threeTuple = ("a", 2, true)

//record types have named fields
type Person = { First:string; Last:string }
let person1 = { First="john"; Last="Doe" }

//union types have choices
type Temp = 
    | DegreesC of string
    | DegreesF of float
let temp = DegreesF 98.6

// ======== Pattern Matching ========

// Match..with.. is a supercharged case/switch statement.
let printTemp temp =
   match temp with
    | DegreesF f -> printfn "%f degrees F" f
    | DegreesC c -> printfn "%s degrees C" c

let printTemp' = function
    | DegreesF f -> printfn "%f degrees F" f
    | DegreesC c -> printfn "%s degrees C" c






  



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
