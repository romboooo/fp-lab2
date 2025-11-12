module Program

open rbdict
open System

type Command =
    | Add of string * int
    | Remove of string
    | Find of string
    | Count
    | List
    | Tree
    | Min
    | Max
    | Check
    | Clear
    | Help
    | Exit

let parseCommand (input: string) =
    let parts = input.Trim().Split(' ') |> Array.filter (fun s -> s <> "")
    match parts with
    | [| "add"; key; value |] -> 
        match System.Int32.TryParse(value) with
        | (true, num) -> Add(key, num)
        | _ -> 
            printfn "Error: Value must be a number"
            Help
    | [| "remove"; key |] -> Remove(key)
    | [| "find"; key |] -> Find(key)
    | [| "count" |] -> Count
    | [| "list" |] -> List
    | [| "tree" |] -> Tree
    | [| "min" |] -> Min
    | [| "max" |] -> Max
    | [| "check" |] -> Check
    | [| "clear" |] -> Clear
    | [| "help" |] -> Help
    | [| "exit" |] -> Exit
    | _ -> 
        printfn "Unknown command. Type 'help' for available commands."
        Help

let rec processCommand dict command =
    match command with
    | Add(key, value) ->
        let newDict = RBDict.add key value dict
        printfn "Added key '%s' with value %d" key value
        newDict
    | Remove(key) ->
        let newDict = RBDict.remove key dict
        printfn "Removed key '%s'" key
        newDict
    | Find(key) ->
        match RBDict.tryFind key dict with
        | Some value -> printfn "Key '%s' has value: %d" key value
        | None -> printfn "Key '%s' not found" key
        dict
    | Count ->
        printfn "Dictionary contains %d elements" (RBDict.count dict)
        dict
    | List ->
        let elements = RBDict.toList dict
        if List.isEmpty elements then
            printfn "Dictionary is empty"
        else
            printfn "Dictionary elements:"
            elements |> List.iter (fun (k, v) -> printfn "  %s -> %d" k v)
        dict
    | Tree ->
        RBDict.printTree dict
        dict
    | Min ->
        match RBDict.minElement dict with
        | Some (k, v) -> printfn "Minimum element: %s -> %d" k v
        | None -> printfn "Dictionary is empty"
        dict
    | Max ->
        match RBDict.maxElement dict with
        | Some (k, v) -> printfn "Maximum element: %s -> %d" k v
        | None -> printfn "Dictionary is empty"
        dict
    | Check ->
        let isValid = RBDict.checkTreeProperties dict
        printfn "Tree properties valid: %b" isValid
        dict
    | Clear ->
        printfn "Cleared dictionary"
        RBDict.empty
    | Help ->
        printfn "Available commands:"
        printfn "  add <key> <number>  - Add key-value pair (value must be number)"
        printfn "  remove <key>        - Remove key"
        printfn "  find <key>          - Find value by key"
        printfn "  count               - Count elements"
        printfn "  list                - List all elements"
        printfn "  tree                - Show tree structure"
        printfn "  min                 - Show minimum element"
        printfn "  max                 - Show maximum element"
        printfn "  check               - Check tree properties"
        printfn "  clear               - Clear dictionary"
        printfn "  help                - Show this help"
        printfn "  exit                - Exit program"
        dict
    | Exit ->
        printfn "Goodbye!"
        dict

let rec mainLoop dict =
    printf "rbdict> "
    let input = Console.ReadLine()
    
    if String.IsNullOrWhiteSpace(input) then
        mainLoop dict
    else
        let command = parseCommand input
        match command with
        | Exit -> ()
        | _ ->
            let newDict = processCommand dict command
            mainLoop newDict

[<EntryPoint>]
let main argv =
    printfn "Type 'help' for available commands"
    
    let initialDict = 
        RBDict.empty
        |> RBDict.add "m" 50
        |> RBDict.add "d" 30
        |> RBDict.add "s" 70
        |> RBDict.add "a" 10
        |> RBDict.add "g" 35
        |> RBDict.add "p" 60
        |> RBDict.add "z" 80
    
    printfn "Initial tree with 7 elements (should show red nodes):"
    // RBDict.printTree initialDict
    printfn ""
    
    mainLoop initialDict
    0