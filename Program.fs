module Program

open rbdict
open System

type Command =
    | Add of string * int
    | Remove of string
    | Find of string
    | Count
    | List
    | MinKey
    | MaxKey
    | MinValue
    | MaxValue
    | FoldLeft
    | FoldRight
    | Check
    | Clear
    | Init of int
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
    | [| "minkey" |] -> MinKey
    | [| "maxkey" |] -> MaxKey
    | [| "minvalue" |] -> MinValue
    | [| "maxvalue" |] -> MaxValue
    | [| "foldleft" |] -> FoldLeft
    | [| "foldright" |] -> FoldRight
    | [| "check" |] -> Check
    | [| "clear" |] -> Clear
    | [| "init" |] -> Init 50
    | [| "init"; size |] -> 
        match System.Int32.TryParse(size) with
        | (true, num) when num > 0 -> Init num
        | _ -> 
            printfn "Error: Size must be a positive number"
            Help
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
            elements 
            |> List.iter (fun (k, v) -> printfn "  %s -> %d" k v)
        dict
    | MinKey ->
        match RBDict.minKey dict with
        | Some (k, v) -> printfn "Minimum by key: %s -> %d" k v
        | None -> printfn "Dictionary is empty"
        dict
    | MaxKey ->
        match RBDict.maxKey dict with
        | Some (k, v) -> printfn "Maximum by key: %s -> %d" k v
        | None -> printfn "Dictionary is empty"
        dict
    | MinValue ->
        match RBDict.minValue dict with
        | Some (k, v) -> printfn "Minimum by value: %s -> %d" k v
        | None -> printfn "Dictionary is empty"
        dict
    | MaxValue ->
        match RBDict.maxValue dict with
        | Some (k, v) -> printfn "Maximum by value: %s -> %d" k v
        | None -> printfn "Dictionary is empty"
        dict
    | FoldLeft ->
        let keyConcat = RBDict.fold (fun acc k _ -> acc + k + " ") "" dict
        let valueSum = RBDict.fold (fun acc _ v -> acc + v) 0 dict
        printfn "Left fold results:"
        printfn "  Key concatenation (first 100 chars): %s" (if keyConcat.Length > 100 then keyConcat.Substring(0, 100) + "..." else keyConcat)
        printfn "  Value sum: %d" valueSum
        dict
    | FoldRight ->
        let keyConcat = RBDict.foldBack (fun k _ acc -> k + " " + acc) dict ""
        let valueSum = RBDict.foldBack (fun _ v acc -> acc + v) dict 0
        printfn "Right fold results:"
        printfn "  Key concatenation (first 100 chars): %s" (if keyConcat.Length > 100 then keyConcat.Substring(0, 100) + "..." else keyConcat)
        printfn "  Value sum: %d" valueSum
        dict
    | Check ->
        let isValid = RBDict.checkTreeProperties dict
        printfn "Tree properties valid: %b" isValid
        dict
    | Clear ->
        printfn "Cleared dictionary"
        RBDict.empty
    | Init size ->
        Initializer.initializeWithSize size
    | Help ->
        printfn "Available commands:"
        printfn "  add <key> <number>  - Add key-value pair"
        printfn "  remove <key>        - Remove key"
        printfn "  find <key>          - Find value by key"
        printfn "  count               - Count elements"
        printfn "  list                - List all elements (first 10)"
        printfn "  minkey              - Show element with minimum key"
        printfn "  maxkey              - Show element with maximum key"
        printfn "  minvalue            - Show element with minimum value"
        printfn "  maxvalue            - Show element with maximum value"
        printfn "  foldleft            - Left fold (key concat + value sum)"
        printfn "  foldright           - Right fold (key concat + value sum)"
        printfn "  check               - Check tree properties"
        printfn "  clear               - Clear dictionary"
        printfn "  init [size]         - Initialize with random data (default: 100)"
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
    
    let initialDict = Initializer.defaultInitialize()
    
    mainLoop initialDict
    0