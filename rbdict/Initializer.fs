namespace rbdict

module Initializer =
    open System

    let private random = Random()

    let generateRandomDict (keyGenerator: int -> 'Key) (valueGenerator: int -> 'Value) count =
        let rec generate dict remaining =
            if remaining <= 0 then
                dict
            else
                let key = keyGenerator (random.Next(1000, 999999999))
                let value = valueGenerator (random.Next(1, 100000000))

                if RBDict.containsKey key dict then
                    generate dict remaining
                else
                    generate (RBDict.add key value dict) (remaining - 1)

        generate RBDict.empty count

    let generateSequentialDict (keyGenerator: int -> 'Key) (valueGenerator: int -> 'Value) count =
        let rec generate dict i =
            if i >= count then
                dict
            else
                let key = keyGenerator i
                let value = valueGenerator (i * 10)
                generate (RBDict.add key value dict) (i + 1)

        generate RBDict.empty 0

    // Специализированные инициализаторы для string-int словарей
    let generateRandomStringIntDict count =
        generateRandomDict (fun i -> sprintf "key%d" i) (fun i -> i) count

    let generateSequentialStringIntDict count =
        generateSequentialDict (fun i -> sprintf "key%04d" i) (fun i -> i * 10) count

    let defaultInitialize () = generateRandomStringIntDict 50

    let initializeWithSize size =
        printfn "done"
        generateRandomStringIntDict size

    let initializeSequential size =
        printfn "done"
        generateSequentialStringIntDict size
