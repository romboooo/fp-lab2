namespace rbdict

module Initializer =
    open System

    let private random = Random()

    let generateRandomDict count =
        let rec generate dict remaining =
            if remaining <= 0 then
                dict
            else
                let key = sprintf "key%d" (random.Next(1000, 9999))
                let value = random.Next(1, 1000)

                if RBDict.containsKey key dict then
                    generate dict remaining
                else
                    generate (RBDict.add key value dict) (remaining - 1)

        generate RBDict.empty count

    let generateSequentialDict count =
        let rec generate dict i =
            if i >= count then
                dict
            else
                let key = sprintf "key%04d" i
                let value = i * 10
                generate (RBDict.add key value dict) (i + 1)

        generate RBDict.empty 0

    let defaultInitialize () = generateRandomDict 50

    let initializeWithSize size =
        printfn "done"
        generateRandomDict size

    let initializeSequential size =
        printfn "done"
        generateSequentialDict size
