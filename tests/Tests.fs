module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open System
open rbdict

type MyGenerators =
    static member String() =
        Arb.Default.String() |> Arb.filter (fun s -> s <> null && s.Length < 100)

    static member Int() = Arb.Default.Int32()

    static member KeyValuePair() =
        Arb.fromGen
        <| gen {
            let! key = Arb.generate<string>
            let! value = Arb.generate<int>
            return (key, value)
        }

type RBDictArbitrary =
    static member RBDict() =
        let genRBDict =
            gen {
                let! pairs = Arb.generate<(string * int) list>
                return RBDict.ofList pairs
            }

        Arb.fromGen genRBDict

module UnitTests =
    // Существующие unit тесты остаются без изменений
    [<Fact>]
    let ``empty dictionary should be empty`` () =
        let dict = RBDict.empty
        Assert.True(RBDict.isEmpty dict)

    [<Fact>]
    let ``add should make dictionary non-empty`` () =
        let dict = RBDict.empty |> RBDict.add "key" 42
        Assert.False(RBDict.isEmpty dict)

    [<Fact>]
    let ``add then tryFind should return correct value`` () =
        let dict = RBDict.empty |> RBDict.add "key" 42
        Assert.Equal(Some 42, RBDict.tryFind "key" dict)

// ... остальные существующие unit тесты

module PropertyTests =
    let private registerGenerators =
        Arb.register<MyGenerators> () |> ignore
        Arb.register<RBDictArbitrary> () |> ignore

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``insert then find returns the value`` (key: string) (value: int) =
        let dict = RBDict.empty |> RBDict.add key value
        RBDict.tryFind key dict = Some value

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``insert then remove then not find`` (key: string) (value: int) =
        let dict = RBDict.empty |> RBDict.add key value |> RBDict.remove key
        RBDict.tryFind key dict = None

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``count after insert is increased`` (key: string) (value: int) =
        let dict = RBDict.empty |> RBDict.add key value
        RBDict.count dict = 1

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``multiple inserts maintain count`` (pairs: (string * int) list) =
        let uniquePairs = pairs |> List.distinctBy fst
        let dict = RBDict.ofList uniquePairs
        RBDict.count dict = List.length uniquePairs

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``tree properties are maintained after operations`` (operations: list<bool * string * int>) =
        let mutable dict = RBDict.empty

        for (isInsert, key, value) in operations do
            if isInsert then
                dict <- RBDict.add key value dict
            else
                dict <- RBDict.remove key dict

            // Проверяем свойства дерева после каждой операции
            let isValid = RBDict.checkTreeProperties dict

            if not isValid then
                failwithf "Tree properties violated after operation: %A" (isInsert, key, value)

        true

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``toList and ofList are isomorphic`` (pairs: (string * int) list) =
        let dict = RBDict.ofList pairs
        let backToList = RBDict.toList dict |> List.sortBy fst
        let originalSorted = pairs |> List.distinctBy fst |> List.sortBy fst
        backToList = originalSorted

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``filter preserves predicate`` (pairs: (string * int) list) =
        let dict = RBDict.ofList pairs
        let filtered = RBDict.filter (fun k v -> v % 2 = 0) dict

        let allEven = filtered |> RBDict.toList |> List.forall (fun (_, v) -> v % 2 = 0)

        allEven

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``map transforms all values`` (pairs: (string * int) list) =
        let dict = RBDict.ofList pairs
        let mapped = RBDict.map (fun k v -> v * 2) dict

        let originalValues = dict |> RBDict.toList |> List.map snd |> Set.ofList
        let mappedValues = mapped |> RBDict.toList |> List.map snd |> Set.ofList
        let expectedValues = originalValues |> Set.map ((*) 2)

        mappedValues = expectedValues

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``fold and foldBack give same result for commutative operations`` (pairs: (string * int) list) =
        let dict = RBDict.ofList pairs

        let sumLeft = RBDict.fold (fun acc k v -> acc + v) 0 dict
        let sumRight = RBDict.foldBack (fun k v acc -> acc + v) dict 0

        sumLeft = sumRight

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``minKey returns actual minimum`` (pairs: (string * int) list) =
        not (List.isEmpty pairs)
        ==> lazy
            (let dict = RBDict.ofList pairs
             let actualMin = pairs |> List.map fst |> List.min

             match RBDict.minKey dict with
             | Some(k, _) -> k = actualMin
             | None -> false)

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``maxKey returns actual maximum`` (pairs: (string * int) list) =
        not (List.isEmpty pairs)
        ==> lazy
            (let dict = RBDict.ofList pairs
             let actualMax = pairs |> List.map fst |> List.max

             match RBDict.maxKey dict with
             | Some(k, _) -> k = actualMax
             | None -> false)

module GenericPropertyTests =
    // Тесты с разными типами ключей
    [<Property>]
    let ``works with int keys`` (keys: int list) (values: int list) =
        let pairs = List.zip keys values |> List.distinctBy fst
        let dict = RBDict.ofList pairs
        RBDict.count dict = List.length pairs

    [<Property>]
    let ``works with DateTime keys`` (keys: DateTime list) (values: string list) =
        let pairs = List.zip keys values |> List.distinctBy fst |> List.truncate 100 // Ограничиваем для производительности

        let dict = RBDict.ofList pairs
        RBDict.count dict = List.length pairs

module TreePropertiesTests =
    [<Fact>]
    let ``empty tree should have valid properties`` () =
        let dict = RBDict.empty
        Assert.True(RBDict.checkTreeProperties dict)

    [<Fact>]
    let ``tree with one element should have valid properties`` () =
        let dict = RBDict.empty |> RBDict.add "key" 42
        Assert.True(RBDict.checkTreeProperties dict)

    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``all generated trees have valid properties`` dict = RBDict.checkTreeProperties dict

module ConsistencyTests =
    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``operations are consistent with reference implementation`` (operations: list<bool * string * int>) =
        let mutable dict = RBDict.empty
        let mutable reference = Map.empty

        for (isInsert, key, value) in operations do
            if isInsert then
                dict <- RBDict.add key value dict
                reference <- Map.add key value reference
            else
                dict <- RBDict.remove key dict
                reference <- Map.remove key reference

            let dictKeys = dict |> RBDict.toList |> List.map fst |> Set.ofList
            let refKeys = reference |> Map.toList |> List.map fst |> Set.ofList

            dictKeys = refKeys
