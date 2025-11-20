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

    [<Fact>]
    let ``containsKey should return true for existing key`` () =
        let dict = RBDict.empty |> RBDict.add "key" 42
        Assert.True(RBDict.containsKey "key" dict)

    [<Fact>]
    let ``containsKey should return false for non-existing key`` () =
        let dict = RBDict.empty |> RBDict.add "key" 42
        Assert.False(RBDict.containsKey "nonexistent" dict)

    [<Fact>]
    let ``remove should remove existing key`` () =
        let dict = RBDict.empty |> RBDict.add "key" 42 |> RBDict.remove "key"
        Assert.False(RBDict.containsKey "key" dict)

    [<Fact>]
    let ``count should return correct number of elements`` () =
        let dict = RBDict.empty |> RBDict.add "a" 1 |> RBDict.add "b" 2 |> RBDict.add "c" 3
        Assert.Equal(3, RBDict.count dict)

    [<Fact>]
    let ``toList should return all elements`` () =
        let dict = RBDict.empty |> RBDict.add "a" 1 |> RBDict.add "b" 2
        let list = RBDict.toList dict
        Assert.Equal(2, List.length list)
        Assert.Contains(("a", 1), list)
        Assert.Contains(("b", 2), list)

    [<Fact>]
    let ``map should transform values`` () =
        let dict = RBDict.empty |> RBDict.add "a" 1 |> RBDict.add "b" 2
        let mapped = RBDict.map (fun k v -> v * 2) dict
        Assert.Equal(Some 2, RBDict.tryFind "a" mapped)
        Assert.Equal(Some 4, RBDict.tryFind "b" mapped)

    [<Fact>]
    let ``filter should filter elements`` () =
        let dict = RBDict.empty |> RBDict.add "a" 1 |> RBDict.add "b" 2 |> RBDict.add "c" 3
        let filtered = RBDict.filter (fun k v -> v % 2 = 0) dict
        Assert.False(RBDict.containsKey "a" filtered)
        Assert.True(RBDict.containsKey "b" filtered)
        Assert.False(RBDict.containsKey "c" filtered)

    [<Fact>]
    let ``minKey should return minimum key`` () =
        let dict = RBDict.empty |> RBDict.add "z" 3 |> RBDict.add "a" 1 |> RBDict.add "m" 2

        match RBDict.minKey dict with
        | Some(k, v) ->
            Assert.Equal("a", k)
            Assert.Equal(1, v)
        | None -> Assert.True(false, "minKey should return Some")

    [<Fact>]
    let ``maxKey should return maximum key`` () =
        let dict = RBDict.empty |> RBDict.add "z" 3 |> RBDict.add "a" 1 |> RBDict.add "m" 2

        match RBDict.maxKey dict with
        | Some(k, v) ->
            Assert.Equal("z", k)
            Assert.Equal(3, v)
        | None -> Assert.True(false, "maxKey should return Some")

    [<Fact>]
    let ``minValue should return minimum value`` () =
        let dict = RBDict.empty |> RBDict.add "z" 3 |> RBDict.add "a" 1 |> RBDict.add "m" 2

        match RBDict.minValue dict with
        | Some(k, v) -> Assert.Equal(1, v)
        | None -> Assert.True(false, "minValue should return Some ")

    [<Fact>]
    let ``maxValue should return maximum value`` () =
        let dict = RBDict.empty |> RBDict.add "z" 3 |> RBDict.add "a" 1 |> RBDict.add "m" 2

        match RBDict.maxValue dict with
        | Some(k, v) -> Assert.Equal(3, v)
        | None -> Assert.True(false, "maxValue should return Some")

    [<Fact>]
    let ``fold should aggregate values`` () =
        let dict = RBDict.empty |> RBDict.add "a" 1 |> RBDict.add "b" 2 |> RBDict.add "c" 3
        let sum = RBDict.fold (fun acc k v -> acc + v) 0 dict
        Assert.Equal(6, sum)

    [<Fact>]
    let ``foldBack should aggregate values`` () =
        let dict = RBDict.empty |> RBDict.add "a" 1 |> RBDict.add "b" 2 |> RBDict.add "c" 3
        let sum = RBDict.foldBack (fun k v acc -> acc + v) dict 0
        Assert.Equal(6, sum)

    [<Fact>]
    let ``combine should merge dictionaries`` () =
        let dict1 = RBDict.empty |> RBDict.add "a" 1
        let dict2 = RBDict.empty |> RBDict.add "b" 2
        let combined = RBDict.combine dict1 dict2
        Assert.True(RBDict.containsKey "a" combined)
        Assert.True(RBDict.containsKey "b" combined)
        Assert.Equal(2, RBDict.count combined)

    [<Fact>]
    let ``equals should return true for equal dictionaries`` () =
        let dict1 = RBDict.empty |> RBDict.add "a" 1 |> RBDict.add "b" 2
        let dict2 = RBDict.empty |> RBDict.add "a" 1 |> RBDict.add "b" 2
        Assert.True(RBDict.equals dict1 dict2)

    [<Fact>]
    let ``equals should return false for different dictionaries`` () =
        let dict1 = RBDict.empty |> RBDict.add "a" 1
        let dict2 = RBDict.empty |> RBDict.add "b" 2
        Assert.False(RBDict.equals dict1 dict2)

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
    let ``toList and ofList are isomorphic`` (pairs: (string * int) list) =
        let dict = RBDict.ofList pairs
        let backToList = RBDict.toList dict |> List.sortBy fst

        let expected =
            pairs |> List.rev |> List.distinctBy fst |> List.rev |> List.sortBy fst

        backToList = expected

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
    [<Property>]
    let ``works with int keys`` (pairs: (int * int) list) =
        let dict = RBDict.ofList pairs
        RBDict.count dict = (pairs |> List.distinctBy fst |> List.length)

    [<Property>]
    let ``works with DateTime keys`` (pairs: (DateTime * string) list) =
        let limitedPairs = pairs |> List.truncate 100
        let dict = RBDict.ofList limitedPairs
        RBDict.count dict = (limitedPairs |> List.distinctBy fst |> List.length)

module TreePropertiesTests =
    [<Fact>]
    let ``empty tree should have valid properties`` () =
        let dict = RBDict.empty
        Assert.True(RBDict.checkTreeProperties dict)

    [<Fact>]
    let ``tree with one element should have valid properties`` () =
        let dict = RBDict.empty |> RBDict.add "key" 42
        Assert.True(RBDict.checkTreeProperties dict)

    [<Property>]
    let ``all generated trees have valid properties`` (pairs: (string * int) list) =
        let dict = RBDict.ofList pairs
        RBDict.checkTreeProperties dict

module ConsistencyTests =
    [<Property(Arbitrary = [| typeof<MyGenerators>; typeof<RBDictArbitrary> |])>]
    let ``operations are consistent with reference implementation`` (operations: list<bool * string * int>) =
        let mutable dict = RBDict.empty
        let mutable reference = Map.empty
        let mutable allConsistent = true

        for (isInsert, key, value) in operations do
            if isInsert then
                dict <- RBDict.add key value dict
                reference <- Map.add key value reference
            else
                dict <- RBDict.remove key dict
                reference <- Map.remove key reference

            let dictKeys = dict |> RBDict.toList |> List.map fst |> Set.ofList
            let refKeys = reference |> Map.toList |> List.map fst |> Set.ofList

            let consistent = dictKeys = refKeys
            allConsistent <- allConsistent && consistent

        allConsistent
