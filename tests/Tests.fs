module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open System
open rbdict

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
        | None -> Assert.True(false, "minValue should return Some")

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

    [<Property>]
    let ``monoid identity: empty combine dict = dict`` (keys: string list) (values: int list) =
        let uniqueKeys = keys |> List.distinct

        let pairs =
            List.zip
                uniqueKeys
                (values @ [ for i in 1 .. List.length uniqueKeys -> 0 ]
                 |> List.take (List.length uniqueKeys))

        let dict = RBDict.ofList pairs
        let leftIdentity = RBDict.combine RBDict.empty dict
        let rightIdentity = RBDict.combine dict RBDict.empty

        RBDict.equals dict leftIdentity && RBDict.equals dict rightIdentity

    [<Property>]
    let ``monoid associativity: (a combine b) combine c = a combine (b combine c)``
        (keys1: string list, values1: int list)
        (keys2: string list, values2: int list)
        (keys3: string list, values3: int list)
        =

        let createDict keys values =
            let uniqueKeys = keys |> List.distinct

            let pairs =
                List.zip
                    uniqueKeys
                    (values @ [ for i in 1 .. List.length uniqueKeys -> 0 ]
                     |> List.take (List.length uniqueKeys))

            RBDict.ofList pairs

        let dict1 = createDict keys1 values1
        let dict2 = createDict keys2 values2
        let dict3 = createDict keys3 values3

        let left = RBDict.combine (RBDict.combine dict1 dict2) dict3
        let right = RBDict.combine dict1 (RBDict.combine dict2 dict3)

        RBDict.equals left right

    [<Property>]
    let ``insert then find returns the value`` (key: string) (value: int) =
        let dict = RBDict.empty |> RBDict.add key value
        RBDict.tryFind key dict = Some value

    [<Property>]
    let ``insert then remove then not find`` (key: string) (value: int) =
        let dict = RBDict.empty |> RBDict.add key value |> RBDict.remove key
        RBDict.tryFind key dict = None

    [<Property>]
    let ``count after insert is increased`` (key: string) (value: int) =
        let dict = RBDict.empty |> RBDict.add key value
        RBDict.count dict = 1

    [<Property>]
    let ``minKey returns the smallest key`` (keys: string list) (values: int list) =
        let uniqueKeys = keys |> List.distinct

        let pairs =
            List.zip
                uniqueKeys
                (values @ [ for i in 1 .. List.length uniqueKeys -> 0 ]
                 |> List.take (List.length uniqueKeys))

        not (List.isEmpty pairs)
        ==> lazy
            (let dict = RBDict.ofList pairs
             let minKey = pairs |> List.minBy fst |> fst

             match RBDict.minKey dict with
             | Some(k, _) -> k = minKey
             | None -> false)

    [<Property>]
    let ``maxKey returns the largest key`` (keys: string list) (values: int list) =
        let uniqueKeys = keys |> List.distinct

        let pairs =
            List.zip
                uniqueKeys
                (values @ [ for i in 1 .. List.length uniqueKeys -> 0 ]
                 |> List.take (List.length uniqueKeys))

        not (List.isEmpty pairs)
        ==> lazy
            (let dict = RBDict.ofList pairs
             let maxKey = pairs |> List.maxBy fst |> fst

             match RBDict.maxKey dict with
             | Some(k, _) -> k = maxKey
             | None -> false)

module TreePropertiesTests =

    [<Fact>]
    let ``empty tree should have valid properties`` () =
        let dict = RBDict.empty
        Assert.True(RBDict.checkTreeProperties dict)

    [<Fact>]
    let ``tree with one element should have valid properties`` () =
        let dict = RBDict.empty |> RBDict.add "key" 42
        Assert.True(RBDict.checkTreeProperties dict)

    [<Fact>]
    let ``tree with multiple elements should have valid properties`` () =
        let dict =
            RBDict.empty
            |> RBDict.add "m" 50
            |> RBDict.add "d" 30
            |> RBDict.add "s" 70
            |> RBDict.add "a" 10
            |> RBDict.add "g" 35

        Assert.True(RBDict.checkTreeProperties dict)

module ConsistencyTests =

    [<Fact>]
    let ``fold and foldBack should give same sum for commutative operations`` () =
        let dict = RBDict.empty |> RBDict.add "a" 1 |> RBDict.add "b" 2 |> RBDict.add "c" 3

        let sumLeft = RBDict.fold (fun acc k v -> acc + v) 0 dict
        let sumRight = RBDict.foldBack (fun k v acc -> acc + v) dict 0

        Assert.Equal(sumLeft, sumRight)

    [<Fact>]
    let ``toList and ofList should be isomorphic`` () =
        let original = [ ("a", 1); ("b", 2); ("c", 3) ]
        let dict = RBDict.ofList original
        let backToList = RBDict.toList dict

        let sortedOriginal = original |> List.sortBy fst
        let sortedResult = backToList |> List.sortBy fst

        Assert.True((sortedOriginal = sortedResult))

