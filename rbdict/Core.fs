namespace rbdict

module Core =
    open Types

    let empty: RBDict<'Key, 'Value> = { Tree = Leaf }

    let isEmpty (dict: RBDict<'Key, 'Value>) =
        match dict.Tree with
        | Leaf -> true
        | _ -> false

    let rec private tryFindTree (key: 'Key) (tree: RBTree<'Key, 'Value>) =
        match tree with
        | Leaf -> None
        | Node(_, node, left, right) ->
            if key = node.Key then Some node.Value
            elif key < node.Key then tryFindTree key left
            else tryFindTree key right

    let tryFind (key: 'Key) (dict: RBDict<'Key, 'Value>) = tryFindTree key dict.Tree

    let containsKey (key: 'Key) (dict: RBDict<'Key, 'Value>) = tryFind key dict |> Option.isSome

    let private balance (tree: RBTree<'Key, 'Value>) =
        match tree with
        | Node(Black, z, Node(Red, y, Node(Red, x, a, b), c), d)
        | Node(Black, z, Node(Red, x, a, Node(Red, y, b, c)), d)
        | Node(Black, x, a, Node(Red, z, Node(Red, y, b, c), d))
        | Node(Black, x, a, Node(Red, y, b, Node(Red, z, c, d))) ->
            Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        | tree -> tree

    let rec private insertEx (node: TreeNode<'Key, 'Value>) (tree: RBTree<'Key, 'Value>) =
        match tree with
        | Leaf -> Node(Red, node, Leaf, Leaf)
        | Node(color, currentNode, left, right) ->
            if node.Key < currentNode.Key then
                balance (Node(color, currentNode, insertEx node left, right))
            elif node.Key > currentNode.Key then
                balance (Node(color, currentNode, left, insertEx node right))
            else
                Node(color, node, left, right)

    let add (key: 'Key) (value: 'Value) (dict: RBDict<'Key, 'Value>) =
        let node = { Key = key; Value = value }
        let newTree = insertEx node dict.Tree

        match newTree with
        | Node(_, node, left, right) -> { Tree = Node(Black, node, left, right) }
        | Leaf -> { Tree = Leaf }

    let rec private inOrderFold
        (folder: 'State -> 'Key -> 'Value -> 'State)
        (state: 'State)
        (tree: RBTree<'Key, 'Value>)
        =
        match tree with
        | Leaf -> state
        | Node(_, node, left, right) ->
            let stateLeft = inOrderFold folder state left
            let stateCurrent = folder stateLeft node.Key node.Value
            inOrderFold folder stateCurrent right

    let fold (folder: 'State -> 'Key -> 'Value -> 'State) (state: 'State) (dict: RBDict<'Key, 'Value>) =
        inOrderFold folder state dict.Tree

    let rec private inOrderFoldBack
        (folder: 'Key -> 'Value -> 'State -> 'State)
        (tree: RBTree<'Key, 'Value>)
        (state: 'State)
        =
        match tree with
        | Leaf -> state
        | Node(_, node, left, right) ->
            let stateRight = inOrderFoldBack folder right state
            let stateCurrent = folder node.Key node.Value stateRight
            inOrderFoldBack folder left stateCurrent

    let foldBack (folder: 'Key -> 'Value -> 'State -> 'State) (dict: RBDict<'Key, 'Value>) (state: 'State) =
        inOrderFoldBack folder dict.Tree state

    let map (mapping: 'Key -> 'Value -> 'T) (dict: RBDict<'Key, 'Value>) =
        let folder acc k v = add k (mapping k v) acc
        fold folder empty dict

    let filter (predicate: 'Key -> 'Value -> bool) (dict: RBDict<'Key, 'Value>) =
        let folder acc k v =
            if predicate k v then add k v acc else acc

        fold folder empty dict

    let toList (dict: RBDict<'Key, 'Value>) =
        fold (fun acc k v -> (k, v) :: acc) [] dict |> List.rev

    let ofList (list: ('Key * 'Value) list) =
        List.fold (fun dict (k, v) -> add k v dict) empty list

    let count (dict: RBDict<'Key, 'Value>) =
        fold (fun count _ _ -> count + 1) 0 dict
        
    let private isRed tree =
        match tree with
        | Node(Red, _, _, _) -> true
        | _ -> false

    let private makeRed tree =
        match tree with
        | Node(_, node, left, right) -> Node(Red, node, left, right)
        | Leaf -> Leaf

    let private makeBlack tree =
        match tree with
        | Node(_, node, left, right) -> Node(Black, node, left, right)
        | Leaf -> Leaf

    let rec private balanceAfterRemove (tree: RBTree<'Key, 'Value>) =
        match tree with
        | Node(Black, node, Node(Red, lNode, lLeft, lRight), right) ->
            Node(Black, node, Node(Black, lNode, lLeft, lRight), right)
        | Node(Black, node, left, Node(Red, rNode, rLeft, rRight)) ->
            Node(Black, node, left, Node(Black, rNode, rLeft, rRight))
        | Node(Black,
               node,
               Node(Black, lNode, Node(Red, llNode, llLeft, llRight), lRight),
               Node(Black, rNode, rLeft, rRight)) ->
            Node(
                Black,
                lNode,
                Node(Black, llNode, llLeft, llRight),
                Node(Black, node, lRight, Node(Black, rNode, rLeft, rRight))
            )
        | Node(Black,
               node,
               Node(Black, lNode, lLeft, Node(Red, lrNode, lrLeft, lrRight)),
               Node(Black, rNode, rLeft, rRight)) ->
            Node(
                Black,
                lrNode,
                Node(Black, lNode, lLeft, lrLeft),
                Node(Black, node, lrRight, Node(Black, rNode, rLeft, rRight))
            )
        | Node(Black,
               node,
               Node(Black, lNode, lLeft, lRight),
               Node(Black, rNode, Node(Red, rlNode, rlLeft, rlRight), rRight)) ->
            Node(
                Black,
                rlNode,
                Node(Black, node, Node(Black, lNode, lLeft, lRight), rlLeft),
                Node(Black, rNode, rlRight, rRight)
            )
        | Node(Black,
               node,
               Node(Black, lNode, lLeft, lRight),
               Node(Black, rNode, rLeft, Node(Red, rrNode, rrLeft, rrRight))) ->
            Node(
                Black,
                rNode,
                Node(Black, node, Node(Black, lNode, lLeft, lRight), rLeft),
                Node(Black, rrNode, rrLeft, rrRight)
            )
        | Node(Black, node, Node(Black, lNode, lLeft, lRight), Node(Black, rNode, rLeft, rRight)) ->
            Node(Red, node, Node(Black, lNode, lLeft, lRight), Node(Black, rNode, rLeft, rRight))
        | tree -> tree

    let rec private removeMin (tree: RBTree<'Key, 'Value>) =
        match tree with
        | Leaf -> Leaf, None
        | Node(color, node, Leaf, right) -> right, Some node
        | Node(color, node, left, right) ->
            let newLeft, minNode = removeMin left
            balanceAfterRemove (Node(color, node, newLeft, right)), minNode

    let rec private removeTree (key: 'Key) (tree: RBTree<'Key, 'Value>) =
        match tree with
        | Leaf -> Leaf
        | Node(color, node, left, right) ->
            if key < node.Key then
                let newLeft = removeTree key left
                balanceAfterRemove (Node(color, node, newLeft, right))
            elif key > node.Key then
                let newRight = removeTree key right
                balanceAfterRemove (Node(color, node, left, newRight))
            else
                match left, right with
                | Leaf, Leaf -> if color = Red then Leaf else balanceAfterRemove Leaf
                | Leaf, right -> if color = Black then makeBlack right else right
                | left, Leaf -> if color = Black then makeBlack left else left
                | left, right ->
                    let newRight, minNode = removeMin right

                    match minNode with
                    | Some minNode ->
                        let newTree = Node(color, minNode, left, newRight)

                        if color = Black then
                            balanceAfterRemove newTree
                        else
                            newTree
                    | None -> left

    let remove (key: 'Key) (dict: RBDict<'Key, 'Value>) =
        let newTree = removeTree key dict.Tree

        match newTree with
        | Node(_, node, left, right) -> { Tree = Node(Black, node, left, right) }
        | Leaf -> { Tree = Leaf }

    let equals (dict1: RBDict<'Key, 'Value>) (dict2: RBDict<'Key, 'Value>) =
        let list1 = toList dict1 |> List.sortBy fst
        let list2 = toList dict2 |> List.sortBy fst
        list1 = list2

    let minKey (dict: RBDict<'Key, 'Value>) =
        let rec findMin tree =
            match tree with
            | Leaf -> None
            | Node(_, node, Leaf, _) -> Some(node.Key, node.Value)
            | Node(_, _, left, _) -> findMin left

        findMin dict.Tree

    let maxKey (dict: RBDict<'Key, 'Value>) =
        let rec findMax tree =
            match tree with
            | Leaf -> None
            | Node(_, node, _, Leaf) -> Some(node.Key, node.Value)
            | Node(_, _, _, right) -> findMax right

        findMax dict.Tree

    let minValue (dict: RBDict<'Key, 'Value>) =
        let elements = toList dict

        if List.isEmpty elements then
            None
        else
            elements |> List.minBy snd |> Some

    let maxValue (dict: RBDict<'Key, 'Value>) =
        let elements = toList dict

        if List.isEmpty elements then
            None
        else
            elements |> List.maxBy snd |> Some

    let checkTreeProperties (dict: RBDict<'Key, 'Value>) =
        let rec checkBlackHeight tree =
            match tree with
            | Leaf -> true, 1
            | Node(color, _, left, right) ->
                let leftOK, leftBlackHeight = checkBlackHeight left
                let rightOK, rightBlackHeight = checkBlackHeight right

                let blackHeight =
                    if color = Black then
                        leftBlackHeight + 1
                    else
                        leftBlackHeight

                let heightsEqual = leftBlackHeight = rightBlackHeight
                leftOK && rightOK && heightsEqual, blackHeight

        let rec checkRedProperty tree =
            match tree with
            | Leaf -> true
            | Node(Red, _, Node(Red, _, _, _), _) -> false
            | Node(Red, _, _, Node(Red, _, _, _)) -> false
            | Node(_, _, left, right) -> checkRedProperty left && checkRedProperty right

        let rec checkRootIsBlack tree =
            match tree with
            | Leaf -> true
            | Node(Black, _, _, _) -> true
            | _ -> false

        let blackHeightOK, _ = checkBlackHeight dict.Tree
        let redPropertyOK = checkRedProperty dict.Tree
        let rootIsBlack = checkRootIsBlack dict.Tree

        blackHeightOK && redPropertyOK && rootIsBlack

    let rec private unionTrees (tree1: RBTree<'Key, 'Value>) (tree2: RBTree<'Key, 'Value>) =
        match tree1, tree2 with
        | Leaf, _ -> tree2
        | _, Leaf -> tree1
        | Node(color1, node1, left1, right1), Node(color2, node2, left2, right2) ->
            let insertNode tree node =
                insertEx node tree
                
            let rec insertAll tree target =
                match tree with
                | Leaf -> target
                | Node(_, node, left, right) ->
                    let targetWithNode = insertEx node target
                    let targetWithLeft = insertAll left targetWithNode
                    insertAll right targetWithLeft
            
            insertAll tree2 tree1

    let union (dict1: RBDict<'Key, 'Value>) (dict2: RBDict<'Key, 'Value>) =
        let newTree = unionTrees dict1.Tree dict2.Tree
        match newTree with
        | Node(_, node, left, right) -> { Tree = Node(Black, node, left, right) }
        | Leaf -> { Tree = Leaf }