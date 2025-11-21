namespace rbdict

module Types =
    type Color =
        | Red
        | Black

    [<StructuralEquality; StructuralComparison>]
    type TreeNode<'Key, 'Value> when 'Key: comparison = { Key: 'Key; Value: 'Value }

    type RBTree<'Key, 'Value> when 'Key: comparison =
        | Leaf
        | Node of Color * TreeNode<'Key, 'Value> * RBTree<'Key, 'Value> * RBTree<'Key, 'Value>

    [<StructuralEquality; StructuralComparison>]
    type RBDict<'Key, 'Value> when 'Key: comparison = { Tree: RBTree<'Key, 'Value> }
