datatype node = Node of (string * (node list))

val bt = Node("a", [
  Node("b", [
    Node("d", [
      Node("h", [
      ])
    ]),
    Node("e", [
    ])
  ]),
  Node("c", [
    Node("f", [
    ]),
    Node("g", [
    ])
  ])
])

val dt = Node("a", [
  Node("b", [
    Node("c", [
      Node("d", [
      ])
    ]),
    Node("e", [
    ])
  ]),
  Node("f", [
    Node("g", [
    ]),
    Node("h", [
    ])
  ])
])

fun depthFirstR (Node(s, nodeList)) =
  foldl (fn (node, p) => (p ^ (depthFirstR node))) s nodeList 

fun depthFirstI node = let
  fun loop (stack, result) =
    case stack
      of (Node (s, nodeList) :: stackTail) => 
        loop (nodeList @ stackTail, result ^ s)
      | Nil => 
        result 
in
  loop ([node], "")
end

fun breadthFirstI node = let
  fun loop (queue, result) =
    case queue 
      of (Node (s, nodeList) :: queueTail) => 
        loop (queueTail @ nodeList, result ^ s)
      | Nil => 
        result 
in
  loop ([node], "")
end
