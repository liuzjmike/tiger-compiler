signature FUNCGRAPH=
sig
    type nodeID
    type 'a node
    type 'a edge = {from: nodeID, to: nodeID}
    type 'a graph

    exception NoSuchNode of nodeID
    exception NoSuchEdge of nodeID * nodeID

    val empty: 'a graph

    (* Adds a node *)
    val addNode: 'a graph * nodeID * 'a -> 'a graph
    (* Adds a node, and returns it immediately w/ the new graph *)
    val addNode': 'a graph * nodeID * 'a -> 'a graph * 'a node
    (* Adds a node if there is not a node with the same ID in the graph *)
    val addNewNode: 'a graph * nodeID * 'a -> 'a graph

    (* Removes a node and all of its edges.
    Raises NoSuchNode if not present *)
    val removeNode: 'a graph * nodeID -> 'a graph
    (* Removes a node and all of its edges.
    Returns graph unchanged if node is not present *)
    val removeNode': 'a graph * nodeID -> 'a graph

    val remove: 'a graph * 'a node -> 'a graph

    (* Gets a particular node. Raises NoSuchNode if not found *)
    val getNode: 'a graph * nodeID -> 'a node
    val getNode': 'a graph * nodeID -> 'a node option

    (* Pulls the info out of the node *)
    val nodeInfo: 'a node -> 'a

    (* Adds an edge. Raises NoSuchNode if
    either node is not in the graph *)
    val addEdge: 'a graph * 'a edge -> 'a graph

    (* Adds a doubly linked edge to the graph *)
    val doubleEdge : 'a graph * nodeID * nodeID -> 'a graph

    (* Removes an edge. Raises NoSuchNode if the specified
    nodes do not exists, or NoSuchEdge if the edge does not exist *)
    val removeEdge: 'a graph * 'a edge -> 'a graph

    (* Removes an edge. If the edge does not
    exist, ignores it and returns the graph unchanged.
    If the nodes involved do not exists, raises NoSuchNode *)
    val removeEdge': 'a graph * 'a edge -> 'a graph

    (* Removes an edge. If the edge or nodes do not exist,
    returns the graph unchanged. *)
    val removeEdge'': 'a graph * 'a edge -> 'a graph

    (* Updates the data associated with a node, keeping the edges the same.
    Raises NoSuchNode if the node does not exist *)
    val changeNodeData: 'a graph * nodeID * 'a -> 'a graph

    (* Accessors*)

    (* Gets all the nodes (not in any particularly useful graph order) *)
    val nodes: 'a graph -> 'a node list

    (* Gets all of the successors of the node *)
    val succs:  'a node -> nodeID list
    val succs': 'a graph -> 'a node -> 'a node list
    (* Gets all of the predecessors of the node *)
    val preds:   'a node -> nodeID list
    val preds': 'a graph -> 'a node -> 'a node list
    (* Gets all of the adjacent nodes of the node *)
    val adj:  'a node -> nodeID list
    val adj': 'a graph -> 'a node -> 'a node list

    (* Gets one successor of the node *)
    val oneSucc: 'a node -> nodeID option

    (* Converts a node to a node id*)
    val getNodeID: 'a node -> nodeID

    (* Number of successors *)
    val outDegree: 'a node -> int

    (* Number of predecessors *)
    val inDegree: 'a node -> int

    (* Number of edges *)
    val degree: 'a node -> int

    (* Fold functions *)
    val foldNodes: (('a node * 'b) -> 'b) -> 'b -> 'a graph -> 'b
    val foldSuccs: ((nodeID * 'b) -> 'b) -> 'b -> 'a node -> 'b
    val foldSuccs':'a graph -> (('a node * 'b) -> 'b) -> 'b -> 'a node -> 'b
    val foldPreds: ((nodeID * 'b) -> 'b) -> 'b -> 'a node -> 'b
    val foldPreds':'a graph -> (('a node * 'b) -> 'b) -> 'b -> 'a node -> 'b

    val isAdjacent: 'a node * nodeID -> bool

    (* Debugging*)
    (* Prints the graph. Give it a function that converts any given node's data
    into a string and a boolean that indicates whether the graph is undirected.
    It will print everything out *)
    val writeGraph: TextIO.outstream -> ((nodeID * 'a) -> string) -> bool -> 'a graph  -> unit
    val printGraph: ((nodeID * 'a) -> string) -> bool -> 'a graph -> unit

end
