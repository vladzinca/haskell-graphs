# Haskell Graphs

## ♾️ What is it?

This is a program that implements and works with graphs, including BFS and DFS traversals and an [algebraic representation](https://eprints.ncl.ac.uk/file_store/production/239461/EF82F5FE-66E3-4F64-A1AC-A366D1961738.pdf) of oriented graphs.

It does this by using [Haskell](https://en.wikipedia.org/wiki/Haskell), a programming language that I find hard, yet very beautiful and promising, and the functional programming capabilities embedded in Haskell.

The project is split in 3 parts, `etapa1` - `etapa3`, that I have worked on for a few couple of weeks during April-May 2022, and it's one of my hardest-to-write projects to date.

## 🤖 How to run it?

1.  Clone this repository.
2.  Choose the desired part with `cd etapa1/` - `cd etapa3/`.
3.  Load the files in [GHCi](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler) by typing `stack exec ghci TestGraph.hs`.
4.  Run the automated tests using `checkAll`.
5.  Enjoy!

## 🛣️ What does it do?

The **first part** consists of two files, `StandardGraph` and `Algorithms`. The first file contains functions to create a graph, return its nodes and edges, list a node's neighbors that go in and out of it, remove a node, split a node, and merge nodes.

Inside the second file, I implemented a search function that is used further by BFS and DFS. Here, there's also a function called `countIntermediate` that checks for a path between two nodes in a graph, as well as computing the number of intermediary nodes of such a path.

The **second part** recreates the graph using an algebraic representation approach, which is more specific to functional programming principles.

This can be described by the following data type:

```haskell
data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
```

where `Empty` is the null graph, `Node x` is a graph with only one node, `Overlay` denotes the union of vertices and edges of two graphs, and `Connect` denotes the union of vertices and edges, as well the connection of all nodes in the first graph to the nodes in the second one.

The **third part** is not finished, and what it does is further develop the graph structure by using [type classes](https://en.wikibooks.org/wiki/Haskell/Classes_and_types) specific to Haskell such as `Num`, `Show` and `Eq`.

## 👀 How hard was it?

Considering the reputation Haskell has for being a very hard language to learn, not as hard as you'd expect.

![image](https://user-images.githubusercontent.com/74200913/224431815-ced1b608-3967-4755-a943-0ef21582da4f.png)

Getting the gist of it is the hardest part, as it pretty much feels like learning to code all over again, but after a few frustrating hours you'll definetely figure it out.

And Haskell is clearly worth learning: it's challenging, yet very elegant and beautiful, kind of like how math is elegant and beautiful.

I'll leave this here:

![image](https://user-images.githubusercontent.com/74200913/224431629-200aa69c-b75e-40f8-84a5-2153cd0cf91b.png)

## 🤔 Did you know?

Functional programming is, in my opinion, the future of programming. That's been said in the past, but the way functional programming is intrinsically structured makes it much easier to maintain and add features to than, say, OOP code. If only more people used it... 🤓
