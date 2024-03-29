module Algorithms where

import qualified Data.Set as S
import StandardGraph

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.

    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}
auxsearch :: (Ord a)
       => ([a] -> [a] -> [a])
       -> a
       -> Graph a
       -> [a]
       -> S.Set a
       -> [a]
       -> [a]
auxsearch f node graph staqueue visited result = if staqueue == []
                                then if not $ node `elem` result
                                     then auxsearch f node graph
                                        (filter (\ x -> not $
                                                    x `S.member` visited) $
                                                S.toList $
                                                    outNeighbors node graph)
                                        (S.fromList $
                                            (S.toList visited ++ [node]))
                                        (node : result)
                                     else reverse result
                                else if node `S.member` visited
                                     then auxsearch f (head staqueue) graph
                                        (tail staqueue) visited result
                                     else auxsearch f (head $
                                                        f staqueue
                                                            (S.toList $
                                                                outNeighbors
                                                                    node graph))
                                        graph
                                        (tail $
                                            f staqueue (S.toList $
                                                outNeighbors node graph))
                                        (S.fromList $
                                            (S.toList visited ++ [node]))
                                        (node : result)

search :: (Ord a)
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = auxsearch f node graph
                    (f [] (S.toList $ outNeighbors node graph)) S.empty []

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
bfs :: (Ord a) => a -> Graph a -> [a]
bfs = \ node graph -> search (\ existent neighbors -> existent ++ neighbors)
                    node graph

{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4
    [1,2,4,3]

    > dfs 4 graph4
    [4,1,2,3]
-}
dfs :: (Ord a) => a -> Graph a -> [a]
dfs = \ node graph -> search (\ existent neighbors -> neighbors ++ existent)
                    node graph

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}
countIntermediate :: (Ord a)
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph = if ((length $
                                        fst $
                                            span (\ node -> not $ node == to) $
                                                tail $
                                                    bfs from graph) ==
                                      (length $
                                        tail $
                                            bfs from graph)) ||
                                     ((length $
                                        fst $
                                            span (\ node -> not $ node == to) $
                                                tail $
                                                    dfs from graph) ==
                                      (length $
                                        tail $
                                            dfs from graph))
                                  then Nothing
                                  else Just (length $
                                                fst $
                                                    span (\ node -> not $
                                                        node == to) $
                                                            tail $
                                                               bfs from graph,
                                             length $
                                                fst $
                                                    span (\ node -> not $
                                                        node == to) $
                                                            tail $
                                                                dfs from graph)
