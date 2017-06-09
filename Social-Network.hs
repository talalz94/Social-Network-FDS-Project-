import Data.List
import System.IO
import Control.Monad (when)

--type Node = Int
--type Edge = (Node,Nod9e)
-- type Graph = ([Node])

-- data Node = Node { val :: Int ,
-- edges :: [Int]
-- } deriving (Show)

type Node     = (String, Int, Int) --node that stores data in form of (name, id, year)
type Weight     = Int                 --weight could show the satisfactory level of a collaboration based on the avg from the two people
type Edge       = (Node, Weight , Node)   -- weight is on a scale 1-5 (1-wont work again together, 2-bad experience, 3-neutral, 4-good, 5-would definetly work again)
type Node1 = (Int) --node that stores just ID as Int
type Edge1 = (Node1, Weight, Node1)

--helper functions to get list of edges from the file-----------------------
get_node :: Node -> Node1
get_node q@( _ , a , _ ) = (a)

get_edge :: Edge -> Edge1
get_edge p@(x , y , z) = ((get_node x) , y , (get_node z))

get_edges :: [Edge] -> [Edge1]
get_edges [x] = [(get_edge x)]
get_edges (x:xs) = [(get_edge x)] ++ (get_edges xs)

commaToSpace::[Char]->[Char]
commaToSpace "" = ""
commaToSpace word@(x:"") = [x]
commaToSpace word@(x:xs) = if x==',' then [' ']++(commaToSpace xs) else [x]++(commaToSpace xs)

gimmeWords::[Char]->[[Char]]
gimmeWords xs = words xs

edger::[[Char]]->Edge
edger w = if (length w) ==7
  then (((w!!0),(read (w!!1) :: Int),(read (w!!2)::Int)) , (read (w!!3)::Int) , ((w!!4),(read (w!!5) :: Int),(read (w!!6)::Int)  ))
  else error "edger received input list with incorrect number of arguments"

list_of_edges::[[Char]]->[Edge]
list_of_edges (x:[]) = [(edger (gimmeWords (commaToSpace x)))]
list_of_edges (x:xs) = [(edger (gimmeWords (commaToSpace x)))]++(list_of_edges xs)

--Main-----------------------------------------------------------------------------

main :: IO ()
main = do
        handle <- openFile "input1.csv" ReadMode --read file and get content from file
        contents <- hGetContents handle
        let singleedge = words contents

        let content' = list_of_edges singleedge
        let gr = get_edges content' ---the graph from the input file, this graph will be used in all the functions


        displayMenu gr --display menu
            -- list = singlewords

        hClose handle


displayMenu :: [Edge1] -> IO ()
displayMenu gr = do
        putStrLn "                                                                             "
        putStrLn "-----------------------------------------------------------------------------"

        putStrLn "             Welcome to the Social Network Analysis                          "
        putStrLn "                                                                             "
        putStrLn "                           Start Menu                                        "
        putStrLn "                                                                             "
        putStrLn " Choose a number to retrieve the corresponding attribute of the network      "
        putStrLn "                                                                             "
        putStrLn "1: Are two people friends                                                    "
        putStrLn "2: exist in the list or not                                                  "
        putStrLn "3: connections of a member                                                   "
        putStrLn "4: print all the data                                                        "
        putStrLn "5: find the mutual friends between two people                                "
        putStrLn "6: find the shortest path of a node to all the nodes in the graph            "
        putStrLn "7: To find the most influential person in data                               "
        putStrLn "8: find the Betweeness centrality of a node                                  "
        putStrLn "9: find the shortest distance between two nodes                              "
        putStrLn "10: find the most satisfied people till nth term                             "
        

        putStrLn "press E to exit                                                              "
        putStrLn "-----------------------------------------------------------------------------"


        input <- getLine
        if input == "1"
          then do

            c_isFriend gr
            displayMenu gr
            else if input == "2"
              then do
              c_exist gr
              displayMenu gr
              else if input == "3"
                then do
                  c_connections gr
                  displayMenu gr
                  else if input == "4"
                    then do 
                      c_printAll gr
                      displayMenu gr
                      else if input == "5"
                        then do
                          c_mutuals gr
                          displayMenu gr
                          else if input == "6"
                            then do
                              c_djikstras gr
                              displayMenu gr
                              else if input == "7"
                                then do
                                  c_highestBC gr
                                  displayMenu gr
                                  else if input == "8"
                                    then do
                                      c_bcMain gr
                                      displayMenu gr
                                      else if input == "9"
                                        then do
                                          c_twoNodeSD gr
                                          displayMenu gr
                                          else if input == "10"
                                            then do
                                              c_mostSatisfied gr
                                              displayMenu gr
                                          else if input /=  "e"
                                            then do
                                              putStrLn "press a valid key" 
                                              displayMenu gr
                                              else
                                                putStrLn "Thank you"  

lnode :: Edge1 -> Node1
lnode g@(a , _ , b) = a

rnode :: Edge1 -> Node1
rnode g@(a , _ , b) = b

--Checks if two given people are friends
isFriend :: Node1 -> Node1 -> [Edge1] -> Bool
isFriend _ _ [] = False
isFriend a b (x:xs)
 | edgeCompare a b x == True = True
 | otherwise = isFriend a b xs

c_isFriend :: [Edge1] -> IO ()
c_isFriend gr = do

        putStrLn "enter the ID of 1st member"
        input <- getLine
        let id1 = read input :: Node1
        putStrLn "enter the ID of 2nd member"
        input <- getLine
        let id2 = read input :: Node1
        let a_Friend = isFriend id1 id2 gr
        let output = (show id1) ++ " and " ++ (show id2) ++ " are Friends : " ++ (show a_Friend) 
        putStrLn (output)
--Helper funtion for comparing an edge
edgeCompare :: Node1 -> Node1 -> Edge1 -> Bool
edgeCompare a b (n1, _ , n2)
 | (a == n1 && b == n2) || (a == n2 && b == n1) = True
 | otherwise = False

--Checks if a given person exists in the sample space
exists :: Node1 -> [Edge1] -> Bool
exists _ [] = False
exists a g@(x:xs)
 | lnode x == a || rnode x == a = True
 | otherwise = exists a xs

c_exist :: [Edge1] -> IO ()
c_exist gr = do
        putStrLn "enter the ID of member"
        input <- getLine
        let id1 = read input :: Node1
        let aexist = exists id1 gr
        let output = (show aexist)
        putStrLn (output)
--Finds mutuals friends
mutuals n1 n2 gr = removeDuplicates (connections n1 gr ++ connections n2 gr)

c_mutuals :: [Edge1] -> IO ()
c_mutuals gr = do
        putStrLn "enter the ID of 1st member"
        input <- getLine
        let id1 = read input :: Node1
        putStrLn "enter the ID of 2nd member"
        input <- getLine
        let id2 = read input :: Node1
        let amutuals = mutuals id1 id2 gr
        let output = (show id1) ++ " and " ++ (show id2) ++ " have mutual friends : " ++ (show amutuals) 
        putStrLn (output)
--Prints the connections a person has with other individuals
connections' :: Node1 -> [Edge1] -> [Node1]
connections' _ [] = []
connections' a g@(x:xs)
 | lnode x == a = [rnode x] ++ connections' a xs
 | rnode x == a = [lnode x] ++ connections' a xs
 | otherwise = connections' a xs

--Prints the connections a person has with other individuals
connections n el = removeDuplicates (connections' n el)

c_connections :: [Edge1] -> IO ()
c_connections gr = do
        putStrLn "enter the ID of member"
        input <- getLine
        let id1 = read input :: Node1
        let aconnections = connections id1 gr
        let output = (show aconnections)
        putStrLn output
 ----group detection, strong people detection, Betweenness centrality see vid, Girvan–Newman algorithm, closeness centrality
-- most famous, most influential,
 ----Prints the info of a person



----Prints all individuals in the database
printAll :: [Edge1] -> [Node1]
printAll [] = []
printAll g@(x:xs) = [lnode x] ++ [rnode x] ++ printAll xs
 -- | (length g > 1) && (lnode x == lnode (head xs))  = printAll xs
 -- | otherwise = [lnode x] ++ printAll xs

c_printAll :: [Edge1] -> IO ()
c_printAll gr = do
        let aprintall = printAll gr
        let output = (show aprintall)
        putStrLn output


--Helper that removes duplicate elements from a list
removeDuplicates :: [Node1] -> [Node1]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- graph' = removeDuplicates(printAll gr)
-- Outputs N most Satisfied Pairs
mostSatisfied :: [Edge1] -> Int -> [Edge1]
mostSatisfied l n  = take n (quickSort'' l) 

c_mostSatisfied :: [Edge1] -> IO()
c_mostSatisfied gr = do
        putStrLn "how many top most satisfied people you want? "
        input <- getLine 
        let n = read input :: Int
        let a_mostSatisfied = mostSatisfied gr n
        let output = "the most satisfied people are (ID, satisfactory level out of 5, ID)\n" ++ (show (a_mostSatisfied))
        putStrLn (output)
--Helper : Sorts nodes according to their number of connections
quickSort'' ::  [Edge1] -> [Edge1]
quickSort'' [] = []
quickSort'' ((n,x,n2):xs) = quickSort'' [(n,y,n2) | (n,y,n2) <- xs, (n,y,n2) >= (n,x,n2)] ++ [(n,x,n2)] ++ quickSort'' [(n,y,n2) | (n,y,n2) <- xs, (n,y,n2) < (n,x,n2)]
---------------------------------------------------------------------------------
------------------------------Betweeness Centrality------------------------------

-- Outputs the Node with the Highest Betweeness Centrality in the graph.
highestBC :: [Node1] -> [Edge1] -> (Node1,Float) --most influential
highestBC nl@(x:xs) e = maximum' (bCAll nl e)

c_highestBC :: [Edge1] -> IO()
c_highestBC gr = do
        let graphh = removeDuplicates(printAll gr)
        let highBC = highestBC graphh gr
        let output = "the most influential person in this graph is (ID, Betweeness centrality) : " ++ (show highBC)
        putStrLn (output)
-- Outputs the Betweeness Centrality for all nodes in the graph.
bCAll :: [Node1] -> [Edge1] -> [(Node1,Float)]
bCAll [] _ = []
bCAll nl@(x:xs) e = [(x,bcMain x e)] ++ bCAll xs e

-- Helper for calculating the maximum Betweeness Centrality in the graph.
maximum' ::  [(Node1,Float)] -> (Node1,Float)
maximum' l = swap $ maximum $ map swap l
             where swap (x, y) = (y, x) 
-- Outputs the Betweeness Centrality of a Node in the graph.
bcMain :: Node1 -> [Edge1] -> Float
bcMain n e = (bcMainfeed (bCfilter (gs (removeDuplicates(printAll e))) n) n e) / 2.0

c_bcMain :: [Edge1] -> IO ()
c_bcMain gr = do
        putStrLn "enter the node for which you have to find the Betweeness centrality"
        input <- getLine
        let bc_node = read input :: Node1
        let bc = bcMain bc_node gr
        let output = "the Betweeness centrality of node " ++ (show bc_node) ++ " is " ++ (show bc)
        putStrLn (output)
 
-- Feeds a list of two-nodes and calculates their betweeness points.
bcMainfeed :: [(Node1,Node1)] -> Node1 -> [Edge1] -> Float
bcMainfeed [] _ _ = 0.0
bcMainfeed (x:xs) n e = twoPointSD x n e + bcMainfeed xs n e
-- Helper for shortestDistance
twoHelp :: [Node1] -> Node1 -> [Char]
twoHelp [] _ = ""
twoHelp l@(x:xs) n 
 | length l > 1 = show x ++ "->" ++ twoHelp xs n
 | otherwise = show x

-- Takes two-nodes and calculates their Shortest Path.
shortestDistance :: [Node1] -> Node1 -> [Char]
shortestDistance [] _ = ""
shortestDistance nl n = show n ++ "->" ++ twoHelp nl n

-- Helper for shortestDistance.
twoNodeSD :: Node1-> Node1 -> [Edge1] -> [Char]
twoNodeSD n1 n2 e
 | isConnected (n1,n2) e == False = []
 | otherwise = shortestDistance (twoNodeSD' (bCentrality (findNode n2 (djikstras n1 e)) (djikstras n1 e)) n1) n2

c_twoNodeSD :: [Edge1] -> IO()
c_twoNodeSD gr = do
        putStrLn "enter the first node : "
        input <- getLine
        let n1 = read input :: Node1
        putStrLn "enter the second node : "
        input <- getLine
        let n2 = read input :: Node1
        let a_twoNodeSD = twoNodeSD n1 n2 gr
        let output = "the shortest distance between " ++ (show n1) ++ " and " ++ (show n2) ++ " is " ++ (show a_twoNodeSD)
        putStrLn (output)


--Helper for twoNodeSD

twoNodeSD' :: [Node1] -> Node1 -> [Node1]
twoNodeSD' [] _ = []
twoNodeSD' nl@(x:xs) n 
 | x == n = [x]
 | otherwise = [x] ++ twoNodeSD' xs n

-- Takes two-nodes and calculates their betweeness points.
twoPointSD :: (Node1,Node1) -> Node1 -> [Edge1] -> Float
twoPointSD (n1,n2) n e
 |isConnected (n1,n2) e == False = 0.0
 |otherwise = bPoints (bCentrality (findNode n2 (djikstras n1 e)) (djikstras n1 e)) n n1

-- Checks if two nodes are connected in the djikstras list

isConnected :: (Node1,Node1) -> [Edge1] -> Bool
isConnected (n1,n2) e
 |head (findRouteList n2 (djikstras n1 e)) == -1 = False
 |otherwise = True

-- Helper function. Takes every 2 combination of elements in a list..

gs :: [Int] -> [(Int,Int)]
gs na = do
    a <- na 
    b <- na 
    [(a,b)] 

-- Filters the list from gs.
bCfilter :: [(Int,Int)] -> Int -> [(Int,Int)]
bCfilter [] _ = []
bCfilter l@(xl@(n1,n2):xs) n 
 | ((n == n1) || (n == n2) || (n1 == n2)) = bCfilter xs n
 | otherwise = [xl] ++ bCfilter xs n
 
-- Calculates Betweeness points of two nodes.

bPoints nl n n1 = (fromIntegral (numOfRoutes n (frequency nl))) / (fromIntegral (numOfRoutes n1 (frequency nl)))
                 
-- Constructs a list of Node corresponding to the shortest path.                                                                      

bCentrality :: (Node1,Int,Bool,[Node1]) -> [(Node1,Int,Bool,[Node1])] -> [Node1]
bCentrality (n,i,b,[]) dl = []
bCentrality (n,i,b,((-2):ys)) dl = []
bCentrality (n,i,b,((-1):ys)) dl = []
bCentrality (n,i,b,nl@(y:ys)) dl@((n2,i2,b2,pn2):xs) 
 | length (findRouteList y dl) > 1 = [y] ++ bCentrality (findNode y dl) dl ++ [y] ++ bCentrality (n,i,b,ys) dl
 | (length nl) > 1 = [y] ++ bCentrality (findNode y dl) dl ++ bCentrality (n,i,b,ys) dl --(findNode (head ys) dl) dl
 | otherwise = [y] ++ bCentrality (findNode y dl) dl

-- Feeds a list of Node into FindNode.

findNodeList :: [Node1] -> [(Node1,Int,Bool,[Node1])] -> [(Node1,Int,Bool,[Node1])]
findNodeList [] _ = []
findNodeList nl@(x:xs) dl = [findNode x dl] ++ findNodeList xs dl

-- Converts a Node into a appropiate form.

findNode :: Node1 -> [(Node1,Int,Bool,[Node1])] -> (Node1,Int,Bool,[Node1])
findNode n [] = (n,-1,False,[])
findNode n dl@(dn@(n2,i2,b2,pn2):xs)
 | n == n2 = dn
 | otherwise = findNode n xs  

-- Outputs a list containing nodes of shortest path.

findRouteList :: Node1 -> [(Node1,Int,Bool,[Node1])] -> [Node1]
findRouteList _ [] = []
findRouteList n dl@(dn@(n2,i2,b2,pn2):xs)
 | n == n2 = pn2
 | otherwise = findRouteList n xs

-- Calculates number of shortest routes.

numOfRoutes :: Node1 -> [(Int,Int)] -> Int 
numOfRoutes _ [] = 0
numOfRoutes n ((f,n1):xs)  
 | n1 == n = f
 | otherwise = numOfRoutes n xs 


--Helper function. Calculates the frequency of an element in a list.

frequency :: [Int] -> [(Int,Int)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))





---------------------------------------------------------------------------------
------------------------------Djikstras Algorithm--------------------------------

--Main Djikstras Function: Apllies djikstras Algorithm on a node
--Output: List containing ( All Nodes from a graph, Length of Shortest distance, is Visited?, List of nodes containing shortest path )
djikstras :: Node1 -> [Edge1] -> [(Node1,Int,Bool,[Node1])]
djikstras n e = djikstrasMain [(n,0,False,[targetNode])] (djikstras' n e) e


c_djikstras :: [Edge1] -> IO()
c_djikstras gr = do
        putStrLn "enter the node for which you have to find the shortest path"
        input <- getLine
        let a = read input :: Node1
        let a_djiksras = djikstras a gr
        let output = (show a_djiksras)
        putStrLn (output)

--Helper for djikstras
djikstras' :: Node1 -> [Edge1] -> [(Node1,Int,Bool,[Node1])] 
djikstras' n e = (djikstrasList (removeDuplicates (printAll e)) n e)

--Applies djikstras on a list of neighbours
djikstrasMain :: [(Node1,Int,Bool,[Node1])] -> [(Node1,Int,Bool,[Node1])] -> [Edge1] -> [(Node1,Int,Bool,[Node1])]
djikstrasMain [] d _= d 
djikstrasMain nl@((n,i,b,pn):xs) dl e = djikstrasMain (quickSort (getNeighboursList nl ((foldl (\acc x -> djikstrasFeeder acc x e) dl nl)) e )) (foldl (\acc x -> djikstrasFeeder acc x e) dl nl) e  -- makeVisited n dl

--Makes a node 'visited = True' 
makeVisited :: Node1 -> [(Node1,Int,Bool,[Node1])] -> [(Node1,Int,Bool,[Node1])]
makeVisited _ [] = []
makeVisited n d1@((n2,i,b,pn):xs)
 | n2 == n = [(n2,i,True,pn)] ++ makeVisited n xs
 | otherwise = [(n2,i,b,pn)] ++ makeVisited n xs

--Gets neighbours of a node
getNeighbours :: (Node1,Int,Bool,[Node1]) -> [(Node1,Int,Bool,[Node1])] -> [Edge1] -> [(Node1,Int,Bool,[Node1])]
getNeighbours _ [] _= []
getNeighbours nn@(n,i,b,pn) dl@((n2,i2,b2,pn2):xs) e
 | n `elem` (connections n2 e) && (b == False) = [(n2,i2,b2,pn2)] ++ getNeighbours nn xs e
 | otherwise = getNeighbours nn xs e

--Get Neighbours of all the Node1s in a list
getNeighboursList :: [(Node1,Int,Bool,[Node1])] -> [(Node1,Int,Bool,[Node1])] -> [Edge1] -> [(Node1,Int,Bool,[Node1])]
getNeighboursList [] _ _= []
getNeighboursList nl@((n,i,b,pn):xs) dl e = getNeighbours (n,i,b,pn) dl e++ getNeighboursList xs dl e


--Feeds the Node1s one-by-one into the djikstrasUpdater Function
djikstrasFeeder ::  [(Node1,Int,Bool,[Node1])] -> (Node1,Int,Bool,[Node1]) -> [Edge1] -> [(Node1,Int,Bool,[Node1])] --working ... does one iteration over dlist
djikstrasFeeder n2 n1@(n,i,b,pn) e = (map (djikstrasUpdater n1 e) (makeVisited n n2)) 

--Makes djikstras list
djikstrasList :: [Node1] -> Node1 -> [Edge1] -> [(Node1,Int,Bool,[Node1])]
djikstrasList [] _ _ = []
djikstrasList g@(x:xs) a e  
 | a == x = [(x,(0),False,[targetNode])] ++ djikstrasList xs a e
 | otherwise = [(x,(length (e) + 100),False,[nullNode])] ++ djikstrasList xs a e

--Updates distances of a node on the graph relative to the starting Node1
djikstrasUpdater :: (Node1,Int,Bool,[Node1]) -> [Edge1] -> (Node1,Int,Bool,[Node1])  -> (Node1,Int,Bool,[Node1]) -- takes (node and its distance from source of its neighbours) and its neigbours
djikstrasUpdater (n,i,b,pn) e (n2,i2,b2,pn2) --add (not in visited list and quickSort)
 | n2 `elem` (connections n e) && ((i + 1) < i2) && (b2 == False) =  (n2,(i+1),b2,[n])    --if n2 is in neighbours of n1 and i +1 < i2 (provided i2 is not 0) then update i2
 | n2 `elem` (connections n e) && ((i + 1) == i2) && (b2 == False) =  (n2,(i+1),b2,removeDuplicates (pn2 ++ [n]))  
 | otherwise = (n2,(i2),b2,pn2)  
 
--quickSort for Djikstras List
quickSort ::  [(Node1,Int,Bool,[Node1])] -> [(Node1,Int,Bool,[Node1])]
quickSort [] = []
quickSort ((n,x,b,pn):xs) = quickSort [(n,y,b,pn) | (n,y,b,pn) <- xs, (n,y,b,pn) <= (n,x,b,pn)] ++ [(n,x,b,pn)] ++ quickSort [(n,y,b,pn) | (n,y,b,pn) <- xs, (n,y,b,pn) > (n,x,b,pn)]

--Default distance of node 
targetNode :: Node1 
targetNode = (-2)

--Default distance of all other nodes
nullNode :: Node1
nullNode = (-1)