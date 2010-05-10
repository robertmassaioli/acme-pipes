-- Backtracking
-- Author: Robert Massaioli (2010)
--
-- This is currently an implementation of the acme pipes question from
-- first year.
import Data.List

data Tunnel = Tunnel [Pipe]
              deriving(Show, Eq)

data Pipe = Pipe Int Int
            deriving(Show, Eq)

data SearchNode = Node Tunnel [Pipe]
                  deriving(Show, Eq)

isGoal :: SearchNode -> Bool
isGoal (Node tun []) = isValid tun
isGoal _             = False

extendTunnel :: Tunnel -> Pipe -> Tunnel
extendTunnel (Tunnel ps) p = Tunnel (p : ps)

extends :: Pipe -> Pipe -> Bool
extends (Pipe _ a) (Pipe b _) = a == b

isValid :: Tunnel -> Bool
isValid (Tunnel (a:b:ps)) = extends a b && isValid (Tunnel (b:ps))
isValid (Tunnel [])       = True
isValid (Tunnel [a])      = True

backtrack :: SearchNode -> Maybe Tunnel
backtrack node@(Node tun []) = if isGoal node then Just tun else Nothing
backtrack node = selectOne $ map (backtrack) (generateSucessors node)
                  where 
                    selectOne :: [Maybe Tunnel] -> Maybe Tunnel
                    selectOne []    = Nothing
                    selectOne ((Just x):xs) = Just x
                    selectOne (_:xs) = selectOne xs

generateSucessors :: SearchNode -> [SearchNode]
generateSucessors original@(Node tun pipes) = 
  generate original pipes
  where
    generate :: SearchNode -> [Pipe] -> [SearchNode]
    generate _                       []     = []
    generate orig@(Node otun opipes) (p:ps) =
      if isValid (extendTunnel otun p) then
        (Node (extendTunnel otun p) (delete p opipes)) : generate orig ps
      else 
        generate orig ps
  
-- Test Objects

test1 :: Tunnel
test1 = Tunnel [Pipe 1 2, Pipe 2 3, Pipe 3 4]

test2 :: Tunnel
test2 = Tunnel [Pipe 1 2, Pipe 2 3, Pipe 10 4]

testPipes1 :: [Pipe]
testPipes1 = [Pipe 1 2, Pipe 8 9, Pipe 7 8, Pipe 6 7, Pipe 5 6, Pipe 4 5, Pipe 3 4, Pipe 2 3, Pipe 0 1]

testPipes2 :: [Pipe]
testPipes2 = [Pipe 1 2, Pipe 2 9]

node1 :: SearchNode
node1 = Node (Tunnel []) testPipes1

node2 :: SearchNode
node2 = Node (Tunnel []) [Pipe 1 4, Pipe 1 4]

node3 :: SearchNode
node3 = Node (Tunnel []) [Pipe 0 1, Pipe 1 0, Pipe 0 1, Pipe 1 0]

printResult :: SearchNode -> IO ()
printResult node = case backtrack node of
                    Just a -> do
                                  putStr "I found a tunnel: "
                                  print a
                    Nothing -> putStrLn "Drat, No tunnel could be generated."

main :: IO ()
main = do
          printResult node1
          printResult node2
          printResult node3
