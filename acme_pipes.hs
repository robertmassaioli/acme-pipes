-- Backtracking
-- Author: Robert Massaioli (2010, 2013)
--
-- This is currently an implementation of the acme pipes question from
-- first year.
import Data.List
import Data.Maybe

data Tunnel = Tunnel [Pipe]
              deriving(Show, Eq)

data Pipe = Pipe Int Int
            deriving(Show, Eq)

data PipeOption = PO Pipe [Pipe]

data SearchNode = Node Tunnel [Pipe]
                  deriving(Show, Eq)

findOneTunnel :: [Pipe] -> Maybe Tunnel
findOneTunnel = listToMaybe . findAllTunnels

findAllTunnels :: [Pipe] -> [Tunnel]
findAllTunnels allPipes = fmap reverseTunnel $ continueSearch (Node emptyTunnel allPipes)

continueSearch :: SearchNode -> [Tunnel]
continueSearch (Node current []) = [current]
continueSearch (Node current remainder) = concat $ fmap continueSearch nextNodes
   where
      nextNodes :: [SearchNode]
      nextNodes = catMaybes $ fmap (optionExtend current) options
      options = pipeOptions remainder

optionExtend :: Tunnel -> PipeOption -> Maybe SearchNode
optionExtend (Tunnel []) (PO option remainder) = Just $ Node (Tunnel [option]) remainder
optionExtend t@(Tunnel (head : _)) (PO option remainder) = do
   joiningPipe <- canJoin head option
   return $ Node (extendTunnel t joiningPipe) remainder

canJoin :: Pipe -> Pipe -> Maybe Pipe
canJoin (Pipe _ fb) o@(Pipe oa ob)
   | fb == oa  = Just o
   | fb == ob  = Just . reversePipe $ o
   | otherwise = Nothing

pipeOptions :: [Pipe] -> [PipeOption]
pipeOptions = go []
   where
      go accum [] = []
      go accum (p:ps) = PO p (accum ++ ps) : go (p : accum) ps

reversePipe :: Pipe -> Pipe
reversePipe (Pipe a b) = Pipe b a

reverseTunnel :: Tunnel -> Tunnel
reverseTunnel (Tunnel t) = Tunnel (reverse t)

extendTunnel :: Tunnel -> Pipe -> Tunnel
extendTunnel (Tunnel t) p = Tunnel (p : t)

emptyTunnel = Tunnel []

-- Test Objects

test1 :: Tunnel
test1 = Tunnel [Pipe 1 2, Pipe 2 3, Pipe 3 4]

test2 :: Tunnel
test2 = Tunnel [Pipe 1 2, Pipe 2 3, Pipe 10 4]

testPipes1 :: [Pipe]
testPipes1 = [Pipe 1 2, Pipe 8 9, Pipe 7 8, Pipe 6 7, Pipe 5 6, Pipe 4 5, Pipe 3 4, Pipe 2 3, Pipe 0 1]

testPipes2 :: [Pipe]
testPipes2 = [Pipe 1 2, Pipe 2 9]

node2 :: [Pipe] 
node2 = [Pipe 1 4, Pipe 1 4]

node3 :: [Pipe] 
node3 = [Pipe 0 1, Pipe 1 0, Pipe 0 1, Pipe 1 0]

printResult :: [Pipe] -> IO ()
printResult pipes = case findOneTunnel pipes of
                    Just a -> do
                                  putStr "I found a tunnel: "
                                  print a
                    Nothing -> putStrLn "Drat, No tunnel could be generated."

toPipe :: (Int, Int) -> Pipe
toPipe (a, b) = Pipe a b

node4 :: [Pipe]
node4 = map toPipe [(1, 2), (2, 2), (2, 2), (4, 3), (1, 3)]

willNotWork = map toPipe [(1, 2), (2, 3), (4, 5)]

dadsExample = map toPipe [(13, 17), (4, 7), (12, 24), (27, 13), (24, 25), (23, 29), (1, 16), (18, 22), (100, 6), (17, 9)]

main :: IO ()
main = do
          printResult testPipes1
          printResult node2
          printResult node3
          printResult node4
          printResult willNotWork
          printResult dadsExample
