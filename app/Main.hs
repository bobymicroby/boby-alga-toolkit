import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Dot (renderFollowersGraph, renderFollowingGraph, renderGraphWithDefaultStyle, renderSuggestGraph)
import Lib (Graph (..), Request (..), User (..), handle,generate)
import System.Console.Haskeline

main :: IO ()
main = do
  putStrLn helpText
  evalStateT (runInputT defaultSettings loop) Empty

loop :: InputT (StateT (Graph User) IO) ()
loop = do
  line <- getInputLine "boby.dev > "
  case line of
    Nothing -> return ()
    Just "quit" -> return ()
    Just "q" -> return ()
    Just input -> process input >> loop

process :: [Char] -> InputT (StateT (Graph User) IO) ()
process input = do
  let args = words input
  case args of
    [] -> outputStrLn helpText
    ("help" : _) -> outputStrLn helpText
    ("suggest" : phone : count : _) ->
      do
        state <- lift get
        outputStrLn $ renderSuggestGraph (User phone) (read count) state
    ("show" : _) -> do
      state <- lift get
      outputStrLn $ renderGraphWithDefaultStyle state
    ("followers" : phone : _) -> do
      state <- lift get
      outputStrLn $ renderFollowersGraph (User phone) state
    ("following" : phone : _) -> do
      state <- lift get
      outputStrLn $ renderFollowingGraph (User phone) state
    ("add" : phone : _) -> do
      lift $ modify (handle (Add (User phone)))
    ("follow" : phone_a : phone_b : _) -> do
      lift $ modify (handle (Follow (User phone_a) (User phone_b)))
    ("random" : n : p: _) -> do
       graph <- lift $ lift $ generate (read n) (read p)
       lift $ modify $ const graph
    _ -> outputStrLn helpText

helpText =
  "\n\
  \ Bobby's Algebraic Graph Toolkit \n\n\
  \Commands:                                                                                  \n\n\
  \ random         $n $p                         Generates random graph $n number of nodes    \n\
  \                                              $p probability of nodes to connect 0.0 - 1.0 \n\
  \ add            $phone                        Adds new user.                               \n\
  \ remove         $phone                        Removes a user.                              \n\
  \ follow         $phone_a $phone_b             Make $phone_a follow $phone_b.               \n\
  \ show                                         Show the current graph in your browser.      \n\
  \ followers      $phone                        Show all followers of $phone in your browser.\n\
  \ following      $phone                        Show who the $phone follows in your browser. \n\
  \ suggest        $phone $count                 Shows up to $count `friends of friends`      \n\
  \ help                                         Shows this screen.                           \n\
  \ quit or q                                    Exit.                                        \n\
  \"
