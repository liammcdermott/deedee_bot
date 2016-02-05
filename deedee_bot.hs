import Data.List
import Data.Time
import Network
import System.IO
import System.Exit
import System.Random
import Control.Arrow
import Control.Monad.State
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Text.Printf
import Data.Char (toLower, isAlpha)

server = "irc.twitch.tv"
port   = 6667
chan   = "#sirrufert"
nick   = "enter your nick here"
pass   = "enter your password here"

partyChat = randomItem
  [ privmsg "Fookin'" >> privmsg "I don't even know."
  , privmsg "Fookin' I've been doing a power of sleeping right?" >> privmsg "Any time I've been wakening up, morning noon or night, I've just been like that" >> privmsg "Pfffff aye right. Back to sleep." >> privmsg "Honestly, every hour God sends man pure lying in my scratch all like a mad cat."
  , privmsg "I just want to know if were up on that death slide or no, I could be in trouble with the police." >> (liftIO $ threadDelay (10 * (10 ^ 6))) >> privmsg "Cool right, just enough's enough, just leave it."
  , privmsg "Fookin' heading to The Brew, heading to get my Giro."
  , privmsg "This silence is ridiculous, never used to be like this with the corporation chat."
  , privmsg "Fookin'" >> privmsg "I was on the bus right and out the corner of my eye I spots the scariest thing of my life." >> (liftIO $ threadDelay (5 * (10 ^ 6))) >> privmsg "Hanging out the pocket of the guy standing next to us. Orange peels. Fookin' orange peels." >> privmsg "Just didn't add up."
  , privmsg "Fookin', I was in the shops for milk, and I saw the weirdest thing I've ever seen in my life." >> privmsg "It was a toothbrush."
  , privmsg "Has everyone been subdued?"
  , privmsg "What?!"
  , privmsg "Aye, well that killed the conversation."
  ]

responses = [ ("@deedee", paranoidQuit)
            , ("dee dee", paranoidQuit)
            , ("frying pan", privmsg "Give it a wee dunt.")
            , ("chopping board", privmsg "Chopping board. Aye, chopping board's made to not give a fuck about the kitchen knife.")
            , ("denone", privmsg "But is it 'denone' or 'dennun'? I stayed awake for 24 hours straight once, trying to sort it out.")
            , ("lost crown", privmsg "I was like that http://i.imgur.com/uCAr7zS.gifv")
            , ("laughing cow", randomItem ["Seriously, Dee Dee. Imagine you saw a cow that laughed."
                                          , "Imagine you were walking down some country road one night, lost, and you stopped to see this cow wander up to the fence next to you. And when it got there, it looked you right in the eyes, and laughed."
                                          , "How funny would it be if you opened that door and there was that mad laughing cow like that: hahahahaha. For no reason science could explain, its mad face just pure hovering about like that: hahahahaha. Not funny at all. Cos I’d lose the fucking plot."
                                          , "I don’t think you come back from that kind of spectacle. Nobody does."
                                          ] >>= privmsg)
            , ("yoker born and bred", privmsg "So you've never wondered what Yoker's like? Mind blowing.")
            , ("you're not from yoker", yokerResponse 0) -- @todo: maybe mapreduce these to a generic not from yoker string.
            , ("your not from yoker", yokerResponse 0)
            , ("ur not from yoker", yokerResponse 0)
            , ("ur nay fram yoker", randomItem ["The fook you talking about man?", "I've lost the plot."] >>= privmsg )
            , ("yoker", randomNet (0 :: Int, 10 :: Int) >>= yokerResponse)
            , ("orange peel", randomItem ["Reminds of this bloke I met on the bus. He was a psycho.", "Orange peels. Fookin' orange peels. Just doesn't add up.", "Scariest thing I've ever seen in my life."] >>= privmsg)
            , ("les porter", privmsg "Here, what's the bets his name was Smith or something but he changed it to fit in.")
            , ("fookin", randomItem ["fookin'", "news"] >>= privmsg)
            , ("jonathan boakes", privmsg "Fookin' I was on the train right, and the twat sitting opposite me is like that. Mmmm Ghost Stories. http://www.adventureclassicgaming.com/images/galleries/242/242_1.jpg" >> privmsg "But I thought to myself: Dee Dee, something's not right with this picture. That bloke's had that book open 20 minutes, pure intense reading but he's not got past the first page yet.")
            , ("helloween", privmsg "Fookin' 45 45? I was like that: http://i.imgur.com/kgMvFfg.gifv")
            ]
  where
    paranoidQuit = privmsg "Oh shite they found us. Run Dee Dee!" >> privmsg "/me quits. Just wasn't worth it." >> write "QUIT" ":Just wasn't worth it." >> liftIO (exitWith ExitSuccess)
    yokerResponse r | r == 0 = privmsg "I'm not from Yoker, I've got no business being here!" >> privmsg "Gets to the bus but he wouldn't let me in. I was like that, set up, whole thing's a set up." >> privmsg "Them that were on that front bus, actors, the lot of them actors." >> privmsg "Door opens and I bolts upstairs, right under the seat. Didn't dare poke my head up for the next half hour in case they were going by in a minibus, gasping to feast on me like a shower of zombie pirates." >> privmsg "Picked a moment." >> privmsg "Quit the channel." >> write "QUIT" ":But the best day of my life." >> liftIO (exitWith ExitSuccess)
                    | r >= 1 = randomItem ["Yoker's one of these places I only know from the front of a bus. Never been there, don't know what it's like.", "Pure fabled land.", "Sounds like a pure mad egg yolk."] >>= privmsg

-- The 'Net' monad, a wrapper over IO, carrying the bot's state.
-- type Net = ReaderT Bot IO
type Net = StateT Bot IO
data Bot = Bot { socket :: Handle, lastSeen :: TVar LastSeen, lastPosted :: TVar UTCTime, rng :: TVar StdGen }
data LastSeen = LastSeen { postLastSeen :: String, timeLastSeen :: UTCTime } deriving (Show)

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = evalStateT run st

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
  h <- connectTo server (PortNumber (fromIntegral port))
  t' <- getCurrentTime
  t <- atomically (newTVar t')
  l <- atomically (newTVar $ LastSeen "" t')
  e' <- entropy
  e <- atomically (newTVar $ mkStdGen e')
  hSetBuffering h NoBuffering
  return (Bot h l t e)
    where
      notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a
      entropy = getCurrentTime >>= return . (floor . utctDayTime :: UTCTime -> Int)

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  write "PASS" pass
  write "NICK" nick
  write "USER" (nick ++ " 0 * :deedee_bot")
  write "JOIN" chan
  privmsg "Scary man scary, but the best day of my life."
  sock <- gets socket
  state <- get
  r <- liftIO $ mapConcurrently (\f -> evalStateT (f sock) state) [talk, listen]
  liftIO (putStrLn $ show r)

-- Potentially say something if the room goes quiet.
talk :: Handle -> Net ()
talk h = forever $ do
  l' <- gets lastSeen
  l <- liftIO $ atomically (readTVar l')
  now <- liftIO getCurrentTime
  lastSaid <- lastDiff
  let diff = diffUTCTime now (timeLastSeen l)
  when (diff > min && diff <= max && lastSaid > interval) (join partyChat)
  if (diff > min)
    then liftIO $ threadDelay $ diffTimeToMicroseconds (min - diff)
    else liftIO $ threadDelay $ 30 * (10 ^ 6)
  where
    -- Time after a message is posted on IRC that deedee will consider
    -- saying something himself.
    min = 1 * 60
    -- Time after someone saying something deedee will consider everyone
    -- to have left, so he does not talk to an empty room.
    max = 10 * 60
    -- How often deedee should consider saying something.
    interval = 2 * 60
    forever a = a >> forever a

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
  s <- init `fmap` liftIO (hGetLine h)
  liftIO (putStrLn s)
  st <- get
  liftIO . forkIO $ do
    evalStateT (preEval s) st
  where
    forever a = a >> forever a

preEval :: String -> Net ()
preEval x | ping x     = pong x
          | otherwise  = eval (clean x) >> setLastSeen (clean x)
  where
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping y = "PING :" `isPrefixOf` y
    pong y = write "PONG" (':' : drop 6 y)

randomItem :: [a] -> Net a
randomItem l = randomNet (0, length l - 1) >>= \i -> return $ l !! i

-- Sets the last seen record using string and current time.
setLastSeen :: String -> Net ()
setLastSeen x = gets lastSeen >>= \l -> liftIO getCurrentTime >>= \t -> liftIO $ atomically (writeTVar l $ LastSeen x t)

-- Raw show of last seen post data.
getLastSeen :: Net String
getLastSeen = do
  l' <- gets lastSeen
  l <- liftIO $ atomically (readTVar l')
  return (show l)

-- Dispatch a command
eval :: String -> Net ()
eval     "!quit"               = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval     "!kill jester"        = write "KICK" (chan ++ " smallangrycrab")
eval     "!last seen"          = getLastSeen >>= privmsg'
eval     "!last said"          = getLastPosted >>= \s -> privmsg' s
eval     "!last diff"          = let duntText = " since I last give it a wee dunt."
                                 in lastDiff >>= \t -> privmsg' ((show t) ++ duntText)
eval     "!wait test"          = privmsg "Aye, carry on this shouldn't block a thing" >> liftIO (threadDelay 20000000) >> privmsg "Amazing what they can do these days with threading and that."
-- Pseudo-random, since it's not important.
eval     "!random"             = randomNet ((0 :: Int), (100 :: Int)) >>= privmsg . show
eval     "!source"             = privmsg "My source code is available at: https://github.com/liammcdermott/deedee_bot/blob/master/deedee_bot.hs you can submit changes to my responses there."
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval x                         = otherResponse (strToLower x)

-- Get a random number and update the bot's state with the new generator.
randomNet :: (Random a) => (a, a) -> Net a
randomNet range = gets rng >>= \rt -> liftIO $ atomically (randomAtom rt range) >>= \(i, g) -> return i
  where
    randomAtom rt range = do
      r <- readTVar rt
      let (i, g) = randomR range r
      writeTVar rt g
      return (i, g)

lastDiff :: Net NominalDiffTime
lastDiff = do
  now <- liftIO getCurrentTime
  zero' <- gets lastPosted
  zero <- liftIO $ atomically (readTVar zero')
  return $ diffUTCTime now zero

otherResponse :: String -> Net ()
otherResponse x = case lookupResponse x responses of
                    Just r -> r
                    Nothing -> return ()

-- | 'lookupResponse' @key assocs@ looks up a key in an association list.
lookupResponse :: String -> [(String,b)] -> Maybe b
lookupResponse _key [] = Nothing
lookupResponse  key ((x,y):xys)
  | x `isInfixOf` key  =  Just y
  | otherwise          =  lookupResponse key xys

strToLower :: String -> String
strToLower s = [ toLower s' | s' <- s ]

getLastPosted :: Net String
getLastPosted = do
  t' <- gets lastPosted
  t <- liftIO $ atomically (readTVar t')
  return (show t)

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = privmsg' s
  >> liftIO getCurrentTime >>= \t -> gets lastPosted >>= \lp' -> liftIO $ atomically (writeTVar lp' t)

-- Send a privmsg without updating lastPosted time.
privmsg' s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message out to the server we're currently connected to.
-- Handles timing so bot does not flood the channel.
write :: String -> String -> Net ()
write s t = do
  let min = 2
  diff <- lastDiff
  if diff > min
    then write' s t
    else (liftIO $ threadDelay $ diffTimeToMicroseconds (min - diff)) >> write s t

diffTimeToMicroseconds :: NominalDiffTime -> Int
diffTimeToMicroseconds x = ceiling (x * (10 ^ 6))

-- Low level function, sends a message to the connected server.
write' :: String -> String -> Net ()
write' s t = do
    h <- gets socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t
