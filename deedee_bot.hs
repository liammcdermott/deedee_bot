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
import Control.Concurrent.Chan
import Text.Printf
import Data.Char (toLower, isAlpha)

server = "irc.twitch.tv"
port   = 6667
chan   = "#sirrufert"
nick   = "enter your nick here"
pass   = "enter your password here"
-- Whether to show user join/part messages. Note: Twitch has caching which
-- can make this feature appear to spam the chat.
joins  = False

partyChat = randomItem
  [ privmsg "Fookin' I don't even know."
  , privmsg "Fookin'"
  , privmsg "Do a power of sleeping right, any time you wake up just be like that, pffff aye right. Back to sleep."
  , privmsg "People think that not working makes your head go to pot, but I spot things that other people haven't got the time for."
  , privmsg "I just want to know if were up on that death slide or no, I could be in trouble with the police."
  , privmsg "Fookin' heading to The Brew, heading to get my Giro."
  , privmsg "This silence is ridiculous, never used to be like this with the corporation chats."
  , privmsg "Scariest thing of my life. Hanging out the the pocket of the guy standing next to us. Fookin' orange peels."
  , privmsg "Fookin', weirdest thing I've ever seen in my life. It was a toothbrush."
  , privmsg "Has everyone been subdued?"
  , privmsg "Aye? Well that killed the conversation."
  , privmsg "Haaaaaaaaaaaa"
  , privmsg "On comes Jeremy Kyle, 'He's not my Son He's a Ginger'. Looks like a good one, I'll set the tape."
  , privmsg "Some wean's programme's on. Bird was like that, 'We're going to make a spaceship from cardboard boxes'. Aye right."
  , privmsg "Going to raid the bin shelters for cardboard boxes." >> (liftIO $ threadDelay $ 10 * 60 * 10 ^ 6) >> privmsg "Got some crackers. Sort of cardboard boxes you could build a real spaceship from, never mind a pretend one made of cardboard boxes."
  , privmsg "Having some rocket fuel. A banana skin triple tardis bucket upside down on my couch."
  , privmsg "You'd think the kitchen knife would be the hard man of the kitchen, but chopping board's made to not give a fuck about the kitchen knife."
  , privmsg "Captain of the Starship DeeDee. Fucked oot mah nut."
  , privmsg "Look below at all the people, they're tiny. And they were."
  , privmsg "I've lost the plot, man."
  , privmsg "Stumbled onto this Jobsearch thing, pure pish jobs but I'm out of my face so things are starting to take my fancy."
  , privmsg "Fookin' Nothing ventured…" >> privmsg "What?"
  , privmsg "I'm starting to get tuned into the stream. Starting to get ideas."
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
    paranoidQuit = privmsg "Oh shite they found us. Run Dee Dee!" >> privmsg "/me quits. Just wasn't worth it." >> write QUIT ":Just wasn't worth it." >> liftIO (exitWith ExitSuccess)
    yokerResponse r | r == 0 = privmsg "I'm not from Yoker, I've got no business being here!" >> privmsg "Gets to the bus but he wouldn't let me in. I was like that, set up, whole thing's a set up." >> privmsg "Them that were on that front bus, actors, the lot of them actors." >> privmsg "Door opens and I bolts upstairs , right under the seat. Didn't dare poke my head up for the next half hour in case they were going by in a minibus, gasping to feast on me like a shower of zombie pirates." >> privmsg "Picked a moment." >> privmsg "Quit the channel." >> write QUIT ":But the best day of my life." >> liftIO (exitWith ExitSuccess)
                    | r >= 1 = randomItem ["Yoker's one of these places I only know from the front of a bus. Never been there, don't know what it's like.", "Pure fabled land.", "Sounds like a pure mad egg yolk."] >>= privmsg

-- The 'Net' monad, a wrapper over IO, carrying the bot's state.
-- type Net = ReaderT Bot IO
type Net = ReaderT Bot IO
data MsgType = PRIVMSG | QUIT | PASS | NICK | USER | JOIN | CAP | KICK | PONG deriving (Show)
data Msg = Msg MsgType String Bool deriving (Show)
data Bot = Bot { socket :: Handle, stdOutChan :: Chan String, socketChan :: Chan Msg, lastSeen :: TVar LastSeen, lastPosted :: TVar UTCTime, rng :: TVar StdGen }
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
  oc <- newChan
  sc <- newChan
  hSetBuffering h NoBuffering
  return (Bot h oc sc l t e)
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
  write PASS pass
  write NICK nick
  write USER (nick ++ " 0 * :deedee_bot")
  write JOIN chan
  write CAP "REQ :twitch.tv/membership"
  privmsg "Scary man scary, but the best day of my life."
  state <- get
  r <- liftIO $ mapConcurrently (\f -> evalStateT f state) [putStrLnWriter, hWriter, talk, listen]
  liftIO (putStrLn $ show r)

-- Possibly say something if the room goes quiet.
talk :: Net ()
talk = forever $ do
  h <- gets socket
  l <- getLastSeen
  now <- liftIO getCurrentTime
  r <- randomNet (1 :: Int, 20 :: Int)
  liftIO (putStrLn $ "NIGEL " ++ show r)
  -- When deedee Said something, how long Ago that was.
  sa <- lastDiff
  -- How long ago was any post (by anyone) Last Seen.
  let ls = diffUTCTime now (timeLastSeen l)
  when (ls > min && ls <= max && sa > intervalAsDiff && sa > ls && r == 20) (join partyChat)
  if (ls > min)
    then liftIO $ threadDelay $ interval * 10 ^ 6
    else liftIO $ threadDelay $ diffTimeToMicroseconds (min - ls)
  where
    -- Time after a message is posted on IRC that deedee will consider
    -- saying something himself.
    min = 3 * 60
    -- Time after someone saying something deedee will consider everyone
    -- to have left, so he does not talk to an empty room.
    -- For example: if min is 3, max 10 and interval 3 mins, deedee will
    -- consider talking 3 times.
    max = 10 * 60
    -- How often deedee should consider saying something.
    interval = 3 * 60
    intervalAsDiff = fromIntegral interval :: NominalDiffTime
    forever a = a >> forever a

-- Enqueue message for posting.
enqueue :: Msg -> Net ()
enqueue x = do
  c <- gets socketChan
  liftIO $ writeChan c x

-- Enqueue line for writing to stdout.
putLnQueue :: String -> Net ()
putLnQueue x = do
  c <- gets stdOutChan
  liftIO $ writeChan c x

-- Process each line from the server.
listen :: Net ()
listen = forever $ do
  h <- gets socket
  s <- init `fmap` liftIO (hGetLine h)
  putLnQueue s
  st <- get
  liftIO . forkIO $ do
    evalStateT (preEval s) st
  where
    forever a = a >> forever a

-- Respond to different types of events.
-- If an event is not handled, deedee will think it's someone talking. In
-- the case where deedee uses the last said time, not handling an event
-- can cause bugs.
preEval :: String -> Net ()
preEval x | ping x              = pong x
          | join x && joins     = joinResponse x
          | join x && not joins = return ()
          | part x && joins     = partResponse x
          | part x && not joins = return ()
          | mode x              = return ()
          | otherwise           = eval (clean x) >> setLastSeen (clean x)
  where
    clean          = drop 1 . dropWhile (/= ':') . drop 1
    ping y         = "PING :" `isPrefixOf` y
    pong y         = write PONG (':' : drop 6 y)
    -- Format: ':user!user@user.tmi.twitch.tv JOIN #chan'
    join y         = "JOIN"  == jp y
    part y         = "PART"  == jp y
    mode y         = "MODE"  == jp y
    jp             = reverse . take 4 . reverse . takeCmd
    takeCmd        = init . takeWhile ('#' /=)
    takeName       = changeName . takeWhile ('!' /=) . drop 1
    joinResponse y = when (takeName y /= nick) (privmsg' $ (takeName y) ++ " joined.")
    partResponse y = privmsg' $ (takeName y) ++ " fooked off."

changeName "jeevesbond" = "Jeeves (late again) Bond"
changeName "taspira"    = "Tasrumpia"
changeName "sirrufert"  = "Man who pisses himself as he gets arrested"
changeName x            = x

randomItem :: [a] -> Net a
randomItem l = randomNet (0, length l - 1) >>= \i -> return $ l !! i

-- Sets the last seen record using string and current time.
setLastSeen :: String -> Net ()
setLastSeen x = gets lastSeen >>= \l -> liftIO getCurrentTime >>= \t -> liftIO $ atomically (writeTVar l $ LastSeen x t)

getLastSeen :: Net LastSeen
getLastSeen = do
  l <- gets lastSeen
  liftIO $ atomically (readTVar l)

getLastPosted :: Net UTCTime
getLastPosted = do
  t <- gets lastPosted
  liftIO $ atomically (readTVar t)

-- Dispatch a command.
eval :: String -> Net ()
eval     "!quit"               = write QUIT ":Exiting" >> liftIO (exitWith ExitSuccess)
eval     "!kill jester"        = write KICK (chan ++ " smallangrycrab")
eval     "!last seen"          = getLastSeen >>= privmsg' . show
eval     "!last said"          = getLastPosted >>= privmsg' . show
eval     "!seen diff"          = liftIO getCurrentTime >>= \now -> getLastSeen >>= \l -> privmsg' $ show $ diffUTCTime now (timeLastSeen l)
eval     "!talk possible"      = do
                                   sa <- lastDiff
                                   now <- liftIO getCurrentTime
                                   l <- getLastSeen
                                   privmsg' $ show ((diffUTCTime now $ timeLastSeen l) < sa)
eval     "!last diff"          = let duntText = " since I last give it a wee dunt."
                                 in lastDiff >>= \t -> privmsg' ((show t) ++ duntText)
eval     "!wait test"          = privmsg "Aye, carry on this shouldn't block a thing" >> liftIO (threadDelay (20 * 10 ^ 6)) >> privmsg "Amazing what they can do these days with threading and that."
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
  zero <- getLastPosted
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

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = enqueue $ Msg PRIVMSG (chan ++ " :" ++ s) True

-- Send a privmsg without updating lastPosted time.
privmsg' s = enqueue $ Msg PRIVMSG (chan ++ " :" ++ s) False

-- Enqueue a message for sending to the server we're currently connected to.
write :: MsgType -> String -> Net ()
write PRIVMSG m = privmsg m
write t m       = enqueue $ Msg t m False

diffTimeToMicroseconds :: NominalDiffTime -> Int
diffTimeToMicroseconds x = ceiling (x * 10 ^ 6)

-- Writes the contents of a Chan to stdout without interleaving messages.
putStrLnWriter :: Net ()
putStrLnWriter = forever $ do
  c <- gets stdOutChan
  liftIO $ readChan c >>= putStrLn
  where
    forever a = a >> forever a

hWriter :: Net ()
hWriter = forever $ do
  -- Minimum seconds between deedee posts.
  let min = 2
  diff <- lastDiff
  when (diff < min) (liftIO $ threadDelay $ diffTimeToMicroseconds (min - diff))
  h <- gets socket
  c <- gets socketChan
  m <- liftIO $ readChan c
  msg h m
  putLnQueue (consoleMsg m)
  where
    forever a = a >> forever a
    msg h (Msg PRIVMSG x True) = (liftIO $ hPrintf h "%s %s\r\n" (show PRIVMSG) x) >> updateLastPosted
    msg h (Msg t x _)          = liftIO $ hPrintf h "%s %s\r\n" (show t) x
    consoleMsg (Msg t x _)     = "> " ++ (show t) ++ " " ++ x

updateLastPosted :: Net ()
updateLastPosted = do
  t <- liftIO $ getCurrentTime
  lp <- gets lastPosted
  liftIO $ atomically (writeTVar lp t)
