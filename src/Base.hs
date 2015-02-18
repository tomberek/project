{-# LANGUAGE ExistentialQuantification,FlexibleContexts,
             FlexibleInstances, MultiParamTypeClasses #-}

-- show
module Base where
-- /show

import Unsafe.Coerce
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Map as M
import Data.Dynamic
import Debug.Trace
import Data.Monoid

--import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import GHC.Conc
import Data.Maybe
import System.Mem.StableName
import Data.List
import Auto
import Control.Monad.IO.Class

--(!>) = flip trace 
(!>) = const .id
infixr 0 !>

data EvType = Q | P deriving (Show, Eq, Ord, Enum, Bounded)
data Event a = Event EvType a deriving Show
eventList :: [Event Int]
eventList=[ Event Q 10, Event P 2, Event P 3
            ,Event Q 30, Event P 4]

{-# NOINLINE eventHandlers #-}
eventHandlers :: MVar (M.Map EvType (AutoX m a b))
eventHandlers = unsafePerformIO $ newMVar M.empty

register name f = do
    evs <- takeMVar eventHandlers
    putMVar eventHandlers $ M.insert name f evs !> "register "++ show name

inject :: (r -> Maybe a-> IO ()) -> AutoX IO r a -> AutoX IO r a
inject io ra = AConsX $ \state -> do
    (res, out) <- runAutoX ra state
    io state res
    return (res, inject io out)

runEvent :: (MonadIO m) => EvType -> AutoX m a b
runEvent name = AConsX $ \input -> do
    this <- liftIO $ takeMVar eventHandlers
    (res,out) <- case M.lookup name this of
          Just e -> runAutoX e input
          Nothing -> return (Nothing
                            , error "missing handler"
                            )
    liftIO $ putMVar eventHandlers $ M.insert name out this !> "put back in"
    return (res,out)

runA event value = runAutoX (runEvent event) value >>= return . fromJust . fst
{-
data Loop= Once | Loop | Multithread deriving Eq

genNewId :: TransientIO Int
genNewId=  do
      st <- get
      case replay st of
        True -> do
          let n= mfSequence st
          put $ st{mfSequence= n+1}
          return n
        False -> 
          modifyMVar refSequence $ \n -> return (n+1,n)

waitEvents ::  IO b -> TransientIO b
waitEvents= parallel Loop


async  :: IO b -> TransientIO b
async = parallel Once

parallel  ::  Loop ->  IO b -> TransientIO b
parallel hasloop receive =  AConsX $ do

      cont <- getCont
      id <- genNewId
      let currentRow= row cont
--          mnode=  nodeInfo cont
      mnode  <-   lookTree id currentRow !> ("idToLook="++ show id++ " in: "++ show currentRow)

      case mnode of
        Nothing ->do
                 return () !> "NOT FOUND"
                 do
                   ref <- newMVar $ Node (id,undefined,False,Nothing)

                   modifyMVar_ (row cont) $ \(RowList ns t) -> return $  RowList (ref : ns) t
                   forkIO $ do
                     th <- myThreadId
                     modifyMVar_ ref $ \(Node(id,_,f,n)) -> return $ Node (id,th,f,n)


                     loop hasloop  receive $ \r -> do

                      th <-  myThreadId
                      modifyMVar_  ref $ \(Node(i,_,_,_)) -> return
                                       $ Node(i,th,False,Just $ unsafeCoerce r)
                      case cont of
                        EventF  (i,_,x) f _ _ _ _ _-> do
                          mr <- runAutoX x $
                                cont{replay= True,mfSequence=i,nodeInfo=Just ref}
                             !> "runx" !> ("mfSequence="++ show i)
                          case mr  of
                            (Nothing,_) ->
                               modifyMVar_ ref $ \(Node(i,th,ins,rec)) -> return
                                               $ Node(i,th,True,rec)


                            (Just r,cont') ->do
                               modifyMVar_ ref $ \(Node(i,th,ins,_)) -> return
                                               $Node (i,th,False,Nothing)
                               let row1= row cont'
                               delEvents  row1        !> ("delEvents, activated    "++ show row1)
                               id <- readMVar refSequence
                               n <-  if hasloop== Multithread then return row1 else  addRow  row1
                               (runAutoX $ ( compose $ unsafeCoerce f) r)
                                            cont'{row=n,replay= False,mfSequence=id } !> ("SEQ=" ++ show(mfSequence cont'))
                               return ()
--                      delEvents children []


                   modifyMVar_ (row cont) $ \(RowList ns ch) -> return $  RowList (ref : ns) ch

                 return Nothing 


        Just (node@(id',th',inspectedValue, mrec)) -> do
          modify $ \cont -> cont{nodeInfo=Nothing}
--          Node (node@(id',th',inspectedValue, mrec)) <- liftIO $ readMVar ref
          return $ if isJust mrec then Just $ unsafeCoerce $ fromJust mrec else Nothing
--          th <- liftIO myThreadId
--
--          return () !>  "FOUND" !> show th !> show th' !> show inspectedValue !> show mrec
--
--          if inspectedValue== False && th== th' && isJust mrec then do
--               return $ Just $ unsafeCoerce $ fromJust mrec
--
--          else if inspectedValue == True && isJust mrec then
--               return $ Just $ unsafeCoerce $ fromJust mrec
--
--          else return Nothing


        where


        loop Once rec x  = rec >>= x
        loop Loop rec f = do
            r <- rec
            f r
            loop Loop rec f

        loop Multithread rec f = do
            r <- rec
            forkIO $ f r
            loop Multithread rec f

        lookTree :: EventId -> P RowElem -> IO (Maybe NodeTuple)
        lookTree id ref=  do
            RowList ns _<- readMVar ref
            lookList id ns



        lookList id mn= case mn of
              [] -> return Nothing
              (p:nodes) -> do
                  me <- readMVar p
                  case me of
                    Node(node@((id',_,_,_))) ->
                      if id== id'
                         then return $ Just node
                         else lookList id nodes
                    RowList row _ -> do
                         mx <- lookList id nodes
                         case mx of
                           Nothing -> lookList id row
                           Just x -> return $ Just x
        delEvents :: P RowElem  -> IO()
        delEvents ref = do
            RowList mevs mch <- takeMVar ref
            maybeDel mch
            putMVar ref $ RowList mevs Nothing

        maybeDel mch=  case mch of
              Nothing -> return ()
              Just p -> do
                  RowList es mch' <- readMVar p
                  delList es !> ("toDelete="++ show es)
                  maybeDel mch'


        delList es=  mapM_ del es where
          del p = readMVar p >>= del'
          del' (Node(node@(_,th,_,_)))= killThread th !> ("DELETING " ++ show node)
          del' (RowList l mch)= delList l >> maybeDel mch


type EventSetter eventdata response= (eventdata ->  IO response) -> IO ()
type ToReturn  response=  IO response
react
  :: Typeable eventdata
  => EventSetter eventdata response
  -> ToReturn  response
  -> TransientIO eventdata

react setHandler iob= AConsX $ \state -> do
        cont    <- getCont
        mEvData <- getSessionData
        case mEvData of
          Nothing -> do
            setHandler $ \dat ->do
--              let cont'= cont{mfData = M.insert (typeOf dat)(unsafeCoerce dat) (mfData cont)}
              runAutoX (setSessionData dat >> runCont state cont) cont
              iob
            return Nothing
          Just dat -> delSessionData dat >> return (Just  dat)

--hash f= liftIO $ do
--          st <- makeStableName $! f `seq` f
--          return $hashStableName st

--uhash= unsafePerformIO .hash
             
getLineRef= unsafePerformIO $ newTVarIO Nothing


option1 x  message=  inputLoop `seq` (waitEvents  $ do
     putStrLn $ message++"("++show x++")"
     atomically $ do
       mr <- readTVar getLineRef
       th <- unsafeIOToSTM myThreadId
       case mr of
         Nothing -> retry
         Just r ->
            case reads1 r !> ("received " ++  show r ++  show th) of
            (s,_):_ -> if  s == x  !> ("waiting" ++ show x)
                     then do
                       writeTVar  getLineRef Nothing !>"match"
                       return s

                     else retry
            _ -> retry)
     where
     reads1 s=x where
      x= if typeOf(typeOfr x) == typeOf "" then unsafeCoerce[(s,"")] else readsPrec 0 s
      typeOfr :: [(a,String)] ->  a
      typeOfr  = undefined

option ret message= do
    putStrLn $"Enter ("++show ret++")\tto: " ++ message
    waitEvents  $ getLine' (==ret)
    putStrLn $ show ret ++ " chosen"
    return ret

getLine' cond=   inputLoop `seq` do
     atomically $ do
       mr <- readTVar getLineRef
       th <- unsafeIOToSTM myThreadId
       case mr of
         Nothing -> retry
         Just r ->
            case reads1 r !> ("received " ++  show r ++ show th) of
            (s,_):_ -> if cond s  !> show (cond s)
                     then do
                       writeTVar  getLineRef Nothing !>"match"
                       return s

                     else retry
            _ -> retry
     where
     reads1 s=x where
      x= if typeOf(typeOfr x) == typeOf "" then unsafeCoerce[(s,"")] else readsPrec 0 s
      typeOfr :: [(a,String)] ->  a
      typeOfr  = undefined

inputLoop=  do
    print "Press end to exit"
    inputLoop'
    where
        inputLoop'= do
           r<- getLine                      !> "started inputLoop"
           if r=="end" then putMVar rexit () else do
              atomically . writeTVar  getLineRef $ Just r
              inputLoop'


rexit= unsafePerformIO newEmptyMVar
stay=  takeMVar rexit
-}