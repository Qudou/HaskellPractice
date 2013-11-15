import Data.IORef
data Chanel a = Sender [a] | Receiver [(a -> IO ())]

newChanel :: IO (IORef (Chanel a))
newChanel = newIORef $ Sender []

send :: IORef (Chanel a) -> a -> IO ()
send cr v = do
    ch <- readIORef cr
    case ch of
        Sender vs -> writeIORef cr $ Sender (v:vs)
        Receiver [] -> writeIORef cr $ Sender [v]
        Receiver (p:ps) -> (writeIORef cr $ Receiver ps) >> (p v)

recv :: IORef (Chanel a) -> (a -> IO ()) -> IO ()
recv cr p = do
    ch <- readIORef cr
    case ch of
        Receiver ps -> writeIORef cr $ Receiver (p:ps)
        Sender [] -> writeIORef cr $ Receiver [p]
        Sender (v:vs) -> (writeIORef cr $ Sender vs) >> (p v)

test = do
    x <- newChanel
    send x 3
    recv x (putStrLn . show)

serv :: IORef (Chanel (IORef (Chanel Int), Int)) -> IO ()
serv rch = do
    recv rch (\(sch, v) -> (send sch $ v*v) >> (serv rch))

test2 = do
    ch <- newChanel
    ch2 <- newChanel
    serv ch
    send ch (ch2, 3)
    recv ch2 (putStrLn.show)
    send ch (ch2, 15)
    recv ch2 (putStrLn.show)
