module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Writer
import Data.Array
import Network.Socket
import System.IO

data Action
    = Render Image
    | Stop
    deriving (Show)

ipLeft = "2001:67c:20a1:1095:ba27:ebff:feb9:db12"
ipMiddle = "2001:67c:20a1:1095:ba27:ebff:fe23:60d7"
ipRight = "2001:67c:20a1:1095:ba27:ebff:fe71:dd32"

-- small panel: 16x20
-- panel: 3 x 6 small: 48 x 120

data Panel = P [Image] (Chan Action)

instance Show Panel where
    show (P (i:_) _) = "Panel-image:\n" ++ debugRender i ++ "\n\n"

main :: IO ()
main = do
    l <- startPanel ipLeft
    m <- startPanel ipMiddle
    r <- startPanel ipRight
    hSetBuffering stdin NoBuffering
    xxxLoop l m r
    hSetBuffering stdin LineBuffering
    stopPanel l
    stopPanel m
    stopPanel r

xxxLoop :: Panel -> Panel -> Panel -> IO ()
xxxLoop l m r = do
    i <- readInput
    processInput i l m r

processInput :: Input -> Panel -> Panel -> Panel -> IO ()
processInput Quit _ _ _ = return ()
processInput Tick l m r = do
    let l' = updatePanel l
    sendPanel l'
    xxxLoop l' m r

updatePanel :: Panel -> Panel
updatePanel (P imgs c) = P (drop 1 imgs) c

sendPanel :: Panel -> IO ()
sendPanel = print

data Input
    = Quit
    | Tick

readInput :: IO Input
readInput = do
    c <- getChar
    return
        (case c of
             'q' -> Quit
             ' ' -> Tick)

startPanel :: String -> IO Panel
startPanel host = do
    c <- newChan
    ai <- head <$> getAddrInfo Nothing (Just host) (Just "2323")
    s <- socket (addrFamily ai) Datagram defaultProtocol
    forkIO (panelLoop (addrAddress ai) c s)
    return (P (renderRythm test1) c)

stopPanel = sendAction Stop

renderToPanel p@(P [] _) = stopPanel p
renderToPanel p@(P (i:_) _) =
    sendAction (Render i) p

sendAction a (P _ c) = writeChan c a

panelLoop :: SockAddr -> Chan Action -> Socket -> IO ()
panelLoop a c s = do
    action <- readChan c
    case action of
      Stop ->
          return ()
      Render x ->
          do
             sendTo s (pgmRender x) a
             panelLoop a c s

-- * MUSIC API

type Amplitude = Int
type Duration = Int

data Rythm = Hit Amplitude Duration | Rest Duration | Rythm :+: Rythm
    deriving (Show)

repeatRythm r 0 = r
repeatRythm r n = r :+: repeatRythm r (n-1)

test1 = Hit 12 23 :+: Rest 23

renderRythm :: Rythm -> [Image]
renderRythm r = execWriter (go r newImage)
  where
    go :: Rythm -> Image -> Writer [Image] Image
    go (r1 :+: r2) i = do
        i' <- go r1 i
        go r2 i'
    go (Rest _) i = do
        tell [i]
        return i
    go (Hit a d) i = do
        let i' = flipBlock d a i
        tell [i']
        return i'


-- * Image API
type Image = Array (Int,Int) Char

panelW, panelH :: Int
panelW = 48
panelH = 120

newImage :: Image
newImage =
    array
        ((1, 1), (panelW, panelH))
        [((x, y), '\0') | x <- [1 .. panelW]
                        , y <- [1 .. panelH]]

flipBlock width height img = foldr (flipCol height) img [1 .. width]

flipCol height col img = foldr flipPixel img [(col, y) | y <- [1..height]]

flipPixel (x,y) img = setPixel x y (flipVal (getPixel x y img)) img
  where
    flipVal '\x255' = '\0'
    flipVal _ = '\x255'

setPixel :: Int -> Int -> Char -> Image -> Image
setPixel x y v img = img // [((x, y), v)]

getPixel :: Int -> Int -> Image -> Char
getPixel x y img = img ! (x,y)

debugRender :: Image -> String
debugRender = unlines . map (map renderPixel) . toLines
  where
    renderPixel c =
        if c < 'a'
            then ' '
            else 'X'
    toLines :: Image -> [String]
    toLines img = [extractLine y | y <- [1 .. panelH]]
      where
        extractLine :: Int -> String
        extractLine y =
            [v | ((_,y'),v) <- assocs img
               , y' == y]


pgmRender :: Image -> String
pgmRender img = "P5 " ++ show panelW ++ " " ++ show h ++ " 255\n" ++ pixels
  where
    pixels = "abcdef"
    (_,(w,h)) = bounds img

toPixel :: Int -> Char
toPixel = toEnum
