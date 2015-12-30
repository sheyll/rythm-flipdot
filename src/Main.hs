module Main where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad.Writer
import           Data.Array
import           Data.Bits
import qualified Data.ByteString           as B
import           Data.Complex
import           Data.Function
import           Data.List
import           Data.Word
import           Network.Socket            hiding (sendTo)
import           Network.Socket.ByteString
import           System.IO

artWorkLeft :: Rythm
artWorkLeft = Rest 1 :+: Hit 1 1 :+: incrAmp  (incrDur artWorkLeft)

artWorkRight :: Rythm
artWorkRight = Hit 1 1 :+: Rest 1 :+: incrAmp (incrDur artWorkRight)

artWorkMiddle :: Rythm
artWorkMiddle = Hit 1 1 :+: Hit 1 1 :+: go 1 True
  where
    go :: Int -> Bool -> Rythm
    go dur up =
        Rest 1 :+: Hit 119 dur :+: go dur' up'
      where
        dur' =
            if up
                then dur + 1
                else dur - 1
        up' = up && dur < 119 || (not up && dur < 2)

decrAmpN 0 r = r
decrAmpN n r = r' :+: decrAmpN (n - 1) r'
  where
    r' = decrAmp r

incrAmpN 0 r = r
incrAmpN n r = r' :+: incrAmpN (n-1) r'
  where
    r' = incrAmp r

nTimes r 1 = r
nTimes r n = r :+: nTimes r (n-1)

ssht :: Rythm
ssht = Hit 12 1

incrDur (Hit a d) = Hit a (cap 1 120 (d + 1))
incrDur (r1 :+: r2) = incrDur r1 :+: incrDur r2
incrDur ( = r

decrAmp :: Rythm -> Rythm
decrAmp (Hit a d) | a > 1 = Hit (a - 1) d
                  | otherwise = Hit a d
decrAmp (r1 :+: r2) = decrAmp r1 :+: decrAmp r2
decrAmp r = r

incrAmp :: Rythm -> Rythm
incrAmp (Hit a d) | a < 120 = Hit (a + 1) d
                  | otherwise = Hit a d
incrAmp (r1 :+: r2) = incrAmp r1 :+: incrAmp r2
incrAmp r = r


data Action
    = Render Track
    | Stop
    deriving (Show)

ipLeft = "2001:67c:20a1:1095:ba27:ebff:feb9:db12"
ipMiddle = "2001:67c:20a1:1095:ba27:ebff:fe23:60d7"
ipRight = "2001:67c:20a1:1095:ba27:ebff:fe71:dd32"

-- small panel: 16x20
-- panel: 3 x 6 small: 48 x 120

data Panel = P Track (Chan Action)

main :: IO ()
main = do
    l <- startPanel 'L' artWorkLeft (mandelBrot 0 newImage) ipLeft
    m <- startPanel 'M' artWorkMiddle (mandelBrot 49 newImage) ipMiddle
    r <- startPanel 'R' artWorkRight (mandelBrot 97 newImage) ipRight
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    xxxLoop l m r
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
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
        m' = updatePanel m
        r' = updatePanel r
    renderToPanel l
    renderToPanel m
    renderToPanel r
    xxxLoop l' m' r'

updatePanel :: Panel -> Panel
updatePanel (P t c) = P (dropCurrent t) c
  where
    dropCurrent (_ :>>: t') = t'
    dropCurrent _ = Done

data Input
    = Quit
    | Tick

readInput :: IO Input
readInput = do
    c <- getChar
    return
        (case c of
             ' ' -> Tick
             _ -> Quit)

startPanel :: Char -> Rythm -> Image -> String -> IO Panel
startPanel name artWork initialImage host = do
    c <- newChan
    ai <- head <$> getAddrInfo Nothing (Just host) (Just "2323")
    s <- socket (addrFamily ai) Datagram defaultProtocol
    void $ forkIO (panelLoop name (addrAddress ai) c s)
    return (P (renderRythm artWork initialImage) c)

stopPanel :: Panel -> IO ()
stopPanel = sendAction Stop

renderToPanel :: Panel -> IO ()
renderToPanel p@(P t _) = sendAction (Render t) p

sendAction :: Action -> Panel -> IO ()
sendAction a (P _ c) = writeChan c a

panelLoop :: Char -> SockAddr -> Chan Action -> Socket -> IO ()
panelLoop name a c s = do
    putStr [name, '?']
    action <- readChan c
    case action of
        Stop -> do
            putStr [name, '$']
            return ()
        Render t -> do
            panelRenderNext t
            panelLoop name a c s
  where
    panelRenderNext t =
        case t of
            Done -> do
                putStr [name, 'X']
                return ()
            Sleep d -> do
                putStr [name, '<']
                threadDelay (durationToMicros d)
                putStr [name, '>']
            Draw i -> do
                putStr [name, '#']
                void $ sendTo s (convertToDisplay i) a
            t' :>>: _ -> panelRenderNext t'


-- * MUSIC API

type Amplitude = Int
type Duration = Int

durationToMicros :: Int -> Int
durationToMicros = (microsPerCol *)
  where
    microsPerCol = 1500000 `div` panelW

data Rythm = Hit Amplitude Duration | Rest Duration | Rythm :+: Rythm
    deriving (Show)

type Micros = Int
data Track = Done | Sleep Micros | Draw Image | Track :>>: Track
    deriving (Show)

instance Monoid Track where
    mempty = Done
    mappend Done t = t
    mappend t Done = t
    mappend (ll :>>: lr) r = ll :>>: mappend lr r
    mappend l r = l :>>: r

renderRythm :: Rythm -> Image -> Track
renderRythm r orig = execWriter (go r orig)
  where
    go :: Rythm -> Image -> Writer Track Image
    go (r1 :+: r2) i = do
        i' <- go r1 i
        go r2 i'
    go (Rest d) i = do
        tell (Sleep (cap 1 120 d))
        return i
    go (Hit a d) i = do
        let i' = flipBlock d a i
        tell (Draw i')
        tell (Sleep (cap 1 120 d))
        return i'

cap l u t | l <= t && t <= u = t
          | l <= t         = u
          | otherwise     = l

-- * Image API
type Image = Array (Int,Int) Bool


panelW, panelH :: Int
panelW = 48
panelH = 120

capCoords (x, y) = (cap 1 48 x, cap 1 120 y)

mandelBrot :: Int -> Image -> Image
mandelBrot offset img = foldr mandelPixel img (indices img)
  where
    mandelPixel (x,y) = setPixel x y (inMandelSet (0.0 :+ 0.0) 0)
      where
        maxIter = 100
        c :: Complex Double
        c =
            (fromIntegral (offset + x) * (4.0 / 144.0) - 2.0) :+
            (fromIntegral y * (4.0 / 120.0) - 2.0)
        inMandelSet z i
          | ((magnitude z) <= 2.0) && i < maxIter =
              inMandelSet (z * z + c) (i + 1)
          | otherwise = rem i 2 == 1

newImage :: Image
newImage =
    array
        ((1, 1), (panelW, panelH))
        [((x, y), False) | x <- [1 .. panelW]
                         , y <- [1 .. panelH]]

flipBlock width height img = foldr (flipCol height) img [1 .. width]

flipCol height col img = foldr flipPixel img [(col, y) | y <- [1..height]]

flipPixel (x,y) img = setPixel x y (not (getPixel x y img)) img

setPixel :: Int -> Int -> Bool -> Image -> Image
setPixel x y v img = img // [((x, y), v)]

getPixel :: Int -> Int -> Image -> Bool
getPixel x y img = img ! (x,y)

convertToDisplay :: Image -> B.ByteString
convertToDisplay img = pixels
  where
    pixels = B.pack $ toFlipDotFormat $ toBit <$> elems img
    toFlipDotFormat :: [Word8] -> [Word8]
    toFlipDotFormat x = go x []
      where
        go x acc =
            case take 8 x of
              [] -> reverse acc
              sx -> go (drop 8 x) (toByte (reverse sx) 1 0 : acc)
    toByte [] _ acc =  acc
    toByte (b:rest) p acc = toByte rest (p*2) (acc .|. b * p)
    toBit :: Bool -> Word8
    toBit = fromIntegral . fromEnum

    (_,(w,h)) = bounds img

toPixel :: Int -> Char
toPixel = toEnum
