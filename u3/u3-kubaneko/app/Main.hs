{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Bifunctor                       (second)
import           Data.Tuple
import           GHC.Float
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import          Graphics.Gloss.Interface.IO.Game
import qualified Control.Exception as E
import GHC.Generics (Generic)
import Control.Monad
import Control.Monad.Extra (loopM)
import Data.List
import Data.List.Split
import qualified Data.Vector as V
import Network.Socket
import System.IO
import qualified Text.Read as T
import System.Environment
import Control.DeepSeq
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Chan.Strict

import Debug.Trace

--PARSE COMMAND LINE ARGUMENTS
ip="127.0.0.1"

port="10042"

pair []=[]
pair (x1:x2:xs)=(x1,x2):pair xs

getIp arg=case (filter (("-a"==).fst) arg) of
        [(a,b)] -> Just b
        _ -> Nothing

getPort arg=case (filter (("-p"==).fst) arg) of
        [(a,b)] -> Just b
        _ -> Nothing

checkSocket [x1,x2] (Just a, Just b)=(Just a, Just b)
checkSocket [x1] (Nothing,Just b)=(Just ip,Just b)
checkSocket [x1] (Just b,Nothing)=(Just b, Just port)
checkSocket _ _=(Nothing, Nothing)

parseArgs :: [String]->(Maybe HostName, Maybe ServiceName)
parseArgs []=(Just ip, Just port)
parseArgs arg
        | length arg>4 =(Nothing, Nothing)
        | (length arg) `mod` 2==1 =(Nothing, Nothing)
        | otherwise=checkSocket (pair arg) (getIp$pair arg, getPort$pair arg)

-- Networking

boardSize = (32, 32) :: (Int, Int)

-- stav daného pixelu a jemu příslušná barva

data Tile=Transparent | Dark | Light | Stressed | Selected | Error deriving(Eq, Read, Generic)

instance Show Tile where
        show Dark="Dark"
        show Light="Light"
        show Transparent="Transparent"

instance NFData Tile

fieldChar '.' = Transparent
fieldChar 'o' = Dark
fieldChar 'x' = Light

data Msg
  = DoPix Tile Int Int
  | DoPoll
  | DoTerminate
  deriving (Generic)

instance NFData Msg

instance Show Msg where
    show (DoPix t x y)=show t++ " " ++ show x ++ " " ++ show y
    show DoPoll="Poll"
    show DoTerminate="Quit"

data Update
  = SetBoard (V.Vector (V.Vector Tile))
  | SetPix Tile Int Int
  | Iden
  deriving (Generic)
 
instance NFData Update


parseUpdate a=case (words a) of
        "Poll":xs:[] ->SetBoard$V.fromList (V.fromList<$>chunksOf (fst boardSize) (fieldChar<$>xs))
        "Dark":x:y:[] -> SetPix Dark (read x) (read y)
        "Light":x:y:[] -> SetPix Light (read x) (read y)
        "Transparent":x:y:[] -> SetPix Transparent (read x) (read y)
        _ -> Iden

data ServerCom =
  ServerCom
    { inChan :: Chan Update
    , outChan :: Chan Msg
    }

newServerCom = ServerCom <$> newChan <*> newChan

inThread com sock=(liftM parseUpdate (hGetLine sock))>>=(writeChan com)>>inThread com sock

outThread com sock=(liftM show (readChan com))>>=(hPutStrLn sock) >> outThread com sock

getSocket=do
    args <- getArgs
    sock <- socket AF_INET Stream 0
    addr <-
      addrAddress . head <$>
      getAddrInfo (Just defaultHints) (fst$parseArgs args) (snd$parseArgs args)
    connect sock addr
    h <- socketToHandle sock ReadWriteMode
    com <- newServerCom
    writeChan (outChan com) DoPoll 
    receiver <- forkIO $ inThread (inChan com) h
    sender <- forkIO $ outThread (outChan com) h
    displayScreen (initial$return com)

main=withSocketsDo getSocket

{-
 - Konstanty a ostatní vlastnosti programu
 -}

-- okno pro zobrazeni mapy
window=InWindow "Pixel Drawer" (sizep*fst boardSize,sizep*fst boardSize) (0,0) :: Display
-- pocet pixelu v pixelu :)
sizep=20

-- struktura představuje stav programu
data PixMap=PixMap {picture  :: IO (V.Vector(V.Vector Tile)),
                    selected :: (Int,Int),
                    from     :: (Int,Int),
                    geom     :: GeomObjects,
                    channel      :: IO (ServerCom)
                    }

-- představuje co se s vybraným objektem stane
data Actions=Inverse | Fill deriving(Eq)

-- Specifikuje vybraný geometrický objekt
data GeomObjects=None | PixRectangle | PixLine deriving(Eq)

-- barvy použité pro pixely - na obrázku se vyskytují jen Dark, Light, Transparent - zbytek je návodný
getColour Error=   makeColorI 0 0 255 255
getColour Stressed=makeColorI 0 255 0 255
getColour Selected=makeColorI 255 0 0 255
getColour Light=   makeColorI 0 0 0 255
getColour Dark=    makeColorI 122 122 122 255
getColour Transparent=  makeColorI 255 255 255 0

-- inverse na Pixely na obrázku
invertColour Light= Dark 
invertColour Dark= Light
invertColour Transparent=Transparent

{-
 - Sekce měnící stav
 -}

getIndexUpdate pic action a pix
    | action==Fill=DoPix pix (fst a) (snd a)
    | action==Inverse=DoPix (invertColour$(pic V.! fst a) V.! snd a) (fst a) (snd a)

-- změní geometrický objekt a jeho začátek ve stavu Světa
changeGeom pxm cursor geom=PixMap (picture pxm) (selected pxm) cursor geom (channel pxm)

-- změní zvolený pixel
changeSelect pxm newsel=PixMap (picture pxm) newsel (from pxm) (geom pxm) (channel pxm)

{-
 - Sekce Generující indexi pro geometrické obrazce
 -}

-- Vygeneruje indexi, které zabírají jednotlivé geometrické objekty
getGeomIndexes coor1 coor2 geometr
    | geometr==PixLine=genIndexLine coor1 coor2
    | geometr==PixRectangle=genIndexRectangle coor1 coor2
    | geometr==None =[coor1]

--Vygeneruje souřadnice pixelů v obdélníku
genIndexRectangle coor1 coor2=[(a,b) | a<-getListRange (fst coor1) (fst coor2), b<-getListRange (snd coor1) (snd coor2)]

-- neprázdný list od menšího ze dvou argumentů k většímu
getListRange index1 index2
    | index1>index2 =[index2..index1]
    | index1<=index2 =[index1..index2]

{-
Generuji indexy příslušející k dané úsečce pomocí doporučeného Bresenhamova algoritmu

věřím, že to jde elegantněji, ale mě se to nepovedlo
-}

-- dostane začátek a konec a vrátí list indexů pro úsečku - zaručuje, že slope úsečky vložené do maxBresen je v absolutní hodnotě menší než 1
genIndexLine coor1 coor2
    | abs(fst coor1-fst coor2)<abs(snd coor1-snd coor2) = swap <$> maxBresen (swap coor1) (swap coor2)
    | otherwise =  maxBresen coor1 coor2

-- zaručí, že dx pro brehem funkci bude kladné
maxBresen coor1 coor2
    | fst coor1<fst coor2=brehem coor1 coor2
    | otherwise=brehem coor2 coor1
-- dostane list slope errorů ve formě pro Bresenhamův algo a pomocí funkce newCoor ho naskenuje na indexi úsečky
brehem coor1 coor2=scanl' (newCoor (signum(snd coor2-snd coor1))) coor1  (errorList (fst coor2-fst coor1) (abs(snd coor2-snd coor1)))

-- vrací další souřadnice pro danou úsečku
newCoor sy coor err
    | err>=0=(fst coor +1, snd coor + sy)
    | otherwise=(fst coor +1, snd coor)

-- vytvoří list chyb pro směrnici úsečky v Bresenhamově algoritmu zase scanováním listu, dropnem počáteční hodnotu, aby pro samotný bod byl list prázdý
errorList dx dy=drop 1$scanl' (nextError dx dy) (2*dy-dx) [0| a<-[0..dx-1]]

-- vrátí další error
nextError dx dy err a
    |err>=0 =err+2*dy-2*dx
    |otherwise=err+2*dy

{-
 - sekce vykreslující stav na obrazovku
-}
-- vykreslí geometrické objekty do obrázku
drawGeom pxm pix action=PixMap (sendChanges pix pxm action) (selected pxm) (-1,-1) None (channel pxm)

sendChanges :: Tile -> PixMap -> Actions -> IO (V.Vector (V.Vector Tile))
sendChanges pix pxm action=do
        chan <- channel pxm
        pic <- picture pxm
        toSend <- mapM (\a -> writeChan (outChan chan) (getIndexUpdate pic action a pix)) (getGeomIndexes (selected pxm) (from pxm) (geom pxm))
        return pic

-- vykreslí stav na obrazovku
drawWorld pxm = do
        pict <- picture pxm
        return$pictures$V.toList(V.imap (\x k-> pictures.V.toList$V.imap (drawPixel rectangleSolid x) k) pict)  ++
                                (uncurry (drawPixel2 rectangleWire Stressed) <$> getGeomIndexes (selected pxm) (from pxm) (geom pxm)) ++
                                [uncurry (drawPixel rectangleWire) (selected pxm) Selected]
-- fliplá verze draw Pixel
drawPixel2 s t x y=drawPixel s x y t
-- vykreslý Pixel na obrazovku pro daný tvar, souřadnice a Pixel ~ Barvu
drawPixel s x y t =color (getColour t)$Translate ((int2Float x-15.5)*int2Float sizep) ((int2Float y-15.5)*int2Float sizep)$s (int2Float sizep) (int2Float sizep)


{-
 - Sekce ovládání hry
-}

--funkce handlující eventy
handleEvent (EventKey (Char 'l') Down _ _) pxm = return$changeGeom pxm (selected pxm) PixLine
handleEvent (EventKey (Char 'r') Down _ _) pxm = return$changeGeom pxm (selected pxm) PixRectangle
handleEvent (EventKey (Char 'c') Down _ _) pxm = return$changeGeom pxm (-1,-1) None
handleEvent (EventKey (Char 'i') Down _ _) pxm= return$drawGeom pxm Light Inverse
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) pxm = return$drawGeom pxm Transparent Fill
handleEvent (EventKey (Char 'z') Down _ _) pxm = return$drawGeom pxm Dark Fill
handleEvent (EventKey (Char 'x') Down _ _) pxm = return$drawGeom pxm Light Fill
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) pxm = return$changeSelect pxm (fst (selected pxm), max 0 $ snd (selected pxm) -1)
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) pxm = return$changeSelect pxm (fst (selected pxm), min (snd boardSize-1) $ snd (selected pxm) +1)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) pxm = return$changeSelect pxm (min (fst boardSize-1) $ fst (selected pxm) +1, snd (selected pxm))
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) pxm = return$changeSelect pxm (max 0 $ fst (selected pxm) -1, snd (selected pxm))
handleEvent _ n = return n

--funkce pravidelně updatující stav
updateWorld _ pxm=(return$PixMap (loop pxm) (selected pxm) (from pxm) (geom pxm) (channel pxm))

updateBool :: (IO (Chan Update), IO (V.Vector (V.Vector Tile))) -> IO (Either (IO(Chan Update),IO (V.Vector (V.Vector Tile))) (V.Vector (V.Vector Tile)))
updateBool (chan, p)=do
            channel<-chan
            pic<-p
            bool<-isEmptyChan channel
            update bool channel pic


update :: Bool -> Chan Update -> (V.Vector (V.Vector Tile)) ->IO( Either (IO(Chan Update),IO (V.Vector (V.Vector Tile))) (V.Vector (V.Vector Tile)))
update True chan pic=return$Right pic
update False chan pic=do
            cont<-readChan chan
            return (Left (return chan, return (makeChange cont pic)))

makeChange :: Update -> V.Vector (V.Vector Tile) -> V.Vector (V.Vector Tile)
makeChange (SetBoard a) pic=a
makeChange (SetPix a x y) pic=pic V.// [(x,(pic V.! x) V.// [(y, a)])]
makeChange Iden pic=pic
            
loop :: PixMap -> IO (V.Vector (V.Vector Tile))
loop pxm=loopM (updateBool) (liftM inChan (channel pxm),picture pxm) 

initial :: IO (ServerCom) -> PixMap
initial com=PixMap (return$V.replicate (fst boardSize) (V.replicate (snd boardSize) Transparent)) (0,31) (-1,-1) None com

-- main pro Gloss play
displayScreen initialWorld=playIO window (makeColorI 255 255 255 0) 30 initialWorld drawWorld handleEvent updateWorld
