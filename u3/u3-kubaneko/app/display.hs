import           Control.Monad
import           Data.Bifunctor                       (second)
import           Data.List
import           Data.Tuple
import qualified Data.Vector                          as V
import           GHC.Float
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact

{-
 - Konstanty a ostatní vlastnosti programu
 -}

-- okno pro zobrazeni mapy
window=InWindow "Pixel Drawer" (sizep*nump,sizep*nump) (0,0) :: Display
-- pocet obarvitelnych pixelu
nump=32
-- pocet pixelu v pixelu :)
sizep=20

-- struktura představuje stav programu
data PixMap=PixMap {picture  :: V.Vector(V.Vector Tile),
                    selected :: (Int,Int),
                    from     :: (Int,Int),
                    geom     :: GeomObjects}
-- počáteční stav
initialWorld=PixMap (V.replicate nump (V.replicate nump Transp)) (0,31) (-1,-1) None

-- představuje co se s vybraným objektem stane
data Actions=Inverse | Fill deriving(Eq)

-- Specifikuje vybraný geometrický objekt
data GeomObjects=None | PixRectangle | PixLine deriving(Eq)

-- stav daného pixelu a jemu příslušná barva
data Tile=Transp | Black | Gray | Stressed | Selected | Error deriving(Eq)

-- barvy použité pro pixely - na obrázku se vyskytují jen Black, Gray, Transp - zbytek je návodný
getColour Error=   makeColorI 0 0 255 255
getColour Stressed=makeColorI 0 255 0 255
getColour Selected=makeColorI 255 0 0 255
getColour Black=   makeColorI 0 0 0 255
getColour Gray=    makeColorI 122 122 122 255
getColour Transp=  makeColorI 255 255 255 0

-- inverse na Pixely na obrázku
invertColour Gray=  Black
invertColour Black= Gray
invertColour Transp=Transp

{-
 - Sekce měnící stav
 -}

-- aplikuje tvoří changes pro daný stav pro souřadnice pro daný geometrický útvar
getGeomChanges pix pxm action
    | action==Inverse =inverseChanges pix pxm $getGeomIndexes (selected pxm) (from pxm) (geom pxm)
    | action==Fill =fillChanges pix pxm $getGeomIndexes (selected pxm) (from pxm) (geom pxm)

-- Vyrobí pro obecné zadání ve tvaru (x,(y,Pixel) změny, které mají tvar [(x,[(y,Pixel)])] kde x,y jsou souřadnice tedy Int
makeChangesG changes=(\k -> (fst (head k), snd <$> k))  <$> groupBy (\a b-> fst a == fst b) changes

-- konkrétnější forma makeChangesG, která vezme množinu indexů a pixel a vrátí changes [(x,[(y,Pixel)])]
fillChanges pix _ changes=( \k -> (fst (head k), (\l-> (snd l, pix)) <$> k))  <$> groupBy (\a b-> fst a == fst b) changes

-- funkce, která dostane PixMap a indexy geom objektu a na vyrobí changes s invertovanými barvami pro dané indexi
inverseChanges _ pxm indexes=makeChangesG ((\l -> (fst l, (snd l,invertColour$(picture pxm V.! fst l) V.! snd l))) <$> indexes)

-- změní geometrický objekt a jeho začátek ve stavu Světa
changeGeom pxm=PixMap (picture pxm) (selected pxm)

-- změní zvolený pixel
changeSelect pxm newsel=PixMap (picture pxm) newsel (from pxm) (geom pxm)

-- dostane changes a stav a vrátí stav s obrázkem přepsaný s changes
changeWorld pxm changes=PixMap (picture pxm V.// ((\k -> second ((picture pxm V.! fst k) V.//) k) <$> changes)) (selected pxm) (from pxm) (geom pxm)

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
drawGeom pxm pix action=PixMap (picture$changeWorld pxm (getGeomChanges pix pxm action)) (selected pxm) (-1,-1) None

-- vykreslí stav na obrazovku
drawWorld pxm = pictures$V.toList(V.imap (\x k-> pictures.V.toList$V.imap (drawPixel rectangleSolid x) k) (picture pxm))  ++
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
handleEvent (EventKey (Char 'c') Down _ _) pxm = changeGeom pxm (-1,-1) None
handleEvent (EventKey (Char 'l') Down _ _) pxm = changeGeom pxm (selected pxm) PixLine
handleEvent (EventKey (Char 'r') Down _ _) pxm = changeGeom pxm (selected pxm) PixRectangle
handleEvent (EventKey (Char 'i') Down _ _) pxm=drawGeom pxm Gray Inverse
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) pxm = drawGeom pxm Transp Fill
handleEvent (EventKey (Char 'z') Down _ _) pxm = drawGeom pxm Black Fill
handleEvent (EventKey (Char 'x') Down _ _) pxm = drawGeom pxm Gray Fill
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) pxm = changeSelect pxm (fst (selected pxm), max 0 $ snd (selected pxm) -1)
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) pxm = changeSelect pxm (fst (selected pxm), min (nump-1) $ snd (selected pxm) +1)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) pxm = changeSelect pxm (min (nump-1) $ fst (selected pxm) +1, snd (selected pxm))
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) pxm = changeSelect pxm (max 0 $ fst (selected pxm) -1, snd (selected pxm))
handleEvent _ n = n

--funkce pravidelně updatující stav
updateWorld _ = id

-- main pro Gloss play
main = play window (makeColorI 255 255 255 0) 30 initialWorld drawWorld handleEvent updateWorld
