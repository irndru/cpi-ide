-- Example of an drawing graphics onto a canvas.
import Data.Default.Class
import Graphics.UI.Gtk.Misc.DrawingArea
import System.Random
import Prelude
import Graphics.Rendering.Chart
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk 
import Graphics.Rendering.Chart.Layout as Lay
--import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Colour
import Control.Monad
--import Data.Accessor
import Control.Lens
import qualified Control.Exception as X
import qualified Numeric.LinearAlgebra as LA
import CPi.Lib
import CPi.MyGTKRender
import System.IO
    ( IOMode(ReadMode, WriteMode),
      hPutStrLn,
      hGetContents,
      openFile,
      hClose )
import System.IO.Unsafe
import qualified System.Glib.Attributes as A
import Control.Concurrent.MVar
    ( MVar,
      takeMVar,
      putMVar,
      newMVar,
      newEmptyMVar,
      readMVar,
      modifyMVar_,
      modifyMVar )



main = do
	

	let values = [[0..100],(reverse [0..100])]

	--print $ show values
	initGUI
	dia <- dialogNew
	dialogAddButton dia stockOk ResponseOk
	contain <- dialogGetContentArea dia
	canvas <- drawingAreaNew
	widgetSetSizeRequest canvas  400 400
	ctxt <- cairoCreateContext Nothing
	text <- layoutEmpty ctxt


	{--let values = [[0,50,100],[25,75,0]]
		
	let tstep = timestep "0" "10" "100"
	let result = map (addTimes tstep (fromIntegral(read "0")) (length(values!!(0))) (length(values!!(0))) ) values
	let adjusted = map (adjustTimesForArea 400 400 10) result
	

	--let listOfValues = [(0,0),(10,10),(20,20),(30,30),(40,40),(50,50),(60,60)]
	--text `layoutSetText` "Hello World."--}
	print "A"
	let renderbrackets1 = render (chart 0.25) (fromIntegral 400, fromIntegral 400) 
	let renderbrackets2 = render (toRenderable testLayout) (fromIntegral 640,  fromIntegral 480)
	let cairobackend = (defaultEnv bitmapAlignmentFns)
	A.set canvas [maybeFigure :=  (Just (runBackend cairobackend ( void renderbrackets1)))]
	
	on canvas draw $ runBackend cairobackend ( void renderbrackets1)
	on canvas draw $ runBackend cairobackend ( void renderbrackets2)
	print "B"
	
	(Just redr) <- get canvas maybeFigure
	print "C"
	on canvas draw redr
	boxPackStart (castToBox contain) canvas PackGrow 0
	widgetShow canvas
	dialogRun dia
	return ()


getandset :: (Render a) -> DrawingArea -> IO (Render a)
getandset f da = do
			A.set da [ maybeFigure := (Just f)]
			(Just fr) <- get da maybeFigure
			return fr
 
{-- | the figure attribute
figure :: Attr DrawingArea (Render a)
figure = newAttr getFigure setFigure
   where getFigure o = do
              Just f <- get o maybeFigure 
              readMVar f 
         setFigure o f = A.set o [maybeFigure :~> (\(Just h) -> do
              modifyMVar_ h (\_ -> return f)
              return $ Just h)]
--}
maybeFigure :: Attr DrawingArea (Maybe (Render a) )
maybeFigure = unsafePerformIO $ objectCreateAttribute
--{-# NOINLINE maybeFigure #-}



{--
getWidths :: [Double] -> Double -> [[(Double,Double)]]
getWidths vls ht =  do 		
			let gap = ( fromIntegral ht ) / ( fromIntegral(length vls))
			let pairsBy = (zip [0,gap..ht] vls)
			return pairsBy

addTimes vs2 timestep (fromIntegral(read tabstarttemp)) (length(vs2)) (length(vs2))

timestep :: String -> String -> String -> Double
timestep tabstarttemp tabendtemp tabpointstemp =  (fromInteger $ round $ (((read tabendtemp)-(read tabstarttemp))/((read tabpointstemp)-1)) * (10^2)) / (10.0^^2)

addTimes ::  Double -> Double -> Int -> Int -> [Double] -> [(Double,Double)]
addTimes  _ _ _ _ []  = []
addTimes  ts strt n i (x:xs) = do
				let temp = ((( (fromIntegral(n - i))*ts) + strt) , x) 
				temp : addTimes  ts strt n (i-1) xs
adjustTimesForArea _ _ _ [] = []
adjustTimesForArea dw dh ts ((x,y):xys) = (((dw/ts)*x),y): adjustTimesForArea dw dh ts xys


newLineTo :: Double -> (Double,Double) ->  Render()
newLineTo height (x,y)  = lineTo x (height-y)

drawLine ht lv = do 
			mapM_ (newLineTo ht ) $ lv
			stroke   

updateCanvas :: WidgetClass widget => widget -> PangoLayout -> [[(Double,Double)]] -> Render ()
updateCanvas canvas text listOfValues = do
  width'  <- liftIO $ widgetGetAllocatedWidth  canvas
  height' <- liftIO $ widgetGetAllocatedHeight canvas
  let width  = realToFrac width'
      height = realToFrac height'


  
  

  setSourceRGB 1 0 0
  setLineWidth 1


  moveTo 0 height
  mapM_ (drawLine height) listOfValues
  stroke


  setSourceRGB 1 1 0
  setLineWidth 1

  save
  translate (width / 2) (height / 2)
  scale (width / 2) (height / 2)
  arc 0 0 1 (135 * pi/180) (225 * pi/180)
  restore
  stroke

  setSourceRGB 0 0 0
  moveTo 30 (realToFrac height / 4)
  rotate (pi/4)
  showLayout text
--}

chart :: Double -> Renderable ()
chart lwidth = toRenderable (clayout 1001 (trial bits) :: Lay.Layout Double LogValue)
  where
    bits = randoms $ mkStdGen 0

    clayout n t = layout_title .~ "Simulation of betting on a biased coin"
               $ layout_plots .~ [
                      toPlot (cplot "f=0.05" s1 n 0 (t 0.05)),
                      toPlot (cplot "f=0.1" s2 n 0 (t 0.1))
                     ]
               $ def

    cplot tt s n m t = plot_lines_style .~ s
                 $ plot_lines_values .~
                       [[(fromIntegral x, LogValue y) | (x,y) <-
                         filter (\(x,_)-> x `mod` (m+1)==0) $ take n $ zip [0..] t]]
                 $ plot_lines_title .~ tt
                 $ def

    b = 0.1

    trial bits frac = scanl (*) 1 (map f bits)
      where
        f True = (1+frac*(1+b))
        f False = (1-frac)

    s1 = solidLine lwidth $ opaque green
    s2 = solidLine lwidth $ opaque blue



testT = [0.0,0.1..2500.0]::[Double]
testD1 = [0,0.1..2500]::[Double]
testD2 = [x*x|x<-[0,0.1..2500]]::[Double]

testPlot1 = plot_lines_style .~ solidLine 1 (opaque $ sRGB 0.5 0.5 1)
            $ plot_lines_values .~ [zip testT testD1]
            $ plot_lines_title .~ "test1"
            $ def

testPlot2 = plot_lines_style .~ solidLine 1 (opaque red)
            $ plot_lines_values .~ [zip testT testD2]
            $ plot_lines_title .~ "test2"
            $ def

testLayout = layout_title .~"Test graph!"
             $ layout_plots .~ [(toPlot testPlot1)]
             $ def

colours :: Int -> [AlphaColour Double]
colours n
    | n<=0 = []
    | n==1 = [clr 0 1 0]
    | n==2 = [clr 0 1 0,clr 0 1 1]
    | n==3 = [clr 0 1 0,clr 0 1 1,clr 0 0 1]
    | n==4 = [clr 0 1 0,clr 0 1 1,clr 0 0 1,clr 1 0 1]
    | n==5 = [clr 0 1 0,clr 0 1 1,clr 0 0 1,clr 1 0 1,clr 1 0 0]
    | n==6 = [clr 0 1 0,clr 0 1 1,clr 0 0 1,clr 1 0 1,clr 1 0 0,clr 0 0 0]
    | otherwise = sec 1 ++ sec 2 ++ sec 3 ++ sec 4 ++ sec 5
    where
      s = fromIntegral(n `div` 5)
      e = fromIntegral(n `mod` 5)
      f x y
          | x<=y = 1.0
          | otherwise = 0.0
      g x = [1..(s+(f x e))]
      sec x
          | x==1 = [clr 0 1 ((m-1)/(s+(f x e)-1)) | m<-g x]
          | x==2 = [clr 0 ((s+(f x e)-m)/(s+(f x e))) 1 | m<-[1..(s+(f x e))]]
          | x==3 = [clr (m/(s+(f x e))) 0 1 | m<-[1..(s+(f x e))]]
          | x==4 = [clr 1 0 ((s+(f x e)-m)/(s+(f x e))) | m<-[1..(s+(f x e))]]
          | x==5 = [clr ((s+(f x e)-m)/(s+(f x e))) 0 0 | m<-[1..(s+(f x e))]]
          | otherwise = undefined

clr :: Double -> Double -> Double -> AlphaColour Double
clr r g b = opaque(sRGB r g b)

