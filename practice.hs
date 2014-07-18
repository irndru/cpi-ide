
import Data.Default.Class
import Graphics.UI.Gtk.Misc.DrawingArea
import System.Random

import Graphics.Rendering.Chart
import Graphics.UI.Gtk 
import Graphics.Rendering.Chart.Layout as Lay
--import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Colour
--import Data.Accessor
import Control.Lens
import qualified Control.Exception as X
import qualified Numeric.LinearAlgebra as LA
import CPi.Lib
import CPi.MyGTKRender

main = do 
		renderableToWindow (chart 0.25) 200 200
		renderableToWindow (toRenderable testLayout) 640 480


-- Takes data from the ODE solver and plots them
plotTimeSeries :: LA.Vector Double -> LA.Matrix Double -> [Species] -> IO ()
plotTimeSeries ts soln ss = plot (LA.toList ts) (zip (map pretty ss) (map LA.toList (LA.toColumns soln)))


-- Plots the time series in a GTK window
plot :: [Double] -> [(String,[Double])] -> IO ()
plot ts dims = renderableToWindow (toRenderable (layout ts dims)) 640 480


-- gets a plot layout with plots for each dimension
layout ts dims = layout_plots .~  (plots ts (colours (length dims)) dims)
	       $ def


plots _ _ [] = []
plots ts (colour:cs) ((lbl,pts):dims) =( toPlot  
				      $	plot_lines_style .~ solidLine 1 colour 
				      $ plot_lines_values .~ [zip ts pts] 
				      $ plot_lines_title .~ lbl 
				      $ def) : plots ts cs dims
plots _ [] _ = X.throw $ CpiException 
               "CPi.Plot.plots: Run out of colours!"




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



