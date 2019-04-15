module Wrapper.Rendering.PlotRendering (render, tRender) where

import qualified Graphics.Rendering.Chart.Renderable as R
import qualified Graphics.Rendering.Chart.Backend.Cairo as BEC
import Graphics.Rendering.Chart.Plot.Lines
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis.Types

import Control.Lens
import Data.Maybe
import Data.Default.Class

import Wrapper.ChartData

{-
Render a graph based on its settings.
-}
tRender = BEC.renderableToFile def "test.png" . render

render :: Settings Double Double -> R.Renderable()
render = R.toRenderable . toLayout

toLayout :: Settings Double Double -> Layout Double Double
toLayout settings = layout_title .~ "TODO" 
                    $ layout_plots .~ plots
                    $ def
    where
        plots = [transform' settings]

transform' :: Settings Double Double -> Plot Double Double 
transform' settings = case graphType settings of
    Lines -> toPlot $ plotLines settings
    _ -> error "Unknown graph type"

plotLines :: Settings Double Double -> PlotLines Double Double
plotLines settings = plot_lines_title .~ plotTitle 
                        $ plot_lines_values .~ plotData
                        $ def
    where
        plotTitle = fromMaybe "" (title settings)
        plotData = case inputData settings of
            LinesData ldata -> l_values ldata
            _ -> error "Invalid input data" 