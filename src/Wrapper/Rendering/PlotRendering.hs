module Wrapper.Rendering.PlotRendering (render) where

import qualified Graphics.Rendering.Chart.Renderable as R
import qualified Graphics.Rendering.Chart.Backend.Cairo as BEC
import Graphics.Rendering.Chart.Plot.Lines
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Plot.Bars

import Control.Lens
import Data.Maybe
import Data.Default.Class

import Wrapper.ChartData

render :: Settings Double Double -> R.Renderable()
render = R.toRenderable . toLayout

toLayout :: Settings Double Double -> Layout Double Double
toLayout settings = layout_title .~ plotTitle
                    $ layout_plots .~ plots
                    $ def
    where
        plots = [transform' settings]
        plotTitle = fromMaybe "" (title settings)

transform' :: Settings Double Double -> Plot Double Double
transform' settings = case graphType settings of
    Lines -> toPlot $ plotLines settings
    Bars -> plotBars $ plotBars' settings
    _ -> error "Unknown GraphType"

plotLines :: Settings Double Double -> PlotLines Double Double
plotLines settings = plot_lines_title .~ plotTitle 
                        $ plot_lines_values .~ plotData
                        $ def
    where
        plotTitle = fromMaybe "" (title settings)
        plotData = case inputData settings of
            LinesData ldata -> l_values ldata

plotBars' :: Settings Double Double -> PlotBars Double Double
plotBars' settings = case inputData settings of
    BarsData ldata -> plot_bars_titles .~ fromMaybe [] (titles ldata)
                      $ plot_bars_values .~ bars_values ldata
                      $ def
    _ -> error "Invalid input data"