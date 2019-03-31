module Transformer where

import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Plot.Lines
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Backend.Cairo

import Control.Lens
import Data.Maybe
import Data.Default.Class

import ChartData

newtype TSettings x y = P (Plot x y)

test = Settings { 
    inputData = LinesData testData,
    graphType = Lines,
    title = Just "Test",
    properties = [],
    output = ChartData.SVG
}

testData = LinesData' {
    l_values = [[(0.0,0.0)]],
    limit_values = Just [[]]
}

main = renderableToFile def "test.png" (renderGraph test)

renderGraph :: Settings Double Double -> Renderable()
renderGraph = toRenderable . toLayout

toLayout :: Settings Double Double -> Layout Double Double
toLayout settings = layout_title .~ "TODO" $ def
    -- where
    --     plots = [plot settings]
    --     tSettings = convert settings

convert :: Settings Double Double -> TSettings Double Double 
convert settings = case graphType settings of
    Lines -> P (plot settings)
    _ -> error "Unknown graph type"

plot :: Settings Double Double -> Plot Double Double
plot settings = toPlot plotLines
    where
        plotLines :: PlotLines Double Double
        plotLines = plot_lines_title .~ plotTitle $ def
        plotTitle = fromMaybe "" (title settings)

-- Values to add to Settings
-- LayoutTitle String, 