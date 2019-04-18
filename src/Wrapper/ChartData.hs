module Wrapper.ChartData where

{-
-}
data Settings x y = Settings { 
   inputData  :: InputData x y,
   graphType  :: GraphType,
   title      :: Maybe String,
   properties :: [PropertyType],
   output     :: OutputType
  }
  
data GraphType = Pie | Points | Lines | ErrorBars | Bars

data OutputType = SVG | PNG | PS

data InputData x y = PlotPoints [(x,y)]
                   | LinesData (LinesData x y)
                   | BarsData (BarsData x y)
                   | PieData [PieItem]
   
{-
values: The lines to be plotted.
limit_values: Additional lines to be plotted specified unsing the Limit type 
              to allow referencing the edges of the plot area.
-}
data LinesData x y = LinesData' {
                       l_values :: [[(x,y)]],
                       limit_values :: Maybe [[(x, y)]]
                       }
                                              
{- All Data fields for a Bars plot:
item_styles: style for each bar.
titles:      title of each bar. Will be shown in a legend.
reference:   starting level of the chart.
singleton_w: width of the plotted bars.
-}
data BarsData x y = BarsData' {
                       bars_values     :: [(x,[y])],
                       item_styles     :: Maybe [Color], 
                       titles          :: Maybe [String], 
                       reference       :: Maybe y,        
                       singleton_width :: Maybe Double    
                       }
   
{- Left out line style -}
data PieItem = PieItem {
                    label            :: String,
                    pie_color        :: Maybe Color,
                    offset           :: Double,
                    value            :: Double
                    }
                           
data PropertyType = Border Color
                      | Width Double
                      | Centre Double
                      | PointStyle PointStyle
                      | LineStyle LineStyle
                      | Scale Double
                      | FillStyle Color
                      | TickLength Double
                      | Overhang Double 
                      | CandleStyle CandleStyle
                      | BarsStyle BarsStyle
                      | SpotsStyle SpotsStyle
                      | PieLayout PieLayout
                  
data PointStyle = PointStyle' { 
                    color       :: Maybe Color,
                    borderColor :: Maybe Color,
                    borderWidth :: Maybe Double,
                    pointRadius :: Maybe Double
                    }

data LineStyle = LineStyle' {
                    lineWidth  :: Maybe Double, 
                    lineColor  :: Maybe Color,
                    lineDashes :: Maybe [Double]
                           }

data PieLayout = PieLayout' {
                         background :: Maybe Color,
                         margin     :: Maybe Double
                         }
                          
data Color = Red | Blue | Black | White  deriving Eq            
                 
data Limit a = LMin | LValue a | LMAx