module ChartData where

{-

Important changes (for now) with respect to the Chart library: 

* FontStyle:  Left out the option to change the font style (so many options, may be
              included in the future) 

* PointStyle: Left out the option PointShape. 

* LineStyle:  Left out the options LineCap and LineJoin. 

* FillStyle:  Changed this directly into Color.

* Color:      For now only has the options red, blue, white and black, but should be
              changed to a numerical value in the future.

* Plotbars:   Left out the spacing and allignment options.

* AreaSpots:  Ignored the 4D option in areaspots.

* Annotation: Did not add all style options that are in the Chart library, only added
              the option to add simple string annotations.

* Pie:        Added color in PieItem constructor instead of in a separate list.

-}

data Settings x y = Settings { 
   inputData  :: InputData x y,
   graphType  :: GraphType,
   title      :: Maybe String,
   properties :: [PropertyType],
   output     :: OutputType
  }
  
data GraphType = Pie | Vector | Points | Lines | Histogram | Fill | ErrorBars
               | Hidden | Candle | Bars | Spots | Annotation | Types

data OutputType = SVG | PNG | PS

data InputData x y = PlotPoints [(x,y)]
                     | FillData [(x,(y,y))]
                     | VectorData x y
                     | LinesData (LinesData x y)
                     | HistoData x y
                     | ErrorData [ErrorPoint x y]
                     | HiddenValues x y
                     | CandleData x y
                     | BarsData x y
                    --  | SpotsData [(x,y,z)]
                     | AnnotationData [(x,y,String)]
                     | PieData [PieItem]
                     | TypesData ([x],[y])

{-
A vector plot can have three different types of input data, of which only the
input values are required. Grid provides a square-tiled regular grid and mapf
provides a vector field R2->R2 function. 
-}                   
data VectorData x y = VectorData' {
                        grid :: Maybe [(x,y)],
                        mapf :: Maybe ((x,y) -> (x,y)),
                        v_values :: [((x,y),(x,y))]
                        }
{-
values: The lines to be plotted.
limit_values: Additional lines to be plotted specified unsing the Limit type 
              to allow referencing the edges of the plot area.
-}
data LinesData x y = LinesData' {
                       l_values :: [[(x,y)]],
                       limit_values :: Maybe [[(x, y)]]
                       }

{- 
Everything related to the input data of a histogram. The last four attributes
are all optional. 
-}
data HistoData x y = HistoData' {
                      bins        :: Int,
                      hist_values :: [x],
                      no_zeros    :: Maybe Bool,
                      range       :: Maybe (x,x),
                      drop_lines  :: Maybe Bool,
                      norm_func   :: Maybe (Double -> Int -> y) 
                      }
                      
data ErrorPoint x y = ErrorPoint {
                        err_x :: ErrorValue x,
                        err_y :: ErrorValue y
                        }
                        
data ErrorValue x = ErrorValue' {
                      low  :: x,
                      best :: x,
                      high :: x
                      }

{- Values used for scaling axis -}
data HiddenValues x y = HiddenValues' {
                            x_values :: [x],
                            y_values :: [y]
                            }

{- All required fields for a candle data plot. -}
data CandleData x y = CandleData' {
                           candle_x     :: x,
                           candle_low   :: y,
                           candle_open  :: y,
                           candle_mid   :: y,
                           candle_close :: y,
                           candle_high  :: y
                           }
{- All Data fields for a Bars plot:
item_styles: style for each bar.
titles:      title of each bar. Will be shown in a legend.
reference:   starting level of the chart.
singleton_w: width of the plotted bars.
-}
data BarsData x y = BarsData' {
                       bars_values     :: [(x,[y])],
                       item_styles     :: Maybe [(Color, Maybe LineStyle)], 
                       titles          :: Maybe [String], 
                       reference       :: Maybe y,        
                       singleton_width :: Maybe Double    
                       }
                       
data PieItem = PieItem {
                    label            :: String,
                    pie_color        :: Maybe Color,
                    offset           :: Double,
                    value            :: Double,
                    label_line_style :: Maybe LineStyle
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
                    
data VectorStyle = VectorStyle' { 
                      lineStyle  :: Maybe LineStyle,
                      pointStyle :: Maybe PointStyle
                      }  
                      
data CandleStyle = CandleStyle' {
                           fill            :: Bool,
                           rise_fill_style :: Maybe Color,
                           fall_fill_style :: Maybe Color
                           }
                           
data SpotsStyle = SpotsStyle' {
                           linethick  :: Maybe Double,
                           linecolor  :: Maybe Color,
                           fillcolor  :: Maybe Color,
                           opacity    :: Maybe Double,
                           max_radius :: Maybe Double
                           }                      
                           
data PieLayout = PieLayout' {
                         background :: Maybe Color,
                         margin     :: Maybe Double
                         }
                           
data BarsStyle = Stacked | Clustered                          

data Color = Red | Blue | Black | White                  
                 
data Limit a = LMin | LValue a | LMAx