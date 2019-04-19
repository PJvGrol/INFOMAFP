# INFOMAFP
Project for the Advanced Functional Programming

# Commands
You can type ```ChartWrapper --help``` for a description of all optionas available to the ChartWrapper.
Here is a small demonstration for generating a bar graph as PNG from a Json file:

Json file example:
```
{
  "inputData": "[(3,[3,1,2]),(1,[1,7,8]),(1,[4,3,2])]",
  "graphType": "bars",
  "title": "Bar Example",
  "properties" : { "titles" : "[\"bar_1\", \"bar_2\", \"bar_3\"]"},
}
```
Command:
```
./ChartWrapper -i bars_correct.json -o bars.png
```

Generates:
![alt text](https://github.com/PJvGrol/INFOMAFP/blob/master/bars.png)

# Setup Instructions
You might need to install the Chart library if you want to build ChartWrapper.

Instructions can be found here: https://github.com/timbod7/haskell-chart/wiki
Then you can simply run:
``` 
cabal install
cabal build
```

You will find the executable in the dist/build/ChartWrapper.
