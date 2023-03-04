# Littoral-Lake-Metabolism
Modeling ecosystem energetics in the littoral or near shore of Lake Tahoe.

## R script names
- "NS" referes to a cinder block sensor deployment in either position 1, 2, or 3 at either Blackwood "BW" or Glenbrook "GB"

  - step 1: cleans and aggregates the miniDOT DO data, validation data for PAR extinction, local weather, and lake profiles for water temperature and DO from either a handheld YSI pro plus or an RBR multi probe sensor.
  - step 2: formats the data into the stan model list and calculates some of the model paramters
  - step 3 or model: deploys the lake analyzer model in stan.  

## Refs
- Lotting2021: the paper that the mode was built for
- Phillips2019: paper that our model was base on
- Rose2009: best guess for par exction coeff data 

