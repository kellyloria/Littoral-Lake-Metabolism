# Littoral-Lake-Metabolism
Modeling ecosystem energetics in the littoral or near shore of Lake Tahoe.

## R script names
- "NS" referes to a cinder block sensor deployment in either position 1, 2, or 3 at either Blackwood "BW" or Glenbrook "GB"

  - step 1: cleans and aggregates the miniDOT DO data, validation data for PAR extinction, local weather, and lake profiles for water temperature and DO from either a handheld YSI pro plus or an RBR multi probe sensor.
  - step 2: formats the data into the stan model list and calculates some of the model paramters
  - step 3 or model: deploys the lake analyzer model in stan.  

## Refs
- Lotting et al. 2021: Light transformations and mixing dynamic https://aslopubs.onlinelibrary.wiley.com/doi/epdf/10.1002/lom3.10471
- Rose et al. 2009: best guess for par exction coeff data https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2008JG000816
- Metabolism implementation - Scordo et al. 2022 https://agupubs.onlinelibrary.wiley.com/doi/epdf/10.1029/2021WR031094)
- Model structure - Phillips 2019: https://aslopubs.onlinelibrary.wiley.com/doi/10.1002/lno.11333
