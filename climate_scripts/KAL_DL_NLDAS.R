
# API route ##
# still need to put parameter requests


#' Downloads NLDAS light data
#' @description This function downloads NLDAS incoming shortwave radiation data
#' (w m-2) for a given Latitude and Longitude.
#'
#' @param save_dir The save directory for files to be placed in. For example, "C:/myfolder
#' @param Site_ID The site ID, for example "NC_NHC"
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#' @param startDate The starting date for the download (YYYY-MM-DD)
#'
#' @return Returns a time series of incoming shortwave solar radiation from the start
#' date to the most recent available data
#' @export

#===============================================================================
#Function for downloading NLDAS light data via data rods
#https://disc.gsfc.nasa.gov/information/tools?title=Hydrology%20Data%20Rods
#Created 01/19/2024
#===============================================================================
## LIGHT ##
NLDAS_DL <- function(save_dir, Site_ID, Lat, Lon, startDate, endDate){
  #The initial string to build the URL
  http_string <- paste("https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi?variable=NLDAS:NLDAS_FORA0125_H.002:DSWRFsfc")

  #Separating the date information
  start_split <- strsplit(startDate, "-")[[1]]
  end_split <- strsplit(endDate, "-")[[1]]

  #Build individual components of the url
  location_string <- paste0("&location=GEOM:POINT(", Lon, ",%20", Lat, ")")
  start_string <- paste0("&startDate=", start_split[1], "-", start_split[2], "-",
                         start_split[3], "T00")
  end_string <- paste0("&endDate=", end_split[1], "-", end_split[2], "-",
                       end_split[3], "T00")
  #Generating the URL
  url <-paste0(http_string, location_string, start_string, end_string, "&type=asc2")

  #Downloading the data
  destfile <- paste(save_dir, "/", Site_ID, "_NLDAS.asc", sep = "")

  #Error catch in case the page is inaccessible. A little inelegant at present...
  try_result <- try(download.file(url, destfile, method = "libcurl"), silent = FALSE)

  if(class(try_result) == "try-error") {file.remove(destfile)}

} #End DL_NLDAS function

# call dat for 1 site at time
NLDAS_DL(
  save_dir = "~/Documents/LittoralMetabModeling/RawData/NLDAS/",
  Site_ID = "BWL",
  Lat = "39.1075",
  Lon = "-120.16468",
  startDate = "2020-01-01",
  endDate = "2023-11-01"
)


# call dat for 1 site at time
NLDAS_DL(
  save_dir = "~/Documents/LittoralMetabModeling/RawData/NLDAS/",
  Site_ID = "BWU",
  Lat = "39.1053",
  Lon = "-120.19551",
  startDate = "2020-01-01",
  endDate = "2023-11-01"
)


NLDAS_DL(
  save_dir = "~/Documents/LittoralMetabModeling/RawData/NLDAS/",
  Site_ID = "BWNS2",
  Lat = "39.10697",
  Lon = "-120.15721",
  startDate = "2020-01-01",
  endDate = "2023-11-01"
)


NLDAS_DL(
  save_dir = "~/Documents/LittoralMetabModeling/RawData/NLDAS/",
  Site_ID = "SSNS2",
  Lat = "39.13904",
  Lon = "-120.15236",
  startDate = "2020-01-01",
  endDate = "2023-11-01"
)


NLDAS_DL(
  save_dir = "~/Documents/LittoralMetabModeling/RawData/NLDAS/",
  Site_ID = "SHNS2",
  Lat = "39.0944",
  Lon = "-119.9436",
  startDate = "2020-01-01",
  endDate = "2023-11-01"
)


NLDAS_DL(
  save_dir = "~/Documents/LittoralMetabModeling/RawData/NLDAS/",
  Site_ID = "GBNS2",
  Lat = "39.0880",
  Lon = "-119.9421",
  startDate = "2020-01-01",
  endDate = "2023-11-01"
)
