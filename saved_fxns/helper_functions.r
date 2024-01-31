##============================================================================
## script modified from https://github.com/GLEON/LakeMetabolizer/tree/main

# ================================
# = Calculate sampling frequency =
# ================================
calc.freq <- function(datetime){
  freq <- round(Mode(1/diff(date2doy(datetime))))
}
# ==================
# = Calculate Mode =
# ==================
#RDB
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
date2doy <- function(x){
  stopifnot(any(grepl("^POSIX",class(x[1]))))
  day <- as.numeric(format(x, "%j"))
  pat <- quote("([0-9]{2}:){2}[0-9]{2}")
  midnight <- as.POSIXct(gsub(pat, "00:00:00", x), tz="GMT")
  frac <- as.numeric(difftime(x, midnight, units="days"))
  day+frac
}
#function to remove points
identifyPch <- function(x, y = NULL, n = 2, pch = 19, ...)
{
  xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
  sel <- rep(FALSE, length(x)); res <- integer(0)
  while(sum(sel) < n) {
    ans <- identify(x[!sel], y[!sel], n = 1, plot = FALSE, ...)
    if(!length(ans)) break
    ans <- which(!sel)[ans]
    points(x[ans], y[ans], pch = pch)
    sel[ans] <- TRUE
    res <- c(res, ans)
  }
  res
}
