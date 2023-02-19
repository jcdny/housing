library(tictoc)
tic("extract.R")
library(plyr)
library(tidyverse)
library(jsonlite)
library(pathmapping)

measure <- function(lon1,lat1,lon2,lat2) {
    ## distance in meters between 2 coord.

    R <- 6378.137                                # radius of earth in Km
    dLat <- (lat2-lat1)*pi/180
    dLon <- (lon2-lon1)*pi/180
    a <- sin((dLat/2))^2 + cos(lat1*pi/180)*cos(lat2*pi/180)*(sin(dLon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    d <- R * c
    return (d * 1000)                            # distance in meters
}

INFILE <- "data/marin-geojson.json.bz2"
#INFILE <- "data/test.json"
js <- fromJSON(read_file(INFILE))

## pull out data and fix up some fields
pr.df <- js$features$properties
for (f in c("jurisdiction", "usedescription", "zipcode","usecd")) {
    pr.df[[f]] <- factor(pr.df[[f]])
}
for (f in c( "shape_stlength","avgslope","shape_starea")) {
    pr.df[[f]] <- as.numeric(pr.df[[f]])
}
for (f in c("objectid", "livingunits")) {
    pr.df[[f]] <- as.integer(pr.df[[f]])
}

pr.geom <- llply(js$features$geometry$coordinates, function(x) matrix(x,,2))

## ldply(pr.geom, nrow)

## matrix(js$features$geometry$coordinates[3][[1]],,2)

toc()
## 50x100 75x125

ii <- grep("Family|Resid", pr.df$usedescription)
ggplot(pr.df[ii,],aes(x=shape_starea)) + facet_wrap(~ usedescription) + geom_histogram(breaks=seq(0,25000,100))
