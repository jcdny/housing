library(tictoc)
tic("extract.R")
library(tidyverse)
library(jsonlite)

INFILE <- "data/marin-geojson.json.bz2"
INFILE <- "data/test.json"
js <- fromJSON(read_file(INFILE))

## matrix(js$features$geometry$coordinates[3][[1]],,2)

toc()
