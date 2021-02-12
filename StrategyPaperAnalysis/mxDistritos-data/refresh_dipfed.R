# original dipfed files from emagars project
# run this to pull in new versions

library(tidyverse)

url.file.2018 <- "https://raw.githubusercontent.com/emagar/mxDistritos/master/data/dipfed-seccion-vraw-2018.csv"
url.file.2015 <- "https://raw.githubusercontent.com/emagar/mxDistritos/master/data/dipfed-seccion-vraw-2015.csv"

download.file(url.file.2018,"raw-seccion-2018.csv")
download.file(url.file.2015,"raw-seccion-2015.csv")


