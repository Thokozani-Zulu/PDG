install.packages("rgdal")
install.packages("sf")
install.packages("maptools")
install.packages("raster")
install.packages("rgeos")
install.packages("sp")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("viridis")
install.packages("RColorBrewer")

require(rgdal)

library(rgdal)
library(maptools)
library(raster)
library(sp)
library(ggplot2)
library(RColorBrewer)
library(plyr)

# ENTER THE DBF THAT ACCOMPANIES THE SHAPEFILE OR THE SHAPEFILE LAYER
wards2016Map <- readOGR(dsn=choose.files(),layer="Wards_2016_Cape_Town" )
wards2016Frame <- fortify(wards2016Map, region = "WARD_NAME")

plot(wards2016Map)

# INSERT THE GROWTH DATA WITH TOTAL GROWTH CSV FILE
growth <-read.csv(choose.files(),header=TRUE,sep=";",)
ward_growth <- merge(growth,wards2016Map,by="WARD_NAME")

# WHAT COLUMNS DO WE HAVE?
colwise(class)(ward_growth)

ggplot() + geom_map(data = ward_growth, aes(map_id = WARD_NAME, fill = TOT_GROWTH), color="grey10",
                    map = wards2016Frame) + expand_limits(x = wards2016Frame$long, y = wards2016Frame$lat)

f <- function(x, output){
  if (is.na(x[4])) 0 else
  if (x[4] > 90) 100 else
  if (x[4] > 80) 90 else
  if (x[4] > 70) 80 else
  if (x[4] > 60) 70 else
  if (x[4] > 50) 60 else
  if (x[4] > 40) 50 else
  if (x[4] > 30) 40 else
  if (x[4] > 20) 30 else
  if (x[4] > 10) 20 else
  if (x[4] > 0) 10 else 0
}

ward_growth$TOT_GROWTH_AMOUNT <- apply(ward_growth,1, f)
class(ward_growth$TOT_GROWTH_AMOUNT)

ggplot() + geom_map(data = ward_growth, aes(map_id = WARD_NAME, fill = TOT_GROWTH_AMOUNT),
                    map = wards2016Frame) + 
         expand_limits(x = wards2016Frame$long, y = wards2016Frame$lat) +
         xlab("") + 
         ylab("") + 
         ggtitle("Total Wards Growth")

#theme(legend.position="bottom", legend.box = "horizontal") OPTIONAL









