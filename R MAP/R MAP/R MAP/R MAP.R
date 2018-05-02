
## install packages

install.packages(c("plyr", "ggplot2","rgeos", "maptools","sp"))

library(plyr)
library(sp)
library(ggplot2)
library(rgeos)
library(maptools)


## tutorial read-in

# edu63<-read.csv(choose.files())

edu63<-read.csv(choose.files(), colClasses=c("factor","factor", "factor", "factor", 
                "numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                "numeric","numeric","numeric","numeric","numeric","numeric"))
              

## str(edu63)

## clear environment

# rm(list=ls())

# MAP

np_dist <- readShapeSpatial(choose.files())

# VERIFY IT LOADED PROPERLY

plot(np_dist)

np_dist <- fortify(np_dist, region = "NAME_3")

np_dist$id <- toupper(np_dist$id)  #change ids to uppercase

ggplot() + geom_map(data = edu63, aes(map_id = DISTRICT, fill = PASS.PERCENT), 
                    map = np_dist)+ expand_limits(x = np_dist$long, y = np_dist$lat)


## mapping the data right

# Take the mean of PASS.PERCENT by District
districtpassavg63<-ddply(edu63, .(DISTRICT), summarize, PassMean63=mean(PASS.PERCENT))

# Same plot, but use the right dataset and fill parameter
ggplot() + geom_map(data = districtpassavg63, aes(map_id = DISTRICT, fill = PassMean63), 
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat)

# Making map look pretty 

library(scales)

# limits is for max and min value range for legend

ggplot() + geom_map(data = districtpassavg63, aes(map_id = DISTRICT, fill = PassMean63), map = np_dist) +
                    expand_limits(x = np_dist$long, y = np_dist$lat) +
                    scale_fill_gradient2(low = muted("red"), mid="white", midpoint = 50, high=muted("blue"),
                    limits = c(0, 100))

# turns out its important that the "+" is at end of line, not beginning

## LABELLING

# making a dataset that creates centroids per district, 
# and map a layer of text onto this map using that district-centers dataset.

distcenters <- ddply(np_dist, .(id), summarize, clat = mean(lat), clong = mean(long))

ggplot() + geom_map(data = districtpassavg63,
                    aes(map_id = DISTRICT, fill = PassMean63), 
                    map = np_dist) +
                    expand_limits(x = np_dist$long, y = np_dist$lat) +
                    scale_fill_gradient2(limits = c(-20, 60), 
                    low = muted("red"), mid = "white", midpoint = 0, high = muted("blue")) +  
                    geom_text(data = distcenters, 
                    aes(x = clong, y = clat, label = id, size = 0.2))



### MY EXAMPLE USING CAPE TOWN PLANNING DISTRICTS


dist_val<-read.csv(choose.files(), colClasses=c("factor","numeric"))

districts <- readShapeSpatial(choose.files())

districts<-fortify(districts, region = "Distr_Numb")

districts$id<-toupper(districts$id)

ggplot() + geom_map(data = dist_val, aes(map_id = Distr_Numb, fill = VAL), 
                    map = districts)+ expand_limits(x = districts$long, y = districts$lat)

distcenters <- ddply(districts, .(id), summarize, clat = mean(lat), clong = mean(long))

ggplot() + geom_map(data = dist_val,
                    aes(map_id = Distr_Numb, fill = VAL), 
                    map = districts) +
  expand_limits(x = districts$long, y = districts$lat) +
  scale_fill_gradient2(limits = c(0, 20), 
        low = muted("red"), mid = "white", midpoint = 10, high = muted("blue")) +  
  geom_text(data = distcenters, 
            aes(x = clong, y = clat, label = id, size = 0.2))

## rename labels

# add new fields

dist_labels<-merge(distcenters,dist_val,by.x="id", by.y="Distr_Numb")

ggplot() + geom_map(data = dist_val,
                    aes(map_id = Distr_Numb, fill = VAL), 
                    map = districts) +
  expand_limits(x = districts$long, y = districts$lat) +
  scale_fill_gradient2(limits = c(0, 20), 
                       low = muted("red"), mid = "white", midpoint = 10, high = muted("blue")) +  
  geom_text(data = dist_labels, 
            aes(x = clong, y = clat, label = Name, size = 0.2))

