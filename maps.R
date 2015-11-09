library(rworldmap)
library(ggmap)

biz <- readRDS("biz.rds")
newmap <- getMap(resolution = "low")

#World
plot(newmap)

#Europe
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)

#UK
#See http://www.milanor.net/blog/?p=534
#See https://en.wikipedia.org/wiki/Extreme_points_of_the_United_Kingdom for UK
uk.limits <- geocode(c("Shetland",
                       "Isles of Scilly",
                       "Rockall", 
                       "Lowestoft Ness, Suffolk"))
plot(newmap,
     xlim = range(uk.limits$lon),
     ylim = range(uk.limits$lat),
     asp = 1)


#Businesses
#with this type of map, only makes sense to show world
points(biz$longitude, biz$latitude, col = "red", cex = .6)


#########################################
# Analysis only of Edinburgh businesses #
#########################################

library(ggmap)
map <- get_map(location = 'Edimburgh', zoom = 11, maptype = "roadmap")

biz$city <- as.factor(biz$city)
biz.ed <- biz[which(biz$city=="Edinburgh"),]

mapPoints <- ggmap(map) +
          geom_point(aes(x = biz.ed$longitude, y = biz.ed$latitude),
                         data = biz.ed, size = 1, alpha = 0.5)
mapPoints

#Only restaurants (as in category)
biz.ed.rest<-biz.ed[which(grepl("Restaurants", biz.ed$categories)),]
mapPoints <- ggmap(map) +
    geom_point(aes(x = biz.ed.rest$longitude, y = biz.ed.rest$latitude),
               data = biz.ed.rest, size = 1, alpha = 0.5)
mapPoints
