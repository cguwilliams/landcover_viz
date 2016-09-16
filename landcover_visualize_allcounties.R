library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(ggplot2)
library(RColorBrewer)

# function to quantize data - Found on stackexchange, written by Hadley
qcut <- function(x, n) {
  cut(x, quantile(x, seq(0, 1, length = n + 1)), labels = seq_len(n),
      include.lowest = T)
}

# load filepaths
county.all.fp <- '~/Documents/berman/data/cb_2014_us_county_20m/'
mydata.fp <- '~/Documents/berman/data/allstate_cover.csv'

# load all state value data
mydata <- read.csv(mydata.fp, header = T, sep = ',', stringsAsFactors = F)

mydata$countygeoid <- formatC(mydata$countygeoid, digits = 5, width = 5, flag = 0)
# load county shapefile
setwd(county.all.fp)
county.all.shp <- readOGR(".","cb_2014_us_county_20m")
# transform shapefile to match projection of landcover projection
# county.all.shp <- spTransform(county.all.shp, coverraster.r@crs)
# remove states and territories not in landcover map
# 02 = Alaska; 11 = D.C.; 15 = Hawaii ; 72 = Puerto Rico
states.rm      <- c("02","11","15","72") 
county.all.shp <- county.all.shp[!as.character(county.all.shp$STATEFP)
                                 %in% states.rm, ]
# sort county data by statefip then countyfip
county.all.shp <- county.all.shp[order(county.all.shp$GEOID),]
gpclibPermit() # the fortify command will not run without this line!
# fortify county shape data to make into data.frame
tract_geom <- fortify(model = county.all.shp, region="GEOID")
# tract_geom$id <- tract_geom$GEOID
# merge county and land cover value data
tract_poly <- merge(mydata,tract_geom, by.x = "countygeoid", by.y = "id", all = F)
tract_poly <- tract_poly[order(tract_poly$order),]

#combine similar categories
tract_poly$devall <- rowSums(data.frame(tract_poly$devopen,tract_poly$devlow,
                                        tract_poly$devmed,tract_poly$devhigh))

# quantize landcover for easier visualization
tract_poly$desidueous_q <- qcut(tract_poly$desidueous, 8)
tract_poly$evergreen_q  <- qcut(tract_poly$evergreen, 8)

tract_poly$haypasture_q <- qcut(tract_poly$haypasture, 8)
tract_poly$cultivated_q <- qcut(tract_poly$cultivated, 8)

tract_poly$devhigh_q    <- qcut(tract_poly$devhigh, 8)
tract_poly$devall_q     <- qcut(tract_poly$devall, 8)

tract_poly$openwater_q     <- qcut(tract_poly$openwater, 8)

# plot data
#d desidueous forest
plot.desidueous <- ggplot(tract_poly, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.5, aes(fill = desidueous_q)) +
  scale_fill_manual(values = brewer.pal(8,"Greens")) +
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        panel.grid.major = element_line(colour = "grey"))

plot.evergreen <- ggplot(tract_poly, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.5, aes(fill = evergreen_q)) +
  scale_fill_manual(values = brewer.pal(8,"Greens")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")

# plot.forest <- ggplot(tract_poly, aes(x = long, y = lat, group = group)) +
#   geom_polygon(color = "white", size = 0, alpha = .8, aes(fill = desidueous_q)) +
#   scale_fill_manual(values = brewer.pal(8,"Greens")) +
#   geom_polygon(color = "red", size = 0, alpha = 0.5, aes(fill = evergreen_q)) +
#   scale_fill_manual(values = brewer.pal(8,"Reds")) +
#   theme(legend.position="none")
#   
# plot.forest

plot.haypasture <- ggplot(tract_poly, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.5, aes(fill = haypasture_q)) +
  scale_fill_manual(values = brewer.pal(8,"Oranges")) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "white"))

plot.cultivated <- ggplot(tract_poly, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.5, aes(fill = cultivated_q)) +
  scale_fill_manual(values = brewer.pal(8,"YlOrBr")) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "white"))

plot.devhigh <- ggplot(tract_poly, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "blue", size = 0.1, aes(fill = devhigh_q)) +
  scale_fill_manual(values = brewer.pal(8,"Reds")) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "white"))

plot.devall <- ggplot(tract_poly, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, alpha=.8, aes(fill = devall_q)) +
  scale_fill_manual(values = rev(brewer.pal(8,"YlGnBu"))) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "white"))

plot.water <- ggplot(tract_poly, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, aes(fill = openwater_q)) +
  scale_fill_manual(values = rev(brewer.pal(8,"Blues"))) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "white"))

png("~/Documents/berman/evergreen.png", width = 12, height = 8, units = 'in', res = 300)
plot.evergreen
dev.off()
