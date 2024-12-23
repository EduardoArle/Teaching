library(rgbif); library(bRacatus)

#get and prepare occ points 

koala <- occ_search(scientificName="Phascolarctos cinereus",
                    hasCoordinate=T)

koala_pts <- koala[[3]]

koala_pts2 <- giveOcc(koala_pts, 
                      longitude = "decimalLongitude",
                      latitude = "decimalLatitude")

coordinates(koala_pts) <- ~decimalLongitude+decimalLatitude  #inform coordinates

#get variables
bioclim <- getData("worldclim",var="bio",res=10)

#select variables
anual_prec <- bioclim[[12]]
anual_mean_temp <- bioclim[[1]]

#crop raster for Australia
australia_prec <- crop(anual_prec,extent(109,156,-44,-10))
australia_temp <- crop(anual_mean_temp,extent(109,156,-44,-10))

#make map without values
australia <- australia_prec
australia[which(!is.na(australia[]))] <- 0

#plot points in map
plot(australia, col = 'gray90', box = F, bty="n", axes = F, legend = F)
plot(koala_pts,add=T,pch=19,cex=.5, col = 'darkgreen')  #save map width 1000

#plot current variables
plot(australia_prec, box = F, bty="n", axes = F, legend = F)  #save map width 1000
plot(australia_temp, box = F, bty="n", axes = F, legend = F)  #save map width 1000

#extract values from rasters at point locations
australia_var <- stack(australia_prec,australia_temp)
values <- extract(australia_var,koala_pts)
values <- as.data.frame(values)

min_temp <- min(values$bio1)
max_temp <- max(values$bio1)

min_prec <- min(values$bio12)
max_prec <- max(values$bio12)

## Produce a very simple SDM for the Koala
climatic_suitability <- australia_prec

for(i in 1:length(climatic_suitability[]))
{
  climatic_suitability[i] <- ifelse(australia_temp[i] > min_temp &
                                      australia_temp[i] < max_temp &
                                      australia_prec[i] > min_prec &
                                      australia_prec[i] < max_prec,
                                    1,0)
  
}

plot(climatic_suitability, box = F, bty="n", axes = F, legend = F) #save map width 1000

#crop raster for South America
SA_prec <- crop(anual_prec,extent(-85.35,-32,-57,13.76))
SA_temp <- crop(anual_mean_temp,extent(-85.35,-32,-57,13.76))

#plot South American variables
plot(SA_prec, box = F, bty="n", axes = F, legend = F)  #save map width 1000
plot(SA_temp, box = F, bty="n", axes = F, legend = F)  #save map width 1000

#project suitability for South America
climatic_suitability_SA <- SA_prec

for(i in 1:length(climatic_suitability_SA[]))
{
  climatic_suitability_SA[i] <- ifelse(SA_temp[i] > min_temp &
                                       SA_temp[i] < max_temp &
                                       SA_prec[i] > min_prec &
                                       SA_prec[i] < max_prec,
                                    1,0)
  print(i)
}

plot(climatic_suitability_SA, box = F, bty="n", axes = F, legend = F) #save map width 1000

#load climatic data for 2070
setwd('/Users/carloseduardoaribeiro/Documents/Post-doc/Teaching/Community Ecology/cc85bi70')
vars_fut <- lapply(list.files(),raster)

#select anual precipitation and anual mean temperatura
anual_prec_2070 <- vars_fut[[4]]
anual_mean_temp_2070 <- vars_fut[[1]]

#crop australia
aust_prec_2070 <- crop(anual_prec_2070,extent(109,156,-44,-10))
aust_mean_temp_2070 <- crop(anual_mean_temp_2070,extent(109,156,-44,-10))

#plot vars 2070
plot(aust_prec_2070, box = F, bty="n", axes = F, legend = F)  #save map width 1000
plot(aust_mean_temp_2070, box = F, bty="n", axes = F, legend = F)  #save map width 1000

#project suitability for 2070
climatic_suitability_2070 <- aust_prec_2070

for(i in 1:length(climatic_suitability_2070[]))
{
  climatic_suitability_2070[i] <- ifelse(aust_mean_temp_2070[i] > min_temp &
                                         aust_mean_temp_2070[i] < max_temp &
                                         aust_prec_2070[i] > min_prec &
                                         aust_prec_2070[i] < max_prec,
                                         1,0)
  
}

plot(climatic_suitability_2070, box = F, bty="n", axes = F, legend = F) #save map width 1000
