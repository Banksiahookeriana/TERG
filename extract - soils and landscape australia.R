#######soil and landscape grid######

#https://obrl-soil.github.io/slga/reference/index.html
#http://www.clw.csiro.au/aclep/soilandlandscapegrid/ProductDetails-SoilAttributes.html
#https://obrl-soil.github.io/slga/articles/slga.html#point-data
#https://obrl-soil.github.io/slga/reference/get_soils_point.html




devtools::install_github("obrl-soil/slga")

library(slga)
options(stringsAsFactors = FALSE)
library(raster)
library()
data('ki_surface_clay')

?slga_product_info
?slga_attribute_info

data('ki_surface_clay')

aoi <- c(143.75, -40.17, 144.18, -39.57)
aoi <- c(115, -30, 116, -29)

ki_surface_clay1 <- get_soils_data(product = 'WA', attribute = 'CLY',
                                  component = 'VAL', depth = 2,
                                  aoi = aoi, write_out = FALSE)
ki_surface_clay2 <- get_soils_data(product = 'WA', attribute = 'CLY',
                                  component = 'VAL', depth = 3,
                                  aoi = aoi, write_out = FALSE)

clay1 <- brick(ki_surface_clay1)
#plot(clay1)
clay2 <- brick(ki_surface_clay2)

ki_surface_sand1 <- get_soils_data(product = 'WA', attribute = 'SND',
                                   component = 'VAL', depth = 2,
                                   aoi = aoi, write_out = FALSE)
ki_surface_sand2 <- get_soils_data(product = 'WA', attribute = 'SND',
                                   component = 'VAL', depth = 3,
                                   aoi = aoi, write_out = FALSE)

sand1 <- brick(ki_surface_sand1)
#plot(sand1)
sand2 <- brick(ki_surface_sand2)

ki_surface_silt1 <- get_soils_data(product = 'WA', attribute = 'SLT',
                                   component = 'VAL', depth = 2,
                                   aoi = aoi, write_out = FALSE)
ki_surface_silt2 <- get_soils_data(product = 'WA', attribute = 'SLT',
                                   component = 'VAL', depth = 3,
                                   aoi = aoi, write_out = FALSE)

silt1 <- brick(ki_surface_silt1)
#plot(silt1)
silt2 <- brick(ki_surface_silt2)

library(readxl)
setwd("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/GIS_Jatin_data")
sites <- read_excel("sites.xlsx") #read in my sites
sites
lon.pts <- sites$lon # subset my lons
lat.pts <- sites$lat # subset my lat
sites1 <- cbind(lon.pts,lat.pts)


#extract
ext.clay1 <- extract(clay1,sites1,method="simple") # extract the values for these points from the raster
ext.clay2 <- extract(clay2,sites1,method="simple") # extract the values for these points from the raster

ext.sand1 <- extract(sand1,sites1,method="simple") # extract the values for these points from the raster
ext.sand2 <- extract(sand2,sites1,method="simple") # extract the values for these points from the raster

ext.silt1 <- extract(silt1,sites1,method="simple") # extract the values for these points from the raster
ext.silt2 <- extract(silt2,sites1,method="simple") # extract the values for these points from the raster

ext.clay3 <- extract(clay1,sites1,method="simple") # extract the values for these points from the raster
ext.clay4 <- extract(clay2,sites1,method="simple") # extract the values for these points from the raster

ext.sand3 <- extract(sand1,sites1,method="simple") # extract the values for these points from the raster
ext.sand4 <- extract(sand2,sites1,method="simple") # extract the values for these points from the raster

ext.silt3 <- extract(silt1,sites1,method="simple") # extract the values for these points from the raster
ext.silt4 <- extract(silt2,sites1,method="simple") # extract the values for these points from the raster

soildata <- cbind(sites1, ext.clay1, ext.clay3, ext.clay2, ext.clay4, 
                  ext.sand1, ext.sand3, ext.sand2, ext.sand4, 
                  ext.silt1, ext.silt3, ext.silt2, ext.silt4)
soildata1 <- as.data.frame(soildata)

write.csv(soildata1,'slp.exp1.csv')





