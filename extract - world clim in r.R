#####Worldclim in R#####

library(raster)
library(sp)

r <- getData("worldclim",var="bio",res=0.5, lon =115 , lat =-30)
r
r <- r[[c(1, 5, 6, 8, 10, 11, 12, 16, 17, 18)]]
names(r) <- c("Bio_1","Bio_5", "Bio_6", "Bio_8","Bio_10","Bio_11", "Bio_12","Bio_16","Bio_17", "Bio_18")
r

library(readxl)
setwd("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/GIS_Jatin_data")
sites <- read_excel("sites.xlsx") #read in my sites
sites
lons <- sites$lon # subset my lons
lats <- sites$lat # subset my lat
#sites1 <- cbind(lon.pts,lat.pts)
coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)
values <- extract(r,points)
df <- cbind.data.frame(coordinates(points),values)
df

setwd("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/GIS_Jatin_data")
write.csv(df,'R_worldclim.csv')

r <- getData("worldclim",var="bio",res=0.5, lon =115 , lat =-29)
r
r <- r[[c(1, 5, 6, 8, 10, 11, 12, 16, 17, 18)]]
names(r) <- c("Bio_1","Bio_5", "Bio_6", "Bio_8","Bio_10","Bio_11", "Bio_12","Bio_16","Bio_17", "Bio_18")
r

library(readxl)
setwd("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/GIS_Jatin_data")
sites <- read_excel("sites.xlsx") #read in my sites
sites
lons <- sites$lon # subset my lons
lats <- sites$lat # subset my lat
#sites1 <- cbind(lon.pts,lat.pts)
coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)
values <- extract(r,points)
df <- cbind.data.frame(coordinates(points),values)
df #divide temps by 10 in excel

setwd("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/GIS_Jatin_data")
write.csv(df,'R_worldclim2.csv')



