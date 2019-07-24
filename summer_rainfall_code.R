
#let me find a way to upload the picture to go along with this.

#mapping with rasters in R
#this creates a mean summmer rainfall map of the last 60 years of data, ontop of that I overlay the australian coastline
#and the number of Phytophthora dections I found at my sites

library(chron)
library(lattice)
library(RColorBrewer)
library(ncdf4)
library(raster)
library(ggplot2)
library(dplyr)

# I have my data in a nc file. you just need to get your data into a raster
setwd("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/GIS_Jatin_data")
fname <- "all_precip_1960_2017.nc"
ncin <- nc_open(fname)

#you will notice the links. there are some of the pages I used to gather the code. I suggest reading them if you are adpating this
#https://stackoverflow.com/questions/41245494/monthly-average-from-netcdf-files-in-r

#I'm turning a ncfile into a stack of raster layers here
#if you have some data turn it into a raster
file2 <- stack(fname)


#https://stackoverflow.com/questions/31798389/sum-nlayers-of-a-rasterstack-in-r/31800679#31800679
#setting the dat format in the stack
indices <- format(as.Date(names(file2), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)

datasum<- stackApply(file2, indices, fun = sum)
plot(datasum)

#this is separating the months data that I was (1 = jan, 2 = feb 12 = dec) summer
sum_summer_precip <- datasum[[c(1,2,12)]]
#ind month av then added together
rast <- sum(sum_summer_precip)/58
plot(rast)

#okay at this stage rast = mean monthly rainfall across summer (dec - end of feb for last 58 yrs)
#use the function below on a RasterLayer so you can convert it for use in ggplot2

#https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r
gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}
dat1 <- gplot_data(rast)

#sites data
# this is to import my site points
#you can import any type of data you want to overlay
{
  library(readxl)
  phyt_surv_sites <- read_excel("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/sampling data/R maps/phyt_surv_sites.xlsx")
  
  #I want my sites on specific sides of the points on the map so they dont overlap hence this.
  Map3 <- phyt_surv_sites
  
  vect2 <- c("", "", "5", "6", "", "", "", "", "", "", "14", "", "", "", "18", "", "", "", "", "")
  Map4 <- cbind(Map3, vect2)
  #14, 6, 5, 18
  
  vect1 <- c("1", "2", "", "", "8", "9", "10", "11", "12", "13", "", "15", "16", "17", "", "19", "20", "3", "4", "7")
  Map3 <- cbind(Map3, vect1)
}
#

#number of detection - added to sample site dataset
#this is the data for point size
{
  
  #This is my own data - basically its the number of phytophthora detections at a site.
  #I use this for geom point, where size is the number of detections
  #i add this to the same data set where I have my site locations and numbers
  
  library(readxl)
  end_phyt_surv <- read_excel("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/ngs_hts/end_phyt.perno_surv.xlsx")
  end_phyt_surv_order <- read_excel("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/ngs_hts/end_phyt.perno_surv_order.xlsx")
  
  phyt_matrix <- as.matrix(end_phyt_surv)
  phyt_var <- end_phyt_surv_order
  
  #binary model
  phyt_matrix_bin <- ifelse(phyt_matrix>0,1,phyt_matrix)
  
  #gives the number of individuals found in each plot,#negative 1 is just removing a dummy species I coded in
  spAbund <- rowSums(phyt_matrix_bin)-1  
  spAbund
  phyt_matrix_bin <- as.data.frame(phyt_matrix_bin)
  #I wanted to have a look at an important species. so i had a look and overlayed it as well for another figure
  verse <- phyt_matrix_bin$`Phytophthora versiformis complex`
  
  phyt_var <- cbind(phyt_var, spAbund, verse)
  phyt_var$spAbund <- ifelse(phyt_var$spAbund>0,1,phyt_var$spAbund)
  phyt_var$site <- as.factor(phyt_var$site2) 
  
  library(plyr)
  detect.dat <- ddply(phyt_var, c("site"), summarize,
                      pos.pts = sum(spAbund),
                      verse2 = sum(verse))
  detect.dat
  
  Map3 <- cbind(Map3, detect.dat$pos.pts, detect.dat$verse2)
  Map3$abund <- as.factor(Map3$`detect.dat$pos.pts`)
  Map3$verse2 <- as.factor(Map3$`detect.dat$verse2`)
  library(dplyr)
  Map3$others <- as.numeric(Map3$abund)-as.numeric(Map3$verse2)
  Map3$others <- as.factor(Map3$others)
  #
  
}

#australian coats line
#http://www.flutterbys.com.au/stats/tut/tut5.4.html
library(maps)
library(mapdata)
library(sp)
aus<-map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),
         ylim=c(-45,-5), mar=c(0,0,0,0))
map2SpatialPolygons <- function(df, proj4string=CRS("+proj=longlat")) {
  Plys <- list()
  i<-1
  mtch <- which(is.na(df$x))
  if(length(mtch)==0) {
    mtch <- length(df$x)+1
  } else {mtch <-mtch}
  shps <- length(mtch)
  #make sure the names are unique
  nms <- df$names
  nms[duplicated(nms)] <- paste(nms[duplicated(nms)],1:length(nms[duplicated(nms)]))
  for (j in 1:shps){
    Plys[[j]] <- Polygons(list(Polygon(cbind(df$x[i:(mtch[j]-1)],
                                             df$y[i:(mtch[j]-1)]))),ID=nms[j])
    i <- mtch[j]+1
  }
  SpatialPolygons(Plys,proj4string=proj4string)
}
aus.sp <- map2SpatialPolygons(aus)
plot(aus.sp, asp=1)

#you can use oz_data - pretty inacurate tho
#http://www.elaliberte.info/code

#abund = all phyt detections
#verse2 = p versiformis detections
#others = abund - verse2, so all other phytophthora


# the ggplot2 code
library(ggplot2)
p2 <- ggplot() +
  geom_tile(data = dat1, aes(x = x, y = y, fill = value)) +
  geom_point(data = Map3, aes(x = lon, y = lat, size = abund), colour = "black") +
  geom_text(data = Map3, aes(x = lon, y = lat, label = paste("  ", as.character(vect1), sep=""), fontface = 2), 
            size = 3.2, angle = 0, hjust = 0, vjust = 0.2, color = "black") +
  geom_text(data = Map4, aes(x = lon, y = lat ,label = paste("  ", as.character(vect2), sep=""), fontface = 2), 
            size = 3.2, angle = 0, hjust = 1.4, vjust = 0.2, color = "black") +
  geom_path(data = aus.sp, aes(y=lat, x=long, group=group)) +
  scale_fill_gradient2(name = "Rainfall (mm)", limits = c(20,42), midpoint = 30,
                       guide = guide_colorbar(title.position = "top", order = 1)) +
  scale_size_manual(values = c(0.5,1,2,3,4), name = "Detections",
                    guide = guide_legend(title.position = "top", order = 2))+
  scale_x_continuous(name = "Longitude", limits = c(114.8, 115.8), expand = c(-0.05, -0.05)) + 
  scale_y_continuous(name = "Latitude", limits = c(-31.025, -29.5), expand = c(-0.015, -0.015)) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.2), "cm"),
        axis.text = element_text(colour = "black"),
        legend.title = element_text(size = 8),
        legend.position = "bottom",
        legend.box = 'vertical',
        legend.title.align =0.5, 
        legend.margin=margin(0,0,0,0),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.key.height =unit(0.5,"line"))
p2
#
# the warnings are just the coastline and all the other data tiles in the raster layer. they arent displayed because of the x and y lim

#lets an an A to the plot for adding to a palette of plots in my thesis
# try the egg package if you have facets
library(cowplot)
p3 <- ggdraw(p2) + draw_label("A", x = 0.07, y = 0.95, fontface = "bold", size = 12)

#lets export the plot
#you need to export it into a verticle rectangle if you actually want it to look good
setwd("D:/Users/Chris/Desktop/PhD - Work/Experiment 2 - survey/GIS_Jatin_data")
png("smmr_rf_detect7.png", width = 8, height = 16, units = 'cm', res = 600)
p3
dev.off()
#
