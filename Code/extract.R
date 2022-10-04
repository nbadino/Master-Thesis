library(sf)
library(raster)
library(exactextractr)
library(terra)
library(beepr)


years <- list.files("F:/tmp/Raster_img", pattern = ".tif", full.names=TRUE)
data <- data.frame(GID_0=c(""),year=c(""),sum=c(""))
borders <- st_read("F:/tmp/borders/gadm36_0.shp")
for (i in years){
  rast <- terra::rast(i)
  #ifelse((basename(i) %in% c("2012.tif","2013.tif","2014.tif","2015.tif","2016.tif")),(rast=clamp((rast+0.125),lower=0.125, values=FALSE)),(rast=clamp(rast, lower=0, values=FALSE)))
  extract <- exact_extract(rast, borders,fun="sum",append_cols=c("GID_0"))
  yr <- tools::file_path_sans_ext(basename(i))
  extract1 <- data.frame(append(extract, c(year=yr), after=1))
  data
  data <- rbind(data,extract1)
}

write.csv(data, "F:/tmp/dataset_sum.csv")
ifelse(basename(i) %in% c("2012.tif","2013.tif","2014.tif","2015.tif","2016.tif"), "vero","falso")
basename(i) %in% c("2012.tif","2013.tif","2014.tif","2015.tif","2016.tif")


#extraction without outliers

years <- list.files("F:/tmp/Raster_img", pattern = ".tif", full.names=TRUE)
data <- data.frame(GID_0=c(""),year=c(""),sum=c(""))
borders <- st_read("F:/tmp/borders/gadm36_0.shp")
for (i in years){
  rast <- rast(i)
  #ifelse((basename(i) %in% c("2012.tif","2013.tif","2014.tif","2015.tif","2016.tif")),(rast=clamp((rast+0.125),lower=0.125, values=FALSE)),(rast=clamp(rast, lower=0, values=FALSE)))
  rast <- terra::clamp(rast, upper=800, values=TRUE)
  extract <- exact_extract(rast,borders,fun="sum",append_cols=c("GID_0"))
  yr <- tools::file_path_sans_ext(basename(i))
  extract1 <- data.frame(append(extract, c(year=yr), after=1))
  data
  data <- rbind(data,extract1)
}

write.csv(data, "F:/tmp/dataset_outl.csv")
data$sum2 <- as.numeric(data$sum)
beep()
