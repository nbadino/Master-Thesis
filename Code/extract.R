library(sf)
library(raster)
library(exactextractr)
library(terra)

years <- list.files("F:/tmp/Raster_img", pattern = ".tif", full.names=TRUE)
data <- data.frame(GID_0=c(""),year=c(""),sum=c(""))
borders <- st_read("F:/tmp/borders/gadm36_0.shp")
for (i in years){
  rast <- rast(i)
  extract <- exact_extract(rast,borders,fun="sum",append_cols=c("GID_0"))
  yr <- tools::file_path_sans_ext(basename(i))
  extract1 <- data.frame(append(extract, c(year=yr), after=1))
  data
  data <- rbind(data,extract1)
}

write.csv(data, "F:/tmp/dataset.csv")

