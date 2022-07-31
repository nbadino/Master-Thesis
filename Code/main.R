library(sf)
library(raster)
library(exactextractr)
library(terra)
library(countrycode)
library(wbstats)
library(dplyr)
library(readr)
library(tidyverse)
library(dagitty)
library(ggdag)
library(ggplot2)
library(devtools)
library(LidarProcessoR)
countries <- c("ita","de","gbr","fr","usa")
borders <- st_read("F:/tmp/borders/gadm36_0.shp")
st_layers("F:/gadm404-levels.gpkg")
rast <- rast("F:/tmp/2021.tif")
ventuno <- exact_extract(rast,borders,fun="sum",append_cols=c("GID_0"))
ventuno
years <- list.files("F:/tmp", pattern = ".tif", full.names=TRUE)
data <- data.frame(GID_0=c(""),year=c(""),sum=c(""))
ventuno <- data.frame(append(ventuno, c(x1=tools::file_path_sans_ext(basename("F:/tmp/2012.tif"))), after=1))
tools::file_path_sans_ext(basename("F:/tmp/2012.tif"))

for (i in years){
  rast <- rast(i)
  extract <- exact_extract(rast,borders,fun="sum",append_cols=c("GID_0"))
  yr <- tools::file_path_sans_ext(basename(i))
  extract <- data.frame(append(extract, c(year=yr), after=1))
  data
  data <- rbind(data,extract)
}

pill<- wb_search("gdp")
pill
gdp <- wb_data("NY.GDP.MKTP.KD", start_date = 2012, end_date = 2021)
gdp$iso2c <- NULL
gdp$unit <- NULL
gdp$footnote <- NULL
gdp$last_updated <- NULL
gdp$obs_status <- NULL
names(gdp)[3] <- "year"
names(data)[1] <- "country"
names(gdp)[2] <- "country_long"
names(gdp)[1] <- "country"
names(gdp)[4] <- "gdp"
gdp$country_long <- NULL
panel <- merge(data, gdp, by=c("year", "country"))
a <- 1/2
panel$sum <- as.numeric(panel$sum)
panel$sum_log <- log(panel$sum+1)
panel$gdp_log <- log(panel$gdp+1)
pool <- lm(panel$gdp_log~panel$sum_log)
summary(pool)
fe_model_lm <- lm(panel$gdp_log ~ panel$sum_log+factor(panel$year))
summary(fe_model_lm)
plot(panel$gdp_log,panel$sum_log)
tredici <- panel[panel$year == '2013', ]
mod <- lm(tredici$gdp_log~tredici$sum_log)
plot(tredici$gdp_log~tredici$sum_log)
summary(mod)
reg_shp <- st_read("F:/tmp/borders/eu_reg/NUTS_RG_01M_2021_4326.shp")
zonal_inequalities <- exact_extract(rast,reg_shp,fun="sum",append_cols=c("NUTS_ID"))
zonal_in<-zonal_inequalities[grepl("\\d", zonal_inequalities$NUTS_ID), ]
plot(reg_shp)
#Regional level elaboration
eu_borders <- st_read("F:/tmp/borders/eu_reg/NUTS_RG_01M_2021_4326.shp")
st_layers("F:/tmp/borders/eu_reg/NUTS_RG_01M_2021_4326.shp")
for (i in years){
  rast <- rast(i)
  extract <- exact_extract(rast,eu_borders,fun="sum",append_cols=c("GID_0"))
  yr <- tools::file_path_sans_ext(basename(i))
  extract <- data.frame(append(extract, c(year=yr), after=1))
  data_eu_nuts2
  data_eu_nuts2 <- rbind(data,extract)
}
nuts2_gdp <- read_csv("F:/tmp/eurostat_gdp_nuts2/nama_10r_2gdp_1_Data.csv")


###outlier detection
#funzione che taglia raster con shp
ita <- crop(ita_mask_raster,italy,mask=TRUE)
russia <- vect(getData('GADM', country='rus', level=0))
rus <- terra::crop(ita_mask_raster,russia,mask=TRUE,overwrite=TRUE, touches=FALSE)

##plotta valori più alti di 100 in blu. Non funziona con stat grandi come la Russia.
myColorRamp <- colorRampPalette(c("white", "blue"))
popRaster <- rus
values(popRaster) <- as.numeric(values(popRaster) >= 100)
popRaster
plot(popRaster, col=myColorRamp(2))


#funzione rimozione outliers. Forse non funziona perchè taglia valori bassi.
f <- function(x) {
  q <- quantile(x, .9999, na.rm=TRUE)
  x[x>q] <- NA
  x
}
xi <- terra::app(rus,f)
prova <- rast("F:/tmp/2021.tif")
prova[prova>500] <- NA
prova[prova<0] <- NA



