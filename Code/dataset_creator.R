library(wbstats)
library(ggmap)
library(readr)
library(scatterplot3d)
##Data collection
data <- read.csv("F:/tmp/dataset.csv")
wb_search("population")
gdp <- wb_data("NY.GDP.MKTP.KD",start_date = 2012, end_date = 2021)
gdp$iso2c <- NULL
gdp$unit <- NULL
gdp$footnote <- NULL
gdp$last_updated <- NULL
gdp$obs_status <- NULL
gdp$country_long <- NULL
data$X <- NULL
data <- data[-c(1),]
names(data)[1] <- "country"
names(gdp)[3] <- "year"
names(gdp)[2] <- "country_long"
names(gdp)[1] <- "country"
names(gdp)[4] <- "gdp"
panel <- merge(data, gdp, by=c("year", "country"))
population <- wb_data("SP.POP.TOTL", start_date=2012, end_date=2021)
population[c(1,3,6,7,8,9)] <- NULL
names(population) <- c("country", "year", "population")
panel <- merge(panel,population, by=c("year", "country"))
population_density <- wb_data("EN.POP.DNST", start_date=2012, end_date=2021)
population_density[c(1,3,6,7,8,9)] <- NULL
names(population_density) <- c("country", "year", "population_density")
panel <- merge(panel,population_density, by=c("year", "country"))
#sectors
agri_sector <- wb_data("NV.AGR.TOTL.ZS", start_date=2012, end_date=2021)
agri_sector[c(1,3,6,7,8,9)] <- NULL
names(agri_sector) <- c("country", "year", "agri_sector")
panel <- merge(panel,agri_sector, by=c("year", "country"))
serv_sector <- wb_data("NV.SRV.TOTL.ZS", start_date=2012, end_date=2021)
serv_sector[c(1,3,6,7,8,9)] <- NULL
names(serv_sector) <- c("country", "year", "serv_sector")
panel <- merge(panel,serv_sector, by=c("year", "country"))
panel$secondary <- 100-panel$agri_sector-panel$serv_sector
##Exploring components of night-time lights
coord <- as.data.frame(read_csv("https://raw.githubusercontent.com/mihai-craita/countries_center_box/master/countries.csv", 
                      col_names = FALSE))
names(coord) <- c("id","long_name","short_name","center_lat","center_lng","sw_lat","sw_lng","ne_lat","ne_lng")
coord$country <- countrycode(coord$long_name, "country.name","wb")
#micronesia
coord[108,]$country <- countrycode(coord[108,]$short_name,"iso2c","iso3c")
#vatican
coord[178,]$country <- countrycode(coord[178,]$short_name,"iso2c","iso3c")
coord[c(1,2,3,5,6,7,8,9)] <- NULL
panel <- merge(panel,coord, by="country")
surface_area <- wb_data("AG.SRF.TOTL.K2", start_date=2012, end_date=2021)
surface_area[c(1,3,6,7,8,9)] <- NULL
names(surface_area) <- c("country", "year", "surface_area")
panel <- merge(panel,surface_area, by=c("year", "country"))
panel$gdppcp <- panel$gdp/panel$population
#source https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2021-2022

panel$econ_groupings <- ifelse(panel$gdppcp>=12000, "HI", ifelse(panel$gdppcp>=4000, "UMI", "LI"))

model <- lm(log(sum+1)~log(gdp+1)+abs(center_lat),data=panel)
summary(model)
scatterplot3d(log(panel$gdp+1), log(panel$sum+1), log(panel$population+1))
