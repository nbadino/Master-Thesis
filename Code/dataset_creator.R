library(wbstats)
library(ggmap)
library(readr)
library(scatterplot3d)
library(dplyr)
library(tidyr)

#import sum lights
data <- read.csv("F:/tmp/lightsextraction.csv")
data$X <- NULL
data <- data[-c(1),]
names(data )[1] <- "country"


#gdp constant prices
gdp_cp <- wb_data("NY.GDP.MKTP.KD",start_date = 2012, end_date = 2021)
gdp_cp$iso2c <- NULL
gdp_cp$unit <- NULL
gdp_cp$footnote <- NULL
gdp_cp$last_updated <- NULL
gdp_cp$obs_status <- NULL
gdp_cp$country_long <- NULL
names(gdp_cp)[3] <- "year"
names(gdp_cp)[2] <- "country_long"
names(gdp_cp)[1] <- "country"
names(gdp_cp)[4] <- "gdp_cp"
panel <- merge(data, gdp_cp, by=c("year", "country"))

#outlier removed NTL
data_outl <- read.csv("F:/tmp/dataset_outl.csv")
data_outl$X <- NULL
names(data_outl[3]) <- "sumoutl"
data_outl <- data_outl[-c(1),]
names(data_outl)[1] <- "country"
panel <- merge(panel, data_outl, by=c("year", "country"))

#gdp local currency unit constant
gdp_lcu_constant <- wb_data("NY.GDP.MKTP.KN", start_date=2012, end_date=2021)
gdp_lcu_constant[c(1,3,6,7,8,9)] <- NULL
names(gdp_lcu_constant) <- c("country", "year", "gdp_lcu_constant")
panel <- merge(panel, gdp_lcu_constant, by=c("year", "country"))
#panel <- drop_na(panel)

#gdp local currency unit
gdp_lcu_current <- wb_data("NY.GDP.MKTP.CN", start_date=2012, end_date=2021)
gdp_lcu_current[c(1,3,6,7,8,9)] <- NULL
names(gdp_lcu_current) <- c("country", "year", "gdp_lcu_current")
panel <- merge(panel, gdp_lcu_current, by=c("year", "country"))

#gdp ppp

gdp_ppp <- wb_data("NY.GDP.MKTP.PP.KD", start_date=2012, end_date=2021)
gdp_ppp[c(1,3,6,7,8,9)] <- NULL
names(gdp_ppp) <- c("country", "year", "gdp_ppp")
panel <- merge(panel, gdp_ppp, by=c("year", "country"))


# population
# population <- wb_data("SP.POP.TOTL", start_date=2012, end_date=2021)
# population[c(1,3,6,7,8,9)] <- NULL
# names(population) <- c("country", "year", "population")
# panel <- merge(panel,population, by=c("year", "country"))

#gdp growth
gdp_growth_official <- wb_data("NY.GDP.MKTP.KD.ZG", start_date=2012, end_date=2021)
gdp_growth_official[c(1,3,6,7,8,9)] <- NULL
names(gdp_growth_official) <- c("country", "year", "gdp_growth_official")
panel <- merge(panel,gdp_growth_official, by=c("year", "country"))
panel$gdp_growth_official <- panel$gdp_growth_official/100
# #population density
# population_density <- wb_data("EN.POP.DNST", start_date=2012, end_date=2021)
# population_density[c(1,3,6,7,8,9)] <- NULL
# names(population_density) <- c("country", "year", "population_density")
# panel <- merge(panel,population_density, by=c("year", "country"))

# #surface area
# #panel <- select(panel, -surface_area)
# surface_area <- wb_data("AG.SRF.TOTL.K2", start_date=2012, end_date=2021)
# surface_area[c(1,3,6,7,8,9)] <- NULL
# names(surface_area) <- c("country", "year", "surface_area")
# panel <- merge(panel,surface_area, by=c("year", "country"))
# for (i in panel$country[]){
#   panel$surface_area[panel$country==i] <-   mean(na.omit(panel$surface_area[panel$country==i]))
#   
# }
# 
# #gdp/area
# 
# panel$sum_area <- panel$sum/panel$surface_area

#grading
grading <- wb_data("IQ.SPI.OVRL", start_date = 2016, end_date=2019)
grading[c(1,3,6,7,8,9)] <- NULL
names(grading) <- c("country", "year", "grading")
grad <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("country", "grade")
colnames(grad) <- x
for(i in grading$country[]){
  m <- (mean(na.omit(grading[grading$country==i,]$grading)))
  grad[nrow(grad) + 1,] <- c(i,m)
}
grad <- distinct(grad)
grad$grading_index <- ((as.numeric(grad$grade))/10)
# grad$grading_index_lett <- ifelse(grad$grading_index<=4,"D",
#                                           ifelse(grad$grading_index<=6,"C",
#                                                  ifelse(grad$grading_index<=8,"B","A")))
# grad$gradindnumb <- ifelse(grad$grading_index<=10/3,3,
#                            ifelse(grad$grading_index<=20/3,2,1))


grad$grading_index_lett <- ifelse(grad$grading_index<=2.5,"D",
                                  ifelse(grad$grading_index<=5,"C",
                                         ifelse(grad$grading_index<=7.5,"B","A")))

panel <- merge(panel,grad,by=c("country"))
panel <- panel[do.call(order, panel),]




#panel <- panel[which(!is.na(panel$gradindnumb)),]
# panel$surface_area[panel$country==panel$country[]] <- mean(na.omit(panel$surface_area[panel$country==panel$country[]]))
# panel$country[]

#some calculations
panel <- transform(panel, sum_growth = ave(sum.x, country, FUN = function(x) c(NA, diff(x)/x[-length(x)])))
panel <- transform(panel, sum_growth_outl_rem = ave(sum.y, country, FUN = function(x) c(NA, diff(x)/x[-length(x)])))
panel <- transform(panel, gdp_cp_growth = ave(gdp_cp, country, FUN = function(x) c(NA, diff(x)/x[-length(x)])))
panel <- transform(panel, gdp_lcu_constant_growth = ave(gdp_lcu_constant, country, FUN = function(x) c(NA, diff(x)/x[-length(x)])))
panel <- transform(panel, gdp_lcu_current_growth = ave(gdp_lcu_current, country, FUN = function(x) c(NA, diff(x)/x[-length(x)])))
panel <- transform(panel, gdp_ppp_growth = ave(gdp_ppp, country, FUN = function(x) c(NA, diff(x)/x[-length(x)])))

# plot(panel$sum_growth, panel$gdp_lcu_current_growth)

# panel$gdp_lcu_log <- log(panel$gdp_lcu)
# panel$gdp_lcu_current_log <- log(panel$gdp_lcu)
# panel$sum_log <- log(panel$sum)
# panel$gdp_log <- log(panel$gdp)





#exporting dataset
write.csv(panel,"F:/tmp/dataset_fin.csv")


# #sectors
# agri_sector <- wb_data("NV.AGR.TOTL.ZS", start_date=2012, end_date=2021)
# agri_sector[c(1,3,6,7,8,9)] <- NULL
# names(agri_sector) <- c("country", "year", "agri_sector")
# panel <- merge(panel,agri_sector, by=c("year", "country"))
# serv_sector <- wb_data("NV.SRV.TOTL.ZS", start_date=2012, end_date=2021)
# serv_sector[c(1,3,6,7,8,9)] <- NULL
# names(serv_sector) <- c("country", "year", "serv_sector")
# panel <- merge(panel,serv_sector, by=c("year", "country"))
# panel$secondary <- 100-panel$agri_sector-panel$serv_sector
# ##Exploring components of night-time lights
# coord <- as.data.frame(read_csv("https://raw.githubusercontent.com/mihai-craita/countries_center_box/master/countries.csv", 
#                       col_names = FALSE))
# names(coord) <- c("id","long_name","short_name","center_lat","center_lng","sw_lat","sw_lng","ne_lat","ne_lng")
# coord$country <- countrycode(coord$long_name, "country.name","wb")
# #micronesia
# coord[108,]$country <- countrycode(coord[108,]$short_name,"iso2c","iso3c")
# #vatican
# coord[178,]$country <- countrycode(coord[178,]$short_name,"iso2c","iso3c")
# coord[c(1,2,3,5,6,7,8,9)] <- NULL
# panel <- merge(panel,coord, by="country")
# surface_area <- wb_data("AG.SRF.TOTL.K2", start_date=2012, end_date=2021)
# surface_area[c(1,3,6,7,8,9)] <- NULL
# names(surface_area) <- c("country", "year", "surface_area")
# panel <- merge(panel,surface_area, by=c("year", "country"))
# panel$gdppcp <- panel$gdp/panel$population
# #source https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2021-2022
# 
# panel$econ_groupings <- ifelse(panel$gdppcp>=12000, "HI", ifelse(panel$gdppcp>=4000, "UMI", "LI"))
# 
# model <- lm(log(sum+1)~log(gdp+1)+abs(center_lat),data=panel)
# summary(model)
# scatterplot3d(log(panel$gdp+1), log(panel$sum+1), log(panel$population+1))
# plot(log(panel$gdp+1), log(panel$sum+1), col=factor(panel$year))
  
