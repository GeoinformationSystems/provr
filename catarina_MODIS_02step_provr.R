## This script is to calculate the pixel number of each LULC class per region and year for the MODIS gridded product

## libraries
library(raster)
library(sf)
library(stringr)
library(tidyverse)
library(rgdal)
library(gdalUtils)
library(devtools)
load_all()


rasterOptions(tmpdir = "/work/barrasso/Rtmp")

## establish code for the different folders
dir<-'/gpfs1/data/idiv_meyer/01_projects/Caterina/Chapter_01/00_data/01_griddedProducts/MODIS_500'
folders<-list.dirs(path = dir, full.names = TRUE, recursive = TRUE)
folders<-folders[2:19]
i=as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))
folder<-folders[i]
date<-sub(".*MODIS_500/", "", folder)
setwd(paste0(dir,'/',date))

## find the files
files<-list.files('.', '.tif')

## upload the regions
countries<-read_sf('/gpfs1/data/idiv_meyer/01_projects/Caterina/Chapter_01/00_data/03_countryBorders/leve0simple_and_TDWG_regions.shp')

## calculate the LULC class pixel number per TDWG region

file  <- list()

for (j in seq_along(files)) {

r<-raster(files[j])

countries <- st_transform(countries, crs =st_crs(r))

subset  <- NULL
for (n in 1:nrow(countries)) { 
 country<-countries[n,]
 e <- st_bbox(country)
 try({
 MODIS<-crop(r,e)
 cropped_raster<-mask(MODIS,country)
 f<-freq(cropped_raster)
 f<-as.data.frame(f)
 names(f)[1]<-'LULC_class'
 f$TDWG_region<-country$TDWG_rg},silent=T)
 if (!exists('f')){
 print('no country')
} else {
 subset <- rbind(subset, f)
}
print(n)
}

if (!exists('subset')){
 print('no intersection')
} else {
file[[j]]<- subset
}
print(j)
}

frequence<- data.table::rbindlist(file)
frequence$year<-as.numeric(date)
frequence$product<-'MODIS'

## remove NA rows introduced with the masking
frequence<-frequence[!is.na(frequence$LULC_class), ]

## summarize pixel number per region and LULC class
summary<-frequence%>% 
group_by(LULC_class,TDWG_region) %>% 
summarise(count = sum(count))

summary<-as.data.frame(summary)
summary$year<-unique(frequence$year)
summary$product<-unique(frequence$product)

print('summary created')

## save the results
write.csv(summary, paste0('/gpfs1/data/idiv_meyer/01_projects/Caterina/Chapter_01/01_analysis/Workflow_B/MODIS/MODIS_freq_',unique(summary$year),'.csv'),row.names=F)