##############################################################
##############################################################
##############################################################
##############################################################
####################  R script C_07 ##########################
##############################################################
##############################################################
##############################################################

## SUMMARY: the script is about finding and storing the 5 x 5 km least cloudy Landsat image per year (based on GLASS_GLC grids)
## where the validation data are available.

## load libraries
library(sf)
library(raster)
library(sp)
library(rgee)
library(tidyverse)

## loading the pre-selected GLASS-GLC grids to download Landsat images
dir <- "//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data"
files <- list.files(dir, ".shp", full.names = TRUE)

## creating the full file
file <- NULL
for (j in files) {
  year <- sub(".*grid_Landsat_", "", j)
  year <- as.numeric(substr(year, 1, 4))
  x <- st_read(j)
  comb <- cbind(x, year)
  file <- rbind(file, comb)
}

## clean duplicates
file <- file %>% distinct(cell_ID, year, .keep_all = TRUE)

## Connect with Google Earth Engine
ee_Initialize(email = "caterina.barrasso@gmail.com")

## Functions to filter out the clouds later

maskL875sr <- function(image) {
  cloudShadowBitMask <- 3
  cloudsBitMask <- 5

  ## Select the QA band.
  qa <- image$
    select("pixel_qa")

  ## Get the internal_cloud_algorithm_flag bit.
  mask <- qa$bitwiseAnd(cloudShadowBitMask)$
    eq(0)$And(qa$bitwiseAnd(cloudsBitMask)$
    eq(0))

  ## Return an image masking out cloudy areas
  image$
    updateMask(mask$Not())$
    select("B.*")
}

setwd("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat")

## Dividing the file in chuncks to download the images
max <- 50
x <- nrow(file)
rows <- ceiling(x / max)
list <- split(file, (as.numeric(rownames(file)) - 1) %/% rows)

## Starting the iteration
i <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

subset <- list[[i]]
print("subset created")
rm(list = (ls()[ls() != "subset"]))

# download Landsat
for (j in 1:nrow(subset)) {
  cell <- subset[j, ]

  ## check if the image was already downloaded
  setwd(paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year))
  files <- list.files(".", ".tif")
  tile <- sub(".*cell_ID_", "", files)
  tile <- as.numeric(str_remove(tile, ".tif"))

  if (cell$cell_ID %in% tile) {
    print("already downloaded")
  } else {

    ## otherwise download it and decide with Landsat collection to use depending on the year
    if (cell$year == 2012) {
      dataset <- ee$ImageCollection("LANDSAT/LE07/C01/T1_SR")
    } else if (cell$year > 2012) {
      dataset <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")
    } else {
      dataset <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")
    }

    start <- paste0(cell$year, "-01-01")
    end <- paste0(cell$year, "-12-31")

    Landsat <- dataset$
      filterDate(start, end)

    object <- sf_as_ee(cell)

    geom_params <- list(
      scale = 30,
      crs = "EPSG:4326",
      region = object$geometry()
    )

    Landsat_cropped <- Landsat$filterBounds(object)$sort("CLOUDY_PIXEL_PERCENTAGE")$mosaic()

    no_clouds <- maskL875sr(Landsat_cropped)

    # Download the images and prepare the stack

    try(
      {
        path <- no_clouds$getDownloadUrl(geom_params)

        dir.create(paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year))

        download.file(path, paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year, "/GLASS-GLC_cell_ID_", cell$cell_ID, ".zip"), mode = "wb")

        dir.create(paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year, "/GLASS-GLC_cell_ID_", cell$cell_ID))

        unzip(paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year, "/GLASS-GLC_cell_ID_", cell$cell_ID, ".zip"), exdir = paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year, "/GLASS-GLC_cell_ID_", cell$cell_ID))

        setwd(paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year))

        unlink(paste0("GLASS-GLC_cell_ID_", cell$cell_ID, ".zip"))

        setwd(paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year, "/GLASS-GLC_cell_ID_", cell$cell_ID))

        rasterFiles <- list.files(pattern = "tif")

        stack <- stack(rasterFiles)

        setwd(paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year))

        writeRaster(stack, paste0("GLASS-GLC_cell_ID_", cell$cell_ID, ".tiff"))

        dir_to_clean <- paste0("//idiv.de/public/homes/cb58hypi/GeoKur/2021-2022/04. Information/1.Chapter/GeoKur_case_study/28102021/provenance_CB/data/Landsat/", cell$year, "/GLASS-GLC_cell_ID_", cell$cell_ID)

        file.remove(dir(dir_to_clean, pattern = "download", full.names = TRUE))

        unlink(paste0("GLASS-GLC_cell_ID_", cell$cell_ID), recursive = TRUE)
      },
      silent = F
    )
  }

  print(j)
}