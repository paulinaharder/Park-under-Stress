# script of functions 

# Content
# 1. function for downloading the soil moisture and evaporation data from DWD
# 2. function for downloading climate data from DWD
# 3. functions for processing the NDVI layers and calculating the avergae NDVI for 
#   each available day 

################################################################################
# 1. function for Soil moisture etc.
##################################################################################
# usage: This function downloads and combines soil moisture data from DWD for a specified station.
# input: station (character string): The name of the station as a string in quotes.
# output: A data frame containing:
# Columns for soil moisture and related variables
# Filtered data starting from January 1, 2017

load_soil_moisture <- function(station) {
  # DWD-Basis-URL
  dbase <- "ftp://opendata.dwd.de/climate_environment/CDC/derived_germany"
  
  # load index
  soilIndex <- indexFTP(folder = "soil/daily", base = dbase) %>%
    createIndex(base = dbase)
  
  # Call hisotric data
  soil_link_hist <- selectDWD(station, res = "", var = "", per = "historical",
                              base = dbase, findex = soilIndex)
  
  # Call recent data
  soil_link_recent <- selectDWD(station, res = "", var = "", per = "recent",
                                base = dbase, findex = soilIndex)
  
  # load data 
  soil_data_hist <- dataDWD(soil_link_hist, base = dbase)
  soil_data_recent <- dataDWD(soil_link_recent, base = dbase)
  
  # combine historic and recent data
  data <- bind_rows(soil_data_hist[[2]], soil_data_recent[[2]])
  
  # selecting the relevant columns 
  data <- data %>%
    select(1:2, 13:18, 25:27) %>%
    mutate(Datum = as.Date(Datum)) %>%
    filter(Datum >= as.Date("2004-01-01"))
  
  return(data)
}

################################################################################
# 2. function for loading the climate data 
################################################################################
# usage: This function downloads climate data for a specified station in daily resolution
# The selected dataset is downloaded, read, and filtered to keep 
# only the relevant columns. Finally, more intuitive column names are given for easier use.
# input: Station (character string), the name of the station, provided as a 
# string in quotes.
# for the available station and the right station name pls consult
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt
# output: A data frame containing columns for:
# Datum: Date
# Prec: Daily precipitation
# Temp_mean: Mean daily temperature
# Temp_max: Maximum daily temperature
# Temp_min: Minimum daily temperature

load_climate<-function(Station){
  # select a dataset, we want 
  link_hist <- selectDWD(Station, res="daily", var="kl", per="historical")
  link_recent<- selectDWD(Station, res="daily", var="kl", per="recent")
#  download that dataset, returning the local storage file name:
file_hist <- dataDWD(link_hist, read=FALSE)
file_recent <- dataDWD(link_recent, read=FALSE)

# Read the file from the zip folder:
clim_hist <- readDWD(file_hist, varnames=TRUE)
clim_recent <- readDWD(file_recent, varnames=TRUE)

clim <- bind_rows(clim_hist, clim_recent)
# select the relevant columns
clim<-clim[,c(2,7,14,16,17)]
# give them more intuitive names 
colnames(clim)<-c("Datum","Prec","Temp_mean","Temp_max","Temp_min")

return(clim)
}

################################################################################
# 3. the functions for NDVI 
################################################################################
#  function crop_raster 
# usage: can be used to crop raster to desired extent, in this case, it is used for 
# cropping the NDVI data and the QFLAG data. the desired extent will be stored 
# in seperate directory, cropped images that hold no values will not be included 
# input:
# tiff_file (character string): Path to the satellite image file in .tif format.
# The file should be readable by the rast() function and contain valid geospatial data.
# Example: "path/to/satellite_image.tif"
# outline: A spatial shapefile used to define the desired extent for cropping.
# output_dir (character string): Directory path where the cropped raster image will be saved.
# The directory must exist or be writable for the output to be saved.
# Example: "path/to/output_directory"
# output:
# If valid data exists in the cropped raster, the function saves the result as a
# .tif file in the  output_dir and returns the file path as a character string.
# If there is no valid data, the function returns NULL and doesn't save a file.

crop_raster <- function(tiff_file, outline, output_dir) {
  # load .tif-file
  raster <- rast(tiff_file) # rasterise
  
  # match tifs crs with the outlines crs 
  outline <- st_transform(outline, crs = crs(raster)) 
  # crop tif 
  cropped_raster <- crop(raster, vect(outline))
  
  # mask tif, to get rid of all other values 
  masked_raster <- mask(cropped_raster, vect(outline))
  
  # check whether there is data in the cropped extent
  # check cropped extents for non-NA_values
  valid_data <- global(masked_raster, fun = "notNA", na.rm = TRUE)
  # when there is data its stored
  if (valid_data > 0) {
    # results stored in directory 
    output_file <- file.path(output_dir, basename(tiff_file))
    writeRaster(masked_raster, output_file, overwrite = TRUE)
    return(output_file) 
  } else {
    # when there is no data in the cropped extent, nothing is saved  
    return(NULL) # return NULL to mark that nothing is saved 
  }
}

################################################################################
# function cloud control
# usage: there is data to an indice (eg NDVI) und quality data, showing where clouds disturb the images 
# so the function masks the indice raster with the quality and keeps only the data with 
# sufficient quality, see also the link in the main script 

# input: tif containing the indice values (cropped_tif) and quality image (quality_file, holds numbers that 
# determine clouded, shaded etc.)  (for the same time!!!!!! and same spatial extent)
# for more info on how keep_quality is determined, see: 
# https://land.copernicus.eu/en/technical-library/product-user-manual-of-vegetation-indices/@@download/file 

# output: in the output directory the indice file, values removed that were determined 
# to be clouded by quality data mask 

cloud_control <- function(cropped_tif, quality_file, output_dir) {
  # rasterise indice & Q data 
  indice_raster <- rast(cropped_tif)
  quality_raster <- rast(quality_file)
  
  # decide which quality values will be kept, see above
  keep_quality <- c(1,2,256,512,1024, 2048, 4096, 8192)
  # create a mask of quality raster with the areas to keep 
  mask <- quality_raster == keep_quality
  
  # put the mask on ndvi data 
  cleaned_indice <- mask(indice_raster, mask, maskvalues = FALSE)
  
  valid_data <- global(cleaned_indice, fun = "notNA", na.rm = TRUE)
  if (sum(valid_data) > 0) {
    # results stored in directory 
    output_file <- file.path(output_dir, basename(cropped_tif))
    writeRaster(cleaned_indice, output_file, overwrite = TRUE)
    return(output_file) 
  } else {
    # when there is no data in the cropped extent, nothing is saved  
    return(NULL) # return NULL to mark that nothing is saved 
  }
}


################################################################################
# usage: with the functions above, the NDVI has been cropped and masked by the 
# QFLAG layers. This function first iterates through the NDVI layer to calculate 
# the mean NDVI value of each layer. Then it checks how many pixels are available.
# If there is less then a quarter of the possible pixels available, it will omit 
# the layer to prevent biases.

# input: a list of the NDVI layers that you want to have the average NDVI of each

# output: a dataframe with the date and the average NDVI value of each

calculate_pixel_ndvi <- function(file_list) {
  # create datafrma to store the results
  pixel_count_df <- data.frame(
    File = character(),
    PixelCount = integer(),
    MeanNDVI = numeric(),
    stringsAsFactors = FALSE
  )
  
  # iterate through raster data 
  for (file in file_list) {
    # load raster
    raster_data <- raster(file)
    
    # calculate amount of valid pixels
    valid_pixel_count <- sum(!is.na(getValues(raster_data)))
    
    # calculate average NDVI
    mean_ndvi <- cellStats(raster_data, stat = "mean", na.rm = TRUE)
    
    # add results to dataframe
    pixel_count_df <- rbind(pixel_count_df, data.frame(
      File = basename(file),
      PixelCount = valid_pixel_count,
      MeanNDVI = mean_ndvi
    ))
  }
  
  # filter raster data 
  filtered_df <- pixel_count_df[pixel_count_df$PixelCount > 8000, ]
  filtered_df$Datum <- substr(basename(filtered_df$File), 4, 11)
  # convert to date 
  filtered_df$Datum <- as.POSIXct(filtered_df$Datum, format = "%Y%m%d")
  filtered_df<-filtered_df[,c(4,3)]
  
  # Ergebnisse zurückgeben
  return(filtered_df)
}
################################################################################
# usage: this function comprises all the previous functions so that with one click 
# you can crop the QFLAG and NDVI, mask the NDVI layer with the QFLAG and calculate the 
# average NDVI omitting NA data and all layers with only few pixels. It is important 
# the NDVI and QFLAG is stored in the suggested folder structure otherwise it will not
# work. For the folder strucutre please consult the manual

# input: the year you want to have the NDVI of 

# output: a dataframe with the date and the corresponding NDVI value
process_years <- function(years, outline_shp) {
  outline<-st_read(outline_shp)
  # empty dataframe for the results
  all_results <- data.frame()
  
  # iterate through the years 
  for (year in years) {
    
    #  read and list NDVI raw data
    ndvi_dir <- paste0(year, "/NDVI_raw")
    ndvi_files <- list.files(ndvi_dir, pattern = "\\.tif$", full.names = TRUE)
    
    # directory for output NDVI
    output_NDVI <- paste0(year, "/NDVI_cropped")
    dir.create(output_NDVI, showWarnings = FALSE)
    
    # crop the ndvi data
    lapply(ndvi_files, crop_raster, outline = outline, output_dir = output_NDVI)
    
    # read and list QFLAG data
    qflag_dir <- paste0(year, "/QFLAG")
    qflag_files <- list.files(qflag_dir, pattern = "\\.tif$", full.names = TRUE)
    
    # output path qflag 
    output_qflag <- paste0(year, "/QFLAG_cropped")
    dir.create(output_qflag, showWarnings = FALSE)
    
    # crop qflag
    lapply(qflag_files, crop_raster, outline = outline, output_dir = output_qflag)
    
    # combine NDVI and qflag for quality filtering 
    ndvi_cr_files <- list.files(paste0(year, "/NDVI_cropped"), pattern = "\\.tif$", full.names = TRUE)
    qflag_cr_files <- list.files(paste0(year, "/QFLAG_cropped"), pattern = "\\.tif$", full.names = TRUE)
    
    # Matching of NDVI- and QFLAG-layers
    ndvi_basenames <- gsub("_NDVI\\.tif$", "", basename(ndvi_cr_files))
    qflag_basenames <- gsub("_QFLAG2\\.tif$", "", basename(qflag_cr_files))
    matching_qflag_files <- qflag_cr_files[qflag_basenames %in% ndvi_basenames]
    
    # 'Clean' the NDVI data by masking it with QGLAG layers
    cleaned_dir <- paste0(year, "/clean_data")
    dir.create(cleaned_dir, showWarnings = FALSE)
    
    mapply(cloud_control, ndvi_cr_files, matching_qflag_files, MoreArgs = list(output_dir = cleaned_dir))
    
    # calculating the mean NDVI
    #list first the clean NDVI data
    ndvi_clean <- list.files(paste0(year, "/clean_data"), pattern = "\\.tif$", full.names = TRUE)
    
    # calculating the mean NDVI-value 
    # 
    mean_ndvi_values <- sapply(ndvi_clean, function(file) {
      
      raster_data <- raster(file)  # falls `raster` geladen ist
      mean_value <- mean(raster_data[], na.rm = TRUE)
      return(mean_value)
    })
    
    # add the values of each year to the dataframe 
    year_results <- data.frame(
      year = rep(year, length(mean_ndvi_values)),
      mean_ndvi = mean_ndvi_values
    )
    
    all_results <- rbind(all_results, year_results)  # Ergebnis hinzufügen
  }
  # there are always some NAs taht are omitted
  all_results<-na.omit(all_results)
  # sometimes the calculation fails so it has to be adjusted
  all_results$mean_ndvi[all_results$mean_ndvi > 1] <- all_results$mean_ndvi[all_results$mean_ndvi > 1] / 10000
  all_results <- all_results[all_results$mean_ndvi >= 0.1, ]
  # some final touch ups: adding correct row names
  all_results$filename <- rownames(all_results)
  
  # Datum extrahieren (Muster YYYYMMDD)
  all_results$Datum <- sub(".*(\\d{8})T.*", "\\1", all_results$filename)
  
  all_results$Datum <- as.Date(all_results$Datum, format = "%Y%m%d")
  
  all_results$filename <- NULL
  
  # Row-Namen deletion -> tehy are not necessary 
  rownames(all_results) <- NULL
  # extract the necessary columns 
  all_results<-all_results[,c(2:3)]
  return(all_results)  # returning the final df 
}
################################################################################