################################################################################
# Script for facilitating the download of NDVI data from wekeo.com
# dowloaded data is NDVI derived from  Sentinel 2, 10 m resolution
# Paulina Harder p.harder@tu-berlin.de
# 8.1.2025
################################################################################


# libraries
library(hdar)
library(jsonlite)

# working directory
setwd("~/Recherche/Bodenfeuchte/code")

# prior to using this download routine please register on https://www.wekeo.eu/ 

# log in to platform 
username <- "your_username"
password <- "your_password"
client <- Client$new(username, password, save_credentials = TRUE) # log in
# token check to verify if login was successful
client$get_token() 

client$terms_and_conditions(term_id = 'all') # accept terms and conds

# optional: list all available datasets
# this doesn't belong to the search query, but might be interesting
# all_datasets <- client$datasets() # provides list of all available data sets, takes 2 mins, should be around 936 elements

# stichwort <- "ndvi" # the word you might want to look for in the dataset catalogue 

# search without considering uppercase and lowercase letters
# search_results <- grep(stichwort, all_datasets, ignore.case = TRUE)
# all_datasets[82] # dataset that I wanted (explained above)

# Download process

# creating individual search query 
# initiate query with code that belongs to the desired data set, found through search above or the wekeo viewer 
query_template <-client$generate_query_template("EO:EEA:DAT:CLMS_HRVPP_VI")


# make the query editable
query_template <- fromJSON(query_template, flatten = FALSE)


# Limit the time range
query_template$startdate <- "2017-04-01T00:00:00.000Z"
query_template$enddate   <- "2017-11-15T23:59:00.000Z"

# select desired product 
query_template$`productType`<-"NDVI"


# how many should be downloaded in one batch (100 ist the max)
query_template$itemsPerPage <- 100

# which satelite 
query_template$platformSerialIdentifier <-"S2B"

# select tile ID
query_template$tileId <-"32UQD"

# Convert back to JSON format, so it can be used 
query_template <- toJSON(query_template, auto_unbox = TRUE, digits = 17)

# create list of files that match the query
matches <- client$search(query_template)


# Display the IDs of the search results, to check prior download
sapply(matches$results, FUN = function(x) { x$id })

# Output
# create directory where you want your output
wd<-"~/Recherche/Bodenfeuchte/data/sentinel/2017/NDVI"
matches$download(wd) # download only 100 per hour
Y

