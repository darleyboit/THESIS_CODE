library(haven)

#AL data
AL_2000 <- read_dta("~/Desktop/dataverse_files/AL_2000.dta")
AL_2004 <- read_dta("~/Desktop/dataverse_files/AL_2004.dta")
AL_2008 <- read_dta("~/Desktop/dataverse_files/AL_2008.dta")
AL_2012 <- read_dta("~/Desktop/dataverse_files/AL_2012.dta")
AL_2012$county <- toupper(AL_2012$county)
  
al1 <- merge(AL_2000, AL_2004, by = c("county", "precinct", "year"), all = TRUE)
al2 <- merge(AL_2008, AL_2012, by = c("county", "precinct", "year"), all = TRUE)
al_all <- merge(al1, al2, by = c("county", "precinct", "year"), all = TRUE) 

#AR data
AR_2000 <- read_dta("~/Desktop/dataverse_files/AR_2000.dta")
AR_2004 <- read_dta("~/Desktop/dataverse_files/AR_2004.dta")
AR_2008 <- read_dta("~/Desktop/dataverse_files/AR_2008.dta")
AR_2012 <- read_dta("~/Desktop/dataverse_files/AR_2012.dta")
AR_2012$county <- toupper(AR_2012$county)

ar1 <- merge(AR_2000, AR_2004, by = c("county", "precinct", "year"), all = TRUE)
ar2 <- merge(AR_2008, AR_2012, by = c("county", "precinct", "year"), all = TRUE)
ar_all <- merge(ar1, ar2, by = c("county", "precinct", "year"), all = TRUE) 

View(al_all)


#WY data
WY_2000 <- read_dta("~/Desktop/dataverse_files/WY_2000.dta")
WY_2004 <- read_dta("~/Desktop/dataverse_files/WY_2004.dta")
WY_2008 <- read_dta("~/Desktop/dataverse_files/WY_2008.dta")
WY_2012 <- read_dta("~/Desktop/dataverse_files/WY_2012.dta")
WY_2012$county <- toupper(WY_2012$county)

##NEED TO SELECT+RENAME COLUMNS HERE.... 

colnames(WY_2000) <- c("state", "year", "county", "precinct", "votes_dem", "votes_rep", "votes_total")
WY_2000 <- WY_2000[, c("state", "year", "county", "precinct", "votes_dem", "votes_rep", "votes_total")]
colnames(WY_2004) <- c("state", "year", "county", "precinct", "votes_dem", "votes_rep", "votes_total")
WY_2004 <- WY_2004[, c("state", "year", "county", "precinct", "votes_dem", "votes_rep", "votes_total")]
colnames(WY_2008) <- c("state", "year", "county", "precinct", "votes_dem", "votes_rep", "votes_total")
WY_2008 <- WY_2008[, c("state", "year", "county", "precinct", "votes_dem", "votes_rep", "votes_total")]
colnames(WY_2012) <- c("state", "year", "county", "precinct", "votes_dem", "votes_rep", "votes_total")
WY_2012 <- WY_2012[, c("state", "year", "county", "precinct", "votes_dem", "votes_rep", "votes_total")]

WY_all <- rbind(WY_2000, WY_2004, WY_2008, WY_2012)

library(readr)
cleanpatent <- read_csv("data_cleaned/cleanpatent.csv")
WY_patent <- cleanpatent |>
  filter(state == "WY")
View(WY_patent)

#install.packages("sf")
library(sf)
wy_shape <- st_read("WY_Shapefile/WY_final.shp")  # Just the .shp file
print(wy_shape)

coords_sf <- st_as_sf(WY_patent, coords = c("lng", "lat"), crs = 4326)
joined_data <- st_join(coords_sf, wy_shape)
print(joined_data)

st_crs(coords_sf)  # CRS of the points
st_crs(wy_shape)   # CRS of the shapefil









