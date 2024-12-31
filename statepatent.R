#goal of this script: cleaning our data set

#loading packages
library(tidyverse)
library(vroom)
library(dplyr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SEPARATING PATENT DATA ---- THIS DOESNT HAVE PER CAPITA MEASURE THO WHICH IS AN ISSUE. 
# opening patent data
setwd("/Users/darleyboit/Desktop")

uspatent <- read_csv("Gov99r_Quant/data_cleaned/only_half_clean/uspatent.csv") |>
  mutate(filing_date = substr(filing_date, 1, 4)) |>
  select(appln_id, filing_date, lat, lng, name_2, name_1)
#renaming columns  
colnames(uspatent) <- c("appln_id", "year", "lat", "lng", "county", "state")

#Alabama - AL
alpatent <- subset(uspatent, state == "Alabama")

alpatent <- alpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

View(alpatent)

#Alaska - AK
akpatent <- subset(uspatent, state == "Alaska")

akpatent <- akpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Arizona - AZ
azpatent <- subset(uspatent, state == "Arizona")

azpatent <- azpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Arkansas - AR
arpatent <- subset(uspatent, state == "Arkansas")

arpatent <- arpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#California - CA
capatent <- subset(uspatent, state == "California")

capatent <- capatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Colorado - CO
copatent <- subset(uspatent, state == "Colorado")

copatent <- copatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Connecticut - CT
ctpatent <- subset(uspatent, state == "Connecticut")

ctpatent <- ctpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Delaware - DE
depatent <- subset(uspatent, state == "Delaware")

depatent <- depatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#DO I NEED TO DO DC?

#Florida - FL
flpatent <- subset(uspatent, state == "Florida")

flpatent <- flpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Georgia - GA
gapatent <- subset(uspatent, state == "Georgia")

gapatent <- gapatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Hawaii - HI
hipatent <- subset(uspatent, state == "Hawaii")

hipatent <- hipatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Idaho - ID
idpatent <- subset(uspatent, state == "Idaho")

idpatent <- idpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Illinois - IL
ilpatent <- subset(uspatent, state == "Illinois")

ilpatent <- ilpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Indiana - IN
inpatent <- subset(uspatent, state == "Indiana")

inpatent <- inpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Iowa - IA
iapatent <- subset(uspatent, state == "Iowa")

iapatent <- iapatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)
  
#Kansas - KS
kspatent <- subset(uspatent, state == "Kansas")

kspatent <- kspatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)


#Kentucky - KY
kypatent <- subset(uspatent, state == "Kentucky")

kypatent <- kypatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Louisiana - LA
lapatent <- subset(uspatent, state == "Louisiana")

lapatent <- lapatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Maine - ME
mepatent <- subset(uspatent, state == "Maine")

mepatent <- mepatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Maryland - MD
mdpatent <- subset(uspatent, state == "Maryland")

mdpatent <- mdpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Massachusetts - MA
mapatent <- subset(uspatent, state == "Massachusetts")

mapatent <- mapatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Michigan - MI
mipatent <- subset(uspatent, state == "Michigan")

mipatent <- mipatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Minnesota - MN
mnpatent <- subset(uspatent, state == "Minnesota")

mnpatent <- mnpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Mississippi - MS
mspatent <- subset(uspatent, state == "Mississippi")

mspatent <- mspatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Missouri - MO
mopatent <- subset(uspatent, state == "Missouri")

mopatent <- mopatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Montana - MT
mtpatent <- subset(uspatent, state == "Montana")

mtpatent <- mtpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Nebraska - NE
nepatent <- subset(uspatent, state == "Nebraska")

nepatent <- nepatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Nevada - NV
nvpatent <- subset(uspatent, state == "Nevada")

nvpatent <- nvpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#New Hampshire - NH
nhpatent <- subset(uspatent, state == "New Hampshire")

nhpatent <- nhpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#New Jersey - NJ
njpatent <- subset(uspatent, state == "New Jersey")

njpatent <- njpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#New Mexico - AK
nmpatent <- subset(uspatent, state == "New Mexico")

nmpatent <- nmpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#New York - NY
nypatent <- subset(uspatent, state == "New York")

nypatent <- nypatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#North Carolina - NC
ncpatent <- subset(uspatent, state == "North Carolina")

ncpatent <- ncpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#North Dakota - ND
ndpatent <- subset(uspatent, state == "North Dakota")

ndpatent <- ndpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Ohio - OH
ohpatent <- subset(uspatent, state == "Ohio")

ohpatent <- ohpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Oklahoma - OK
okpatent <- subset(uspatent, state == "Oklahoma")

okpatent <- okpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Oregon - OR
orpatent <- subset(uspatent, state == "Oregon")

orpatent <- orpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Pennsylvania - PA
papatent <- subset(uspatent, state == "Pennsylvania")

papatent <- papatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Rhode Island - RI
ripatent <- subset(uspatent, state == "Rhode Island")

ripatent <- ripatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#South Carolina - SC
scpatent <- subset(uspatent, state == "South Carolina")

scpatent <- scpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#South Dakota - SD
sdpatent <- subset(uspatent, state == "South Dakota")

sdpatent <- sdpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Tennessee - TN
tnpatent <- subset(uspatent, state == "Tennessee")

tnpatent <- tnpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Texas - TX
txpatent <- subset(uspatent, state == "Texas")

txpatent <- txpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Utah - UT
utpatent <- subset(uspatent, state == "Utah")

utpatent <- utpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Vermont - VT
vtpatent <- subset(uspatent, state == "Vermont")

vtpatent <- vtpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Virginia - VA
vapatent <- subset(uspatent, state == "Virginia")

vapatent <- vapatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Washington - WA
wapatent <- subset(uspatent, state == "Washington")

wapatent <- wapatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#West Virginia - WV
wvpatent <- subset(uspatent, state == "West Virginia")

wvpatent <- wvpatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Wisconsin - WI
wipatent <- subset(uspatent, state == "Wisconsin")

wipatent <- wipatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#Wyoming - WY
wypatent <- subset(uspatent, state == "Wyoming")

wypatent <- wypatent |>
  group_by(year, county, state) |>
  summarise(
    patent = sum(patent, na.rm = TRUE),
    lat = first(lat),                  
    lng = first(lng), 
    .groups = "drop"                  
  ) |>
  filter(year >= 1968)

#I want to consolidate all of these into one
cleanpatent <- bind_rows(alpatent, akpatent, azpatent, arpatent, capatent, copatent, ctpatent, depatent, flpatent, gapatent, hipatent, idpatent, ilpatent, inpatent, iapatent, kspatent, kypatent, lapatent, mepatent, mdpatent, mapatent, mipatent, mnpatent, mspatent, mopatent, mtpatent, nepatent, nvpatent, nhpatent, njpatent, nmpatent, nypatent, nhpatent, ncpatent, ndpatent, ohpatent, okpatent, orpatent, papatent, ripatent, scpatent, sdpatent, tnpatent, txpatent, utpatent, vtpatent, vapatent, wapatent, wvpatent, wipatent, wypatent)

cleanpatent <- cleanpatent |>
  select(county, state, year, patent)
cleanpatent <- cleanpatent |> 
  mutate(year = as.numeric(year))

cleanpatent <- cleanpatent |>
  mutate(county = toupper(county))

# Recode state names to abbreviations
cleanpatent <- cleanpatent |>
  mutate(state = recode(state,
                        "Alabama" = "AL",
                        "Alaska" = "AK",
                        "Arizona" = "AZ",
                        "Arkansas" = "AR",
                        "California" = "CA",
                        "Colorado" = "CO",
                        "Connecticut" = "CT",
                        "Delaware" = "DE",
                        "Florida" = "FL",
                        "Georgia" = "GA",
                        "Hawaii" = "HI",
                        "Idaho" = "ID",
                        "Illinois" = "IL",
                        "Indiana" = "IN",
                        "Iowa" = "IA",
                        "Kansas" = "KS",
                        "Kentucky" = "KY",
                        "Louisiana" = "LA",
                        "Maine" = "ME",
                        "Maryland" = "MD",
                        "Massachusetts" = "MA",
                        "Michigan" = "MI",
                        "Minnesota" = "MN",
                        "Mississippi" = "MS",
                        "Missouri" = "MO",
                        "Montana" = "MT",
                        "Nebraska" = "NE",
                        "Nevada" = "NV",
                        "New Hampshire" = "NH",
                        "New Jersey" = "NJ",
                        "New Mexico" = "NM",
                        "New York" = "NY",
                        "North Carolina" = "NC",
                        "North Dakota" = "ND",
                        "Ohio" = "OH",
                        "Oklahoma" = "OK",
                        "Oregon" = "OR",
                        "Pennsylvania" = "PA",
                        "Rhode Island" = "RI",
                        "South Carolina" = "SC",
                        "South Dakota" = "SD",
                        "Tennessee" = "TN",
                        "Texas" = "TX",
                        "Utah" = "UT",
                        "Vermont" = "VT",
                        "Virginia" = "VA",
                        "Washington" = "WA",
                        "West Virginia" = "WV",
                        "Wisconsin" = "WI",
                        "Wyoming" = "WY"
  ))

# View the updated dataset
View(cleanpatent)

#Saving as an independent csv file
write.csv(cleanpatent, "Gov99r_Quant/data_cleaned/cleanpatent.csv", row.names = FALSE)









