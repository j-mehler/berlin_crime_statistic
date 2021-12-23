# Import data and clean it for further analysis

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Loading required libraries

library(tidyverse)
library(readxl) 
library(sf)
library(ggplot2)
library(naniar)


#---------------------------------------------------------------------------------------------------------------------------------------------------

# First step: Using a function, read in the complete excel sheet containing the data with absolute and frequency numbers of crimes in Berlin per Lor-Disrict between 2012 and 2020

read_excel_allsheets <- function(filename, tibble = FALSE) {
  
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

sheets <- read_excel_allsheets("data/Fallzahlen&HZ 2012-2020.xlsx")



#---------------------------------------------------------------------------------------------------------------------------------------------------

# Step 2: Preparing the 2019 dataset with absolute crime numbers

# select the Dataframe for 2019 and exclude empty first three lines

fallzahlen2019 <- sheets$Fallzahlen_2019[-c(1:3), ]

# Convert first row to colnames
fallzahlen2019_new <- fallzahlen2019                
colnames(fallzahlen2019_new) <- fallzahlen2019[1, ]         

fallzahlen2019_new <- fallzahlen2019_new[-c(1), ]

# Adding a new column containing the name of the city district. The LOR key can be used for creating this variabe. 
fallzahlen2019_new <- fallzahlen2019_new %>%
  mutate(Stadtteil = case_when(
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "01") ~ "Mitte",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "02") ~ "Friedrichshain-Kreuzberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "03") ~ "Pankow",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "04") ~ "Charlottenburg-Wilmersdorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "05") ~ "Spandau",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "06") ~ "Steglitz-Zehlendorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "07") ~ "Tempelhof-Schöneberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "08") ~ "Neukölln",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "09") ~ "Treptow-Köpenick",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "10") ~ "Marzahn-Hellersdorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "11") ~ "Lichtenberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "12") ~ "Reinickendorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "99") ~ "NA"
  ))

# delete lines with values for Stadtteil in total
fallzahlen2019_new <- subset(fallzahlen2019_new, fallzahlen2019_new$`Bezeichnung (Bezirksregion)`!= fallzahlen2019_new$Stadtteil)

# Adding a year variable 
fallzahlen2019_new$year <- "2019"



#---------------------------------------------------------------------------------------------------------------------------------------------------

# Step 3: Preparing the 2020 dataset with absolute crime numbers

# select the Dataframe for 2019 and exclude empty first three lines

fallzahlen2020 <- sheets$Fallzahlen_2020[-c(1:3), ]

# Convert first row to colnames
fallzahlen2020_new <- fallzahlen2020                    
colnames(fallzahlen2020_new) <- fallzahlen2020[1, ]         


fallzahlen2020_new <- fallzahlen2020_new[-c(1), ]

# Adding a new column containing the name of the city district. The LOR key can be used for creating this variabe. 

fallzahlen2020_new <- fallzahlen2020_new %>%
  mutate(Stadtteil = case_when(
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "01") ~ "Mitte",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "02") ~ "Friedrichshain-Kreuzberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "03") ~ "Pankow",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "04") ~ "Charlottenburg-Wilmersdorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "05") ~ "Spandau",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "06") ~ "Steglitz-Zehlendorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "07") ~ "Tempelhof-Schöneberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "08") ~ "Neukölln",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "09") ~ "Treptow-Köpenick",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "10") ~ "Marzahn-Hellersdorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "11") ~ "Lichtenberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "12") ~ "Reinickendorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "99") ~ "NA"
  ))

# delete lines with values for Stadtteil in total
fallzahlen2020_new <- subset(fallzahlen2020_new, fallzahlen2020_new$`Bezeichnung (Bezirksregion)`!= fallzahlen2020_new$Stadtteil)

# Adding a year variable 
fallzahlen2020_new$year <- "2020"


#---------------------------------------------------------------------------------------------------------------------------------------------------

# Step 4:Appending 2019 and 2020 and further tidying


# Appending both years
fallzahlen <- bind_rows(fallzahlen2019_new, fallzahlen2020_new)

# replace NA strings with logical values
fallzahlen <- fallzahlen %>% 
  replace_with_na(replace = list(Stadtteil = "NA"))

# show list of variable names
fallzahlen %>% colnames()

# Delete spaces from column names
colnames(fallzahlen) <- c("LOR_Schlüssel_Bezirksregion", "Bezeichnung_Bezirksregion", "Straftaten_insgesamt", "Raub", "Straßenraub_Handtaschenraub", "Körperverletzungen_insgesamt", "Gefährliche_und_schwere_Körperverletzung", "Freiheitsberaubung_Nötigung_Bedrohung_Nachstellung", "Diebstahl_insgesamt", "Diebstahl_Kraftwagen", "Diebstahl_aus_Kfz", "Fahrraddiebstahl", "Wohnraumeinbruch", "Branddelikte_insgesamt", "Brandstiftung", "Sachbeschädigung_insgesamt", "Sachbeschädigung_Graffiti", "Rauschgiftdelikte", "Kieztaten", "Stadtteil", "Jahr")

# check and adjust data types
glimpse(fallzahlen)

# change all numerical observations into numeric values, change year variable into date type
fallzahlen <- fallzahlen %>% 
  mutate_at(c(3:19), as.numeric)


# sort columns (strings first, then numeric observations)
fallzahlen <- subset(fallzahlen, select = c(1, 2, 20, 21, 3:19)) 


#---------------------------------------------------------------------------------------------------------------------------------------------------

# Step 5: Reading in Geometry Data and joining with Crime Data

# import shapefiles
shapes_berlin_raw <- st_read("data/Shapefiles/lor_bezirksregionen.shp")

# select relevant columns
shapes_berlin <- shapes_berlin_raw %>% select(broker.Dow, geometry)

# change column name of LOR to use it as key column for merging
colnames(shapes_berlin) <- c("LOR_Schlüssel_Bezirksregion", "geometry")

# merge geometry and Crime data
fallzahlen_geo <- merge(shapes_berlin, fallzahlen, by.x = "LOR_Schlüssel_Bezirksregion", by.y = "LOR_Schlüssel_Bezirksregion")


#---------------------------------------------------------------------------------------------------------------------------------------------------

# Step 6: Same procedure for preparing the 2019 dataset with frequency numbers


## select the Dataframe for 2019 and exclude empty first three lines
hz2019 <- sheets$HZ_2019[-c(1:3), ]

#put first row as colnames
hz2019_new <- hz2019                                    # Duplicate data frame
colnames(hz2019_new) <- hz2019[1, ]     # Convert first row to header

hz2019_new <- hz2019_new[-c(1), ]

# Adding a new column containing the name of the city district. The LOR key can be used for creating this variabe. 

hz2019_new <- hz2019_new %>%
  mutate(Stadtteil = case_when(
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "01") ~ "Mitte",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "02") ~ "Friedrichshain-Kreuzberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "03") ~ "Pankow",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "04") ~ "Charlottenburg-Wilmersdorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "05") ~ "Spandau",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "06") ~ "Steglitz-Zehlendorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "07") ~ "Tempelhof-Schöneberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "08") ~ "Neukölln",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "09") ~ "Treptow-Köpenick",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "10") ~ "Marzahn-Hellersdorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "11") ~ "Lichtenberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "12") ~ "Reinickendorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "99") ~ "NA"
  ))

hz2019_new <- subset(hz2019_new, hz2019_new$`Bezeichnung (Bezirksregion)`!= hz2019_new$Stadtteil)

hz2019_new$year <- "2019"



#---------------------------------------------------------------------------------------------------------------------------------------------------

# Step 7: Same procedure for preparing the 2020 dataset with frequency numbers


# Code for 2020

hz2020 <- sheets$HZ_2020[-c(1:3), ]

# Convert first row to colnames
hz2020_new <- hz2020                  
colnames(hz2020_new) <- hz2020[1, ]  


hz2020_new <- hz2020_new[-c(1), ]

# Adding a new column containing the name of the city district. The LOR key can be used for creating this variabe. 

hz2020_new <- hz2020_new %>%
  mutate(Stadtteil = case_when(
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "01") ~ "Mitte",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "02") ~ "Friedrichshain-Kreuzberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "03") ~ "Pankow",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "04") ~ "Charlottenburg-Wilmersdorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "05") ~ "Spandau",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "06") ~ "Steglitz-Zehlendorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "07") ~ "Tempelhof-Schöneberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "08") ~ "Neukölln",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "09") ~ "Treptow-Köpenick",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "10") ~ "Marzahn-Hellersdorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "11") ~ "Lichtenberg",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "12") ~ "Reinickendorf",
    startsWith(`LOR-Schlüssel (Bezirksregion)`, "99") ~ "NA"
  ))

# delete lines with values for Stadtteil in total
hz2020_new <- subset(hz2020_new, hz2020_new$`Bezeichnung (Bezirksregion)`!= hz2020_new$Stadtteil)

# Adding a year variable 
hz2020_new$year <- "2020"


#---------------------------------------------------------------------------------------------------------------------------------------------------

# Step 8: Appending frequency numbers of 2019 and 2020 and further tidying


# Appending both years
hz <- bind_rows(hz2019_new, hz2020_new)

# replace NA strings with logical values
hz <- hz %>% 
  replace_with_na(replace = list(Stadtteil = "NA"))

# show list of variable names
fallzahlen %>% colnames()

# Delete spaces from column names
colnames(hz) <- c("LOR_Schlüssel_Bezirksregion", "Bezeichnung_Bezirksregion", "hz_Straftaten_insgesamt", "hz_Raub", "hz_Straßenraub_Handtaschenraub", "hz_Körperverletzungen_insgesamt", "hz_Gefährliche_und_schwere_Körperverletzung", "hz_Freiheitsberaubung_Nötigung_Bedrohung_Nachstellung", "hz_Diebstahl_insgesamt", "hz_Diebstahl_Kraftwagen", "hz_Diebstahl_aus_Kfz", "hz_Fahrraddiebstahl", "hz_Wohnraumeinbruch", "hz_Branddelikte_insgesamt", "hz_Brandstiftung", "hz_Sachbeschädigung_insgesamt", "hz_Sachbeschädigung_Graffiti", "hz_Rauschgiftdelikte", "hz_Kieztaten", "Stadtteil", "Jahr")

# check and adjust data types
glimpse(hz)

# change all numerical observations into numeric values, change year variable into date type
hz <- hz %>% 
  mutate_at(c(3:19), as.numeric)


# sort columns (strings first, then numeric observations)
hz <- subset(hz, select = c(1, 2, 20, 21, 3:19)) 



#---------------------------------------------------------------------------------------------------------------------------------------------------

# Step 9:merge geometry and Crime data

# join shapes and frequency numbers (hz)
hz_geo <- merge(shapes_berlin, hz, by.x = "LOR_Schlüssel_Bezirksregion", by.y = "LOR_Schlüssel_Bezirksregion")


#---------------------------------------------------------------------------------------------------------------------------------------------------

# Step 10: create a dataset combining absolute and frequency numbers

# exclude columns that are included in both original dataset

fallzahlen_abs_hz <- bind_cols(fallzahlen, hz[ , -c(1:4)])

# add the geometry data

fallzahlen_abs_hz_geo <- merge(shapes_berlin, fallzahlen_abs_hz, by.x = "LOR_Schlüssel_Bezirksregion", by.y = "LOR_Schlüssel_Bezirksregion")

