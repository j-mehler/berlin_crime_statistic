#Creating a small multiples plot that displays the percentage change in crime frequencies from 2019 to 2020, differentiating between different 
#types of crime and Berlin's city districts.


#---------------------------------------------------------------------------------------------------------------------------------------------------

# Loading required libraries

library(tidyverse)
library(sf)
library(tmap)
library(RColorBrewer)

#---------------------------------------------------------------------------------------------------------------------------------------------------

# First step: Summing up the total number of crimes grouped by year and city district. For that purpose, I can use the `Fallzahlen```-object
# which has been created in the tidy_data script, 

percentages_fallzahlen <- fallzahlen %>% 
  group_by(Jahr, Stadtteil) %>% 
  summarise(across(Straftaten_insgesamt:Kieztaten, sum)) %>% 
  na.omit()

# In order to be able to plot the crimes across city district, we need an alternative shapefile that contrains the location vectors of the district
# borders 

district_shape <- st_read("data/district_shape/bezirksgrenzen.shp")

# Selecting relevant columns only (Gemeinde_n is the districts's name, geometry stores the respective location vectos)

district_shape <- district_shape %>% 
  select(Gemeinde_n, geometry)

# In the next step, we want to add the location vectors to the dataframe containing crime numbers. For this purpose, we are changing the column name 
# indicating the name of the city district in the shapefile dataframe to "Stadtteil". Doing so, both the crime number dataframe and the shapefile data-
# frame contain a column with identical name for the city districts. This can then be used for merging. 

colnames(district_shape) <- c("Stadtteil", "geometry")

# Merging dataframe with the district geometry data 
percentages_fallzahlen <- merge(district_shape, percentages_fallzahlen, by.x = "Stadtteil", by.y = "Stadtteil")

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Calculating the percentage change in crimes between 2019 and 2020 across city districts and crime types 

pct <- function(x) {(x - lag(x))/lag(x)*100}

new_percentages <- percentages_fallzahlen %>% 
  group_by(Stadtteil) %>% 
  mutate_each(funs(pct), c(Straftaten_insgesamt:Kieztaten)) %>% 
  na.omit()

# For creating the final small multiples plot, we need to transform the data into long format (pivot). More specifically, we want to have one 
# column/variable that contains information on each crime type instead of an own coliumn for each crime type. Thus, we use the pivot_longer command
# for transforming the dataframe and storing it in the object piv_data. 

piv_data <- new_percentages %>% 
  pivot_longer(cols = c(3:19), 
               names_to = "Straftat", 
               values_to = "Percentage_Change")

# As the last step before creating the final plot, we decided to discard some of the crime categories provided by the original data set. More 
# specifically, we decided to disregard some sub-categories that are part of also available in broader categories (as bag-snatching, which is part of 
# theft in general). In case of numerous subcategories (as theft from vehicles, car theft, and bicycle theft), we only kept these for making potential
# differences comparable. Ultimately, the numbers of crimes in general are not considered since they are part of an ealier plot, and "Kieztaten" are 
# not considered since they contain numerous sub-categories which are not comparable. 

small_multiple_crime <- subset(piv_data, Straftat!="Brandstiftung" & 
                                 Straftat!="Sachbeschädigung_Graffiti" & 
                                 Straftat!="Straßenraub_Handtaschenraub" & 
                                 Straftat!="Straftaten_insgesamt" & 
                                 Straftat!="Diebstahl_insgesamt" & 
                                 Straftat!="Kieztaten" & 
                                 Straftat!="Gefährliche_und_schwere_Körperverletzung")

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Ultimately, we are creating the final small multiples plot using the small_multiple_crime object we created in the previous step. We are using 
# a red-blue color palette and specify that negative values (reduction of crime figures) are colored on blue, whereas increase is colored in red. 
# Furthermore, we are specifying six brackets that group percentage changes together. The color palette makes differences in rates visible by
# gradually changing the color for the different brackets. Ultimately, we are translating the crime types. 

plot_1 <- 
tm_shape(small_multiple_crime) +
  tm_facets(by = "Straftat") +
  tm_polygons("Percentage_Change", palette = "-RdBu", breaks = c(-Inf, -20, -10, 0, 10, 20, Inf)) +
  tm_layout(panel.labels = c("Arson", 
                             "Theft from Vehicles", 
                             "Car Theft", 
                             "Bicycle Theft", 
                             "Coercion and Threat", 
                             "Bodily Harm", 
                             "Robbery", 
                             "Drug-related Offences", 
                             "Property Damage", 
                             "Domestic Burglary")) +
  tmap_mode("plot")

tmap_save(plot_1, "figures//fig_1.png", width = 7)

