#Creating a faceted interactive map that displays the change in crime frequency numbers from 2019 to 2020 in Lor-Districts
#by clicking on the Lor-district, the frequency number as well as the absolute number of offences is shown


#---------------------------------------------------------------------------------------------------------------------------------------------------

# Loading required libraries
library(sp) #for spatial coding
library(tmap) #for spatial visualiziation

#---------------------------------------------------------------------------------------------------------------------------------------------------

# First step: setting tmap on interactive mode
tmap_mode("view")
tmap_last()


#### replace the values of "Forst Grunewald" with NAs as this Lor-district has too few residents to make sens of the frequency number per 100.000 residents

fallzahlen_abs_hz_geo_new <- replace_with_na(fallzahlen_abs_hz_geo, replace = list(hz_Straftaten_insgesamt = c(204478,187692)))

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Next step: create facet plot by year
# adding popup.vars element to display bith, absolute and frequency numbers

plot_3 <- tm_shape(fallzahlen_abs_hz_geo_new) +
  tm_polygons("hz_Straftaten_insgesamt",
              title="Criminal Offences", 
              popup.vars=c(
                "Number of offences per 100,000 residents: "="hz_Straftaten_insgesamt",
                "Absolute number of  offences: " = "Straftaten_insgesamt"),
              id = "Bezeichnung_Bezirksregion", style="cont", palette="Reds", showNA = FALSE) +
  tm_facets(by = "Jahr", nrow = 2)
 # tm_layout(main.title = "Criminal Offences in 2019 and 2020")



