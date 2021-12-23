# create table as summary and bar plot

#--------------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(knitr)
library(formattable)

# ----- 2.1 - Overview (Summary table) ------------------------------------------------------------------------------------

# create summarizing table (not differentiated by district)

# summarize all crimes
table_1_raw <- 
  fallzahlen %>% 
  group_by(Jahr) %>% 
  summarise(Straftaten_gesamt = sum(Straftaten_insgesamt),
            Raub = sum(Raub),
            #Straßenraub_Handtaschenraub = sum(Straßenraub_Handtaschenraub),
            Körperverletzungen_insgesamt = sum(Körperverletzungen_insgesamt),
            #Gefährliche_und_schwere_Körperverletzung = sum(Gefährliche_und_schwere_Körperverletzung),
            Freiheitsberaubung_Nötigung_Bedrohung_Nachstellung = sum(Freiheitsberaubung_Nötigung_Bedrohung_Nachstellung),
            #Diebstahl_insgesamt = sum(Diebstahl_insgesamt),
            Diebstahl_Kraftwagen = sum(Diebstahl_Kraftwagen),
            Diebstahl_aus_Kfz = sum(Diebstahl_aus_Kfz),
            Fahrraddiebstahl = sum(Fahrraddiebstahl),
            Wohnraumeinbruch = sum(Wohnraumeinbruch),
            Branddelikte_insgesamt = sum(Branddelikte_insgesamt),
            #Brandstiftung = sum(Brandstiftung),
            Sachbeschädigung_insgesamt = sum(Sachbeschädigung_insgesamt),
            #Sachbeschädigung_Graffiti = sum(Sachbeschädigung_Graffiti),
            Rauschgiftdelikte = sum(Rauschgiftdelikte),
            #Kieztaten = sum(Kieztaten)
  )

# As the last step before creating the final plot, we decided to discard some of the crime categories provided by the original data set. More 
# specifically, we decided to disregard some sub-categories that are part of also available in broader categories (as bag-snatching, which is part of 
# theft in general). In case of numerous subcategoreis (as theft from vehicles, car theft, and bicycle theft), we only kept these for making potential
# differences comparable. Ultimately, the numbers of crimes in general are not considered since they are part of an ealier plot, and "Kieztaten" are 
# not considered since they contain numerous sub-categories which are not comparable. 

# pivot longer: change crimes from variables to values
table_1_long <- table_1_raw %>% 
  pivot_longer(cols = 2:12, names_to ="Crime", values_to = "absolute_amount")

# pivot wider: change year values to columns for 2019 and 2020
table_1 <- table_1_long %>% 
  pivot_wider(names_from = Jahr, values_from = absolute_amount)

# calculate change in percent
change <- table_1 %>% 
  summarise(change_in_percent = (`2020`/`2019`-1)) %>% 
  round(digits = 3)

# add column with the change_in_percent to the table
table_1 <- table_1 %>% mutate(change)

#arrange by change_in_percent
table_1 <- table_1 %>% arrange(desc(change_in_percent))

# change types of offence to English
table_1$Crime <- c("Drug-related Offences",
                   "Theft from Vehicles",
                   "Property Damage",
                   "Coercion und Threat",
                   "Arson",
                   "Bodily Harm",
                   "Crimes in total", 
                   "Robbery", 
                   "Bicycle Theft",
                   "Domestic Burglary",
                   "Car Theft")

# change column names to English
colnames(table_1) <- c("Type of offence", "2019", "2020", "Trend")

# transform percentage into numeric and display as percent (using formattable), format case numbers
table_1$Trend <- as.numeric(table_1$Trend) %>% percent()

# create formatters for Straftaten_insgesamt and change_in_percent
change_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > 0, "#EF8A62", ifelse(x < 0, "#67A9CF", "#EF8A62"))),
            x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
  )

# insgesamt_formatter
insgesamt_formatter <- formatter("span", 
                                 style = ~ style(font.weight = "bold"))


table_1 <- table_1 %>% 
  formattable(
    align = c("l","r","r","r"),
    list(area(col = 1:4, row = 7) ~ insgesamt_formatter,
         Trend = change_formatter
    ))





# ----- 2.2 - Plot 2 --------------------------------------------------------------------------------------------------

# create a plot showing the evolution of total crimes per district of the city.

# summarize Stadtteil data for 2019
data1_plot2 <- 
  fallzahlen %>% 
  filter(Jahr == "2019") %>% 
  group_by(Stadtteil) %>% 
  summarise(Straftaten = sum(Straftaten_insgesamt), 
            Jahr = Jahr) %>% 
  unique()

# summarize Stadtteil data for 2020
data2_plot2 <- 
  fallzahlen %>% 
  filter(Jahr == "2020") %>% 
  group_by(Stadtteil) %>% 
  summarise(Straftaten = sum(Straftaten_insgesamt), 
            Jahr = Jahr) %>% 
  unique()

# combine years
data_plot2 <- bind_rows(data1_plot2, data2_plot2) %>% drop_na()

# plot 2a (absolute numbers)
plot_2a <- 
  data_plot2 %>% 
  ggplot(aes(x = reorder(Stadtteil, Straftaten),
             y = Straftaten,
             fill = Jahr)) +
  geom_col(width = 0.6, position = position_dodge2(width = 0.6, reverse = T)) +
  labs(title = "Absolute number of crimes per district",
       x = "Districts",
       y = "Total number of crimes") +
  scale_fill_manual(
    values = c("2019" = "#EF8A62", "2020" = "#67A9CF")
  ) +
  coord_flip() +
  theme_light()

# save as png
ggsave("figures/fig_2a.png", width = 9, height = 8, dpi = 300, plot_2a)



# same plot with HZ numbers

# summarize HZ Stadtteil data for 2019
data1_plot2b <- 
  hz %>% 
  drop_na() %>% 
  filter(Jahr == "2019") %>% 
  group_by(Stadtteil) %>% 
  summarise(Straftaten = sum(hz_Straftaten_insgesamt), 
            Jahr = Jahr) %>% 
  unique()

# summarize HZ Stadtteil data for 2020
data2_plot2b <- 
  hz %>% 
  drop_na() %>% 
  filter(Jahr == "2020") %>% 
  group_by(Stadtteil) %>% 
  summarise(Straftaten = sum(hz_Straftaten_insgesamt), 
            Jahr = Jahr) %>% 
  unique()

# combine years
data_plot2b <- bind_rows(data1_plot2b, data2_plot2b) %>% drop_na()

# plot 2b
plot_2b <- 
  data_plot2b %>% 
  ggplot(aes(x = reorder(Stadtteil, Straftaten),
             y = Straftaten,
             fill = Jahr)) +
  geom_col(width = 0.6, position = position_dodge2(width = 0.5, reverse = T)) +
  labs(title = "Frequency number of crimes per district",
       #subtitle = "Crimes per 100,000 inhabitants",
       x = "Districts",
       y = "Frequency number of crimes") +
  scale_fill_manual(
    values = c("2019" = "#EF8A62", "2020" = "#67A9CF")
  ) +
  coord_flip() +
  theme_light()

# save as png
ggsave("figures/fig_2b.png", width = 9, height = 8, dpi = 300, plot_2b)

# wrap graphs together (vertical orientation)
patchwork <- plot_2a / plot_2b

plot_2 <- patchwork + plot_annotation(
  # title = "9 Pay attention to the legend order",
  theme = theme(plot.caption = element_text(hjust = 0))
)

# save as png
ggsave("figures/fig_2.png", width = 9, height = 8, dpi = 300, plot_2)
