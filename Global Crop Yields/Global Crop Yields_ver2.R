#load necessary packages

{
  library(tidyverse)
  library(purrr)
  library(countrycode)
  library(gganimate)
  library(tidytext)
  library(ggthemes)
  library(lubridate)
  library(stringr)
  library(RColorBrewer)
  
  
}

#Load data

tuesdata <- tidytuesdayR::tt_load('2020-09-01')




key_crop_yields <- tuesdata$key_crop_yields
fertilizer  <- tuesdata$cereal_crop_yield_vs_fertilizer_application
tractors <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture
land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production
arable_land <- tuesdata$arable_land_pin



#get the unique country name and code from key_crop_yields data

country <- key_crop_yields[, c(1, 2)] %>% drop_na() %>% unique()


#By using countrycode package get the continent information from country name

country$continent <- countrycode(sourcevar = country$Entity,
                                 origin = "country.name",
                                 destination = "continent")

#some of the continent details not retrieved so manually update it

country$continent <- ifelse(
  country$Code == "OWID_CZS",
  "Europe",
  ifelse(
    country$Code == "OWID_MNS" ,
    "Oceania",
    ifelse(
      country$Code == "FSM" ,
      "Oceania",
      ifelse(
        country$Code == "OWID_PYA" ,
        "Oceania",
        ifelse(
          country$Code == "OWID_SRM" ,
          "Europe",
          ifelse(
            country$Code == "TLS" ,
            "Asia",
            ifelse(country$Code ==
                     "OWID_YGS" , "Europe", country$continent)
          )
        )
      )
    )
  )
)




#using left join, merge the continent column to key_crop_yields dataframe

key_crop_yields <- key_crop_yields %>%
  left_join(country[, c(1, 3)],
            by = "Entity")

#Rearrange the column
key_crop_yields <- key_crop_yields %>%
  select(1:3, 15, everything())


#Highest Crop Yields by Continent Over the year

(
  high_crops <- key_crop_yields %>%
    gather(crops, values, c(5:15)) %>% na.omit() %>%
    filter(values > 0) %>%
    mutate(
      crops = str_remove_all(crops, "\\(tonnes per hectare\\)"),
      crops = str_trim(crops),
      values = round(values, 1)
    )  %>%
    select(Year, continent, crops, values) %>%
    group_by(Year, continent, crops) %>%
    mutate(total = round(sum(values), 1))  %>%
    filter(
      Year %in%
        c("1961", "1971", "1981", "1991", "2001", "2011", "2018"),
      crops %in% c("Bananas", "Cassava", "Potatoes", "Maize",
                   "Wheat")
    ) %>%
    ggplot(aes(
      x = Year, y = total,
      label = total
    )) +
    geom_point(aes(color = crops), show.legend = F) +
    geom_line(aes(color = crops)) +
    geom_text(
      data = . %>% filter(Year == 2018),
      size = 3.5,
      aes(colour = crops,
          fontface = "bold"),
      check_overlap = F,
      show.legend = F
    ) +
    scale_color_brewer(palette = "Dark2") +
    guides(col = guide_legend(
      override.aes = list(size = 5),
      ncol = 1,
      title = "Crops",
      
      byrow = TRUE
    )) +
    labs(
      title = "Highest Crop Yields by Continent Over the year",
      x = "Year",
      y = "Total Tonnes/Hectare",
      caption = "Source: Our World in Data | TidyTuesday"
    ) +
    
    facet_wrap(~ continent, scales = "free") +
    theme(
      legend.position = c(0.85, 0.25),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9, face = "bold"),
      legend.key = element_rect(fill = NA, color = NA),
      axis.ticks = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.caption = element_text(face = "bold"),
      strip.background.x = element_rect(fill = "forestgreen",
                                        linetype = "blank"),
      strip.text.x       =   element_text(face = "bold",
                                          color = "white", size = 15),
      plot.background = element_rect(fill = "lemonchiffon2"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    )
)


#Save the plot

ggsave(
  "Highest Crop Yields by Continent Over the year.jpeg",
  plot = high_crops,
  limitsize = F,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)
