#Load Necessary Pacakges

{
  library(rKenyaCensus)
  library(rgdal)
  #devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
  library(tidyverse)
  library(RColorBrewer)
  library(ggmap)
  library(plyr)
}

#import kenya country shape file to create population choropleth map

KenyaCounties_SHP <- rKenyaCensus::KenyaCounties_SHP


# extract population information and create unique id for rows

map.data <-
  data.frame(id = as.numeric(rownames(KenyaCounties_SHP@data)),
             KenyaCounties_SHP@data)

# convert Population to numeric type

map.data$Population <- as.numeric(as.character(map.data$Population))

# create a variable for population proportion by county

map.data <- map.data %>% mutate(perc = prop.table(Population))

# convert curves , points to dataframe by using fortify function

map.df   <- fortify(KenyaCounties_SHP)

# convert id to numeric

map.df$id <- as.numeric(map.df$id)

# merge population data and map location data by unique id

map.df   <- merge(map.df, map.data, by.x = "id", by.y = "id")

# convert Population to numeric type

map.df$Population <-  as.numeric(as.character(map.df$Population))

# convert group to character type

map.df$group <- as.numeric(as.character(map.df$group))

# convert county to character type

map.df$County <- as.character(map.df$County)


# create a centroid dataframe for map county labels

county.lab.data <- map.df %>%
  select(County, long, lat) %>%
  group_by(County) %>%
  dplyr::summarise(long = mean(range(long)),
                   lat = mean(range(lat)))




# kenya_population

(
  kenya_population <- ggplot(map.df, aes(
    x = long, y = lat, group = id
  )) +
    geom_polygon(aes(fill = perc)) +
    geom_path(colour = "grey") +
    geom_text(
      data = county.lab.data,
      show.legend = F,
      aes(group = 1,
          label = County),
      fontface = "bold",
      size = 2,
      hjust = 0.5
    ) +
    scale_fill_gradientn(
      labels = scales::percent_format(accuracy = .1),
      colours = rev(brewer.pal(10, "Spectral"))
    ) +
    labs(
      title = "Kenya Population Distribution by County",
      x = "",
      y = "",
      subtitle = "",
      caption = "Source:   Shelmith Kariuki | rKenyaCensus | TidyTuesday"
    ) +
    theme(
      legend.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.caption = element_text(face = "bold", hjust = 1.7),
      plot.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    ) +
    coord_fixed()
)

#Save the plot

ggsave(
  "Kenya Population Distribution by County.jpeg",
  plot = kenya_population,
  limitsize = F,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 300
)




# load data gender wise population data from rKenyaCensus package

population_by_gender_county <- rKenyaCensus::V1_T2.2


# remove total category from county and change county names to upper case letters

population_by_gender_county <- population_by_gender_county %>%
  
  filter(!County == "Total") %>%
  mutate(County = str_to_upper(County))




#create a gender wise highest population category column

population_by_gender_county <- population_by_gender_county %>%
  mutate(
    cate = case_when(
      Male > Female ~ "Male Population > Female Population",
      Male < Female ~ "Female Population > Male Population"
    )
  )


# map the id number to county from map.df dataframe by using join function

population_by_gender_county <- population_by_gender_county %>%
  left_join(map.df %>%
              select(id, County) %>%
              distinct(id, County),
            by = "County")

# join location points data from map.df

population_by_gender_county <- population_by_gender_county %>%
  left_join(map.df %>%
              select(id, long, lat, group),
            by = "id")


#Gender Population

(
  gender_pop_county <- ggplot(population_by_gender_county,
                              aes(
                                x = long, y = lat, group = id
                              )) +
    geom_polygon(aes(fill = cate)) +
    geom_path(colour = "black", size = 0.7) +
    geom_text(
      color = "white",
      data = county.lab.data,
      show.legend = F,
      aes(group = 1,
          #color=pr_alpha,
          label = County),
      fontface = "bold",
      size = 2,
      hjust = 0.5
    ) +
    scale_fill_manual(values = c("#008C51", "#922529")) +
    labs(
      title = "Highest Gender Population by County",
      x = "",
      y = "",
      subtitle = "",
      caption = "Source:   Shelmith Kariuki | rKenyaCensus | TidyTuesday"
    ) +
    theme(
      axis.ticks = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.text = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.caption = element_text(face = "bold", hjust = 1.7),
      plot.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    ) +
    coord_fixed()
)

#Save the plot

ggsave(
  "Highest Gender Population by County.jpeg",
  plot = gender_pop_county,
  limitsize = F,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 300
)
