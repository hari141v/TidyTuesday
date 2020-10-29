#load necessary pacakages

{
  library(tidyverse)
  library(mapcan)
  library(patchwork)
  library(cowplot)
  
}



#load canada boundaries data from mapcan library

ca <- mapcan(boundaries = province,
             type = standard)

#import the data

tuesdata <- tidytuesdayR::tt_load(2020, week = 44)

wind_turbine <- tuesdata$`wind-turbine`


#fill the na value in project name column

wind_turbine$project_name[is.na(wind_turbine$project_name)] <-
  "St. Lawrence"


# create a total number of turbines variable

wind_turbine <- wind_turbine %>%
  group_by(province_territory) %>%
  mutate(totalp = n()) %>%
  ungroup()



# select province_territory and totalp  and create new dataframe

wind_turbine_total <- wind_turbine[, c(2, 16)] %>%
  unique()


#create a dataframe for total_capacity_mw in each province

wind_turbine_total_mw <-  wind_turbine %>%
  group_by(province_territory, project_name) %>%
  select(province_territory, project_name, total_project_capacity_mw) %>%
  unique() %>%
  group_by(province_territory) %>%
  summarise(total_capacity_mw = sum(total_project_capacity_mw))




# inner join canada boundaries data frame with wind_turbine_total data frame

wind_turbine_total <- ca %>%
  inner_join(
    wind_turbine_total,
    keep = T,
    by = c("pr_english" = "province_territory")
  )

#inner_join wind_turbine_total data frame with wind_turbine_total_mw  data frame

wind_turbine_total <- wind_turbine_total %>%
  inner_join(wind_turbine_total_mw,
             by = c("pr_english" = "province_territory"))




# create data frame for choropleth map label

region.lab.data <- wind_turbine_total %>%
  group_by(pr_alpha) %>%
  summarise(long = mean(long), lat = mean(lat))



#create choropleth map from coordinates

map <- ggplot(data = wind_turbine_total,
              aes(x = long, y = lat, group = group))

#Total Wind Turbines in Each Province-map

total_wt_map <- map +
  geom_polygon(aes(fill = totalp), color = 'gray', size = 0.1) +
  scale_fill_gradient(high = "#e34a33",
                      low = "#fee8c8",
                      name = "Total Wind Turbines") +
  geom_text(
    data = region.lab.data,
    show.legend = F,
    aes(group = 1,
        #color=pr_alpha,
        label = pr_alpha),
    fontface = "bold",
    size = 4,
    hjust = 0.5
  ) +
  labs(title = "Total Wind Turbines in Each Province") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 18,
                                  face = "bold"))
coord_fixed()

#Total Wind Turbines in Each Province-bar

total_wt_bar <-   wind_turbine_total %>% group_by(pr_english) %>%
  select(pr_english, totalp) %>% unique() %>%
  ggplot(aes(
    x =  reorder(pr_english, totalp),
    y = totalp,
    fill = totalp,
    label = totalp
  )) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8",
                      guide = "colorbar") +
  geom_text(hjust = -0.01, fontface = "bold") +
  labs(
    x = "Province Territory",
    y = "Total Wind Turbines",
    subtitle = "",
    caption = "Source:  Government of Canada | TidyTuesday"
  ) +
  theme_dark() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    plot.caption = element_text(face = "bold"),
    plot.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
    
  ) + coord_flip()

(total_wt_comb <- (total_wt_map / total_wt_bar))



#Save the plot

ggsave(
  "Total Wind Turbines in Each Province.jpeg",
  plot = total_wt_comb,
  limitsize = F,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 300
)






#Wind Generation Capacity in Each Province-map

total_mw_map <- map +
  geom_polygon(aes(fill = total_capacity_mw),
               color = 'gray',
               size = 0.1) +
  scale_fill_gradient(high = "#e34a33",
                      low = "#fee8c8",
                      name = "Total Capacity(MW)") +
  geom_text(
    data = region.lab.data,
    show.legend = F,
    aes(group = 1,
        #color=pr_alpha,
        label = pr_alpha),
    fontface = "bold",
    size = 4,
    hjust = 0.5
  ) +
  labs(title = "Wind Generation Capacity in Each Province") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 18,
                                  face = "bold"))
coord_fixed()

#Wind Generation Capacity in Each Province-bar

total_mw_bar <-   wind_turbine_total %>%
  group_by(pr_english) %>%
  select(pr_english, total_capacity_mw) %>% unique() %>%
  ggplot(
    aes(
      x =  reorder(pr_english, total_capacity_mw),
      y = total_capacity_mw,
      fill = total_capacity_mw,
      label = total_capacity_mw
    )
  ) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8",
                      guide = "colorbar") +
  geom_text(hjust = -0.01, fontface = "bold") +
  labs(
    x = "Province Territory",
    y = "Total Capacity(MW)",
    subtitle = "",
    caption = "Source:  Government of Canada | TidyTuesday"
  ) +
  theme_dark() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    plot.caption = element_text(face = "bold"),
    plot.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
    
  ) + coord_flip()

(total_mw_comb <- (total_mw_map / total_mw_bar))



#Save the plot

ggsave(
  "Wind Generation Capacity in Each Province.jpeg",
  plot = total_mw_comb,
  limitsize = F,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 300
)
