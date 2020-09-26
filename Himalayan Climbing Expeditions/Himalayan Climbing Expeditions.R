#Load Necessary Packages

{
  library(tidyverse)
  library(lubridate)
  library(tidytext)
  library(ggrepel)
  library(randomcoloR)
  library(RColorBrewer)
  library(patchwork)
  
}


#Import Data using tidytuesdayR package
tuesdata <- tidytuesdayR::tt_load(2020, week = 39)

peaks <- tuesdata$peaks
members <- tuesdata$members
expeditions <- tuesdata$expeditions





#Create unique color palatte

n1 <- 15
palette1 <- distinctColorPalette(n1)


#More Number of Successful Expeditions by Peaks

(
  success_exped <-  expeditions %>%
    filter(termination_reason == "Success (main peak)") %>%
    drop_na() %>%
    left_join(members %>% select(1, 7:21), by = "expedition_id") %>%
    filter(success == "TRUE" & died == "FALSE") %>%
    group_by(peak_name, season, sex) %>%
    mutate(counts = n()) %>%
    select(season, peak_name, sex, counts) %>% unique() %>%
    arrange(desc(counts)) %>% group_by(season, peak_name, sex) %>%
    filter(counts > 100) %>%
    mutate(sex = recode(sex, F = "Female", M = "Male")) %>%
    ggplot(aes(
      x = reorder_within(peak_name,-counts, season),
      y = log(counts),
      fill = peak_name
    )) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = paste(counts)),
      size = 3.5,
      hjust = 0.5 ,
      vjust = -.2,
      fontface = "bold"
    ) +
    geom_text(
      aes(y = 2, label = paste(peak_name)),
      size = 5,
      hjust = 0.5 ,
      angle = 90,
      vjust = 0.5,
      fontface = "bold"
    ) +
    scale_x_reordered() +
    scale_fill_manual(values = palette1) +
    labs(
      x = "Peak Name",
      y = "Number of Expeditions",
      title = "More Number of Successful Expeditions by Peaks",
      subtitle = "Season & Gender wise Comparison",
      caption =  "Source: The Himalayan Database | Alex Cookson "
      
    ) +
    facet_grid(sex ~ season, scales = "free", space = "free") +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title =  element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.caption = element_text(face = "bold"),
      strip.background.x = element_rect(fill = "deepskyblue3",
                                        linetype = "blank"),
      strip.text.x       =   element_text(face = "bold",
                                          color = "black", size = 13),
      strip.background.y = element_rect(fill = "lightseagreen",
                                        linetype = "blank"),
      strip.text.y       =   element_text(face = "bold",
                                          color = "black", size = 13),
      plot.background = element_rect(fill = "lightcyan2"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    )
)



#Save Image

ggsave(
  "More Number of Successful Expeditions by Peaks.jpeg",
  plot = success_exped,
  limitsize = F,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)




# Died Expeditions Members Over the Year

year_fil <- seq(1905, 2019, by = 3)

(
  year_died <- members %>%
    group_by(year) %>% filter(died == "TRUE") %>%
    mutate(counts = n()) %>%
    select(year, counts) %>%
    ggplot(aes(x = year, y = counts)) +
    geom_line(color = "indianred", size = 1) +
    geom_text(
      data = . %>%  filter(year %in% year_fil) ,
      aes(label = counts),
      color = "blue",
      vjust = -0.5
    ) +
    geom_point() +
    scale_x_continuous(breaks = seq(1905, 2019, by = 6)) +
    labs(
      x = "Year",
      y = "Died Expeditions Members Count",
      title = "Died Expeditions Members Over the Year",
      caption =  "Source: The Himalayan Database | Alex Cookson "
    ) +
    theme(
      legend.position = "none",
      axis.text = element_text(face = "bold"),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title =  element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5,
                                face = "bold"),
      plot.caption = element_text(face = "bold"),
      plot.background = element_rect(fill = "lightcyan2"),
      panel.background = element_rect(fill
                                      = "transparent"),
      panel.grid = element_blank()
    )
)

#Save Image

ggsave(
  "Died Expeditions Members Over the Year.jpeg",
  plot = year_died ,
  limitsize = F,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)




# Top Causes of Expedition Members Deaths

(
  death_cause <- members %>% group_by(death_cause) %>%
    filter(died == TRUE) %>%
    summarise(counts = n()) %>%
    select(death_cause, counts) %>%
    top_n(counts, n = 10) %>%
    ggplot(aes(
      x = reorder(death_cause, counts),
      y = counts,
      fill = death_cause
    )) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = paste(counts)),
      size = 3.5,
      hjust = -0.2 ,
      fontface = "bold"
    ) +
    scale_fill_manual(values = palette1) +
    labs(
      x = "Death Cause",
      y = "Count",
      title = "Top Causes of Expedition Members Deaths",
      caption =  "Source: The Himalayan Database | Alex Cookson "
    ) +
    theme(
      legend.position = "none",
      axis.text = element_text(face = "bold"),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title =  element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.caption = element_text(face = "bold"),
      strip.background.x = element_rect(fill = "deepskyblue3",
                                        linetype = "blank"),
      strip.text.x       =   element_text(face = "bold",
                                          color = "black", size = 13),
      strip.background.y = element_rect(fill = "lightseagreen",
                                        linetype = "blank"),
      strip.text.y       =   element_text(face = "bold",
                                          color = "black", size = 13),
      plot.background = element_rect(fill = "lightcyan2"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    ) + coord_flip()
)


#Save Image

ggsave(
  "Top Causes of Expedition Members Deaths.jpeg",
  plot = death_cause ,
  limitsize = F,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)
