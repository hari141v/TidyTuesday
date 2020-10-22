#load pacakages

{
  library(tidyverse)
  library(noncensus)
  library(tidytext)
  library(RColorBrewer)
}


#import the data

tuesdata <- tidytuesdayR::tt_load(2020, week = 43)

beer_awards <- tuesdata$beer_awards


#load states data from noncensus package for division map
data("states")








#recode the values

beer_awards <- beer_awards %>% mutate(state = 
                              recode(state, "wa" = "WA",
                                             "Ak" = "AK"))


#by using states data get the division from state
beer_awards <- beer_awards %>% 
                left_join(states[, c(1:4)],
                          by = "state")


#create
year_seq <- seq(1987, 2020, by = 3)

#US Division Wise Medal Counts from 1987 to 2020

(
  div_medal <- beer_awards %>% # filter(medal=="Gold") %>%
    group_by(year, division, medal) %>%
    mutate(counts = n()) %>%
    filter(year %in% year_seq) %>%
    ggplot(aes(
      x = year,
      y = counts,
      color = medal,
      label = counts
    )) +
    geom_line(size = 1) + geom_point(size = 2) +
    geom_text(
      data = . %>% filter(year == 1987),
      color = "black",
      nudge_x = -1,
      nudge_y = 0.5,
      fontface = "bold",
      size = 4
    ) +
    geom_text(
      data = . %>% filter(year == 2020),
      color = "black",
      nudge_x = 1.5,
      nudge_y = 0.2,
      fontface = "bold",
      size = 4
    ) +
    guides(
      col = guide_legend(
        override.aes = list(size = 2),
        nrow = 1,
        title = "Medal",
        title.position = "top",
        byrow = T
      )
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(
      breaks = seq(1987, 2020, by = 3),
      labels = sprintf('%02d', seq(1987, 2020, by = 3) %% 100)
    ) +
    labs(
      title = "US Division Wise Medal Counts from 1987 to 2020",
      x = "Year",
      y = "",
      subtitle = "Great American Beer Festival Medals",
      caption = "Source:  Great American Beer Festival | TidyTuesday"
    ) +
    
    facet_wrap(~ division, scales = "free", nrow = 3) +
    theme(
      legend.position = "bottom",
      legend.background = element_rect(fill = "transparent",
                                       colour = NA),
      legend.title = element_text(size = 10, hjust = 0.5,
                                  face = "bold"),
      legend.text = element_text(size = 9, face = "bold"),
      legend.key = element_rect(fill = NA, color = NA),
      axis.ticks = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        color = "#002868",
        face = "bold"
      ),
      plot.caption = element_text(face = "bold"),
      strip.background.x = element_rect(fill = "#BF0A30",
                                        linetype = "blank"),
      strip.text.x       =   element_text(face = "bold",
                                          color = "white",
                                          size = 15),
      plot.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    )
)


#Save the plot

ggsave(
  "US Division Wise Medal Counts from 1987 to 2020.jpeg",
  plot = div_medal,
  limitsize = F,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 300
)


#Top  Divisions on Various Category


(top_div<-beer_awards %>% group_by(division) %>%
  mutate(
    Beer = sum(!is.na(beer_name)),
    Brewery_Company = sum(!is.na(brewery)),
    Beer_Category = sum(!is.na(category)),
    Total_Gold_Medal = sum(!is.na(medal[medal == "Gold"]))
  ) %>%
  select(division,
         Beer,
         Brewery_Company,
         Beer_Category,
         Total_Gold_Medal) %>%
  gather(
    categories,
    values,
    c(Beer,
      Brewery_Company,
      Beer_Category, Total_Gold_Medal),
    factor_key = T,
    convert = T
  ) %>% unique() %>%
  as.data.frame() %>%
  
  ggplot(aes(
    x = reorder_within(division, values, categories),
    y = values,
    fill = division,
    label = values
  )) +
  geom_bar(stat = "identity") +
  geom_text(color = "white",
            fontface = "bold",
            hjust = 1) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Top  Divisions on Various Category",
    x = "",
    y = "",
    subtitle = "",
    caption = "Source:  Great American Beer Festival | TidyTuesday"
  ) +
  scale_x_reordered() +
  facet_wrap( ~ categories, scales = "free", ncol = 2) +
  theme(
    legend.position = "none",
    legend.background = element_rect(fill = "transparent",
                                     colour = NA),
    legend.title = element_text(size = 10, hjust = 0.5,
                                face = "bold"),
    legend.text = element_text(size = 9, face = "bold"),
    legend.key = element_rect(fill = NA, color = NA),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    plot.title = element_text(
      hjust = 0.5,
      color = "#002868",
      face = "bold"
    ),
    plot.caption = element_text(face = "bold"),
    strip.background.x = element_rect(fill = "#f28e1c",
                                      linetype = "blank"),
    strip.text.x       =   element_text(face = "bold",
                                        color = "white",
                                        size = 15),
    plot.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
  ) + coord_flip())



#Save the plot

ggsave(
  "Top  Divisions on Various Category.jpeg",
  plot = top_div,
  limitsize = F,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 300
)
