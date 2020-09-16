#load necessary packages

{
library(tidyverse)
library(noncensus)
library(RColorBrewer)
}

#get the data

tuesdata <- tidytuesdayR::tt_load(2020, week = 38)

kids <- tuesdata$kids

#load states data from noncensus package to map the division from state

data("states")



#by using this site(https://howmuch.net/articles/us-federal-expenditures-children-2018) reference some programs grouped under seperate category

kids<-kids %>% mutate(category= case_when(
                variable == "Medicaid_CHIP" ~ "Health",
                variable == "other_health" ~ "Health",
                variable == "pubhealth" ~ "Health",
                variable == "SNAP" ~ "Nutrition",
                variable == "TANFbasic" ~ "Income Security",
                variable == "fedSSI" ~ "Income Security",
                variable == "socsec" ~ "Income Security",
                variable == "HCD" ~ "Housing",
                variable == "HeadStartPriv" ~ "Early Education",
                variable == "PK12ed" ~ "Education",
                variable == "highered" ~ "Education",
                variable == "CTC" ~ "Tax Reduction",
                variable == "fedEITC" ~ "Tax Reduction",
                variable == "stateEITC" ~ "Tax Reduction"))


# by using left join match the states and division

kids<-kids %>% left_join(states[,c(2,3,4)] ,
                         by=c("state"="name"))



#US Spending on Children by Division

(ed_hel<-kids %>% drop_na() %>% 
  group_by(year,category,division) %>% 
  summarise(total=round(sum(inf_adj_perchild),1)) %>% 
  filter(category %in% c("Education","Health"),
         year %in% c(1997,2000,2003,2006,2009,2012,2015,2016)) %>% 
  ggplot(aes(x=year,y=total))+
  geom_point(aes(color = category), show.legend = F)+
  geom_line(aes(color = category))+
  scale_y_continuous(labels=scales::dollar_format())+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~division)+
  guides(col = guide_legend(
    override.aes = list(size = 5),
    nrow = 1,
    title = "Category",
    title.position = "top",
    byrow = T
  )) +
  labs(
    title = "US Spending on Children by Division",
    x = "Year",
    y = "inf_adj_perchild",
    subtitle = "Expenditures on Children by Category",
    caption = "Source: Urban Institute | Joshua Rosenberg"
  ) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.title = element_text(size = 10,hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 9, face = "bold"),
    legend.key = element_rect(fill = NA, color = NA),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5,
                              color="#002868",
                              face = "bold"),
    plot.caption = element_text(face = "bold"),
    strip.background.x = element_rect(fill = "#BF0A30",
                                      linetype = "blank"),
    strip.text.x       =   element_text(face = "bold",
                                        color = "white", size = 15),
    plot.background = element_rect(fill = "azure2"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
  ))


#Save the plot

ggsave(
  "US Spending on Children by Division.jpeg",
  plot = ed_hel,
  limitsize = F,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)
