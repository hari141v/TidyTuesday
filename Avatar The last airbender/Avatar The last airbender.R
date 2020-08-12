#Load necessary Libraries
{
library(tidyverse)
library(tidyselect)
library(tidytext)
library(ggrepel)
library(jpeg)
library(grid)
library(patchwork)
library(extrafont)
library(igraph)
library(ggraph)
}  

#load last ninja font
font_import(pattern = "lastninja",prompt =F )
loadfonts(device = "win")
#font_import(prompt = F)
windowsFonts()

#import the data 
tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar<-tuesdata$avatar
scene_desc<-tuesdata$scene_description
genericSummary(avatar$full_text)


# Import the Wallpaper
img <- readJPEG("Avatar01.jpg")

#tokenizing and perform antijoin with stopwords and extract
#then group by book variable and select the top 100 words 
df_word<-avatar %>%
  unnest_tokens  (word, full_text) %>% 
  anti_join(stop_words, by = "word") %>%
  group_by(book) %>%
  count(word) %>% 
  top_n(100, n) 

#create wordcloud for earth

p1<-df_word %>% filter(book=="Earth") %>% 
  ggplot(aes(x = 1, y = 1, size = n, label = word,
                color=n)) +
   # annotation_custom(rasterGrob(img, width=unit(1,"npc"),
   #                              height=unit(1,"npc")),
   #                   -Inf, Inf, -Inf, Inf) +
  geom_text_repel(family = "Last Ninja",segment.size = 0,force = 2, segment.alpha = 0) +
  scale_size(range = c(2, 8), guide = FALSE) +
  scale_color_gradient(low = 'springgreen4', 
                      high = 'green3',guide = "none")+
   
  facet_wrap(~book) +theme_void()+
  theme(
    text = element_text(family = "Last Ninja"),
    # panel.border = element_rect(color = "springgreen4",
    #                             linetype="dashed",
    #                             fill = NA, size = 2),
    strip.background.x = element_rect(fill = "green3",
                                      linetype = "blank"),
    strip.text.x       =  element_text(face = "bold",
                   color = "white",size = 15))


#create wordcloud for fire

p2<-df_word %>% filter(book=="Fire") %>% 
  ggplot(aes(x = 1, y = 1, size = n, label = word,
             color=n)) +
  # annotation_custom(rasterGrob(img, width=unit(1,"npc"),
  #                              height=unit(1,"npc")),
  #                   -Inf, Inf, -Inf, Inf) +
  geom_text_repel(family = "Last Ninja",segment.size = 0, force = 2, segment.alpha = 0) +
  scale_size(range = c(2, 8), guide = FALSE) +
  scale_color_gradient(low = 'darkgoldenrod2', 
                       high = 'red',guide = "none")+
  theme_void () +
  facet_wrap(~book) +
  theme(
    text = element_text(family = "Last Ninja"),
    # panel.border = element_rect(color = "red",
    #                             linetype="dashed",
    #                             fill = NA, size = 2),
    strip.background.x = element_rect(fill = "red",
                                      linetype = "blank"),
    strip.text.x       =  
      element_text(face = "bold",color = "white",
                   size = 15))

#create wordcloud for water

p3<-df_word %>% filter(book=="Water") %>% 
  ggplot(aes(x = 1, y = 1, size = n, label = word,
             color=n)) +
  # annotation_custom(rasterGrob(img, width=unit(1,"npc"),
  #                              height=unit(1,"npc")),
  #                   -Inf, Inf,-Inf, Inf) +
  geom_text_repel(family = "Last Ninja",segment.size = 0,force = 2,segment.alpha = 0) +
  scale_size(range = c(2, 8), guide = FALSE) +
  scale_color_gradient(low = 'deepskyblue3', 
                       high = 'dodgerblue2',guide = "none")+
  theme_void () +
  facet_wrap(~book) +
  theme(
    text = element_text(family = "Last Ninja"),
    # panel.border = element_rect(color = "dodgerblue2",
    #                             linetype="dashed",
    #                             fill = NA, size = 2),
    strip.background.x = element_rect(fill = "dodgerblue2",
                                      linetype = "blank"),
    strip.text.x       =   element_text(face = "bold",
                   color = "white",size = 15))

  

#create a plot with image

df <- data.frame()
p4<-ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 50)+
  annotation_custom(rasterGrob(img, width=unit(1,"npc"),
                               height=unit(1,"npc")),
                    -Inf, Inf, -Inf, Inf)+theme_void()

#arrange the above images by using patchwork library

p5<-(p3 + (p4/p1) + p2)+plot_annotation(
title = "Avatar: The Last Airbender",
subtitle = "Most used words",
caption = "Source:  Avery Robbins. H/t to Kelsey Gonzalez ",
theme=theme(text = element_text(family = "Last Ninja",
                                colour = "steelblue4",face="bold"),
      plot.title = element_text(hjust = 0.5,face="bold"),
      plot.caption = element_text(face="bold"),
      plot.subtitle = element_text(face="bold",hjust = 0.5)))

#save the plot

ggsave(plot=p5,"Most used words.jpeg",units="cm", width=30, height=30, dpi=300)
 



#extract bigram

avatar_bigr <- avatar %>%
  unnest_tokens(bigram, full_text, token = "ngrams", n = 2)


#seperate the bygram by two column

bigr_sepa <- avatar_bigr %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# remove the stopwords from word1&word2 column

bigr_filt<- bigr_sepa %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


#count the number of bigram
bigr_count <- bigr_filt %>% 
  count(word1, word2, sort = TRUE)


bigra_unite <- bigr_filt %>%
  unite(bigram, word1, word2, sep = " ")

#create igraph object to visualize bigram

bigra_graph <- bigr_count %>% drop_na() %>% 
  filter(n > 20) %>%
  graph_from_data_frame()


set.seed(8891)

#create a arrow to connect the bigrams

ar <- grid::arrow(type = "closed", length = unit(.15, "inches"))

#to visualize bigram using ggraph library

biplot<-ggraph(bigra_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = ar, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "darkgoldenrod2", size = 5) +
  geom_node_text(aes(family = "Last Ninja",
                     fontface="bold", label = name),
                 color="dodgerblue2",
                 size=4,vjust = 1, hjust = 1) +
  labs(title = "Avatar: The Last Airbender",
       subtitle = "Common Bigrams",
       caption = "Source:  Avery Robbins. H/t to Kelsey Gonzalez ")+
  theme_void()+theme(
    text = element_text(family = "Last Ninja",
                        colour = "steelblue4",face="bold"),
    plot.title = element_text(hjust = 0.5,face="bold"),
    plot.caption = element_text(face="bold")
    
  )

#save the plot

ggsave(plot=biplot,"common_bigrams.jpeg",units="cm", width=30, height=30, dpi=300)
