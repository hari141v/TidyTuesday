#Import Necessary Packages
{
  library(tidyverse)
  library(sentimentr)
  library(tidytext)
  library(extrafont)
  library(gt)
  library(purrr)
  library(wordcloud2) 
  library(NLP)
  library(openNLP)
  library(ggrepel)
  
  
}


#Import the data

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')


#load  font
font_import(pattern = "Satisfaction",prompt =F )
font_import(pattern = "Beyonce demo",prompt =F )
loadfonts(device = "win")
#font_import(prompt = F)
windowsFonts()




#Analyze the sentiment of each Taylor swift's album


taylor_tidy <- taylor_swift_lyrics %>%
  unnest_tokens(word, Lyrics) %>% 
  anti_join(stop_words) 

taylor_nrc <- taylor_tidy %>%
  inner_join(get_sentiments("nrc"))

# Taylor Swift Album Wise Sentiment

(
  taylor_al_sent <-
    taylor_nrc %>%
    group_by(sentiment, Album) %>%
    summarise(word_count = n()) %>%
    ungroup() %>%
    mutate(sentiment = reorder(sentiment, word_count)) %>%
    ggplot(aes(
      x = reorder_within(sentiment, word_count, Album),
      word_count,
      fill = -word_count
    )) +
    geom_col() +
    scale_x_reordered() +
    scale_fill_gradient2(
      high = "grey",
      mid = "white",
      low = "purple",
      name = "Word Count"
    ) +
    labs(x = "Sentiment",
         title = "Taylor Swift Album Wise Sentiment",
         caption = "Source: TidyTuesday | Rosie Baillie and Dr. Sara Stoudt.") +
    facet_wrap( ~ Album, scales = "free") +
    theme(
      legend.position = c(0.85, 0.15),
      axis.ticks = element_blank(),
      axis.title = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_text(hjust = 0.5, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 18,
        family = "Satisfaction",
        face = "bold"
      ),
      plot.caption = element_text(face = "bold"),
      
      strip.background.x = element_rect(fill = "mediumpurple2",
                                        linetype = "blank"),
      strip.text.x       =   element_text(face = "bold",
                                          color = "White",
                                          size = 18),
      plot.background = element_rect(fill = "NA"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    ) +
    coord_flip()
)


#save the plot

ggsave(plot=taylor_al_sent,"Taylor Swift Album Wise Sentiment.jpeg",units="cm", width=30, height=30, dpi=300)



#Using tf_idf get the important words

tfidf_words_Album <- taylor_swift_lyrics %>%
  unnest_tokens(word, Lyrics) %>%
  distinct() %>%
  count(Album, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, Album, n) %>%
  arrange(desc(tf_idf))

top_tfidf_words_Album <- tfidf_words_Album %>% 
  group_by(Album) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(Album, tf_idf) %>%
  mutate(row = row_number())

#Taylor Swift's Album Wise Important Words

(
  taylor_imp_words <- top_tfidf_words_Album %>%
    ggplot(aes(x = row, tf_idf, fill = tf_idf)) +
    geom_col() +
    scale_fill_gradient2( high = "purple",
                          mid = "white",
                          low = "grey"
                         )+
    facet_wrap( ~ Album,
                scales = "free") +
    scale_x_continuous(breaks = top_tfidf_words_Album$row,
                       labels = top_tfidf_words_Album$word) +
    labs(title = "Taylor Swift's Album Wise Important Words",
         x = "Word",
         caption = "Source: TidyTuesday | Rosie Baillie and Dr. Sara Stoudt.") +
    theme(
      legend.position = c(0.85, 0.15),
      
      axis.ticks = element_blank(),
      axis.title = element_text(hjust = 0.5,
                                face = "bold"),
      axis.text = element_text(hjust = 0.5, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 18,
        family = "Satisfaction",
        face = "bold"
      ),
      plot.caption = element_text(face = "bold"),
      
      strip.background.x = element_rect(fill = "purple",
                                        linetype = "blank"),
      strip.text.x       =   element_text(face = "bold",
                                          color = "white",
                                          size = 18),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    ) +
    coord_flip()
)


#save the plot

ggsave(plot=taylor_imp_words,"taylor album wise important words.jpeg",units="cm", width=30, height=30, dpi=300)






#Store lyric in new variable

taylor_lyric<-as.String(taylor_swift_lyrics$Lyrics)

# define annotators

sent_annot = Maxent_Sent_Token_Annotator()
word_annot = Maxent_Word_Token_Annotator()
loc_annot = Maxent_Entity_Annotator(kind = "location") 
people_annot = Maxent_Entity_Annotator(kind = "person") 

# start annotation

tayloranno = NLP::annotate(taylor_lyric, list(sent_annot, word_annot, 
                                              loc_annot, people_annot))
# extract features

k <- sapply(tayloranno$features, `[[`, "kind")

# extract locations

taylorlocations = names(table(taylor_lyric[tayloranno[k == "location"]]))

# extract people

taylorlpeople = names(table(taylor_lyric[tayloranno[k == "person"]]))


#store location entity

taylor_entity <- data.frame(entity="location",word=taylorlocations)

#store people entity

taylor_entity1 <- data.frame(entity="people",word=taylorlpeople)


#Merge location and people entity dataframe

taylor_entity <- merge(taylor_entity,taylor_entity1, all=T)


#Named Entity Extraction on Taylor Swift's Song Lyrics

(
  taylor_ner <- taylor_entity %>% group_by(entity, word) %>%
    summarise(counts = n()) %>%
    ggplot(aes(
      x = 1,
      y = 1,
      size = counts,
      label = word,
      color = counts
    )) +
    geom_text_repel(
      segment.size = 0,
      force = 30,
      seed = 8891,
      segment.alpha = 0
    ) +
    scale_size(range = c(2, 8), guide = FALSE) +
    scale_color_gradient(high = "purple",
                         low = "purple",
                         guide = "none") +
    labs(title = "Named Entity Extraction on Taylor Swift's Song Lyrics",
         x = "Word",
         caption = "Source: TidyTuesday | Rosie Baillie and Dr. Sara Stoudt.") +
    facet_wrap( ~ entity) +
    theme(
      legend.position = "none",
      
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(hjust = 0.5,
                                size = 18,
                                #family="Satisfaction",
                                face = "bold"),
      plot.caption = element_text(face = "bold"),
      
      strip.background.x = element_rect(fill = "purple",
                                        linetype = "blank"),
      strip.text.x       =   element_text(face = "bold",
                                          color = "white",
                                          size = 18),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    )
)

#save the plot

ggsave(plot=taylor_ner,"taylor_ner.jpeg",units="cm", width=30, height=30, dpi=300)




#Beyonce Song Lyrics

#lyrics are represented in line by line so concatenate the lines by song_id 

beyonce_lyrics_clean<-beyonce_lyrics %>% 
  group_by(song_id) %>%
  mutate(line = paste0(line, collapse = "")) %>%
  select(-song_line) %>% unique() %>% as.data.frame()


beyonce_most_used_word <- beyonce_lyrics_clean %>%
  unnest_tokens(word,line) %>%
  anti_join(stop_words, by = "word") %>%
  count(word) %>%
  top_n(100, n) 


#Most used Words in Beyonce Song Lyrics

(
  beyonce_most_used_words <-  beyonce_most_used_word %>%
    group_by(word) %>%
    summarise(counts = n()) %>%
    ggplot(aes(
      x = 1,
      y = 1,
      size = counts,
      label = word,
      color = counts
    )) +
    geom_text_repel(
      segment.size = 0,
      force = 30,
      seed = 8891,
      segment.alpha = 0
    ) +
    scale_size(range = c(2, 8), guide = FALSE) +
    scale_color_gradient(high = "deeppink2",
                         low = "deeppink2",
                         guide = "none") +
    labs(title = "Most used Words in Beyonce Song Lyrics ",
         caption = "Source: TidyTuesday | Rosie Baillie and Dr. Sara Stoudt.") +
    theme(
      legend.position = "none",
      
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        size = 18,
        family = "Beyonce",
        face = "bold"
      ),
      plot.caption = element_text(face = "bold"),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    )
)


#save the plot

ggsave(plot=beyonce_most_used_words,"Beyonce most used words.jpeg",units="cm", width=30, height=30, dpi=300)



#Analyze the sentiments of each beyonce song

beyonce_tidy <- beyonce_lyrics_clean %>%
  unnest_tokens(word, line) %>% 
  anti_join(stop_words) 


beyoncer_nrc <- beyonce_tidy %>%
  inner_join(get_sentiments("nrc"))


#Beyonce Song Overall Sentiments

(
  overall_senti <- beyoncer_nrc %>%
    group_by(sentiment) %>%
    summarise(word_count = n()) %>%
    ungroup() %>%
    mutate(sentiment = reorder(sentiment, word_count)) %>%
    ggplot(aes(
      x = reorder(sentiment, word_count),
      word_count,
      fill = -word_count
    )) +
    
    geom_col() +
    scale_fill_gradient2(
      name = "Word Count",
      high = "white",
      mid = "grey",
      low = "hotpink"
    ) +
    scale_x_reordered() +
    labs(x = "Sentiments",
         title = "Beyonce Song Overall Sentiments",
         caption = "Source: TidyTuesday | Rosie Baillie and Dr. Sara Stoudt.") +
    theme(
      legend.position = c(0.85, 0.15),
      axis.ticks = element_blank(),
      axis.title = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_text(hjust = 0.5, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 18,
        family = "Beyonce",
        face = "bold"
      ),
      plot.caption = element_text(face = "bold"),
      plot.background = element_rect(fill = "NA"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    ) +
    coord_flip())


#save the plot

ggsave(plot=overall_senti,"Beyonce overall sentiment.jpeg",units="cm", width=30, height=30, dpi=300)



#Store Lyrics in new variable

beyonce_lyric<-as.String(beyonce_lyrics_clean$line)

# start annotation

beyonceanno = NLP::annotate(beyonce_lyric, list(sent_annot, word_annot, 
                                                loc_annot, people_annot))
# extract features

k1 <- sapply(beyonceanno$features, `[[`, "kind")

# extract locations

beyoncelocations = names(table(beyonce_lyric[beyonceanno[k1 == "location"]]))

# extract people

beyoncelpeople = names(table(beyonce_lyric[beyonceanno[k1 == "person"]]))

#store location entity

beyonce_entity <- data.frame(entity="location",word=beyoncelocations)

#store people entity

beyonce_entity1 <- data.frame(entity="people",word=beyoncelpeople)

#Merge location and people entity

beyonce_entity <- merge(beyonce_entity,beyonce_entity1,all=T)


#Named Entity Extraction on Beyonce Song Lyrics

(
  beyonce_ner <- beyonce_entity[1:100, ] %>% group_by(entity, word) %>%
    summarise(counts = n()) %>%
    ggplot(aes(
      x = 1,
      y = 1,
      size = counts,
      label = word,
      color = counts
    )) +
    geom_text_repel(
      segment.size = 0,
      force = 1,
      seed = 8891,
      segment.alpha = 0
    ) +
    scale_size(range = c(5, 10), guide = FALSE) +
    scale_color_gradient(high = "goldenrod",
                         low = "goldenrod",
                         guide = "none") +
    labs(title = "Named Entity Extraction on Beyonce Song Lyrics",
         caption = "Source: TidyTuesday | Rosie Baillie and Dr. Sara Stoudt.") +
    facet_wrap( ~ entity) +
    theme(
      legend.position = "none",
      
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(hjust = 0.5,
                                size = 18,
                                #family="Beyonce",
                                face = "bold"),
      plot.caption = element_text(face = "bold"),
      
      strip.background.x = element_rect(fill = "deeppink2",
                                        linetype = "blank"),
      strip.text.x       =   element_text(face = "bold",
                                          color = "white",
                                          size = 18),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    )
)

#save the plot

ggsave(plot=last_plot(),"beyonce_ner.jpeg",units="cm", width=30, height=30, dpi=300)




