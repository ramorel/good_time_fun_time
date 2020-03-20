library(tidyverse)
library(tidytext)
library(rtweet)
library(ggthemes)
library(stm)
library(lubridate)
library(patchwork)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggdendro)

bibtex::write.bib("rtweet")

theme_set(
  theme_tufte() +
    theme(
      text = element_text(family = "Raleway"),
      axis.title.x = element_text(size = 10, hjust = 1),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6),
      plot.title = element_text(size = 9, colour = "grey25", face = "bold"),
      plot.subtitle = element_text(size = 8, colour = "grey45"),
      plot.caption = element_text(size = 6, colour = "grey45")
    )
)

hs_tweets <- search_tweets2(c("\"home school\"", "homeschool"),
                            include_rts = FALSE,
                            geocode = lookup_coords("usa"), 
                            n = 18000, 
                            retryonratelimit = TRUE,
                            lang = "en")

hs_tweets2  <- search_tweets2(c("\"home school\"", "homeschool"), 
                              include_rts = FALSE,
                              geocode = lookup_coords("usa"), 
                              n = 18000, 
                              retryonratelimit = TRUE,
                              lang = "en")

hs_tweets <- bind_rows(hs_tweets, hs_tweets2)

saveRDS(object = hs_tweets, file = glue::glue("hs_cv_tweets_{as_datetime(Sys.time(), tz = 'America/Chicago')}.rda"))

hs_cleaned <- 
  hs_tweets %>% 
  mutate(text = str_to_lower(text)) %>% 
  mutate(text = str_replace_all(text, "[‘’]", "'")) %>% 
  mutate(text = str_replace_all(text, "%20", "")) %>% 
  mutate(text = str_replace_all(text, "https?://\\S+", " ")) %>% 
  mutate(text = str_replace_all(text, "\\&amp", " ")) %>% 
  mutate(text = str_replace_all(text, "\n", " ")) %>% 
  mutate(text = str_replace_all(text, "\r", " ")) %>%  
  mutate(text = str_replace_all(text, "[0-9]", " ")) %>% 
  mutate(text = str_replace_all(text, "@\\S+ ", " ")) %>%
  mutate(text = str_replace_all(text, "#\\S+ ", " ")) %>%
  mutate(text = tm::removeWords(text, stopwords::data_stopwords_snowball$en)) %>% 
  mutate(text = tm::removeWords(text, stopwords::data_stopwords_smart$en)) %>% 
  mutate(text = str_replace_all(text, "[^[:alnum:]]", " ")) %>%
  mutate(text = str_replace_all(text, "\\b[a-z]{1,2}\\b", " ")) %>%
  mutate(text = str_squish(text))

hs_cleaned %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "home|school|homeschool")) %>% 
  count(word, sort = TRUE) %>%
  ungroup() %>% 
  top_n(25) %>%
  mutate(prop = n/sum(n)) %>% 
  mutate(word = as_factor(word)) %>% 
  ggplot(aes(x = prop, y = word, fill = word)) +
  geom_col(alpha = 0.7) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = "Frequency", y = NULL)


hs_hashtags <- bind_cols(hs_tweets %>% select(-text), str_extract_all(hs_tweets$text, "#[a-zA-Z0-9]+", simplify = TRUE) %>% as_tibble(.name_repair = "unique"))

hs_hashtags <- hs_hashtags %>% 
  select(user_id, contains("...")) %>% 
  pivot_longer(cols = -user_id) %>% 
  filter(value != "") %>% 
  mutate(value = tolower(value)) %>% 
  filter(!str_detect(value, "covid|coronavirus|corona|home|school|homeschool"))

hs_hashtags %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n),
         value = as_factor(value)) %>% 
  top_n(25) %>% 
  ggplot(aes(x = prop, y = value)) +
  geom_col(aes(fill = value)) +
  scale_fill_viridis_d(guide = FALSE)

hs_net <- 
  hs_hashtags %>% 
  select(user_id, value) %>% 
  count(user_id, value) %>% 
  graph_from_data_frame()

V(hs_net)$type <- V(hs_net)$name %in% hs_hashtags$user_id

hashtag_net <- bipartite_projection(hs_net)$proj1 %>% as_tbl_graph()

hashtag_net <- hashtag_net %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree()) %>% 
  arrange(desc(degree)) %>% 
  top_n(30) %>% 
  activate(edges) %>% 
  mutate(log_weight = log(weight)) %>% 
  arrange(desc(weight)) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated())

hashtag_net %>% 
  ggraph(layout = "stress") +
  geom_edge_link(aes(alpha = log_weight), show.legend = FALSE) +
  geom_node_label(aes(label = name), repel = TRUE) +
  geom_node_point(alpha = 0.75) +
  labs(x = NULL, y = NULL) +
  theme_tufte() +
  theme(
    text = element_text(family = "Raleway"),
    axis.ticks = element_blank(), 
    axis.text = element_blank(),
    plot.title = element_text(size = 9, colour = "grey25", face = "bold"),
    plot.subtitle = element_text(size = 8, colour = "grey45"),
    plot.caption = element_text(size = 6, colour = "grey45")
  )

