library(tidyverse)
library(tidytext)
library(rtweet)
library(ggthemes)
library(stm)
library(lubridate)
library(patchwork)

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

us_tweets <- search_tweets(c("covid OR coronavirus"), 
                           include_rts = FALSE,
                           geocode = lookup_coords("usa"), 
                           n = 18000, 
                           retryonratelimit = TRUE,
                           lang = "en")


# Covid v coronavirus
us_cleaned <- 
  us_tweets %>% 
  mutate(text = str_to_lower(text)) %>% 
  mutate(text = str_replace_all(text, "%20", "")) %>% 
  mutate(text = str_replace_all(text, "https?://\\S+", " ")) %>% 
  mutate(text = str_replace_all(text, "\\&amp", " ")) %>% 
  mutate(text = iconv(text, "utf8", "ASCII", sub = " ")) %>% 
  mutate(text = str_replace_all(text, "\n", " ")) %>% 
  mutate(text = str_replace_all(text, "\r", " ")) %>%  
  mutate(text = str_replace_all(text, "[0-9]", " ")) %>% 
  mutate(text = str_replace_all(text, "@\\S+ ", " ")) %>%
  mutate(text = str_replace_all(text, "#\\S+ ", " ")) %>%
  mutate(text = tm::removeWords(text, stopwords::data_stopwords_smart$en)) %>% 
  mutate(text = tm::removeWords(text, stopwords::data_stopwords_snowball$es)) %>% 
  mutate(text = str_replace_all(text, "[^[:alnum:]]", " ")) %>%
  mutate(text = str_replace_all(text, "\\b[a-z]{1,2}\\b", " ")) %>%
  mutate(text = str_squish(text))

covid_v_corona <-
  us_cleaned %>% 
  mutate(covid = ifelse(str_detect(text, "covid"), "Mentions COVID", "Mentions only coronavirus")) %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "coronavirus|covid")) %>% 
  count(covid, word, sort = TRUE) %>%
  ungroup()

us_tfidf <-
  covid_v_corona %>%
  bind_tf_idf(word, covid, n) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))


# Topic modeling
convid_dfm <- 
  us_cleaned %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "coronavirus|covid")) %>% 
  count(user_id, word, sort = TRUE) %>%
  ungroup()%>%
  cast_dfm(user_id, word, n)

k <- 10
covid_fit <- stm(convid_dfm, K = k, verbose = FALSE, init.type = "Spectral")

covid_topics <- tidy(covid_fit)

date <- as_datetime(Sys.time(), tz = "America/Chicago")

p1 <- covid_v_corona %>% 
  mutate(word = reorder_within(word, n, covid)) %>% 
  group_by(covid) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  ggplot(aes(x = n, y = word, fill = covid)) +
  geom_col(alpha = 0.7) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = "Frequency", y = NULL,
       title = "Word frequency for Coronavirus tweets, United States",
       subtitle = glue::glue("Tweets collected {wday(date, label = TRUE, abbr = FALSE)}, {month(date, label = TRUE)} {day(date)} at {format(Sys.time(), '%X %Z')}")) +
  facet_wrap(~ covid, scales = "free") +
  scale_y_reordered()

p2 <- us_tfidf %>% 
  group_by(covid) %>% 
  top_n(15) %>% 
  ggplot(aes(x = tf_idf, y = word, fill = covid)) +
  geom_col(alpha = 0.7) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = "Tf-Idf", y = NULL,
       title = "Text-frequency, Inverse Document Frequency for Coronavirus tweets, United States",
       subtitle = glue::glue("Tweets collected {wday(date, label = TRUE, abbr = FALSE)}, {month(date, label = TRUE)} {day(date)} at {format(Sys.time(), '%X %Z')}"),
       caption = glue::glue("Tweets collected using the `rtweet` package {cite('rtweet', bibtex::read.bib('Rpackages.bib'))}. Analysis of 18000 recent tweets, in English, mentioning 'covid' or 'coronavirus. Includes only terms mentioned at least 25 times.")) +
  facet_wrap(~covid, scales = "free")  + 
  scale_x_continuous(labels = scales::number_format(accuracy = 0.0001))

p3 <- covid_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, desc(beta)) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(y = term, x = beta, fill = factor(topic))) +
  geom_col() +
  scale_fill_viridis_d(guide = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 5) +
  labs(y = NULL, x = "Word-topic probability",
       title = "Topics for COVID-19/Coronavirus tweets, United States",
       subtitle = glue::glue("Tweets collected {wday(date, label = TRUE, abbr = FALSE)}, {month(date, label = TRUE)} {day(date)} at {format(Sys.time(), '%X %Z')}"),
       caption = glue::glue("Tweets collected using the `rtweet` package {cite('rtweet', bibtex::read.bib('Rpackages.bib'))}. Analysis of 18000 recent tweets, in English, mentioning 'covid' or 'coronavirus'.")) +
  scale_y_reordered() + 
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01))

ggsave(glue::glue("freq_cv_{as_datetime(Sys.time(), tz = 'America/Chicago')}.png"), p1, width = 11.5, height = 8)
ggsave(glue::glue("tfidf_cv_{as_datetime(Sys.time(), tz = 'America/Chicago')}.png"), p2, width = 11.5, height = 8)
ggsave(glue::glue("topic_models_cv_{as_datetime(Sys.time(), tz = 'America/Chicago')}.png"), p3, width = 11.5, height = 8)


(p1 | p2) / p3 + plot_layout(nrow = 2, heights = c(1, 2.5))
ggsave(glue::glue("combo_cv_plot_{as_datetime(Sys.time(), tz = 'America/Chicago')}.png"), width = 11.5, height = 8)

saveRDS(object = us_tweets, file = glue::glue("us_cv_tweets_{as_datetime(Sys.time(), tz = 'America/Chicago')}.rda"))


# Hashtags
us_hashtags <- bind_cols(us_tweets %>% select(-text), str_extract_all(us_tweets$text, "#[a-zA-Z0-9]+", simplify = TRUE) %>% as_tibble(.name_repair = "unique"))
us_hashtags <- us_hashtags %>% 
  select(user_id, contains("...")) %>% 
  pivot_longer(cols = -user_id) %>% 
  filter(value != "") %>% 
  mutate(value = tolower(value)) %>% 
  filter(!str_detect(value, "coronavirus|covid"))

hashtag_p <- us_hashtags %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n),
         value = as_factor(value)) %>% 
  top_n(25) %>% 
  ggplot(aes(x = prop, y = rev(value))) +
  geom_col(aes(fill = value)) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = "", y = "", 
       title = "Most common hastags for for COVID-19/Coronavirus tweets, United States",
       subtitle = glue::glue("Tweets collected {wday(date, label = TRUE, abbr = FALSE)}, {month(date, label = TRUE)} {day(date)} at {format(Sys.time(), '%X %Z')}"))

library(tidygraph)
library(igraph)
library(ggraph)

us_net <- 
  us_hashtags %>% 
  select(user_id, value) %>% 
  count(user_id, value) %>% 
  graph_from_data_frame()

V(us_net)$type <- V(us_net)$name %in% us_hashtags$user_id

hashtag_net <- bipartite_projection(us_net)$proj1 %>% as_tbl_graph()

hashtag_net <- hashtag_net %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree()) %>% 
  arrange(desc(degree)) %>% 
  top_n(25) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>% 
  activate(edges) %>% 
  mutate(log_weight = log(weight))

h_net <- hashtag_net %>% 
  ggraph(layout = "stress") +
  geom_edge_link(aes(alpha = log_weight), show.legend = FALSE) +
  geom_node_label(aes(label = name), repel = TRUE) +
  geom_node_point(alpha = 0.75) +
  labs(x = NULL, y = NULL,
       title = "Core hashtag co-deployment network for COVID-19/Coronavirus tweets, United States",
       subtitle = glue::glue("Tweets collected {wday(date, label = TRUE, abbr = FALSE)}, {month(date, label = TRUE)} {day(date)} at {format(Sys.time(), '%X %Z')}"),
       caption = glue::glue("Hashtags in the top 25 of degree centrality. Tweets collected using the `rtweet` package {cite('rtweet', bibtex::read.bib('Rpackages.bib'))}. Analysis of 18000 recent tweets, in English, mentioning 'covid' or 'coronavirus'.")) +
  theme_tufte() +
  theme(
    text = element_text(family = "Raleway"),
    axis.ticks = element_blank(), 
    axis.text = element_blank(),
    plot.title = element_text(size = 9, colour = "grey25", face = "bold"),
    plot.subtitle = element_text(size = 8, colour = "grey45"),
    plot.caption = element_text(size = 6, colour = "grey45")
  )

eq <- hashtag_net %>% 
  as_adjacency_matrix(attr = "weight", sparse = F) %>% 
  sna::sedist(method = "euclidean") 

eq <- 
  graph_from_adjacency_matrix(eq, 
                              diag = F,
                              mode = "undirected", 
                              weighted = T)

V(eq)$name <- hashtag_net %>% 
  activate(nodes) %>% 
  pull(name)

dendro <- eq %>% 
  as_adjacency_matrix(attr = "weight", 
                      sparse = F) %>% 
  as.dist() %>% 
  hclust(method = "complete") %>% 
  ggdendrogram(rotate = T, theme_dendro = F) +
  labs(y = "Dissimilarity",
       x = "Hashtag",
       title = "Hierarchical clustering of hashtags for COVID-19/Coronavirus tweets, United States",
       subtitle = glue::glue("Tweets collected {wday(date, label = TRUE, abbr = FALSE)}, {month(date, label = TRUE)} {day(date)} at {format(Sys.time(), '%X %Z')}")) + 
  theme(axis.text.y = element_text(size = 8))
  
ggsave(glue::glue("hashtag_freg_{as_datetime(Sys.time(), tz = 'America/Chicago')}.png"), hashtag_p, width = 11.5, height = 8)
ggsave(glue::glue("hash_net_{as_datetime(Sys.time(), tz = 'America/Chicago')}.png"), h_net, width = 11.5, height = 8)
ggsave(glue::glue("hash_dendro_{as_datetime(Sys.time(), tz = 'America/Chicago')}.png"), dendro, width = 11.5, height = 8)

((hashtag_p / dendro + plot_layout(nrow = 2)) | h_net)

ggsave(glue::glue("hash_combo_{as_datetime(Sys.time(), tz = 'America/Chicago')}.png"), width = 11.5, height = 8)

