library(tidyverse)
library(tidytext)
library(rtweet)
library(ggthemes)
library(stm)
library(lubridate)
library(patchwork)

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

files <- dir("corona_tweet", pattern = ".csv")
dr <- "corona_tweet"

cv_03_05 <- map(files, ~ read_csv(here::here(dr, .x)) %>% 
                  filter(lang == "en")) %>% 
  bind_rows()


cv_03_05_clean <- 
  cv_03_05 %>% 
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
  mutate(text = tm::removeWords(text, stopwords::data_stopwords_snowball$en)) %>% 
  mutate(text = str_replace_all(text, "[^[:alnum:]]", " ")) %>%
  mutate(text = str_replace_all(text, "\\b[a-z]{1,2}\\b", " ")) %>%
  mutate(text = str_squish(text)) %>% 
  filter(!duplicated(text))

cv_03_05_covid_v_corona <-
  cv_03_05_clean %>% 
  mutate(covid = ifelse(str_detect(text, "covid"), "Mentions COVID", "Mentions only coronavirus")) %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "coronavirus|covid|corona|virus")) %>% 
  count(covid, word, sort = TRUE) %>%
  ungroup()

cv_03_05_tfidf <-
  cv_03_05_covid_v_corona %>%
  bind_tf_idf(word, covid, n) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

cv_03_05_covid_v_corona %>% 
  group_by(covid) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, n, covid)) %>% 
  ggplot(aes(x = n, y = word, fill = covid)) +
  geom_col(alpha = 0.7) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = "Frequency", y = NULL,
       title = "Word frequency for Coronavirus tweets, United States") +
  facet_wrap(~ covid, scales = "free") +
  scale_y_reordered()

cv_03_05_hashtags <- 
  bind_cols(cv_03_05 %>% filter(!str_detect(text, "^RT ")) %>%  select(-text), 
            str_extract_all(cv_03_05$text[!str_detect(cv_03_05$text, "^RT ")], "#[a-zA-Z0-9]+", simplify = TRUE) %>% 
              as_tibble(.name_repair = "unique"))

cv_03_05_hashtags <- cv_03_05_hashtags %>% 
  select(id, contains("...")) %>% 
  pivot_longer(cols = -id) %>% 
  filter(value != "") %>% 
  mutate(value = tolower(value)) %>% 
  filter(!str_detect(value, "coronavirus|covid|corona|virus"))

cv_03_05_hashtags %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n),
         value = as_factor(value)) %>% 
  top_n(25) %>% 
  ggplot(aes(x = prop, y = value)) +
  geom_col(aes(fill = value)) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = "", y = "")

cv_03_05_net <- 
  cv_03_05_hashtags %>% 
  select(id, value) %>% 
  count(id, value) %>% 
  graph_from_data_frame()

V(cv_03_05_net)$type <- V(cv_03_05_net)$name %in% cv_03_05_hashtags$id

hashtag_net <- bipartite_projection(cv_03_05_net)$proj1 %>% as_tbl_graph()

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
       x = "Hashtag") +
  theme(axis.text.y = element_text(size = 8))
