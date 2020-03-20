library(tidyverse)
library(tidytext)
library(rtweet)
library(ggthemes)
library(stm)
library(lubridate)
library(patchwork)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggdendro)

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
  bind_rows() %>% 
  mutate(date = as.Date(created_at, "%a %b %d %H:%M:%S %z %Y"))


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


cv_03_05_1w <-
  cv_03_05_clean %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "coronavirus|covid|corona|virus")) %>% 
  count(date, word, sort = TRUE) %>%
  ungroup()

cv_03_05_covid_v_corona <-
  cv_03_05_clean %>% 
  mutate(covid = ifelse(str_detect(text, "covid"), "Mentions COVID", "Mentions only coronavirus")) %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "coronavirus|covid|corona|virus")) %>% 
  count(covid, word, sort = TRUE) %>%
  ungroup()

cv_03_05_tfidf <-
  cv_03_05_1w %>%
  bind_tf_idf(word, date, n) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

cv_03_05_1w %>% 
  group_by(date) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, n, date)) %>% 
  ggplot(aes(x = n, y = word, fill = factor(date))) +
  geom_col(alpha = 0.7) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = "Frequency", y = NULL,
       title = "Word frequency for Coronavirus tweets, United States") +
  facet_wrap(~ date, scales = "free") +
  scale_y_reordered()

cv_03_05_hashtags <- 
  bind_cols(cv_03_05 %>% filter(!str_detect(text, "^RT ")) %>%  select(-text), 
            str_extract_all(cv_03_05$text[!str_detect(cv_03_05$text, "^RT ")], "#[a-zA-Z0-9]+", simplify = TRUE) %>% 
              as_tibble(.name_repair = "unique"))

cv_03_05_hashtags <- cv_03_05_hashtags %>% 
  select(id, date, contains("...")) %>% 
  pivot_longer(cols = c(-id, -date)) %>% 
  filter(value != "") %>% 
  mutate(value = tolower(value)) %>% 
  filter(!str_detect(value, "coronavirus|covid|corona|virus"))

cv_03_05_hashtags %>% 
  count(value, date) %>% 
  arrange(desc(n)) %>% 
  group_by(date) %>% 
  mutate(prop = n/sum(n),
         value = as_factor(value)) %>% 
  top_n(25) %>% 
  mutate(value = reorder_within(value, prop, date)) %>% 
  ggplot(aes(x = prop, y = value)) +
  geom_col(aes(fill = value)) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = "", y = "") +
  facet_wrap(~ date, scale = "free") +
  scale_y_reordered()

cv_03_05_net <- 
  cv_03_05_hashtags %>% 
  select(id, value,date) %>% 
  count(id, value, date) %>% 
  group_split(date) %>% 
  map(graph_from_data_frame)

cv_03_05_net <- 
  cv_03_05_net %>% 
  map(~ set_vertex_attr(.x, name = "type", value = V(.x)$name %in% cv_03_05_hashtags$id))

hashtag_net <- 
  cv_03_05_net %>% 
  map(~ bipartite_projection(.x, which = "false") %>% as_tbl_graph())

hashtag_net <- hashtag_net %>% 
  map(~ .x %>% 
        activate(nodes) %>% 
        mutate(degree = centrality_degree()) %>% 
        arrange(desc(degree)) %>% 
        top_n(25) %>% 
        activate(nodes) %>% 
        filter(!node_is_isolated()) %>% 
        activate(edges) %>% 
        mutate(log_weight = log(weight)))

h_net <- hashtag_net %>% 
  map(~ .x %>% 
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
        ))

eq <- hashtag_net %>% 
  map(~ .x %>% 
        as_adjacency_matrix(attr = "weight", sparse = F) %>% 
        sna::sedist(method = "euclidean"))

eq <- 
  eq %>% 
  map(~ .x %>% 
        graph_from_adjacency_matrix(diag = F, mode = "undirected", weighted = T))

V(eq)$name <- 
  map2(eq, hashtag_net,
       %>% 
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
