library(rtweet)
library(stringr)
library(purrr)

# authenticate
auth_setup_default()


# Get yr friends
yr_name <- "paquinmorel"
my_friends <- get_friends(yr_name)$to_id

# Pull ids from tweets
ids_in_tweets <- map(
  my_friends,
  ~
    get_timeline(.x)$full_text %>%
    str_subset("rt", negate = TRUE) %>%
    str_subset("@\\S+@\\S+\\.\\S+") %>%
    str_extract("@\\S+@\\S+\\.\\S+")
  ) %>%
  unlist() %>%
  unique()

# Pull ids from bios
ids_in_bios <-
  lookup_users(my_friends)$description %>% 
  str_subset("@\\S+@\\S+\\.\\S+") %>%
  str_extract("@\\S+@\\S+\\.\\S+") %>% 
  unique()

# get em all
all_ids <- c(ids_in_tweets, ids_in_bios)
