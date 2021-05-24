library(wordcloud)
library(tidyverse)
library(lares)

df <- files_functions("R")

fx <- df %>% group_by(fun) %>% summarise(n = sum(n)) %>% arrange(desc(n))
wordcloud(fx$fun, fx$n)
fx <- df %>% group_by(package) %>% summarise(n = sum(n)) %>% arrange(desc(n))
wordcloud(fx$package, fx$n)

df %>% 
  mutate(package = autoline(package, 5)) %>%
  group_by(package, fun) %>%
  summarise(n = sum(n), .groups = "drop") %>% ungroup() %>%
  arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x = reorder(fun, n), y=n)) +
  geom_col() + coord_flip() +
  facet_grid(package~., space = "free", scales = "free")
