library(tidyverse)
poke <- read_csv("https://gist.githubusercontent.com/nathanielwoodward/8638ce5563915f39ce6fe4b75cc70426/raw/92200bc0a673d5ce2110aaad4544ed6c4010f687/pokemon.csv")
#1
glimpse(poke)
#2
poke %>% group_by(Generation) %>% summarize(n = sum(Generation)) 
#3
poke <- poke %>% mutate(Generation = as.factor(Generation))
poke_nums<- poke %>% column_to_rownames("Name") %>% select_if(is.numeric)
cor(poke_nums)
#4 
poke_dist1 <- poke_nums %>% dist %>% as.matrix
glimpse(poke_dist1)
800**2
#5
poke_dist1_df <- poke_dist1 %>% as.data.frame %>% rownames_to_column("poke1") %>% 
  pivot_longer(-1, names_to="poke2", values_to="distance")
poke_dist1_df <- poke_dist1_df %>% filter(poke1!=poke2)
poke_dist1_df <- poke_dist1_df %>% mutate(one = pmax(poke1,poke2), two = pmin(poke1, poke2)) 
#6
poke_dist1_df %>% filter(poke1 == "Weedle") %>% slice_min(distance)
#8
library(cluster)

#create dissimilarity matrix
poke_dist2 <- poke %>% mutate_if(is.character,as.factor) %>% column_to_rownames("Name") %>% daisy("gower") %>% as.matrix 
#make it tidy
poke_dist2_df <- poke_dist2 %>% as.data.frame %>% rownames_to_column("poke1") %>% pivot_longer(-poke1, names_to= "poke2", values_to="gower")
poke_dist2_df %>% filter(poke1 == "Snorlax") %>% arrange(gower)
poke_dist2_df %>% filter(poke1=="Weedle") %>% arrange(gower)
#10
poke_dist2_df %>% group_by(poke1) %>% summarize(n=mean(gower)) %>% arrange(desc(n))



