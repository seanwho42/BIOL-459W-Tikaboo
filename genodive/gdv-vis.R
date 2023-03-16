# genodive visualization of groups on the map

library(ggmap)
library(tidyverse)

groups_gdv <- read_tsv("~/Documents/BIOL 459W/genodive/groups2-15-str.gdv") %>%
  rename(Tree=kMeans)

groups_gdv

grouped_trees <- inner_join(geno_coords, groups_gdv, by = "Tree")

names(grouped_trees)

grouped_trees %>% group_by(k3) %>%
  summarize(number=n())


# for 2 groups
ggmap(map_area) +
  geom_point(grouped_trees, mapping = aes(x=X, y=Y, color=`k2*`)) +
  theme(legend.position="none") +
  ggtitle("two groups")

# for 3 groups
ggmap(map_area) +
  geom_point(grouped_trees, mapping = aes(x=X, y=Y, color=`k3`), size=.7) +
  theme(legend.position="none") +
  ggtitle("three groups")

# for 4 groups
ggmap(map_area) +
  geom_point(grouped_trees, mapping = aes(x=X, y=Y, color=`k4`)) +
  theme(legend.position="none") +
  ggtitle("four groups")

# for 7 groups (best clustering according to BIC)
# worth noting that BIC is very similar among the different group #s
ggmap(map_area) +
  geom_point(grouped_trees, mapping = aes(x=X, y=Y, color=`k7#`)) +
  theme(legend.position="none") +
  scale_color_discrete() +
  ggtitle("seven groups (best according to BIC)")

# because why not.. 15 groups
ggmap(map_area) +
  geom_point(grouped_trees, mapping = aes(x=X, y=Y, color=`k15`), size=.7) +
  ggtitle("...fifteen groups")


