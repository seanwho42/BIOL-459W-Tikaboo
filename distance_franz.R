library(tidyverse)
library(ggmap)
library(geosphere)

cluster_1_franz <- read_csv("franz-grouped/cluster1/parentage.csv")

names(cluster_1_franz)

filtered_franz <- cluster_1_franz %>%
  rename(Tree=Offspring) %>%
  gather(key=parent_num, value=parent_id, `Parent 1`, `Parent 2`, na.rm = T) %>%
  gather(key=parent_num, value=parent_lod_score, `Pair LOD Parent 1`, `Pair LOD Parent 1`) %>%
  filter(parent_lod_score > 3)
  
head(filtered_franz)
names(filtered_franz)

# filtering by high lod scores, gathering parent ids,
# removing na parent matches then doing an inner join off of parent id
convenient_parents_to_join <- coords %>%
  rename(parent_id = Tree)

head(convenient_parents_to_join)

franz_coords <- inner_join(filtered_franz, convenient_parents_to_join, by="parent_id") %>%
  rename(parent_x = X, parent_y = Y) %>%
  inner_join(y=coords, by="Tree") %>%
  rename(offspring_x = X, offspring_y = Y) %>% 
  select(Tree, LOD, parent_lod_score, parent_x, parent_y, offspring_x, offspring_y)

names(franz_coords)

?inner_join
?gather

franz_map_area <- get_stamenmap(bbox=c(
  left = min(franz_coords$offspring_x - 0.003),
  bottom = min(franz_coords$offspring_y - 0.003),
  right = max(franz_coords$offspring_x + 0.003),
  top = max(franz_coords$offspring_y + 0.003)),
  zoom = 14,
  maptype = 'terrain')

ggmap(franz_map_area) +
  # geom_point(geno_coords, mapping=aes(X, Y), size = 0.5) +
  geom_segment(franz_coords,
               mapping=aes(x=parent_x, y=parent_y, xend=offspring_x, yend=offspring_y,
                           color=Tree),
               arrow = arrow(
                 length = unit(0.01, "npc"),
                 type = "closed",
                 angle = 20),
               size=1)

# now we need to do analysis for the distances

franz_distances <- franz_coords %>%
  mutate(offspring_coord_vector=paste(as.character(offspring_x), as.character(offspring_y))) %>%
  mutate(parent_coord_vector=paste(as.character(parent_x), as.character(parent_y))) %>%
  mutate(distance = distm(as.numeric(unlist(strsplit(offspring_coord_vector, " ", fixed=T))),
                          as.numeric(unlist(strsplit(parent_coord_vector, " ", fixed=T)))))


distm(as.vector(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T)))
franz_coords
?strsplit

franz_distances$offspring_coord_vector[1]
strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T)

c(franz_distances$offspring_coord_vector[1])
c(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T))
length(unlist(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T)))


distm(as.numeric(unlist(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T))),
      as.numeric(unlist(strsplit(franz_distances$parent_coord_vector[1], " ", fixed=T))))

