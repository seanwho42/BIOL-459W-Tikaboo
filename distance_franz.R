library(tidyverse)
library(ggmap)
library(geosphere)

cloneless_franz <- read_csv("franz-grouped/all-clusters/cloneless/parentage.csv")

names(cloneless_franz)
# this is from franz run that had clones removed

# made it tall so that each parent relationship was its own row with parent_lod_score
filtered_franz <- cloneless_franz %>%
  rename(Tree=Offspring) %>%
  gather(key=parent_num, value=parent_id, `Parent 1`, `Parent 2`, na.rm = T) %>%
  gather(key=parent_num, value=parent_lod_score, `Pair LOD Parent 1`, `Pair LOD Parent 1`) %>%
  filter(parent_lod_score > 3)

time_travellers <- inner_join(filtered_franz, filtered_franz, by = c("Tree" = "parent_id",
                                                                     "parent_id" = "Tree"),
                              keep = T) %>%
  select(Tree.x, parent_id.x, Tree.y, parent_id.y)

time_travellers %>% print(n=50)

time_travellers <- time_travellers %>%
  mutate(tt = T)
filtered_franz <- full_join(time_travellers, filtered_franz, by = c("Tree" = "Tree", "parent_id" = "parent_id"))

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
  select(Tree, parent_id, LOD, parent_lod_score, parent_x, parent_y, offspring_x, offspring_y, tt)

names(franz_coords)
nrow(franz_coords)


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
               mapping=aes(x=parent_x, y=parent_y, xend=offspring_x, yend=offspring_y, color=tt),
               arrow = arrow(
                 length = unit(0.01, "npc"),
                 type = "closed",
                 angle = 20),
               size=1) +
  facet_grid(.~tt)

# now we need to do analysis for the distances

franz_distances <- franz_coords %>%
  mutate(offspring_coord_vector=paste(as.character(offspring_x), as.character(offspring_y))) %>%
  mutate(parent_coord_vector=paste(as.character(parent_x), as.character(parent_y)))

head(franz_distances)
distm(as.vector(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T)))
franz_coords
?strsplit

franz_distances$offspring_coord_vector[1]
strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T)

c(franz_distances$offspring_coord_vector[1])
c(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T))
length(unlist(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T)))

# okay everything above is messy and overcomplicated but this is going to work so I'm not going to mess with it..
nrow(franz_distances)
head(franz_distances)


distances_m = tibble(offspring_id = "some_id", parent_id = "other_id", distance = 0)
for (i in 1:(nrow(franz_distances)) ){
  distances_m <- distances_m %>% add_row(
    offspring_id = franz_distances$Tree[i],
    parent_id = franz_distances$parent_id[i],
    distance = as.numeric(distm(as.numeric(unlist(strsplit(franz_distances$offspring_coord_vector[i], " ", fixed=T))),
                     as.numeric(unlist(strsplit(franz_distances$parent_coord_vector[i], " ", fixed=T)))))
    )
  #print(i)
  #print(franz_distances$Tree[i])
  #print(as.numeric(distm(as.numeric(unlist(strsplit(franz_distances$offspring_coord_vector[i], " ", fixed=T))),
              #as.numeric(unlist(strsplit(franz_distances$parent_coord_vector[i], " ", fixed=T))))))
}


distm(as.numeric(unlist(strsplit(franz_distances$offspring_coord_vector[3], " ", fixed=T))),
      as.numeric(unlist(strsplit(franz_distances$parent_coord_vector[3], " ", fixed=T))))

distances_m <- distances_m %>% filter(offspring_id != "some_id")

distances_m


# I know the code is a mess, sorry if you are looking at this in the future...
# ran into a bunch of issues with how the vectors were wanting to do things so
# I made a new column with the coords for offspring and coords for parent and
# and then did string splitting

# now lets join this onto franz_filtered
trees_distances <- inner_join(distances_m, franz_coords, by = c("offspring_id"="Tree", 
                                               "parent_id"="parent_id"))


trees_distances %>% group_by(tt) %>%
  summarize(mean_d = mean(distance),
            sd_d = sd(distance))



trees_distances %>% ggplot(aes(x = distance,y=after_stat(density))) +
  geom_histogram(fill="thistle2") +
  facet_grid(.~tt) +
  theme_classic()

t.test(x = trees_distances %>% filter(tt) %>% select(distance),
       y = trees_distances %>% filter(is.na(tt)) %>% select(distance)
)
# t = -1.2068, df = 103.07, p-value = 0.2303









