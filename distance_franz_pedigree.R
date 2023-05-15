library(tidyverse)
library(ggmap)
library(geosphere)

pedigree <- read_delim("franz-grouped/all-clusters/cloneless/with_selfing/different-nmax/pedigree-trimmed.dat", delim="    ") %>% select(-"filler-col")
pedigree
pedigree_tt <- inner_join(pedigree, pedigree, by = c("o" = "p",
                                                     "p" = "o"),
                          keep = F)
pedigree_tt # 0 rows = NO CYCLICAL RELATIONSHIPS

# reading in parentage lods to rule out any pedigree relationships with bad LOD scores, if they exist
parentage_lods <- read_csv("franz-grouped/all-clusters/cloneless/with_selfing/different-nmax/parentage.csv")
parentage_lods


# made it tall so that each parent relationship was its own row with parent_lod_score


# doing filtering for LOD > 3 scores off the parentage file
filtered_parentage_lods <- parentage_lods %>%
  gather(key=parent_num, value=parent_id, `Parent 1`, `Parent 2`, na.rm = T) %>%
  gather(key=parent_num, value=parent_lod_score, `Pair LOD Parent 1`, `Pair LOD Parent 1`) %>%
  filter(parent_lod_score > 3)
  
# joining parentage and pedigree to get pedigree relationships with acceptable LOD scores
?inner_join()
names(pedigree)
pedigree
names(filtered_parentage_lods)
filtered_franz <- inner_join(pedigree, filtered_parentage_lods, by = c("o" = "Offspring", "p" = "parent_id")) %>%
  rename(Tree=o, parent_id=p)

filtered_franz

# REMOVED TT STUFF

filtered_franz
head(filtered_franz)
names(filtered_franz)

coords

# filtering by high lod scores, gathering parent ids,
# removing na parent matches then doing an inner join off of parent id
convenient_parents_to_join <- coords %>%
  rename(parent_id = Tree)

head(convenient_parents_to_join)

franz_coords <- inner_join(filtered_franz, convenient_parents_to_join, by="parent_id") %>%
  rename(parent_x = X, parent_y = Y) %>%
  inner_join(y=coords, by="Tree") %>%
  rename(offspring_x = X, offspring_y = Y) %>% 
  select(Tree, parent_id, parent_x, parent_y, offspring_x, offspring_y) %>%
  filter(!duplicated(Tree, parent_id)) # filter out duplicates in case of selfing where there are two entries for the same tree
# really should be filtering for selfing earlier on but I thought about it here and tried to add it earlier and it was causing weird bugs


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
               mapping=aes(x=parent_x, y=parent_y, xend=offspring_x, yend=offspring_y),
               arrow = arrow(
                 length = unit(0.01, "npc"),
                 type = "closed",
                 angle = 20),
               size=1)
# now we need to do analysis for the distances

franz_distances <- franz_coords %>%
  mutate(offspring_coord_vector=paste(as.character(offspring_x), as.character(offspring_y))) %>%
  mutate(parent_coord_vector=paste(as.character(parent_x), as.character(parent_y)))

# this section was just troubleshooting
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
distances_m
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

distances_m

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
trees_distances


trees_distances %>%
  summarize(median_d = median(distance),
            mean_d = mean(distance),
            sd_d = sd(distance),
            max_d = max(distance),
            min_d = min(distance),
            n = n())

# histograms
trees_distances %>%
  ggplot(aes(x = distance,y=after_stat(density))) +
  geom_histogram(fill="thistle2", binwidth=200) +
  geom_density(color = "red") +
  theme_classic() +
  labs(x='Distance (m)', y='Density', title='Distribution of estimated dispersal distance')

# filter out the super big one for clarity in visualization
trees_distances %>%
  filter(distance < 500, distance != 0) %>%
  ggplot(aes(x = distance,y=after_stat(density))) +
  geom_histogram(fill="thistle2", binwidth=20) +
  geom_density(color = "red") +
  theme_classic() +
  labs(x='Distance (m)', y='Density', title='Distribution of estimated dispersal distance')




# feasible_distances = as.vector(trees_distances %>% filter(is.na(tt)) %>% select(distance))
# feasible_distances
#mean(feasible_distances)

# finding z score of 2006 article distance in relation to this data
# z = (x-mean)/sd
# for some reason it's not liking mean(feasible_distances) so I just used the
# values from the summarize earlier instead
# (30 - 960.3587)/2225
# -0.4181387
# note that the data doesn't seem normally distributed, so don't think a z test
# makes a lot of sense

mean(feasible_distances)



# looking into wilcox test? not working though
# this is for old data made so be cautious of hard coded numbers
trees_distances %>%
  filter(distance > 47.5) %>%
  summarize(above = n())
# half are above median, so wilcox should theoretically be valid


wilcox.test(x=trees_distances$distance, mu=30)

as.vector(trees_distances %>% filter(is.na(tt)) %>% select(distance))

?wilcox.test


# visualizations for results page


trees_distances %>% select(distance) %>%
  ggplot(aes(x=distance)) +
  geom_boxplot() +
  theme_classic()



trees_distances %>%
  filter(is.na(tt)) %>%
  ggplot(aes(x = distance,y=after_stat(density))) +
  geom_histogram(fill="thistle2", bins=40) +
  geom_density(color = "red") +
  theme_classic() +
  labs(x="Distance (m)", y = "Density", title = "Distribution of Joshua tree parent-offspring pair distance")



# map cleaned up slightly
ggmap(franz_map_area) +
  # geom_point(geno_coords, mapping=aes(X, Y), size = 0.5) +
  geom_segment(trees_distances %>% filter(distance > 10000),
               mapping=aes(x=parent_x, y=parent_y, xend=offspring_x, yend=offspring_y),
               arrow = arrow(
                 length = unit(0.007, "npc"),
                 type = "closed",
                 angle = 20),
               size=1) +
  labs(title="Parental-offspring pairs of Joshua trees in Tikaboo Valley")
# arrows go from parent to offspring

?quantile


ggmap(franz_map_area) +
  # geom_point(geno_coords, mapping=aes(X, Y), size = 0.5) +
  geom_segment(trees_distances,
               mapping=aes(x=parent_x, y=parent_y, xend=offspring_x, yend=offspring_y),
               arrow = arrow(
                 length = unit(0.007, "npc"),
                 type = "closed",
                 angle = 20),
               size=1) +
  labs(title="Parental-offspring pairs of Joshua trees in Tikaboo Valley")


