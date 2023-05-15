# dealing with time travelling trees

# some trees are each others parents so lets figure out how to get rid of those

  
# I think I can do a join to filter out if parents and offspring are the same


# lets start with an inner join to find all the confusing areas
filtered_franz
names(filtered_franz)

time_travellers <- inner_join(filtered_franz, filtered_franz, by = c("Tree" = "parent_id",
                                                  "parent_id" = "Tree"),
           keep = F) %>%
  select(Tree, parent_id)

time_travellers %>% print(n=50)

time_travellers %>%
  mutate(tt = T)


# MAKING NO FURTHER CHANGES IN THIS FILE -- MOVED CODE TO distance_franz.R
cloneless_franz

