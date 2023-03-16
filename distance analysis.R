# figuring out distance analysis based on pairs

# should be able to just join the offspring tables to the pairwise maternity/
# paternity tables off of regex matching the last 6 digits of offspring/maternal id

# alternatively, could duplicate the tables but add in the mom- and O- to new
# offspring/parent versions of the tables where I can rename the columns

# seems a bit convoluted though, probably better to just join but only select the
# relevant columns one at a time, renaming after joining them on

# doing that:
library(tidyverse)
library(skimr)
library(ggmap)

pairwise_mat <- read_csv('trees_test.PairwiseMaternity')
# note that there are lots which have low probabilities but not worrying about
# that for now

# change percentage to decimal
pairwise_mat <- pairwise_mat %>% mutate(Confidence = as.numeric(sub("%", "", Confidence))/100)

?inner_join

# pairwise_mat_o_coords
# including both tree ids and respective coords and confidence intervals,
# could be worth including others in select later
mat_pairwise_w_coords <- inner_join(
    pairwise_mat %>%
      mutate(OffspringID = str_sub(OffspringID,-6,-1)) %>%
      rename(Tree = OffspringID),
    geno_coords,
    by = "Tree"
  ) %>%
  select(Tree, CandidateID, Confidence, X, Y) %>%
  mutate(CandidateID = str_sub(CandidateID,-6,-1)) %>%
  rename(child_x = X, child_y = Y, mat_tree = CandidateID) %>%
  inner_join(geno_coords %>%
               rename(mat_tree = Tree),
             by="mat_tree") %>%
  select(Tree, mat_tree, Confidence, child_x, child_y, X, Y) %>%
  rename(mat_x = X, mat_y = Y)

mat_pairwise_w_coords


# possible to use geom_path + geom_points to visualize the relationships?
?geom_path

# I think it is possible to use this if I make this tall for each x and y coord,
# and group by the offspring ID, making that its own column

# never mind that's probably harder than its worth, looking into geom_segment()

?geom_segment

ggmap(map_area) +
  geom_segment(mat_pairwise_w_coords, mapping=aes(x=child_x,
                                                  y=child_y,
                                                  xend=mat_x,
                                                  yend=mat_y))

# testing out with some filtering for confidence

confident_mat_pairwise_coords <- mat_pairwise_w_coords %>%
  filter(Confidence > 0.8)

skim(confident_mat_pairwise_coords)

ggmap(map_area) +
  geom_segment(confident_mat_pairwise_coords,
               mapping=aes(x=child_x, y=child_y, xend=mat_x, yend=mat_y),
               arrow = arrow(length = unit(0.006, "npc")))







# now lets add in the paternity data

pairwise_pat <- read_csv('trees_test.PairwisePaternity')
pairwise_pat

pat_pairwise_w_coords <- inner_join(pairwise_pat %>%
                                      mutate(OffspringID = str_sub(OffspringID,-6,-1)) %>%
                                      rename(Tree = OffspringID),
                                    mat_pairwise_w_coords,
                                    by="Tree") %>% 
  rename(pat_confidence = Confidence.x,
         mat_confidence = Confidence.y,
         pat_tree = CandidateID) %>%
  select(-`#OffGtype`, -`#PrntGtype`, -`#PairGtype`, -`#ExcLoci`) %>%
  mutate(pat_confidence = as.numeric(sub("%", "", pat_confidence))/100,
         pat_tree=str_sub(pat_tree,-6,-1)) %>%
  inner_join(coords %>%
               rename(pat_tree=Tree,
                      pat_x=X,
                      pat_y=Y),
             by="pat_tree") %>%
  select(-Lat, -`Lat '`, -Long, -`Long '`)

all_pairwise_w_coords <- pat_pairwise_w_coords

ggmap(map_area) +
  geom_segment(all_pairwise_w_coords,
               mapping=aes(x=child_x, y=child_y, xend=mat_x, yend=mat_y),
               arrow = arrow(length = unit(0.006, "npc"))) +
  geom_segment(all_pairwise_w_coords,
               mapping=aes(x=child_x, y=child_y, xend=pat_x, yend=pat_y),
               arrow = arrow(length = unit(0.006, "npc")))

# filtering by confidence

confident_pat_pairwise_coords <- pat_pairwise_w_coords %>%
  filter(pat_confidence > 0.8)
# probably worth replacing the coord values with NA for the nonconfident parents
# for the sake of more macroscopic analysis


# reworking the map area so it is surrounding the confident stuff
confident_map_area <- get_stamenmap(bbox=c(
  left = min(confident_pat_pairwise_coords$child_x - 0.003),
  bottom = min(confident_pat_pairwise_coords$child_y - 0.003),
  right = max(confident_pat_pairwise_coords$child_x + 0.003),
  top = max(confident_pat_pairwise_coords$child_y + 0.003)),
  zoom = 14,
  maptype = 'terrain')

# map with arrows
ggmap(confident_map_area) +
  # geom_point(geno_coords, mapping=aes(X, Y), size = 0.5) +
  geom_segment(confident_mat_pairwise_coords,
               mapping=aes(x=mat_x, y=mat_y, xend=child_x, yend=child_y,
                           color=Tree),
               arrow = arrow(
                 length = unit(0.01, "npc"),
                 type = "closed",
                 angle = 20),
               size=1) +
  geom_segment(confident_pat_pairwise_coords,
               mapping=aes(x=pat_x, y=pat_y, xend=child_x, yend=child_y,
                           color=Tree),
               arrow = arrow(
                 length = unit(0.01, "npc"),
                 type = "closed",
                 angle = 20),
               size=1)
  # geom_point(confident_pat_pairwise_coords, mapping=aes(child_x, child_y), size = 0.5) +
  # geom_point(confident_mat_pairwise_coords, mapping=aes(child_x, child_y), size = 0.5)
  





