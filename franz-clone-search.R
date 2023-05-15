# figuring out potential cloning issues

library(tidyverse)

?duplicated

franz_genotypes_slashes %>%
  mutate(cloned = duplicated(franz_genotypes_slashes%>%select(-Tree))) %>%
  filter(cloned==T) %>%
  arrange(yb01,yb15)
# this only shows the first duplicate, and so on.. fromLast is supposed to make
# it so all dubplicates are seen as TRUE, instead of one copy of each duplicate
# being seen as original

names(franz_genotypes_slashes)

franz_genotypes_slashes[!duplicated(franz_genotypes_slashes%>%select(-Tree))]


franz_genotypes_slashes %>% select(-Tree) %>% group_by_all() %>% filter(n()>1) %>% ungroup()
# this shows 26 that are clones

# just going to remove the clones to make it simpler
franz_cloneless <- franz_genotypes_slashes %>%
  filter(!duplicated(franz_genotypes_slashes %>% select(-Tree)))

franz_cloneless
# now making rough franz input file for this
franz_cloneless %>% write_delim("franz_cloneless_rough.dat", delim=" ")








