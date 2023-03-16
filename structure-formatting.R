# working out the formatting for structure data input file

library(tidyverse)

# need to make each sample split across two rows for each allele
# going to use existing file that was created for FRANZ entry
head(franz_genotypes_slashes)


?separate_rows
structure_dat<- franz_genotypes_slashes %>% separate_rows(-Tree, sep = "/") %>% 
  mutate(Tree = str_sub(Tree,5,10))


write_tsv(structure_dat, 'structure-trees.dat')
# need to manually remove the tree column name
# also replaced ? with -9 per conventions in dat file

# genodive relatedness matrix??