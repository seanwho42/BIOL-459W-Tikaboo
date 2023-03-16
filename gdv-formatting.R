# genodive formatting -- sorta similar to franz but less columns and need to
# remove the slashes, replace ? with 000

library(tidyverse)


gdv_trees <- franz_genotypes %>%
  mutate(
    yb01 = paste(str_pad(`yb01 Allele#1`, width=3, pad="0"),
                 str_pad(`yb01 Allele#2`, width=3, pad="0"),
                 sep=""),
    yb15 = paste(str_pad(`yb15 Allele#1`, width=3, pad="0"),
                 str_pad(`yb15 Allele#2`, width=3, pad="0"),
                 sep=""),
    yb21 = paste(str_pad(`yb21 Allele#1`, width=3, pad="0"),
                 str_pad(`yb21 Allele#2`, width=3, pad="0"),
                 sep=""),
    yb04 = paste(str_pad(`yb04 Allele#1`, width=3, pad="0"),
                 str_pad(`yb04 Allele#2`, width=3, pad="0"),
                 sep=""),
    yb06 = paste(str_pad(`yb06 Allele#1`, width=3, pad="0"),
                 str_pad(`yb06 Allele#2`, width=3, pad="0"),
                 sep=""),
    yb12 = paste(str_pad(`yb12 Allele#1`, width=3, pad="0"),
                 str_pad(`yb12 Allele#2`, width=3, pad="0"),
                 sep=""),
    yb13 = paste(str_pad(`yb13 Allele#1`, width=3, pad="0"),
                 str_pad(`yb13 Allele#2`, width=3, pad="0"),
                 sep=""),
    fil36 = paste(str_pad(`fil36 Allele#1`, width=3, pad="0"),
                  str_pad(`fil36 Allele#2`, width=3, pad="0"),
                  sep=""),
    yb08 = paste(str_pad(`yb08 Allele#1`, width=3, pad="0"),
                 str_pad(`yb08 Allele#2`, width=3, pad="0"),
                 sep=""),
    yb22 = paste(str_pad(`yb22 Allele#1`, width=3, pad="0"),
                 str_pad(`yb22 Allele#2`, width=3, pad="0"),
                 sep="")
  ) %>%
  mutate("pop"=1) %>%
  select("pop","Tree","yb01","yb15","yb21","yb04","yb06","yb12","yb13","fil36","yb08","yb22") %>%
  mutate(Tree = str_sub(Tree,5,10))

gdv_trees
write_tsv(gdv_trees, "gdv-trees.tsv")

# note: one allele is notated as 122.5 on yb08.. assuming its 123, given no 122
# allele is recorded in rest of data on that loci?

# have to modify it/exclude it somehow since they only take 6 digits total per loci(3 per allele)
# I guess I could also make them take eight and then paste a 0 on right end of every other allele?

# for now just changing it to 123 for sake of testing


