library(tidyverse)
library(skimr)

# figuring out how to convert the genotype data into FRANz format

head(genotypes)

franz_genotypes <- genotypes %>%
  select(-`Sample id`) %>%
  mutate(Tree = paste("    ", Tree, " 1 ? ? ?", sep = "")) 
  # adding 4 spaces to meet 10 length requirement for input file
  # then adding the columns that aren't relevant

str_length(franz_genotypes$Tree)

head(franz_genotypes)

?write_delim
franz_genotypes %>% write_delim('rough-franz-input.txt', delim = " ")
# hoping that it will accept spaces as a delimiter, but probably not

# okay.. doing it the other way with slash
?paste

franz_genotypes_slashes <- franz_genotypes %>%
  mutate(
    yb01 = paste(`yb01 Allele#1`, "/", `yb01 Allele#2`, sep=""),
    yb15 = paste(`yb15 Allele#1`, "/", `yb15 Allele#2`, sep=""),
    yb21 = paste(`yb21 Allele#1`, "/", `yb21 Allele#2`, sep=""),
    yb04 = paste(`yb04 Allele#1`, "/", `yb04 Allele#2`, sep=""),
    yb06 = paste(`yb06 Allele#1`, "/", `yb06 Allele#2`, sep=""),
    yb12 = paste(`yb12 Allele#1`, "/", `yb12 Allele#2`, sep=""),
    yb13 = paste(`yb13 Allele#1`, "/", `yb13 Allele#2`, sep=""),
    fil36 = paste(`fil36 Allele#1`, "/", `fil36 Allele#2`, sep=""),
    yb08 = paste(`yb08 Allele#1`, "/", `yb08 Allele#2`, sep=""),
    yb22 = paste(`yb22 Allele#1`, "/", `yb22 Allele#2`, sep="")
    )
names(franz_genotypes_slashes)
franz_genotypes_slashes <- franz_genotypes_slashes %>% select("Tree","yb01","yb15","yb21","yb04","yb06","yb12","yb13","fil36","yb08","yb22")
franz_genotypes_slashes %>% write_delim('rough-franz-input-slash.txt', delim = " ")

