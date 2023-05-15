# various wrangling to set up the .dat file

names(genotypes)

markers <- genotypes %>% select(-Tree, -"Sample id") %>% names()

?str_replace
simple_markers <- str_replace(markers, " .*$", "") %>% unique()
tibble(c(simple_markers))

# yb01    yb15    yb21    yb04    yb06    yb12    yb13    fil36    yb08    yb22
genotypes %>% select(-`Sample id`)

?paste
# making offspring/parental files to through into the dealio
offspring <- genotypes %>%
  mutate(Tree = paste('O', Tree, sep="-")) %>%
  select(-`Sample id`)
  
  
# didnt work: offspring %>%  mutate_all(str_replace(pattern="\'?", replacement='0'))

?str_replace

?write_csv

# manually replacing ? with 0 for colony in each resulting tsv,
# also found 122.5 as one of the readings -- replaced with 123 for
# testing purposes but will need to check with chris

write_tsv(offspring, "offspring.txt", col_names=F)

mothers <- genotypes %>%
  mutate(Tree = paste('mom', Tree, sep="-")) %>%
  select(-`Sample id`)

write_tsv(mothers, "mothers.txt", col_names=F)

fathers <- genotypes %>%
  mutate(Tree = paste('fat', Tree, sep="-")) %>%
  select(-`Sample id`)

write_tsv(fathers, "fathers.txt", col_names=F)

# creating known exclusion diads
known_pat_exclusions <- tibble(offspring$Tree)
known_pat_exclusions <- known_pat_exclusions %>% mutate(num_exclusions = 1)
known_pat_exclusions$father_ids <- fathers$Tree
known_pat_exclusions

write_tsv(known_pat_exclusions, 'pat-exclusions.txt', col_names = F)

#same but for mothers
known_mat_exclusions <- tibble(offspring$Tree)
known_mat_exclusions <- known_mat_exclusions %>% mutate(num_exclusions = 1)
known_mat_exclusions$mother_ids <- mothers$Tree
write_tsv(known_mat_exclusions, 'mat-exclusions.txt', col_names = F)







# NEW CODE STARTS HERE
colony_cloneless <- genotypes %>% select(-`Sample id`)

# removing clones from the data
colony_cloneless <- genotypes %>% select(-`Sample id`) %>%
  filter(!duplicated(genotypes %>% select(-Tree, -`Sample id`)))

colony_cloneless # 699 matches the franz cloneless set

cloneless_parents <- colony_cloneless %>%
  mutate(Tree = paste('p', Tree, sep="-"))






