# filtering various input file forms by genodive groups

library(tidyverse)

# renaming the columns so they are more friendly to functions if that'll work

names(groups_gdv)
clean_groups_gdv <- groups_gdv %>% rename(k2 = `k2*`, k7 = `k7#`)

groups_gdv_num = read_tsv("~/Documents/BIOL 459W/genodive/groups2-15.gdv") %>%
  rename(Tree=kMeans)

# creating a master generate outputs function that will generate franz input for
# each group based off of the number of groups that are selected
create_franz_inputs <- function(ds, num_groups) {
  filter_column = paste("k", as.character(num_groups), sep="")
  # print(filter_column)
  select_cols = append(names(ds), filter_column)
  joined_ds <- inner_join(ds, groups_gdv_num, by="Tree") %>%
    select(select_cols)
  print(joined_ds %>% select(filter_column))
  temp_franz <- joined_ds %>%
    mutate(Tree = paste("    ", Tree, " 1 ? ? ?", sep = ""))
  print(temp_franz)
  for (i in 1:num_groups) {
    # print(i)
    franz_to_write <- temp_franz %>%
      filter(select(temp_franz, filter_column)==i) %>%
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
      ) %>%
      select("Tree","yb01","yb15","yb21","yb04","yb06","yb12","yb13","fil36","yb08","yb22")
    print(paste('Current group:', as.character(i)))
    print(franz_to_write)
    write_delim(franz_to_write,
                paste('./franz-grouped/rough-franz-input-slash-',
                      as.character(i),'.txt', sep=""),
                delim = " ",
                col_names=F)
    # print(i)
  }
}

?write_delim()
# need to remove all of the quotes from the resulting files, and add the
# metadata on top of each

# can work this into a function which does the same thing with colony as well


create_franz_inputs(genotypes, num_groups=3)

# ran into same issue with tv4284 allele #6... set to ? for now
