# initial data exploration of coordinates

library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(ggmap)


genotypes <- read_xlsx('genotypes.xlsx')
coords <- read_xlsx('TreeCoordinates.xlsx')

head(coords)
skim(coords)

# for some reason Lat is a character so..
?mutate
coords <- coords %>% mutate(Lat = as.numeric(Lat))

# need to make the coordinates into decimal form instead of minutes
coords <- coords %>%
  mutate(Y = (Lat + `Lat '`/60), X = -(Long + `Long '`/60)) %>%
  unique()
head(coords$Y)
head(coords$X)

head(genotypes)
# id is currently numeric so probably better to make it char to be safe
genotypes <- genotypes %>%
  mutate(`Sample id` = as.character(`Sample id`), ) %>%
  # rename("fil36 Allele#1" = "Fil36 Allele#1") %>%
  unique() # were duplicates of rows so added this

geno_coords <- inner_join(genotypes, coords, by = c("Tree" = "Tree"))
dim(geno_coords)

# 3 duplicates.. might have been left over from not joining after fixing other
# duplicate entries
geno_coords <- unique(geno_coords)


# creating a map
?get_stamenmap
map_area <- get_stamenmap(bbox=c(left = min(geno_coords$X - 0.005),
                                 bottom = min(geno_coords$Y - 0.005),
                                 right = max(geno_coords$X + 0.005),
                                 top = max(geno_coords$Y + 0.005)),
                          zoom = 13,
                          maptype = 'terrain')

ggmap(map_area) +
  theme_classic() +
  geom_point(geno_coords,mapping=aes(x=X,y=Y), size= 0.01) +
  labs(title='Y. brevifolia sample distribution in Tikaboo Valley, NV')
  
  


skim(genotypes)
skim(geno_coords)


