#~~~
# EXAMPLE 1
#
# Tidy up the built-in "Iris" data set
#~~~


# Load all Tidyverse packages
library(tidyverse)


# View the starting format of Iris
View(iris)


#---
# STEP 1
#---

# make a single column for length measurements and a single column for width measurements

iris_long = iris %>%
   # add a unique identifier to measurements
   group_by(Species) %>%
   mutate(day = seq_len(n())) %>%
   ungroup() %>%
   relocate(Species, day) %>%
   # lengthen the data set
   pivot_longer(cols = contains("."),
                names_to = "part",
                values_to = "meas") %>%
   # split the new "part" variable into two variables
   # for plant part and morphometric measurement
   separate(col = part, 
            into = c("part", "morph"))


#---
# STEP 2
#---

iris_clean = iris_long %>%
   # now widen the data set based on unique measurements
   pivot_wider(id_cols = Species:part,
               names_from = morph,
               values_from = meas) %>%
   arrange(day)



#---
# Alternative method to Step 1
# Lengthen data set and split variable names all at once
#---

iris_alt = iris %>%
   # need to change "." to "_" in variable names
   janitor::clean_names() %>%
   # add a unique identifier to measurements
   group_by(species) %>%
   mutate(day = seq_len(n())) %>%
   ungroup() %>%
   relocate(species, day) %>%
   # lengthen the data set
   # now splitting variable names into two new variables
   pivot_longer(cols = contains("_"),
                names_to = c("part", "morph"),
                names_sep = "_",
                values_to = "meas")

