#~~~
# EXAMPLE 2
#
# Working with ISU-DNR Ambient Lake Monitoring Data
#~~~


# Load all Tidyverse packages
library(tidyverse)


# Load the data set
lake_raw = read_csv("2019 ISU Ambient Lake Data.csv")


# View the data
View(lake_raw)

#---
# STEP 1
# Prep the data set (most of these functions come from the 'dplyr' package)
#---

# Select a subset of analytes to work with (to make viewing the results easier)
lake_dat = lake_raw %>%
   filter(cas_rn %in% c("TURB", "TDS", "PH"))

   # Select only the desired columns to work with
   lake_dat = lake_dat %>%
      select(name, sampleDate, cas_rn, result, unit, detect, detectLimit)


# Change date to Lubridate format (YYYY-MM-DD) and remove time (since it is always the same for a sampling event)
lake_dat = lake_dat %>%
   # change date format
   mutate(sampleDate = lubridate::mdy_hms(sampleDate),
          date = lubridate::date(sampleDate)) %>%
   # remove date-time variable
   select(-sampleDate) %>%
   # reorder columns
   relocate(date, .after=name)


#---
# STEP 2
# Create a tidy data set (one row per observation)
#---


#--
# pivot_wider()
#--

# In this case, an observation is a sampling event at a lake.
# Each variable collected/measured will then comprise a new column for that observation. 


# Widen just the result values
lake_dat %>%
   pivot_wider(id_cols = c("name", "date"),
               # variable where new column names will be taken from
               names_from = cas_rn, 
               # variable where values for new columns will come from
               values_from = result)


# Widen the results data along with the units (since units are different for each analyte)
lake_dat %>%
   pivot_wider(id_cols = c("name", "date"),
               names_from = cas_rn,
               # add a second column of values to widen
               values_from = c("result", "unit"))


# This is not very useful, so widen the results data, but add the units to the column name
lake_dat %>%
   mutate(unit = if_else(cas_rn=="TURB", "NTU", unit)) %>%
   pivot_wider(id_cols = c("name", "date"),
               # add a second column to take new column names from
               names_from = c("cas_rn", "unit"),
               # separator to put between the two name components
               names_sep = "_",
               values_from = result)


# What if in addition to the value, we want to know if the result was above the detection limit?
lake_dat %>%
   mutate(unit = if_else(cas_rn=="TURB", "NTU", unit)) %>%
   pivot_wider(id_cols = c("name", "date"),
               # two columns for variable names
               names_from = c("cas_rn", "unit"),
               names_sep = "_",
               # two columns of data to widen
               values_from = c("result", "detect"))



#--
# pivot_longer()
#--

# Lengthen a data set, collapsing variable names into a single column


# Create a new variable with analyte name and units
lake_dat %>%
   mutate(unit = if_else(cas_rn=="TURB", "NTU", unit)) %>%
   # widen the data set first, combining analyte and unit into new variable names
   pivot_wider(id_cols = c("name", "date"),
               names_from = c("cas_rn", "unit"),
               names_sep = "_",
               values_from = result) %>%
   # now lengthen the data set to get the new analyte-unit column
   pivot_longer(cols = TURB_NTU:PH_None,
                names_to = "analyte",
                values_to = "result")


# Alternatively, split the variable names into more than one new variable
lake_dat %>%
   mutate(unit = if_else(cas_rn=="TURB", "NTU", unit)) %>%
   pivot_wider(id_cols = c("name", "date"),
               names_from = c("cas_rn", "unit"),
               names_sep = "_",
               values_from = result) %>%
   # lengthen the data set, but split the analyte and units from the variable name into two new variables
   pivot_longer(cols = TURB_NTU:PH_None,
                names_to = c("analyte", "units"),
                names_sep = "_",
                values_to = "result")



#--
# separate()
#-- 

# If a column/variable contains more than one piece of data/information, use separate() to split the 
#  column/variable into multiple columns/variables.

# In the raw Ambient Lake Monitoring data set, the variable "projectCode" contains two pieces of information,
#  a sampling program, and the organization which collected the data point. All of the projectCode's are the
#  same in this data set, but one could imagine a data set in which these may vary (e.g. multiple organizations
#  collecting the data for a large project or program). 

lake_raw %>%
   separate(col = projectCode,
            into = c("program", "partner"),
            sep = "-")


# Separate the Date variable into Year, Month, and Day
lake_dat %>%
   separate(col = date, 
            into = c("year", "month", "day"),
            sep = "-")


# Split a variable but still keep the original
lake_dat %>%
   separate(col = date, 
            into = c("year", "month", "day"),
            sep = "-",
            remove = F)



#--
# unite()
#--

# Use unite() to combine data/information from multiple columns into one new column


# Add the sampling year to the lake name
lake_dat %>%
   # add the variable "year"
   mutate(year = lubridate::year(date)) %>%
   # combine the new year variable with lake name
   unite(col = "year_lake",
         year, name,
         sep = " - ")


# The unite() function can be handy when formatting tables for a manuscript. 
# For example, creating a column of mean plus/minus standard deviation. 
#  [ this can also be achieved with base::paste() or glue::glue() ]

lake_dat %>%
   # select just the first 8 lakes to work with
   slice_head(n=23) %>%
   group_by(name) %>%
   # create fake mean and SD data for each lake
   summarize(mean = mean(result),
             sd = sd(result)) %>%
   ungroup() %>%
   # round all values to 2 decimal places
   mutate(across(mean:sd, ~round(., 2))) %>%
   # create the mean +/- SD column
   unite(col = "mean \u00B1 SD",
         mean, sd,
         sep = " \u00B1 ")



#--
# nest()
#--

# Condense the input data frame down to a single row for each unique value of a grouping variable
#  by nesting each group from the grouped input data frame. 
# This creates a new list-column containing an individual data frame for each group. 


# Pre-process the data set to make it easier to view
# Select just a subset of analytes and columns for working with
lake_prep = lake_raw %>%
   # select desired analytes
   filter(cas_rn %in% c("NIT-NO3-NO2", "14265-44-2", "479-61-8", "77238-39-2", "11016-15-2")) %>%
   # select desired columns
   select(name, sampleDate, cas_rn, analyte, result, unit, detect) %>%
   # make analyte names easier to work with
   mutate(analyte = case_when(.$cas_rn=="NIT-NO3-NO2" ~ "N_inorg",
                              .$cas_rn=="14265-44-2" ~ "SRP",
                              .$cas_rn=="479-61-8" ~ "Chla",
                              .$cas_rn=="77238-39-2" ~ "Microcystin",
                              .$cas_rn=="11016-15-2" ~ "Phyco"),
          sampleDate = lubridate::mdy_hms(sampleDate),
          date = lubridate::date(sampleDate)) %>%
   # remove now-unnecessary columns
   select(-sampleDate, -cas_rn) %>%
   relocate(date, .after=name) %>%
   # select a few lakes to work with
   filter(name %in% c("Beaver Lake", "Big Spirit Lake", "Center Lake", "Clear Lake", "West Okoboji Lake"))


# Create a nested data frame
lake_nest = lake_prep %>%
   # group the data set by lake
   group_by(name) %>%
   # nest the grouped data frame by the grouping variable
   nest() %>%
   # remove the grouping structure
   ungroup()


# The original list-column created by nesting the grouped data frame retains the long format of the data
# Create a new list-column of data for each lake in wide format
lake_nest = lake_nest %>%
   rename(data_long = data) %>%
   # the family of map() functions from the 'purrr' package are ideal for iterating over list-columns
   mutate(data_wide = map(data_long, ~pivot_wider(.,
                                                  id_cols = date,
                                                  names_from = analyte,
                                                  values_from = c("result", "detect"))))


# Nested data frames are particularly useful for applying models or custom functions to each data set in the list-column

# Run some linear models on nested data for each lake
lake_mods = lake_nest %>%
   mutate(
      # Chlorophyll-a vs inorganic N
      chla_n = map(data_wide, ~lm(result_Chla ~ result_N_inorg, data = .)),
      # Chlorophyll-a vs SRP
      chla_p = map(data_wide, ~lm(result_Chla ~ result_SRP, data = .)))


# View the model outputs for all lakes at once

lake_mods %>%
   # iterate functions across each row (i.e. lake)
   rowwise(name) %>%
   # tidy view of model output
   summarize(broom::glance(chla_n))


lake_mods %>%
   rowwise(name) %>%
   summarize(broom::glance(chla_p))



#--
# unnest()
#--

# Expand a nested data frame back into a regular data frame

lake_nest %>%
   # select the nesting (grouping) variable and the list-columns to unnest
   select(name, data_wide) %>%
   unnest(cols = "data_wide")



