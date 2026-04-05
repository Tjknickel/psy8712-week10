# Script Settings and Resources 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

# Data Import and Cleaning 
# The data was imported from an SPSS file (.sav) using the read_sav function from the haven package, as read_delim cannot be used on .sav files as the delim is not easily stored as in other file types (e.g. tabs, commas). To ensure that missing, don't know, not applicable, etc. values were coded as missing values (NAs in R), the user_na argument was set to false. Although this is the default value for this argument, you can double check using the codebook and by setting the user_na argument to true (and calling attributes on the columns to see these specific values. e.g. -98 = don't know) that any of these values was coded as values between -100 and -10. The read_csv function automatically on import converts these values into NAs. The mosthrs variable was filtered to keep only participants who did not have an NA value for this question, then this column was renamed to work hours. Finally, the hrs1 and hrs2 columns were removed and also columns where 75% or more of the responses were NA values were removed. 
gss_tbl <- read_sav("~/Desktop/psy8712-week10/data/GSS2016.sav", 
                    user_na = FALSE) %>%
  filter(!is.na(mosthrs)) %>%
  rename(`work hours` = mosthrs) %>%
  select(-c(hrs1, hrs2)) %>%
  select(where(~ mean(is.na(.)) < 0.75))

# Visualization 

# Analysis

# Publication 