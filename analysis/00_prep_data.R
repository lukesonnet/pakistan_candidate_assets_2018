# Author: Luke Sonnet
# Date created: 2018-10-09
# Purpose: Clean asset data for merging

# ----------
# Pre-amble
# ----------
library(tidyverse)
library(fuzzyjoin)
library(readxl)
library(stringdist)
ass <- read_csv("data/final_data/cleaned_asset_data_IK.csv", 
                na = ".", 
                col_types = cols(cnic_final = col_character())) %>%
  rename(cnic_old = cnic,
         cnic = cnic_final) %>%
  mutate(constituency_code = paste0(type_seat, "-", const_number),
         # fix uid errors!
         uid_final = ifelse(uid_final == "05949" & candidate_name == "Muhammad Muavia", "05941", uid_final),
         uid_final = ifelse(uid_final == "05018" & constituency_code == "PK-63", "05013", uid_final))

ass$constituency_code
# check constituencies
setdiff(c(paste0("NA-", 1:272),
          paste0("PP-", 1:297),
          paste0("PB-", 1:51),
          paste0("PK-", 1:99),
          paste0("PS-", 1:130)),
        ass$constituency_code)
setdiff( ass$constituency_code,
         c(paste0("NA-", 1:272),
          paste0("PP-", 1:297),
          paste0("PB-", 1:51),
          paste0("PK-", 1:99),
          paste0("PS-", 1:130)))

filter(ass, constituency_code == "PS-209") %>% select(contains("uid"))



## check the UID duplicates!!