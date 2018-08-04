# Title: Error Flagging for Assets Data Entry
# Author: M. Ibrahim Khan
# Date: 1 Aug, 2018

#reading in the latest csv
library(tidyverse)
library(readxl)

check_data <- read_csv("data/Asset Form_WIDE.csv", na = c("", "-"))
scrutiny_list <- read_csv("data/candidate_scrutiny_list.csv")%>%
  mutate(cnic=as.character(candidate_CNIC_ECP))

#str(check_data$uid)

#list of things to check:
# 1. CNIC length (cnic): should have length 13 digits
# 2. Phone number length (contact_num): should be length 10 OR 99999999999
# 3. Unique Identifier (uid): length <=5 or is -9999
# 4. Numeric fields should have value>0 or -99
#    List of Numeric fields:
#    a. starts_with("im_prop_pak")
#    b. bus_cap_wpk_num, bus_cap_wpk_count, starts_with("bus_cap_wpk_am")
#    c. bus_cap_opk_num, bus_cap_opk_count, starts_with("bus_cap_opk_am")
#    d. bank_drafts:motor_cost, motor_g_count:liabilities
#    e. dependents, num_criminal, crimes count, ntn,
#    f. year_income:income_total, starts_with("income_total", "tax_paid") 
#    g. intern_trips_cost:passport_for
#    h. current_FY_net_assets:contact_num
#    exclude: year_income, year_agri_income

errs <- check_data %>%
  mutate_at( #check for numeric entries are all numeric
    vars(starts_with("im_prop_pak"),bus_cap_wpk_num, bus_cap_wpk_count, 
         starts_with("bus_cap_wpk_am"), bus_cap_opk_num, bus_cap_opk_count, 
         starts_with("bus_cap_opk_am"), bank_drafts:motor_cost, motor_g_count:liabilities,
         dependents, num_criminal, crimes_count, ntn,year_income:income_total, starts_with("income_total"),
         starts_with("tax_paid"), intern_trips_cost:passport_for, current_FY_net_assets:contact_num,
         -year_income, -year_agri_income
         ),
    funs(num_val_error = . != "" & !is.na(.) & is.na(as.numeric(.)) )
  ) %>%
  mutate(CNIC_val_error = (nchar(cnic)!=13), #I also want those for which CNIC is blank or -99
         #phone_val_error = !(nchar(contact_num)==10 | contact_num==99999999999 | contact_num=="" | is.na(contact_num)),
         uid_val_error = (nchar(uid)>5 | (uid<=0 & uid!=-9999) | is.na(as.numeric(uid)) | uid=="" ), #flags blanks. Doesn't flag -9999
         cnic_missing = !(cnic %in% scrutiny_list$cnic) & cnic!=9999999999999
         ) %>%
  filter_at(
    vars(ends_with("val_error")),
    any_vars(.)
  )

write.csv(errs, file = "data/flagged_entries.csv", row.names = FALSE)

# CHECKING FOR ERRORS IN CNIC ENTRIES

missing_cnics_raw <- errs%>%
  filter(cnic_missing)%>%
  mutate(seat_id = paste0(type_seat, "-", const_number)) %>%
  select(seat_id, candidate_name, uid, cnic, cnic_missing)

write.csv(missing_cnics_raw, file = "data/missing_cnics.csv", row.names = FALSE)

missing_cnics_categorized <- read_excel("data/missing_cnics_categorized.xlsx")%>%
  mutate(cnic=as.character(cnic), correct_cnic=as.character(correct_cnic), uid=as.character(uid))

# missing_cnics_categorized2 <- left_join(missing_cnics_raw, missing_cnics_categorized, by = c("seat_id", "candidate_name"))
# 
 missing_cnics_raw
 missing_cnics_categorized
# missing_cnics_categorized2
# write.csv(missing_cnics_categorized, file = "data/missing_cnics_categorized.csv", row.names = FALSE)
 
 table(missing_cnics_categorized$error_type, useNA = "always")
 


