# Title: Error Flagging for Assets Data Entry
# Author: M. Ibrahim Khan
# Date: 1 Aug, 2018

#reading in the latest csv
library(tidyverse)

check_data <- read_csv("data/Asset Form_WIDE_v1.0.csv")



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

check_data %>%
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
         uid_val_error = (nchar(uid)>5 | (uid<=0 & uid!=-9999) | is.na(as.numeric(uid)) | uid=="" ) #flags blanks. Doesn't flag -9999
  )%>%
  filter_at(
    vars(ends_with("val_error")),
    any_vars(.)
  )%>%
  write.csv(file = "data/flagged_entries.csv", row.names = FALSE)

  