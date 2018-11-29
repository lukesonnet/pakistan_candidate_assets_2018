# Author: Luke Sonnet
# Date created: 2018-10-11
# Purpose: Merge district data with results data

library(tidyverse)
library(rdrobust)
library(estimatr)

# ----------
# Prep district level data
# ----------

dist <- read_csv("../redistricting_18/dist_2018.csv")
res <- read_csv("../pakistan_election_results_2018/pk_constituency_data_2018.csv")

# Clean constituency names
res$district <- gsub("\\s{0,1}\\-\\s{0,1}[XIV]+", "", res$constituency_name) %>%
  recode(
    .,
    `RahimYar Khan` = "Rahim Yar Khan",
    `Nowshehra` = "Nowshera",
    `Swatll` = "Swat",
    `Rajan Pur` = "Rajanpur",
    `Malakand` = "Malakand Protected Area"
  )

res <- res %>%
  mutate(province = ifelse(district == "Tribal Area", "FATA", province),
         province = ifelse(province == "Khyber Paktunkhwa", "KP", province),
         district = case_when(
           constituency_code %in% c("NA-43", "NA-44") ~ "Khyber Agency",
           constituency_code %in% c("NA-40", "NA-41") ~ "Bajaur Agency", # error should be 3 and kurram should be 1
           constituency_code %in% c("NA-46", "NA-45") ~ "Kurram Agency",
           constituency_code %in% c("NA-47") ~ "Orakzai Agency",
           constituency_code %in% c("NA-42") ~ "Mohmand Agency",
           constituency_code %in% c("NA-48") ~ "North Waziristan Agency",
           constituency_code %in% c("NA-49", "NA-50") ~ "South Waziristan Agency",
           constituency_code %in% c("NA-51") ~ "Frontier Regions",
           TRUE ~ district
         )) %>%
  bind_rows(data_frame(
    election_date = lubridate::as_date("2018-07-25"),
    election_type = "General Election",
    contest_status = c("Unopposed", rep("Postponed", 8)),
    assembly = c("Provincial", rep("Provincial", 6), rep("National", 2)),
    province = c("Sindh", "Sindh", "Punjab", "Punjab", "KP", "KP", "Balochistan", "Punjab", "Punjab"),
    constituency_code = c("PS-6", "PS-87", "PP-87", "PP-103", "PK-78", "PK-99", "PB-35", "NA-60", "NA-103"),
    constituency_number = c(6, 87, 87, 103, 78, 99, 35, 60, 103),
    district = c("Kashmore", "Malir", "Mianwali", "Faisalabad", "Peshawar", "Dera Ismail Khan", "Mastung", "Rawalpindi", "Faisalabad"),
    win_party = c("Pakistan Peoples Party Parliamentarians", rep(NA_character_, 8))
  ))
table(res$contest_status)

setdiff(c(paste0("NA-", 1:272),
          paste0("PP-", 1:297),
          paste0("PB-", 1:51),
          paste0("PK-", 1:99),
          paste0("PS-", 1:130)),
        res$constituency_code)

# Missing from 2018 (cons level)
# 1 balochistan PA (PB-35 postponed for BAP assassination)
# 2 KP PAs (PK-78 by-election, ANP candidate killed; PK-99 by-election, PTI candidate killed)
# 2 punjab NAs (NA-60 by-election; NA-103 by-election)
# 2 punjab PAs (PP-87 by-election, death of candidate; PP-103 by-election, death of ind. candidate)
# 2 sindh PAs (PS-6, unopposed PPP candidate; PS-87 by-election, death of TLP candidate)
dist$district <- gsub("\\(|\\)|", "", gsub("\\r", " ", dist$district)) %>%
  recode(
    .,
    `Attack` = "Attock",
    `Badui` = "Badin",
    `Batagram` = "Battagram",
    `Boner` = "Buner",
    `Gera Bugti` = "Dera Bugti",
    `Gawadar` = "Gwadar",
    `Hydarabad` = "Hyderabad",
    `Jhalmagsi` = "Jhall Magsi",
    `Kachi` = "Kachhi",
    `Kambar Shahdad kot` = "Kamber Shahdadkot",
    `Kashmoor` = "Kashmore",
    `Killa Saifuliah` = "Killa Saifullah",
    `Kolai Pallas Kohistan` = "Kolai Palas Kohistan",
    `Korangi` = "Korangi Karachi",
    `Lasbella` = "Lasbela",
    `Mandl Bahauddin` = "Mandi Bahauddin",
    `Mirpur Khas` = "Mirpurkhas",
    `Muzatfargarh` = "Muzaffargarh",
    `NausheroFeroze` = "Naushahro Feroze",
    `ShaheedBenazirabad` = "Shaheed Benazirabad",
    `ShaheedSikandar Abad` = "Shaheed Sikandarabad",
    `Sohbatpur` = "Sohbat Pur",
    `Sujawai` = "Sujawal",
    `Sukkar` = "Sukkur",
    `Thatha` = "Thatta",
    `Taandoallah Yr` = "Tando Allahyar",
    `Umar Kot` = "Umerkot",
    `VVashuk` = "Washuk",
    `Vahan` = "Vehari"
  )

table(dist$na_seats)
table(dist$pa_seats)

plot(dist$na_seat_share, dist$na_seats)

filter(res, province == "FATA") %>% select(district, starts_with("constituency"))
filter(dist, province == "FATA")

setdiff(res$district, dist$district)
setdiff(dist$district, res$district)

cbind(dist$na_seat_share, dist$na_seats)
dist %>%
  group_by(na_dist_code) %>%
  summarise(less_one = any(na_seats < 1),
         n = n()) %>%
  filter(n > 1 | less_one)

filter(dist, pa_dist_code == 100)
dist %>%
  group_by(pa_dist_code) %>%
  summarise(less_one = any(pa_seats < 1),
         n = n()) %>%
  filter(n > 1 | less_one)

dist$pa_seats
res$consti
min(dist$pa_seat_share, na.rm = TRUE)

dist$sr_no
names(dist)
d2 <- dist %>%
  select(-sr_no) %>%
  gather(dat, val, starts_with("na"), starts_with("pa")) %>%
  separate(dat, into = c("assembly", "var"), extra = "merge") %>%
  spread(var, val) %>%
  group_by(province, assembly, dist_code) %>%
  summarize(district = if (n() > 1) paste0(district, collapse = "-") else unique(district),
            pop_2017 = sum(pop_2017),
            quota = unique(quota),
            seat_share = sum(seat_share),
            seats = round(sum(seats)), 2) %>%
  filter(!is.na(seats)) %>%
  mutate(district = recode(
    district,
    `Killa Saifullah-Sherani-Zhob` = "Killa Saifullah-Zhob-Sherani",
    `Kohistan-Kohistan Lower-Kolai Palas Kohistan-Torghar` = "Kohistan-Lower Kolai Pallas Kohistan",
    `Lehri-Sibi` = "Sibi-Lehri",
    `Harnai-Ziarat` = "Ziarat-Harnai",
    `Duki-Harnai-Loralai-MusaKhail-Ziarat` = "Loralai-Musa Khail-Ziarat-Dukki-Harnai",
    `Gwadar-Lasbela` = "Lasbela-Gwadar",
    `Barkhan-Dera Bugti-Kohlu-Lehri-Sibi` = "Dera Bugti-Kohlu-Barkhan-Sibi-Lehri",
    `Jaffarabad-Sohbat Pur` = "Jafarabad-Sohbatpur"
  ))

filter(d2, district == "Sherani")
filter(d2, district == "MusaKhail")
filter(res, district == "Nasirabad-cum-Kachhi-cum-Jhal Magsi") %>% 
  select(district, constituency_name, constituency_code, province)
filter(res, district == "Nasirabad")%>% 
  select(district, constituency_name, constituency_code, province)
filter(res, district == "Musakhail-cum-Sherani") %>% 
  select(district, constituency_name, constituency_code)
filter(res, district == "Washuk") %>%
  select(district, constituency_name, constituency_code)
  
setdiff(gsub("\\-cum\\-", "\\-", res$district), d2$district)
setdiff(d2$district, gsub("\\-cum\\-", "\\-", res$district))

res$assembly
d2$seat_share
mdist <- res %>%
  mutate(district = gsub("\\-cum\\-", "\\-", district),
         assembly = ifelse(assembly == "Provincial", "pa", "na")) %>%
  left_join(d2, by = c("province", "assembly", "district")) %>%
  mutate(seat_share = pop_2017 / quota,
         seat_treatment = ifelse(seats < 1, 0, seats),
         bin = paste0(assembly, "-", cut(seat_share, breaks = 0:100)),
         share_running = seat_share - trunc(seat_share) - 0.5,
         treated = as.numeric(share_running > 0),
         actual = case_when(
           district == "Jhang" & assembly == "na" ~ 0,
           district == "Swat" & assembly == "pa" ~ 1,
           TRUE ~ treated
         ),
         seats2 = ifelse(seats == 0.5, 1, seats),
         seats2_groups = cut(seats, breaks = c(0, 1, 2, 3, 4, 5, 50)),
         province_assembly = paste0(province, "_", assembly),
         seats_quartile = cut(seats, breaks = quantile(seats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))) %>%
  group_by(province, assembly, district) %>%
  mutate(number_seats_results = n())
m <- mdist
table(m$seats)
sum(m$seats, na.rm = TRUE)
table(m$seats, useNA = "a")
m %>% filter(is.na(seats)) %>% select(constituency_code)
table(table(m$constituency_code))
# m <- left_join(res, d2, by = c("province", "district")) %>%
#   mutate(district = gsub("\\-cum\\-", "\\-", district),
#          na_seat_treatment = ifelse(na_seats < 1, 0, na_seats),
#          na_bin = paste0("NA-", cut(na_seat_share, breaks = 0:100)),
#          na_share_running = na_seat_share - trunc(na_seat_share) - 0.5,
#          na_treated = as.numeric(na_share_running > 0),
#          na_actual = ifelse(district == "Jhang", 0, na_treated), # jhang
#          pa_seat_treatment = ifelse(pa_seats < 1, 0, pa_seats),
#          pa_seat_share = pop_2017 / pa_quota,
#          pa_bin = paste0("PA-", cut(pa_seat_share, breaks = 0:100)),
#          pa_share_running = pa_seat_share - trunc(pa_seat_share) - 0.5,
#          pa_treated = as.numeric(pa_share_running > 0),
#          pa_actual = ifelse(district == "Swat", 1, pa_treated),
#          seat_share = ifelse(assembly == "Provincial", pa_seat_share, na_seat_share),
#          share_running = ifelse(assembly == "Provincial", pa_share_running, na_share_running),
#          treated = ifelse(assembly == "Provincial", pa_treated, na_treated),
#          actual = ifelse(assembly == "Provincial", pa_actual, na_actual),
#          seats = ifelse(assembly == "Provincial", pa_seats, na_seats),
#          seats2 = ifelse(seats == 0.5, 1, seats),
#          bin = ifelse(assembly == "Provincial", pa_bin, na_bin),
#          code = ifelse(assembly == "Provincial", pa_dist_code, na_dist_code),
#          seats2_groups = cut(seats, breaks = c(0, 1, 2, 3, 4, 5, 50)),
#          province_assembly = paste0(province, "_", assembly),
#          seats_quartile = cut(seats, breaks = quantile(seats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))) %>%
#   group_by(province, assembly, district) %>%
#   mutate(number_seats_results = n())

table(m$na_seat_share < 0.5)
table(m$district)
table(table(m$district))
table(m$seats, m$seats2_groups)
lm_robust(effective_parties ~ seats2_groups + province, data = m, clusters = district)
lm_robust(effective_parties ~ seats + province, data = m)

lmro <- lm_robust(effective_parties ~ seats + I(seats^2) + province, data = m)
plot(m$seats[complete.cases(m[, c("effective_parties", "seats", "province")])], lmro$fitted.values)

ggplot(m, aes(x = seats2, y = effective_parties)) +
  geom_point() + 
  geom_smooth()

m$assembly
lm_robust(effective_parties ~ seats + I(seats^2) + province_assembly, data = m)
lm_robust(effective_parties ~ seats + I(seats^2) + province_assembly, data = m, subset = seats < 15)

lm_robust(effective_parties ~ seats + I(seats^2) + province_assembly, data = m, cluster = district)
lm_robust(effective_parties ~ seats + I(seats^2) + province_assembly, data = m, subset = seats < 15, cluster = district)

lm_robust(effective_parties ~ seats_quartile + province_assembly, data = m, cluster = district)
lm_robust(effective_parties ~ seats_quartile + province_assembly, data = m, subset = seats < 15, cluster = district)
bind_cols(m$bin)
quantile(m_trim$seats)

m_trim <- m %>%
  group_by(bin) %>%
  mutate(has_both = var(treated) > 0) %>%
  filter(has_both) %>%
  mutate(trim_seat_quartile = cut(seats, breaks = c(0, 1, 3, 5, 10)),
         bin_all = cut(seat_share, breaks = 0:10),
         ass_dist = paste0(assembly, "_", district))
table(m_trim$province_assembly, m_trim$bin)


lm_robust(effective_parties ~ treated + bin + province, data = m_trim, cluster = ass_dist)
lm_robust(effective_parties ~ treated + bin + province + 0, data = m_trim, cluster = ass_dist)

lm_robust(effective_parties ~ treated * trim_seat_quartile + bin + province, data = m_trim, cluster = district)

m_trim_dum <- fastDummies::dummy_cols(m_trim, c("bin", "province"), remove_first_dummy = TRUE)
str(m_trim_dum)
m_trim_dum
with(m_trim_dum, rdrobust(
  y = effective_parties,
  x = share_running
)) %>%
  summary

table(m_trim$treated, m_trim$bin)
lm_lin(effective_parties ~ treated, ~ bin_all, data = m_trim)
lm_robust(effective_parties ~ treated * bin_all, data = m_trim)
table(m_trim$bin_all, m_trim$seats)
with(m_trim_dum, rdrobust(
  y = effective_parties,
  x = share_running
)) %>%
  summary
lm_robust(effective_parties ~ treated * trim_seat_quartile + bin + province, data = m_trim, cluster = district)
m_trim$share_running

ests <- tidy(lm_robust(effective_parties ~ treated * bin + province + 0, data = m_trim, cluster = district))
ests
table(m_trim$seats)
ests <- tidy(lm_robust(effective_parties ~ treated*seats_quartile + bin + province + 0, data = m_trim, cluster = district))
ests <- tidy(lm_robust(effective_parties ~ treated*seats_quartile + bin + province + 0, data = m_trim, cluster = district))

ests
ests_hc2 <- tidy(lm_robust(effective_parties ~ treated * bin + province + 0, data = m_trim))

ests[grep("treated", ests$term), ]
ests[grep("treated", ests$term), ]

lm_robust(effective_parties ~ treated*bin + province_assembly + 0, data = m_trim, cluster = district)

table(m$bin, m$seats)
lm_robust(effective_parties ~ treated + bin + province_assembly, data = m, cluster = district)
lm_robust(effective_parties ~ bin + treated + province_assembly, data = m, cluster = district)


lm_robust(effective_parties ~ seats + I(seats^2) + province, data = m, subset = seats < 15)
lm_robust(win_pct ~ seats + I(seats^2) + province, data = m, subset = seats < 15)
lm_robust(MOV_pct ~ seats + I(seats^2) + province, data = m, subset = seats < 15)

lm_robust(effective_parties ~ seats + I(seats^2) + province, data = m)
lm_robust(win_pct ~ seats + I(seats^2) + province, data = m)
lm_robust(MOV_pct ~ seats + I(seats^2) + province, data = m)


table(m$seats)
filter(m, seats == 0.5) %>% select(constituency_code)
iv_robust(effective_parties ~ seats | seat_share, data = m)
iv_robust(effective_parties ~ seats | seat_share, data = m, subset = seat_share < 15)
iv_robust(effective_parties ~ seats + province | seat_share + province, data = m, subset = seat_share < 15)

iv_robust(effective_parties ~ seats + province | seat_share + province, data = m)
iv_robust(effective_parties ~ seats + province | seat_share + province, data = m)

filter(m, seats!=number_seats_results) %>%
  select(1:10, number_seats_results, seats) %>%
  as.data.frame()
# Mansehra only has 1 instead of 2 because it has a second that is merged with Torghar
# FATA we have the misallocation of one of bajaur's to kurram
# Balochistan seems to have taken the 1 assigned for washuk and kharan and given one to each, not sure where the extra one comes from

head(m)
plot(m$share_running, m$treated)
plot(m$seats, m$treated)

plot(m$seat_share, m$seats)


lm_robust(MOV_pct ~ seats, data = m, cluster = code)
lm_robust(MOV_pct ~ seats + bin, data = m, cluster = code)
lm_robust(MOV_pct ~ seats*share_running + bin, data = m, cluster = code)
lm_robust(MOV_pct ~ treated*share_running + bin, data = m, cluster = code)

lm_robust(effective_parties ~ seats, data = m, cluster = code)
lm_robust(effective_parties ~ seats + bin, data = m, cluster = code)
lm_robust(effective_parties ~ seats*share_running + bin, data = m, cluster = code)
lm_robust(effective_parties ~ treated*share_running + bin, data = m, cluster = code)
lm_robust(effective_parties ~ treated, data = m, cluster = code)
lm_robust(effective_parties ~ treated + bin + province, data = m, cluster = code)

m_var <- m %>%
  group_by(bin) %>%
  mutate(variation_treatment = sd(treated) != 0) %>%
  filter(variation_treatment) %>%
  mutate(seat_bin = cut(seats, breaks = c(0, 2, 5, 10)))


with(m, table(bin, treated))
with(m_var, table(bin, treated))
with(m_var, table(seats, seat_bin))

lm_robust(effective_parties ~ treated + bin, data = m, cluster = code)
lm_robust(effective_parties ~ treated + bin, data = m_var, cluster = code)
lm_robust(effective_parties ~ treated*seats + bin, data = m_var, cluster = code)

lm_robust(effective_parties ~ treated + bin, data = m_var, cluster = code)
lm_robust(effective_parties ~ treated*seat_bin + bin, data = m_var, cluster = code)
lm_robust(effective_parties ~ treated*seat_bin*share_running + bin, data = m_var, cluster = code)
# Getting more seats leads you to have more effective parties, except in bigger areas it drives it down

names(m_var)
lm_robust(MOV_pct ~ treated + bin, data = m_var, cluster = code)
lm_robust(MOV_pct ~ treated*seat_bin + bin, data = m_var, cluster = code)
lm_robust(MOV_pct ~ treated*seat_bin*share_running + bin, data = m_var, cluster = code)
# Getting more seats decreases the margin of victory (again more competitve), effect goes away in larger areas

lm_robust(win_pct ~ treated + bin, data = m_var, cluster = code)
lm_robust(win_pct ~ treated*seat_bin + bin, data = m_var, cluster = code)
lm_robust(win_pct ~ treated*seat_bin*share_running + bin, data = m_var, cluster = code)

lm_robust(I(win_party == "Pakistan Tehreek-e-Insaf") ~ treated + bin, data = m_var, cluster = code)
lm_robust(I(win_party == "Pakistan Tehreek-e-Insaf") ~ treated*seat_bin + bin, data = m_var, cluster = code)
lm_robust(I(win_party == "Pakistan Tehreek-e-Insaf") ~ treated*seat_bin*share_running + bin, data = m_var, cluster = code)
table(m_var$seats)

filter(m, bin == "PA-(6,7]") %>% select(contains("seats"), contains("share"), treated) %>% as.data.frame
names(m_var)

lm_robust(MOV_pct ~ treated*share_running*seats + bin, data = m, cluster = code)


with(m, rdplot(MOV_pct, share_running))
with(m, rdrobust(MOV_pct, share_running, cluster = code)) %>%
  summary

# 2018 PAs 130+99+51+297
# 2018 NAs
#61 sindh
#141 punjab
#39 kpk
#12 fata
#3 islamabad
#16 balochistan
m %>%
  group_by(province, assembly) %>%
  summarize(n = n())
# All rows accounted for

m %>%
  group_by(province, district) %>%
  summarize_at(vars(na_seats, pa_seats), funs(unique)) %>%
  group_by(province) %>%
  summarize_at(vars(ends_with("seats")), funs(sum(., na.rm = TRUE)))
# Missing from 2018 (dist treat)
# 2 balochistan NA
# 5 balochistan PA
# 1 FATA NA (frontier regions)
table(is.na(m$na_seats), m$province)
filter(m, is.na(na_seats) | is.na(pa_seats)) %>% select(province, assembly, district, na_seats, pa_seats) %>% arrange(province, assembly, district) %>% as.data.frame

table(dist$district[dist$province=="KP"])

identical(272L+130L+99L+51L+297L, nrow(m))

setdiff(alld$district, dyn_dist$district)
sort(unique(dyn_dist$district))[40:length(unique(dyn_dist$district))]
filter(dyn_dist, constype==1)
setdiff(dyn_dist$district, tolower(m$district))
sort(unique(m$district))
setdiff(tolower(dyn_dist$district), tolower(alld$district))
setdiff(tolower(alld$district), tolower(dyn_dist$district))

table(dyn_dist$district)
table(alld$district)
alld$assembly
table(alld$district, alld$constituency_number)
table(alld$constituency_name)
filter(m, district == "Sargodha") %>% select(contains("pa_"))
filter(dist, na_seats == 3, na_seat_share - trunc(na_seat_share) > 0.5)
table(m$na_bin, m$na_seats, m$na_treated)
table(m$pa_bin, m$pa_seats, m$pa_actual)
table(dist$pa_seats)
filter(m, pa_bin == "(9,10]", pa_seats == 10) %>% {table(.$district)}
plot(m$na_share_running, m$na_seats)
table(m$na_bin, m$na_seats)
filter(dist, pa_seats == 10) %>% select(district, contains("pa"))
filter(m, district == "Jhang")
filter(m, district == "Swat")
dist$na_quota
lm_robust(win_pct ~ na_share_running*na_treated + na_bin, m, cluster = na_dist_code)

plot(m$na_seat_share, m$na_seats)

ass <- read_csv("data/final_data/cleaned_asset_data_IK.csv", na = ".") %>%
  mutate_at(vars(cash_cost, cash_bank_cost, current_fy_net_assets), funs(ifelse(is.na(.), 0, .))) %>%
  mutate(cnic = as.character(cnic_final),
         constituency_code = paste0(type_seat, "-", const_number),
         # fix uid errors!
         uid_final = ifelse(uid_final == "05949" & candidate_name == "Muhammad Muavia", "05941", uid_final),
         uid_final = ifelse(uid_final == "05018" & constituency_code == "PK-63", "05013", uid_final),
         total_prob_bis_cash = im_prop_pak_totes + im_prop_pako_totes + invest_totes + bus_cap_opk_totes + bus_cap_wpk_totes + cash_cost + cash_bank_cost,
         avg_assets = (total_prob_bis_cash + current_fy_net_assets) / 2,
         log_avg_assets = log(avg_assets + 1))

ass_cons <- ass %>%
  mutate(constituency_code = paste0(type_seat, "-", const_number)) %>%
  group_by(constituency_code) %>%
  summarize_at(vars(ends_with("avg_assets")),
               funs(
                 n = n(),
                 mean = mean(., na.rm = TRUE),
                 sum = sum(., na.rm = TRUE),
                 hhi = sum((. / sum(., na.rm = TRUE)) ^ 2, na.rm = TRUE)
               )) %>%
  mutate(n_cands = avg_assets_n) %>%
  select(-contains("assets_n"))

m <- left_join(m, ass_cons)
m_trim <- m %>%
  group_by(bin) %>%
  mutate(has_both = var(treated) > 0) %>%
  filter(has_both) %>%
  mutate(trim_seat_quartile = cut(seats, breaks = c(0, 1, 3, 5, 10)),
         bin_all = cut(seat_share, breaks = 0:10),
         ass_dist = paste0(assembly, "_", district),
         trim_seat_group = cut(seats, breaks = c(0, 1, 2, 3, 4, 10)),
         trim_seat_group_median = cut(seats, breaks = c(0, 3, 10)),
         trim_seat_group_tercile = cut(seat_share, breaks = c(0, 3, 5, 10)))


ggplot(m_trim, aes(x = seats, y = log_avg_assets_hhi)) +
  geom_point() +
  geom_smooth()
plot(m_trim$seat_share, m_trim$trim_seat_group_tercile)

table(m_trim$bin_all, m_trim$seats)
plot(m_trim$seats, m_trim$bin_all)
library(DeclareDesign)

m_trim

quantile(d2$seats, breaks = c(0, 1/3, 2/3, 1))
m_trim$trim_seat_group_median
m_trim$trim_seat_group_median
quart
mean(d2$seats)
median(d2$seats)
m_trim$bin
lm_robust(win_pct ~ treated*bin_all + assembly + province, data = m_trim, clusters = paste0(assembly, district), se_type = "stata")

lm_robust(win_pct ~ treated + bin + province, data = m_trim, clusters = paste0(assembly, district), se_type = "stata")
lm_robust(win_pct ~ treated * bin + province, data = m_trim, clusters = paste0(assembly, district), se_type = "stata", subset = assembly == "na")
lm_robust(MOV_pct ~ treated * trim_seat_group_tercile + province, data = m_trim, clusters = ass_dist, se_type = "stata")

lm_robust(MOV_pct ~ treated * trim_seat_group_tercile + province, data = m_trim, clusters = ass_dist, se_type = "stata")

table(m_trim$trim_seat_group_tercile, m_trim$bin, m_trim$treated)

lm_robust(MOV_pct ~ treated * trim_seat_group_tercile + bin + province, data = m_trim, clusters = ass_dist, se_type = "stata")
library(quantreg)

table(is.na(m_trim$treated))
table(is.na(m_trim$province))

table(is.na(m_trim$ass_dist))

lm_robust(MOV_pct ~ treated + bin + province, data = m_trim, clusters = paste0(assembly, district), se_type = "stata")

m_trim$seats
ggplot(m_trim, aes(x = factor(seats), y = log_avg_assets_mean)) +
  geom_violin()

table(m_trim$ass_dist, m_trim$treated)

table(m_trim$treated, m_trim$bin)
table(m_trim$treated, m_trim$bin_all)

with(m_trim, table(bin_all, trim_seat_group_tercile, treated, useNA = "a"))

m_trim$share_running
m_trim$trim_seat
to <- lm_robust(log_avg_assets_mean ~ treated * trim_seat_group_tercile, data = m_trim, clusters = ass_dist)

randomizr::block_and_cluster_ra(blocks = m_trim$bin, clusters = m_trim$ass_dist)
outd <- map_dfr(seq_len(1000), ~ {
  m_trim$fake_treat <- randomizr::block_and_cluster_ra(blocks = m_trim$bin, clusters = m_trim$ass_dist)
  data.frame(t(coef(lm_robust(log_avg_assets_mean ~ fake_treat * trim_seat_group_tercile, data = m_trim, clusters = ass_dist))))
})

ggplot(outd, aes(x=fake_treat)) +
  geom_histogram() + 
  geom_vline(xintercept = to$coefficients[to$term=="treated"])
ggplot(outd, aes(x=fake_treat.trim_seat_group_tercile.3.5.)) +
  geom_histogram() + 
  geom_vline(xintercept = to$coefficients[to$term=="treated:trim_seat_group_tercile(3,5]"])
ggplot(outd, aes(x=fake_treat.trim_seat_group_tercile.5.10.)) +
  geom_histogram() + 
  geom_vline(xintercept = to$coefficients[to$term=="treated:trim_seat_group_tercile(5,10]"])

outd <- map_dfr(seq_len(1000), ~ {
  m_trim$fake_treat <- randomizr::block_and_cluster_ra(blocks = m_trim$bin, clusters = m_trim$ass_dist)
  data.frame(t(coef(lm_robust(log_avg_assets_mean ~ fake_treat * trim_seat_group_tercile, data = m_trim, clusters = ass_dist))))
})


outd <- map_dfr(seq_len(1000), ~ {
  m_trim$fake_treat <- randomizr::block_and_cluster_ra(blocks = m_trim$bin, clusters = m_trim$ass_dist)
  lm_d <- t(coef(lm_robust(log_avg_assets_mean ~ fake_treat * trim_seat_group_tercile, data = m_trim, se_type = "none")))
  
  rq_d <- cbind(rbind(
    t(coef(rq(log_avg_assets_mean ~ fake_treat * trim_seat_group_tercile, tau = 0, data = m_trim))),
    t(coef(rq(log_avg_assets_mean ~ fake_treat * trim_seat_group_tercile, tau = 0.25, data = m_trim))),
    t(coef(rq(log_avg_assets_mean ~ fake_treat * trim_seat_group_tercile, tau = 0.5, data = m_trim))),
    t(coef(rq(log_avg_assets_mean ~ fake_treat * trim_seat_group_tercile, tau = 0.75, data = m_trim))),
    t(coef(rq(log_avg_assets_mean ~ fake_treat * trim_seat_group_tercile, tau = 1, data = m_trim)))
  ),
  "tau" = c(0, 0.25, 0.5, 0.75, 1))
  rd <- bind_rows(data.frame(lm_d), data.frame(rq_d))
})

rq(log_avg_assets_mean ~ treated * trim_seat_group_tercile, tau = 1, data = m_trim)

ggplot(outd, aes(x=fake_treat, xintercept = )) +
  facet_wrap(~tau) +
  geom_histogram() + 
  geom_vline(xintercept = to$coefficients[to$term=="treated"])
ggplot(outd, aes(x=fake_treat.trim_seat_group_tercile.3.5.)) +
  geom_histogram() + 
  geom_vline(xintercept = to$coefficients[to$term=="treated:trim_seat_group_tercile(3,5]"])
ggplot(outd, aes(x=fake_treat.trim_seat_group_tercile.5.10.)) +
  geom_histogram() + 
  geom_vline(xintercept = to$coefficients[to$term=="treated:trim_seat_group_tercile(5,10]"])
to$term

unique_ass_dists <- unique(m_trim$ass_dist)


sd(m_trim$log_avg_assets_mean, na.rm = TRUE)

 %>%
  summary(., se = "boot")

rq(log_avg_assets_mean ~ treated * trim_seat_group_tercile + bin_all, tau = 0, data = m_trim) %>%
  summary(., se = "boot")

m_trim$ass
difference_in_means(MOV_pct ~ treated, blocks = bin_all, clusters = ass_dist, data = m_trim)
difference_in_means(MOV_pct ~ treated, blocks = bin_all, clusters = ass_dist, data = m_trim)
lm_robust(MOV_pct ~ treated, m_trim, clusters = ass_dist)

lm_robust(log_avg_assets_mean ~ seats2_groups + assembly + province, data = m_trim, clusters = paste0(assembly, district), se_type = "stata")
lm_robust(log_avg_assets_hhi ~ seats2_groups + assembly + province, data = m_trim, clusters = paste0(assembly, district), se_type = "stata")

lm_robust(log_avg_assets_mean ~ treated*bin_all + assembly + province, data = m_trim, clusters = paste0(assembly, district), se_type = "stata")

lm_lin(win_pct ~ treated, covariates = ~ bin, data = m_trim, subset = !is.na(bin), clusters = paste0(assembly, district), se_type = "stata")

with(m, table(seats, treated, useNA = "always"))
with(m, table(bin, treated, useNA = "always"))

lm_lin(win_pct ~ treated, covariates = ~ bin, data = m, subset = !is.na(bin), clusters = paste0(assembly, code), se_type = "stata")

with(m, rdrobust::rdrobust(win_pct, share_running, fuzzy = actual)) %>%
  summary
with(m, rdrobust::rdrobust(win_pct, share_running, fuzzy = actual)) %>%
  summary

plot(m$share_running, m$treated)
qplot(m$na_seat_share, m$na_seats, xlab = "Expected Seats", ylab = "Actual Seats")

rdrobust::rdplot(m$win_pct, m$share_running)

names(m)
with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(win_pct, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(effective_parties, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(effective_parties, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(MOV_pct, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(turnout, na_share_running)) %>%
  summary
assets
with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(turnout, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(ln_current_assets_mean, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(ln_current_assets_hhi, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(current_assets_mean / 100000, na_share_running)) %>%
  summary






setdiff(ass_cons$constituency_code, m$constituency_code)
setdiff( m$constituency_code, ass_cons$constituency_code)

table(ass_cons$type_seat)
names(ass_cons$)
m$constituency_code
ass_cons
with(filter(m, !is.na(na_bin)), rdrobust::rdrobust(win_pct, na_share_running, fuzzy = na_actual)) %>%
  summary
with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(win_pct, na_share_running, fuzzy = na_actual)) %>%
  summary

names(m)
with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(win_pct, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(effective_parties, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(effective_parties, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(MOV_pct, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(turnout, na_share_running)) %>%
  summary
assets
with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(turnout, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(ln_current_assets_mean, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(ln_current_assets_hhi, na_share_running)) %>%
  summary

with(filter(m, na_seats < 10, !is.na(na_bin)), rdrobust::rdrobust(current_assets_mean / 100000, na_share_running)) %>%
  summary



# get old districts
table(d$election_date)
unique(dyn_dist$district)
# alld <- read_csv("../ccookman_election/pk_constituency_data_consolidated_080318.csv") %>%
#   filter(election_type == "General Election", election_date == "2013-05-11") %>%
#   mutate(district = trimws(gsub("\\-", " ", gsub("[\\sIiVvXxl0-9]*$", "", constituency_name))),
#          district = gsub("\\s*[cC]um\\s+", "-", district),
#          district = tolower(gsub("\\s[iIvV]*\\-", "-", district)),
#          district = recode(
#            district,
#            `badin-t.m.khan` = "badin-tando muhammad khan",
#            `bahawlpur` = "bahawalpur",
#            `batagram` = "battagram",
#            `buner` = "bunair",
#            `chagai` = "chaghai", # Chiniot split from jhang, jhang-chiniot
#            `chitra` = "chitral",
#            `d.i. khan-tank` = "d.i.khan-tank",
#            `dadau` = "dadu",
#            `dara bugt` = "dera bugti",
#            `dera ghazi khan` = "d.g.khan",
#            `dera ismail khan` = "d.i.khan",
#            `gujranwalla` = "gujranwala",
#            `harnai-sib` = "harnai-sibi",
#            `kachhi` = "kech",
#            `kallat` = "kalat",
#            `kambar shahdadkot` = "kamber shahdadkot",
#            `kharan-panjgur` = "kharan-washuk-panjgur",
#            ``
#          )) %>%
#   group_by(district, assembly) %>%
#   summarize(n_cons = n())
alld
table(alld$district)
dyn_dist <- haven::read_dta("../../dynasties/Dynastic_2018/data_cleaning_2018/data/Elections_27072018.dta") %>%
  filter(year == 2013, rank == 1) %>%
  mutate(district = tolower(as.character(as_factor(distcode))),
         prov = as.character(as_factor(prov))) %>%
  select(prov, district, constype, consname) %>%
  group_by(prov, district, constype) %>%
  summarize(n_cons = n())

dyn_dist %>%
  group_by(prov, constype) %>%
  summarize(n_cons = sum(n_cons))
# 2013 PAs 130+99+51+297
# 2013 NAs
#61 sindh
#148 punjab
#35 kpk
#2 islamabad
#12 fata
#14 balochistan

# Missing from 2013
# 1 balochistan PA
# 2 FATA NA
# 2 punjab NA
# 4 punjab PA
# 1 sindh NA
# 3 sindh PA
