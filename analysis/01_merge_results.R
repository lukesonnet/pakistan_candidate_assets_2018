# Author: Luke Sonnet
# Date created: 2018-10-09
# Purpose: Clean and merge asset data for analysis

# ----------
# Pre-amble
# ----------
library(tidyverse)
library(fuzzyjoin)
library(readxl)
library(stringdist)
ass <- read_csv("data/final_data/cleaned_asset_data_IK.csv", na = ".") %>%
  mutate(cnic = as.character(cnic_final),
         constituency_code = paste0(type_seat, "-", const_number),
         # fix uid errors!
         uid_final = ifelse(uid_final == "05949" & candidate_name == "Muhammad Muavia", "05941", uid_final),
         uid_final = ifelse(uid_final == "05018" & constituency_code == "PK-63", "05013", uid_final),
         total_prob_bis_cash = im_prop_pak_totes + im_prop_pako_totes + invest_totes + bus_cap_opk_totes + bus_cap_wpk_totes + cash_cost + cash_bank_cost,
         avg_assets = (total_prob_bis_cash + current_fy_net_assets) / 2,
         log_avg_assets = log(avg_assets + 1))
filter(ass, uid_final == "05949")


# Check that NAs were read correctly
table(ass$type_seat, useNA = "a")
# Check car data
table(case_when(ass$motor_cost == 0 ~ "Zero",
                ass$motor_cost < 0 ~ "Missing",
                ass$motor_cost > 0 ~ "Count"),
      useNA = "always")

uids <- read_excel("data/candidate_uids_complete.xls")
res_c <- read_csv("../pakistan_election_results_2018/pk_candidate_data_2018.csv")
res_u <- left_join(res_c, uids, by = c("constituency_code" = "pa_id", "candidate_name" = "candidate_name"))

# ----------
# Prep results data
# ----------

# Creates 26 duplicates
table(table(res_u$uid))
table(duplicated(uids[,1:2])) # yep, 13 you can't distinguish...

table(is.na(res_u$uid))
filter(res_u, is.na(uid))
filter(ass, uid_final == "05941")
setdiff(res_u$uid, ass$uid_final)
setdiff(ass$uid_final, res_u$uid)

# check dupes in res_u_a
res_u_a <- left_join(res_u, ass, by = c("uid" = "uid_final"), suffix = c("", "_asset"))

cons_comp <- res_u_a %>%
  filter(candidate_rank == 1, !is.na(key)) %>%
  select(constituency_code, log_avg_assets)
res_u_a
mdist$constituency_code
m_rank1 <- left_join(mdist, cons_comp)
m_trim <- m_rank1 %>%
  group_by(bin) %>%
  mutate(has_both = var(treated) > 0) %>%
  filter(has_both) %>%
  mutate(trim_seat_quartile = cut(seats, breaks = c(0, 1, 3, 5, 10)),
         bin_all = cut(seat_share, breaks = 0:10),
         ass_dist = paste0(assembly, "_", district),
         trim_seat_group = cut(seats, breaks = c(0, 1, 2, 3, 4, 10)),
         trim_seat_group_median = cut(seats, breaks = c(0, 3, 10)),
         trim_seat_group_tercile = cut(seat_share, breaks = c(0, 3, 5, 10)))

outcomes <- c("log_avg_assets", "MOV_pct", "win_pct", "effective_parties", "turnout")

map(outcomes, ~ {
  m2 <- m_trim %>%
    group_by(bin_all) %>%
    nest()
  
  map(m2$data, ~ {
    print(tidy(
      lm_robust(as.formula(paste0(.x, "~ treated")),
                data = .,
                clusters = ass_dist)
    )[2, ])
  })
    
    # unnest(mod) %>%
    # ggplot(., aes(x = bin_all, y = estimate, ymin = conf.low, ymax = conf.high)) +
    # geom_linerange(size = 1.1) +
    # ggtitle(.x) + 
    # geom_point(size = 2) +
    # theme_bw(base_size = 16)
})





lm_robust(log_avg_assets ~ treated * bin_all, data = m_trim)

m_dist <- merge(m_trim, con)

table(is.na(res_u_a$key), res_u_a$candidate_rank)
res_uid_matched <- filter(res_u_a, !is.na(enum_name)) %>%
  rename(uid.x = uid) %>%
  mutate(uid.y = uid.x)
res_uid_unmatched <- filter(res_u_a, is.na(enum_name))
filter(res_uid_unmatched, constituency_code == "PP-126") %>% select(starts_with("uid"), starts_with("candidate"), 1:8) %>% as.data.frame()
res_uid_unmatched$candidate_name

select(res_u_a, starts_with("candidate_name"))

res_names <- res_uid_unmatched %>%
  mutate(candidate_name_lower = tolower(candidate_name)) %>%
  select(names(res_u), candidate_name_lower) %>%
  left_join(mutate(ass, candidate_name_lower = tolower(candidate_name)),
            by = c("constituency_code", "candidate_name_lower"))

res_name_matched <- filter(res_names, !is.na(enum_name))
res_name_unmatched <- filter(res_names, is.na(enum_name))

res_uid_matched$uid
ass_unmatched <- filter(ass, !(uid_final %in% res_uid_matched$uid.x)) %>%
  mutate(candidate_name_lower = tolower(candidate_name)) %>%
  select(candidate_name_lower, uid_final, constituency_code)


matches <- map(setdiff(res_name_unmatched$constituency_code,
                       gsub("\\.csv", "", list.files("data/manual_fuzzy_match"))),
    ~ {
      print(sprintf("CONSTITUENCY: %s", .x))
      source <- res_name_unmatched$candidate_name_lower[res_name_unmatched$constituency_code == .x]
      target <- ass_unmatched$candidate_name_lower[ass_unmatched$constituency_code == .x]
      print(sort(target))
      if (length(target) == 0 | any(is.na(source))) {
        write_csv(
          data.frame(candidate_name_lower = source, matched_ass_name = NA, constituency_code = .x, stringsAsFactors = FALSE),
          path = sprintf("data/manual_fuzzy_match/%s.csv", .x)
        )
        return(NULL)
      }
      distmat <- stringdistmatrix(target, source)
      matched_chars <- imap_chr(source, ~ {
        drank <- order(distmat[, .y])
        for (i in drank) {
          print(sprintf("Res name: %s", .x))
          print(sprintf("Asset name: %s", target[i]))
          print(sprintf("Score: %d", distmat[i, .y]))
          m <- readline("Match (y/n)?")
          if (m == "y") {
            return(target[i])
          } else if (m == "q") {
            stop("")
          } else if (m == "next") {
            return(NA_character_)
          }
        }
        return(NA_character_)
      })
      write_csv(
        data.frame(candidate_name_lower = source, matched_ass_name = matched_chars, constituency_code = .x, stringsAsFactors = FALSE),
        path = sprintf("data/manual_fuzzy_match/%s.csv", .x)
      )
    }
)

mfiles <- list.files("data/manual_fuzzy_match", full.names = TRUE)
clean_match <- map_dfr(mfiles, read_csv)
names(res_name_unmatched)
res_fuzzy <- res_name_unmatched %>%
  select(setdiff(names(res_u), c("candidate_name", "uid")), starts_with("candidate_name"), uid.x, uid.y) %>%
  left_join(clean_match) %>%
  left_join(mutate(ass, candidate_name_lower = tolower(candidate_name)),
            by = c("matched_ass_name" = "candidate_name_lower", "constituency_code"))
names(res_fuzzy)

table(is.na(ass$uid_final), useNA = "a")

merged_dat <- bind_rows(res_uid_matched, res_name_matched, res_fuzzy)

table(is.na(clean_match$matched_ass_name))
table(merged_dat$enum_name, useNA = "a")
win_dat <- filter(merged_dat, is.na(enum_name), candidate_rank <= 3) %>%
  select(candidate_rank, candidate_name_lower, constituency_code) %>%
  as.data.frame
missing_nas <- setdiff(paste0("NA-", 1:272), filter(ass, type_seat == "NA") %>% {sort(unique(.$constituency_code))})
filter(win_dat, substr(constituency_code, 1, 2) == "NA", !(constituency_code %in% missing_nas))
missing_PPs <- setdiff(paste0("PP-", 1:297), filter(ass, type_seat == "PP") %>% {sort(unique(.$constituency_code))})
filter(win_dat, substr(constituency_code, 1, 2) == "PP", !(constituency_code %in% missing_PPs))
missing_PKs <- setdiff(paste0("PK-", 1:99), filter(ass, type_seat == "PK") %>% {sort(unique(.$constituency_code))})
filter(win_dat, substr(constituency_code, 1, 2) == "PK", !(constituency_code %in% missing_PKs))
missing_PBs <- setdiff(paste0("PB-", 1:52), filter(ass, type_seat == "PB") %>% {sort(unique(.$constituency_code))})
missing_PBs
filter(win_dat, substr(constituency_code, 1, 2) == "PB", !(constituency_code %in% missing_PBs))
missing_PSs <- setdiff(paste0("PS-", 1:130), filter(ass, type_seat == "PS") %>% {sort(unique(.$constituency_code))})
missing_PSs
filter(win_dat, substr(constituency_code, 1, 2) == "PS", !(constituency_code %in% missing_PSs))

table(win_dat$candidate_name)
# manual first place matching
filter(merged_dat, is.na(enum_name), candidate_rank <= 3) %>%
  select(candidate_rank, candidate_name_lower, constituency_code) %>%
  mutate(uid_final = case_when(
    candidate_name_lower == "imran ahmed khan niazi" ~ "01824",
    candidate_name_lower == "mian muhammad shehbaz sharif" ~ "06587",
    TRUE ~ NA_character_
  ),
  uid_alternate = case_when(
    candidate_name_lower == "muhammad ali" & constituency_code == "PK-64" ~ "05013", # 05018
    TRUE ~ NA_character_
  ))

filter(ass, constituency_code == "NA-201") %>% select(candidate_name) %>% as.data.frame
na_names <-  tolower(ass$candidate_name[ass$type_seat=="NA"])

grep("kzai", gsub(" ", "", na_names))
na_names[4257]
na_names[order(stringdist("mr. ali pervaiz", na_names))[1:20]]
na_names[stringdist("ali pervaiz", na_names)]

filter(clean_match, constituency_code == "NA-3")
filter(ass, constituency_code == "NA-243", candidate_name == "Imran Ahmed Khan Niazi") %>% as.data.frame
filter(ass, constituency_code == "PS-103") %>% select(candidate_name, father_name, contact_num, uid_final, educ, enum_name) %>% as.data.frame
filter(merged_dat, uid.y == "05949")
filter(uids, uid == "07632")
res
filter(res_c, candidate_name == "ALI RAZA KHAN")
filter(ass, candidate_name == "Balash Ahmad Junejo") %>% as.data.frame # Son of 201 MNA?
filter(ass, candidate_name == "Mian Muhammad Shehbaz Sharif") %>% as.data.frame
filter(ass, candidate_name == "Ali Pervaiz") %>% as.data.frame
ass$const_number
table(res_fuzzy$uid_final, useNA = "a")
filter(ass, uid_final == "05018") %>% select(starts_with("candidate"), starts_with("uid"), 1:10) %>% as.data.frame()
filter(ass, uid_final == "05013") %>% select(starts_with("candidate"), starts_with("uid"), 1:10) %>% as.data.frame()
filter(ass, uid_final == "05659") %>% select(starts_with("candidate"), starts_with("uid"), 1:10) %>% as.data.frame()
filter(uids, uid == "05659")
filter(uids, uid == "05677")
filter(ass, enum_name == 157) %>% select(1:10, uid_final) %>% as.data.frame
filter(uids, uid == "05693")
ass$enum_other
table(substr(uids$pa_id, 1, 2)[substr(uids$uid, 1, 2) == "05"])
table(ass$type_seat[substr(ass$uid_final, 1, 2) == "05"], ass$const_number[substr(ass$uid_final, 1, 2) == "05"])
table(ass$type_seat[substr(ass$uid_final, 1, 2) == "05"])

# check UID match for wrong provinces
# PS 56 dillip kumar check results (third place)
# PS 91-97 all wrong UIDs, enum_num 156 157


# check unmatched UIDs for incorrect constituency code
res_name_unmatched$uid
.x <- "NA-2"
stringdistmatrix(res_name_unmatched$candidate_name_lower[res_name_unmatched$constituency_code == .x],
           ass_unmatched$candidate_name_lower[ass_unmatched$constituency_code == .x])

.x <- "NA-5"
stringdistmatrix(res_name_unmatched$candidate_name_lower[res_name_unmatched$constituency_code == .x],
           ass_unmatched$candidate_name_lower[ass_unmatched$constituency_code == .x])

ass_unmatched
select(res_name_unmatched, uid.x, candidate_name_lower, candidate_rank, constituency_code)
ass_unmatched

# fuzzy matching
stringdist(res_name_unmatched$candidate_name_lower, res_names)

filter(ass, constituency_code == "PS-99", uid == "-9999") %>%
  select(candidate_name, uid) %>%
  arrange(candidate_name) %>%
  as.data.frame
filter(res_name_unmatched, constituency_code == "PS-99") %>%
  select(constituency_code, candidate_rank, candidate_name_lower) %>%
  arrange(candidate_name_lower) %>%
  as.data.frame

table(res_name_unmatched$constituency_code)

sum(is.na(a$enum_name))
table(is.na(res_u_a$enum_name), res_u_a$candidate_rank, useNA = "a")
filter(a, const_number == 3, type_seat == "NA") %>%
  select(candidate_name, uid)
filter(res_u_a, is.na(res_u_a$enum_name), constituency_code == "NA-3") %>%
  select(constituency_code, candidate_rank, candidate_name.x, candidate_name.y, uid)


# ----------
# Merge results in to asset data
# ----------
