I'll add all the corrections to a new df called "cleaned\_assets". Also,
please note that since I went in a variable by variable manner rather
than grouping by type of cleaning needed (so that I don't miss
anything), there is a lot of extra code than would otherwise be
unecessary to do all this. So bear with me. If I get time later, I can
produce a file from this with much shorter code.

Also, there's a list of stuff still left to do at the end.

In the code, the parts that don't make any changes to the data and are
only as checks have been commented out in the final version.

### Part 1: Cleaning identifying Variables

#### Constituency Codes

    constituency_codes <- paste0(cleaned_assets$type_seat,
                                 "-",
                                 cleaned_assets$const_number)
    cons_ids <- c(paste0("NA-", 1:272),
              paste0("PP-", 1:297),
              paste0("PB-", 1:51),
              paste0("PK-", 1:99),
              paste0("PS-", 1:130))
    setdiff(cons_ids,
            constituency_codes)

    #>  [1] "NA-172" "NA-211" "NA-212" "NA-213" "NA-214" "NA-215" "NA-260"
    #>  [8] "NA-262" "NA-267" "NA-268" "NA-271" "NA-272" "PP-268" "PP-269"
    #> [15] "PB-7"   "PB-10"  "PB-11"  "PB-12"  "PB-17"  "PB-18"  "PB-19" 
    #> [22] "PB-20"  "PB-21"  "PB-22"  "PB-25"  "PB-28"  "PB-29"  "PB-31" 
    #> [29] "PB-32"  "PB-33"  "PB-34"  "PB-35"  "PB-42"  "PB-43"  "PB-44" 
    #> [36] "PB-50"  "PS-27"  "PS-115" "PS-119"

    setdiff(constituency_codes,
            cons_ids)

    #> [1] "PS-207"      "PS-209"      "PS-210"      "PS-211"      "PS-212"     
    #> [6] "PS-213"      "PS-214"      "PS-215"      "Reserved-NA"

    cleaned_assets <- mutate(cleaned_assets,
                             type_seat = ifelse(type_seat == "PS" & const_number > 206,
                                                "NA",
                                                type_seat),
                             constituency_code = paste0(type_seat, "-", const_number))
    setdiff(cleaned_assets$constituency_code,
            cons_ids)

    #> [1] "Reserved-NA"

    # only reserved NA in surplus of the constituency ids
    setdiff(cons_ids,
            cleaned_assets$constituency_code)

    #>  [1] "NA-172" "NA-260" "NA-262" "NA-267" "NA-268" "NA-271" "NA-272"
    #>  [8] "PP-268" "PP-269" "PB-7"   "PB-10"  "PB-11"  "PB-12"  "PB-17" 
    #> [15] "PB-18"  "PB-19"  "PB-20"  "PB-21"  "PB-22"  "PB-25"  "PB-28" 
    #> [22] "PB-29"  "PB-31"  "PB-32"  "PB-33"  "PB-34"  "PB-35"  "PB-42" 
    #> [29] "PB-43"  "PB-44"  "PB-50"  "PS-27"  "PS-115" "PS-119"

    # NA 172, some sindh, and much of balochistan just seem to be missing, according to AT

#### Candidate Names

Here I check if there are any `candidate_name`s that need cleaning and
make minor changes.

    # CLEANING CANDIDATE NAMES

    # 
    # I generate two variables, A. the first will flag names that have any numeric values,
    #                           B. the second will flag names that have ONLY numeric values
    #                           C. the third will flag names that have BOTH numeric and chrs (A&!B)

    # B -> will be converted to NA
    # C -> names will be individually corrected

    cleaned_assets <- cleaned_assets %>%
      mutate(name_has_any_nums=grepl("\\d", candidate_name), 
             name_has_only_nums=check.numeric(candidate_name),
             name_has_chrs_nums=name_has_any_nums&!name_has_only_nums, 
             candidate_name=ifelse(name_has_only_nums, NA, candidate_name),
             candidate_name = recode(
               candidate_name,
               `2Malik Hashim Khan` = "Malik Hashim Khan",
               `Rab Nawaz Taniy0` = "Rab Nawaz Taniyo",
               `Muhammad Qais 4` = "Muhammad Qais",
               `Shah8D Raza` = "Shahid Raza",
               `Mian M7Hammad Asghar` = "Mian Muhammad Asghar",
               `Syed Mu4Taza Mehmood` = "Syed Murtaza Mehmood",
               `Mal8K Muhmmad Ramzan` = "Malik Muhammad Ramzan",
               `S6Eda Farah Azmi` = "Syeda Farah Azmi",
               `Zarida Nasir -7` = "Zarida Nasir"
               )
             ) %>%
      select(-starts_with("name_has_")) #to delete the intermediate variables we made to clean the names

    # We have cleaned the candidate name variable, except we now have 25 candidates for whom candidate name is NA.
    table(is.na(cleaned_assets$candidate_name))

    #> 
    #> FALSE  TRUE 
    #> 18053    25

#### CNICs:

    # The scrutiny forms released by the ECP serve as the basis for which
    # we check the CNICs of candidates in the asset data entry
    # This comes from the other scraping exercise with Colin
    scrutiny_list <- read_csv("data/other_data/ecp_scrutiny_form_cnics.csv") %>%
      mutate(cnic = as.character(candidate_CNIC_ECP))

    #> Parsed with column specification:
    #> cols(
    #>   province = col_character(),
    #>   constituency_number = col_character(),
    #>   candidate_CNIC_ECP = col_double(),
    #>   candidate_name = col_character()
    #> )

    cnics_not_in_scrutiny_corrections <- 
      read_excel("data/rsols_corrections/cnics_not_in_scrutiny_corrections.xlsx",
                 col_types = c("text", "text")) %>%
      rename(cnic_not_in_scrutiny_correction = Correction) %>%
      filter(!duplicated(.)) %>% # some duplicated rows
      group_by(key) %>% # still some duplicated keys
      filter(!(n() > 1 & cnic_not_in_scrutiny_correction %in% c("8888888888888", "0"))) %>%
      filter(!(key == "uuid:7016ff6a-3c4e-4e3a-97db-c026c70533b5" & cnic_not_in_scrutiny_correction == "9999999999999")) %>%
      filter(!(key == "uuid:f6f060ac-3e22-466d-b9e2-d1baaa75a260" & cnic_not_in_scrutiny_correction == "1350356493209"))

    # check one ambiguity
    "1350356423209" %in% scrutiny_list$cnic

    #> [1] TRUE

    "1350356493209" %in% scrutiny_list$cnic

    #> [1] FALSE

    # check for remaining duplicates
    cnics_not_in_scrutiny_corrections %>%
      filter(duplicated(key) | duplicated(key, fromLast = TRUE))

    #> # A tibble: 0 x 2
    #> # Groups:   key [0]
    #> # ... with 2 variables: key <chr>, cnic_not_in_scrutiny_correction <chr>

    cnics_not_in_scrutiny_corrections %>%
      filter(duplicated(cnic_not_in_scrutiny_correction) | duplicated(cnic_not_in_scrutiny_correction, fromLast = TRUE))

    #> # A tibble: 0 x 2
    #> # Groups:   key [0]
    #> # ... with 2 variables: key <chr>, cnic_not_in_scrutiny_correction <chr>

    cleaned_assets <- left_join(cleaned_assets, 
                                cnics_not_in_scrutiny_corrections,
                                by = "key")


    # PART 2: CNIC NUM Length corrections
    cnic_num_length_correct <- read_csv("data/rsols_corrections/cnic_length_num_errors_corrections.csv",
                                        col_types = cols(Correcction = col_character())) %>%
      rename(cnic_num_length_correction = Correcction) %>%
      select(key, cnic_num_length_correction)

    cleaned_assets <- left_join(cleaned_assets,
                                cnic_num_length_correct,
                                by = "key")

    # table(cleaned_assets$cnic_num_length_correction, useNA = "always")

    with(cleaned_assets,
         table(!is.na(cnic_not_in_scrutiny_correction), 
               !is.na(cnic_num_length_correction))
    )

    #>        
    #>         FALSE  TRUE
    #>   FALSE 14714     4
    #>   TRUE   3339    21

    filter(cleaned_assets, !is.na(cnic_not_in_scrutiny_correction), !is.na(cnic_num_length_correction)) %>%
      select(contains("cnic"))

    #> # A tibble: 21 x 3
    #>    cnic         cnic_not_in_scrutiny_correction cnic_num_length_correction
    #>    <chr>        <chr>                           <chr>                     
    #>  1 0            9999999999999                   9999999999999             
    #>  2 888888888888 5340591911591                   5340591911591             
    #>  3 540161898870 1540161899967                   8888888888888             
    #>  4 0            8888888888888                   8888888888888             
    #>  5 0            9999999999999                   1550122356083             
    #>  6 0            9999999999999                   8888888888888             
    #>  7 0            9999999999999                   8888888888888             
    #>  8 310114432201 3102114422201                   3101144322201             
    #>  9 0            9999999999999                   9999999999999             
    #> 10 0            3310090934327                   3310090934327             
    #> # ... with 11 more rows

    cleaned_assets <- cleaned_assets %>%
      mutate(
        cnic_final = case_when(
          !is.na(cnic_num_length_correction) ~ cnic_num_length_correction,
          !is.na(cnic_not_in_scrutiny_correction) ~ cnic_not_in_scrutiny_correction,
          TRUE ~ cnic
        )
      ) %>%
      mutate_at(vars(cnic_final, cnic_num_length_correction, cnic_not_in_scrutiny_correction),
                funs(found = !is.na(.),
                     clean = ifelse(nchar(gsub("[-89]", "", .)) == 0, NA, .)))

    # Check if any of the cnics we added overrode existing CNICs from other data sources
    cleaned_assets %>%
      select(starts_with("cnic")) %>%
      filter(
        (cnic_num_length_correction_found  | cnic_not_in_scrutiny_correction_found),
        (!is.na(cnic_num_length_correction_clean) | !is.na(cnic_not_in_scrutiny_correction_clean)),
        is.na(cnic_final_clean)
      ) %>%
      select(-ends_with("found")) %>%
      as.data.frame

    #>           cnic cnic_not_in_scrutiny_correction cnic_num_length_correction
    #> 1 540161898870                   1540161899967              8888888888888
    #> 2            0                               0              9999999999999
    #> 3            0                               0              9999999999999
    #> 4            0                               0              9999999999999
    #> 5 342200330403                    342200330403              8888888888888
    #> 6 230191111337                    230191111337              9999999999999
    #>      cnic_final cnic_final_clean cnic_num_length_correction_clean
    #> 1 8888888888888             <NA>                             <NA>
    #> 2 9999999999999             <NA>                             <NA>
    #> 3 9999999999999             <NA>                             <NA>
    #> 4 9999999999999             <NA>                             <NA>
    #> 5 8888888888888             <NA>                             <NA>
    #> 6 9999999999999             <NA>                             <NA>
    #>   cnic_not_in_scrutiny_correction_clean
    #> 1                         1540161899967
    #> 2                                     0
    #> 3                                     0
    #> 4                                     0
    #> 5                          342200330403
    #> 6                          230191111337

    # Looks like only one of the right length, and that one isn't in the scrutiny forms, so let's jettison it
    "1540161899967" %in% cleaned_assets$cnic_final_clean

    #> [1] FALSE

    "1540161899967" %in% scrutiny_list$cnic

    #> [1] FALSE

    cleaned_assets <- cleaned_assets %>%
      select(-starts_with("cnic_num"),
             -starts_with("cnic_not_in_scrutiny"),
             -ends_with("_found"),
             -cnic,
             -cnic_final) %>%
      rename(cnic = cnic_final_clean)

    cleaned_assets %>%
      select(contains("cnic"))

    #> # A tibble: 18,078 x 1
    #>    cnic         
    #>    <chr>        
    #>  1 1520180553675
    #>  2 4220184094665
    #>  3 1730123558311
    #>  4 3740504048361
    #>  5 1520214841539
    #>  6 4320360891447
    #>  7 1520161913233
    #>  8 1520261432777
    #>  9 1520174149689
    #> 10 1520156395457
    #> # ... with 18,068 more rows

    #checking
    cleaned_assets%>%
      mutate(
        cnic_length_error=nchar(cnic)!=13,
        cnic_not_in_scrutiny=!(cnic %in% scrutiny_list$cnic)
      )%>%
      {table(.[!is.na(.$cnic),]$cnic_not_in_scrutiny, useNA = "always")}

    #> 
    #> FALSE  TRUE  <NA> 
    #> 14440  2437     0

    cleaned_assets%>%
      mutate(
        cnic_hai=!is.na(cnic)
      )%>%
      {table(.$cnic_hai)}

    #> 
    #> FALSE  TRUE 
    #>  1201 16877

    # check no merges from duplication
    #table(duplicated(cleaned_assets))

TODO: Correct this text Made two types of corrections: num\_length and
scrutiny\_list. The reason of doing the num\_length corrections in part
2 is that if we do the other way around, still left with some
num\_length errors that are overwritten.

The new variable **cnic** is to be treated as the correct cnic version.

There are no length errors in this final version. Despite the not in
scrutiny CNIC back-checking, we find that there are 3591 flagged as not
in scrutiny, and when we filter out the NAs from these 4316, we are left
with **2386** CNICs not found in the scrutiny list. Our *original*
number of flags of not\_in\_scrutiny prior to asking AT for corrections
via back-checking was **4068**. The file they sent us was of **3616**
cnics. This implies that a lot of these flags were because the candidate
was not in the scrutiny list. Your call on whether we should investigate
more or not. At the least it would be good to ask for a breakdown of the
back-checking, asking if they know how many of the CNICs back-checked
needed to be corrected.

Overall, we have CNICs on 16316 candidates. CNIC is NA for 1205 rows ~
7.4 percent of total rows.

#### UIDs

Using the file of corrected uids sent by AT, I create a new variable
that is the final\_uid

    # These are the uids originally assigned to the candidates in the results data
    # scraped by Colin
    uids <- read_excel("data/other_data/candidate_uids_complete.xls")
    # However, I assign them new ids as colin changed the underlying data after the ECP
    # updated their website
    res_c <- read_csv("../pakistan_election_results_2018/pk_candidate_data_2018.csv") %>%
      mutate(id = 1:n())

    #> Parsed with column specification:
    #> cols(
    #>   .default = col_character(),
    #>   election_date = col_date(format = ""),
    #>   constituency_number = col_integer(),
    #>   voter_reg = col_integer(),
    #>   votes_polled = col_integer(),
    #>   turnout = col_double(),
    #>   turnout_calc = col_double(),
    #>   valid_votes = col_integer(),
    #>   valid_calc = col_integer(),
    #>   votes_disq = col_integer(),
    #>   candidate_votes = col_integer(),
    #>   candidate_share = col_double(),
    #>   candidate_rank = col_integer()
    #> )

    #> See spec(...) for full column specifications.

    # This line checks to make sure the ids we have have not shifted
    if (!all.equal(res_c, read_csv("data/other_data/pk_candidate_data_2018_fixed_ids.csv"))) {
      stop("Change in results or results ids")
    }

    #> Parsed with column specification:
    #> cols(
    #>   .default = col_character(),
    #>   election_date = col_date(format = ""),
    #>   constituency_number = col_integer(),
    #>   voter_reg = col_integer(),
    #>   votes_polled = col_integer(),
    #>   turnout = col_double(),
    #>   turnout_calc = col_double(),
    #>   valid_votes = col_integer(),
    #>   valid_calc = col_integer(),
    #>   votes_disq = col_integer(),
    #>   candidate_votes = col_integer(),
    #>   candidate_share = col_double(),
    #>   candidate_rank = col_integer(),
    #>   id = col_integer()
    #> )
    #> See spec(...) for full column specifications.

    res_u <- left_join(res_c,
                       uids,
                       by = c("constituency_code" = "pa_id",
                              "candidate_name" = "candidate_name"))
    nrow(uids)

    #> [1] 11660

    nrow(res_c)

    #> [1] 11697

    sum(uids$uid %in% res_u$uid)

    #> [1] 11608

    nrow(res_u)

    #> [1] 11723

    nrow(res_c)

    #> [1] 11697

    uids %>% filter(pa_id == "NA-46") %>% as.data.frame

    #>    pa_id        candidate_name   uid
    #> 1  NA-46         Abdul Ghaffar 02670
    #> 2  NA-46        Ali Begum Khan 02671
    #> 3  NA-46            Amir Abbas 02672
    #> 4  NA-46            Anayat Ali 02673
    #> 5  NA-46        Anayat Hussain 02674
    #> 6  NA-46            Fazal Rabi 02675
    #> 7  NA-46       Hashmat Hussain 02676
    #> 8  NA-46         Ibrar Hussain 02677
    #> 9  NA-46         Jamil Hussain 02678
    #> 10 NA-46         Jamil Hussain 02679
    #> 11 NA-46            Mansab Ali 02680
    #> 12 NA-46      Mehmood Ali Jaan 02681
    #> 13 NA-46 Muhammad Hussain Turi 02682
    #> 14 NA-46        Muhammad Nasib 02683
    #> 15 NA-46      Muhammad Shehzad 02684
    #> 16 NA-46        Mumtaz Hussain 02685
    #> 17 NA-46           Mushtaq Ali 02686
    #> 18 NA-46    Sajid Hussain Turi 02687
    #> 19 NA-46        Shabir Hussain 02688
    #> 20 NA-46          Sher Hussain 02689
    #> 21 NA-46       Syed Iqbal Mian 02690
    #> 22 NA-46   Syed Irshad Hussain 02691
    #> 23 NA-46       Syed Munir Syed 02692
    #> 24 NA-46           Waqar Ahmed 02693
    #> 25 NA-46  Zahid Hussain Torrie 02694

    res_u %>% filter(constituency_code == "NA-46") %>%
      select(constituency_code, candidate_name, uid, candidate_rank) %>%
      as.data.frame

    #>    constituency_code        candidate_name   uid candidate_rank
    #> 1              NA-46    Sajid Hussain Turi 02687              1
    #> 2              NA-46       Syed Iqbal Mian 02690              2
    #> 3              NA-46   Syed Irshad Hussain 02691              3
    #> 4              NA-46         Ibrar Hussain 02677              4
    #> 5              NA-46 Muhammad Hussain Turi 02682              5
    #> 6              NA-46        Ali Begum Khan 02671              6
    #> 7              NA-46            Fazal Rabi 02675              7
    #> 8              NA-46       Syed Munir Syed 02692              8
    #> 9              NA-46           Waqar Ahmed 02693              9
    #> 10             NA-46  Zahid Hussain Torrie 02694             10
    #> 11             NA-46        Anayat Hussain 02674             11
    #> 12             NA-46         Jamil Hussain 02678             12
    #> 13             NA-46         Jamil Hussain 02679             12
    #> 14             NA-46        Muhammad Nasib 02683             13
    #> 15             NA-46           Mushtaq Ali 02686             14
    #> 16             NA-46            Amir Abbas 02672             15
    #> 17             NA-46      Muhammad Shehzad 02684             15
    #> 18             NA-46         Abdul Ghaffar 02670             16
    #> 19             NA-46          Sher Hussain 02689             17
    #> 20             NA-46        Shabir Hussain 02688             18
    #> 21             NA-46            Anayat Ali 02673             19
    #> 22             NA-46       Hashmat Hussain 02676             20
    #> 23             NA-46            Mansab Ali 02680             21
    #> 24             NA-46        Mumtaz Hussain 02685             22
    #> 25             NA-46      Mehmood Ali Jaan 02681             23
    #> 26             NA-46         Jamil Hussain 02678             24
    #> 27             NA-46         Jamil Hussain 02679             24

    # Duplication is due to matching names within constituency, we should just keep the first and last match as that was the original ordering!
    res_u <- res_u %>%
      group_by(id) %>%
      filter(n() > 1) %>%
      select(id, constituency_code, candidate_name, uid, candidate_rank) %>%
      ungroup() %>%
      mutate(drop = rep(c(FALSE, TRUE, TRUE, FALSE), n() / 4)) %>%
      right_join(res_u) %>%
      filter(!drop | is.na(drop)) %>%
      select(-drop)

    #> Joining, by = c("id", "constituency_code", "candidate_name", "uid", "candidate_rank")

    nrow(res_u)

    #> [1] 11697

    nrow(res_c)

    #> [1] 11697

    rm(uids, res_c)

    dup_uid_correct <- read_csv("data/rsols_corrections/duplicated_uids_corrections.csv") %>%
      select(key, `Updated UID`) %>%
      rename(uid_correction = `Updated UID`) %>%
      mutate(
        uid_chars = nchar(uid_correction),
        uid_correction = str_pad(uid_correction, width = 5, side = "left", pad = "0")
      )

    #> Parsed with column specification:
    #> cols(
    #>   key = col_character(),
    #>   type_seat = col_character(),
    #>   const_number = col_integer(),
    #>   candidate_name = col_character(),
    #>   uid = col_integer(),
    #>   cnic = col_double(),
    #>   `Updated UID` = col_character()
    #> )

    cleaned_assets <- left_join(cleaned_assets, dup_uid_correct, by = "key")

    second_uid_correct <- read_xlsx("data/rsols_corrections/uid_correction_second.xlsx") %>%
      select(key, `Correct UID`) %>%
      rename(uid_correction_second = `Correct UID`) %>%
      mutate(
        uid_chars_second = nchar(uid_correction_second),
        uid_correction_second = str_pad(uid_correction_second, width = 5, side = "left", pad = "0")
      )

    cleaned_assets <- left_join(cleaned_assets, second_uid_correct, by = "key")

Let's explore what those two uid changes look like.

    table(cleaned_assets$uid_correction)

    #> 
    #>       -9999       00035       00041       00174       00255       00362 
    #>         339           1           1           1           1           1 
    #>       00370       00371       00372       00373       00374       00375 
    #>           1           1           1           1           1           1 
    #>       00376       00377       00378       00393       00394       00395 
    #>           1           1           1           1           1           1 
    #>       00396       00397       00399       00400       00401       00402 
    #>           1           1           1           1           1           1 
    #>       00403       00404       00405       00407       00408       00409 
    #>           1           1           1           1           1           1 
    #>       00480       00490       00555       00621       00749       00878 
    #>           1           1           1           1           1           1 
    #>       01037       01041       01089       01132       01146       01563 
    #>           1           1           1           1           1           1 
    #>       01785       01827       01829       01837       01865       01871 
    #>           1           1           1           1           1           1 
    #>       02520       03098       03282       03344       03401       05795 
    #>           1           1           1           1           1           1 
    #>       06066       06221       06348       06349       09954       10799 
    #>           1           1           1           1           1           1 
    #>       10837       10909       10921       10922       10923       10940 
    #>           1           1           1           1           1           1 
    #>       10949       10958       11481       11503 Correct UID 
    #>           1           1           1           1         342

    table(cleaned_assets$uid_correction_second)

    #> 
    #> -9999 00552 00555 00559 00560 00562 00563 00564 00566 00568 00569 00570 
    #>   464     1     1     1     1     1     1     1     1     1     2     1 
    #> 00571 00572 00577 00578 00580 00581 00582 00583 00584 00585 00586 00587 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 00588 00589 00590 00591 00595 00596 00597 00598 02843 02847 02848 02849 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 02850 02851 02852 02854 02855 02856 02858 02860 02861 02871 02873 02874 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 02875 02876 02877 02878 02879 02880 02882 02884 02891 02901 02912 02913 
    #>     1     1     1     1     1     1     1     2     1     1     1     1 
    #> 02914 02915 02916 02917 02918 02919 02920 02921 02922 02924 02925 02926 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 03005 03006 03007 03008 03010 03011 03013 03014 03015 03016 03017 03019 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 03307 03308 03309 03310 03311 03312 03326 03327 03328 03329 03330 03331 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 03332 03333 03334 03335 03340 03341 03342 03343 03344 03345 03346 03347 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 03348 03349 03350 03351 03352 03353 03355 03356 03357 03358 03360 03363 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 03364 05505 05506 05507 05508 05509 05511 05512 06950 06951 06953 06955 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 06957 06960 06961 06963 06965 06966 06967 06968 06969 06970 06971 06972 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 06973 06974 06976 06977 06978 06979 06980 06981 06982 06983 06984 06985 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 06986 06987 06988 06989 06990 06991 06992 06993 06994 06995 06996 06997 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 06998 06999 07000 07001 07002 07003 07004 07006 07007 07009 07010 07011 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 07012 07013 07015 07016 07017 07018 07019 07020 07021 07022 07023 07024 
    #>     2     1     1     1     1     1     1     1     1     1     1     1 
    #> 07025 07027 07029 07030 07032 07033 07034 07038 07039 07042 07043 07044 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 07045 07046 07048 07051 07052 07054 07055 07057 07058 07059 07060 07061 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 07062 07072 07073 07074 07075 07077 07078 07080 07081 07082 07084 07085 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 07187 07188 07189 07190 07191 07192 07193 07431 07432 07433 07434 07435 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 07436 07437 07438 07550 07551 07553 07554 07555 07556 07558 07559 07695 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 07696 07697 07698 07699 07700 07701 08472 08474 08475 08476 08477 08622 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 08623 08624 08625 08626 08753 08754 08755 08756 08757 08758 08759 08761 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 08762 09123 09124 09126 09128 09129 09130 09131 09132 09133 09135 09136 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09137 09138 09139 09140 09141 09142 09143 09144 09145 09146 09147 09148 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09149 09150 09151 09159 09160 09161 09162 09163 09288 09289 09290 09291 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09292 09293 09294 09295 09296 09297 09298 09300 09301 09302 09303 09304 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09306 09308 09309 09310 09312 09313 09315 09316 09317 09318 09507 09510 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09512 09513 09516 09517 09518 09519 09520 09522 09523 09524 09525 09526 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09527 09528 09529 09530 09531 09532 09534 09535 09536 09538 09539 09540 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09541 09542 09543 09544 09545 09546 09547 09548 09549 09550 09551 09553 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09554 09555 09557 09558 09560 09561 09562 09564 09566 09567 09568 09570 
    #>     1     1     2     1     1     1     1     1     1     1     1     1 
    #> 09571 09572 09573 09574 09575 09576 09578 09579 09581 09582 09583 09584 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09585 09586 09588 09590 09591 09592 09593 09594 09595 09596 09597 09602 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09604 09605 09615 09621 09623 09624 09625 09626 09627 09628 09632 09633 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09634 09636 09637 09638 09639 09641 09642 09643 09645 09646 09647 09651 
    #>     1     1     1     1     2     1     1     1     1     1     1     1 
    #> 09652 09653 09654 09656 09657 09658 09659 09660 09661 09662 09663 09664 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 09666 09667 09669 09671 09672 09673 09674 09677 09679 11573 11576 11579 
    #>     1     1     1     1     1     1     1     1     1     1     1     1 
    #> 11585 11586 11587 11588 
    #>     1     1     1     1

    with(cleaned_assets, table(!is.na(uid_correction), !is.na(uid_correction_second)))

    #>        
    #>         FALSE  TRUE
    #>   FALSE 16419   909
    #>   TRUE    731    19

    filter(cleaned_assets, !is.na(uid_correction), !is.na(uid_correction_second)) %>%
      select(contains("uid"), constituency_code)

    #> # A tibble: 19 x 6
    #>    uid   uid_correction uid_chars uid_correction_second uid_chars_second
    #>    <chr> <chr>              <int> <chr>                            <int>
    #>  1 02913 -9999                  5 -9999                                5
    #>  2 02913 Correct UID           11 02913                                5
    #>  3 09304 -9999                  5 -9999                                5
    #>  4 03330 Correct UID           11 03330                                5
    #>  5 03330 -9999                  5 03331                                5
    #>  6 03345 03344                  4 03344                                5
    #>  7 03345 Correct UID           11 03345                                5
    #>  8 05555 00555                  3 00555                                5
    #>  9 00569 Correct UID           11 00569                                5
    #> 10 05583 -9999                  5 00583                                5
    #> 11 00595 Correct UID           11 00595                                5
    #> 12 01247 -9999                  5 -9999                                5
    #> 13 09304 Correct UID           11 09304                                5
    #> 14 09317 Correct UID           11 09317                                5
    #> 15 00593 -9999                  5 -9999                                5
    #> 16 05762 Correct UID           11 -9999                                5
    #> 17 05751 -9999                  5 11579                                5
    #> 18 05758 -9999                  5 11586                                5
    #> 19 05759 Correct UID           11 11587                                5
    #> # ... with 1 more variable: constituency_code <chr>

    filter(cleaned_assets, uid_correction == "-9999") %>%
      select(contains("uid"), constituency_code)

    #> # A tibble: 339 x 6
    #>    uid   uid_correction uid_chars uid_correction_second uid_chars_second
    #>    <chr> <chr>              <int> <chr>                            <int>
    #>  1 02913 -9999                  5 -9999                                5
    #>  2 03088 -9999                  5 <NA>                                NA
    #>  3 03184 -9999                  5 <NA>                                NA
    #>  4 09304 -9999                  5 -9999                                5
    #>  5 03330 -9999                  5 03331                                5
    #>  6 03412 -9999                  5 <NA>                                NA
    #>  7 03415 -9999                  5 <NA>                                NA
    #>  8 06929 -9999                  5 <NA>                                NA
    #>  9 01369 -9999                  5 <NA>                                NA
    #> 10 00593 -9999                  5 <NA>                                NA
    #> # ... with 329 more rows, and 1 more variable: constituency_code <chr>

    filter(res_u, constituency_code == "PS-96") %>%
      select(uid, province)

    #> # A tibble: 15 x 2
    #>    uid   province
    #>    <chr> <chr>   
    #>  1 11578 Sindh   
    #>  2 11579 Sindh   
    #>  3 11586 Sindh   
    #>  4 11585 Sindh   
    #>  5 11588 Sindh   
    #>  6 11577 Sindh   
    #>  7 11583 Sindh   
    #>  8 11590 Sindh   
    #>  9 11589 Sindh   
    #> 10 11587 Sindh   
    #> 11 11580 Sindh   
    #> 12 11582 Sindh   
    #> 13 11581 Sindh   
    #> 14 11576 Sindh   
    #> 15 11584 Sindh

    # seems we should always prefer uid_correction_second

Now let's merge them in. Now we have to deal with many duplicate UIDs,
when there should really be none.

    cleaned_assets <- cleaned_assets %>%
      mutate(uid = ifelse(uid %in% c("99999", "-999"), "-9999", uid)) %>%
      rename(uid_initial = uid) %>%
      mutate(uid = case_when(
        !is.na(uid_correction_second) ~ uid_correction_second,
        is.na(uid_correction) | uid_correction == "Correct UID" ~ as.character(uid_initial),
        !is.na(uid_correction) ~ as.character(uid_correction),
        TRUE ~ as.character(uid_initial)
      )) %>%
      select(-uid_correction, -uid_correction_second)
    res_u$found_initially <- res_u$uid %in% cleaned_assets$uid

    cleaned_assets_full <- cleaned_assets

Exploring duplicates.

    # check for duplicate uids and mark them needing to be resolved

    cleaned_assets <- cleaned_assets %>%
      mutate(constituency_code = ifelse(key == "uuid:b911ce48-85cf-4270-ab09-190f641ed2fe",
                                        "NA-35",
                                        constituency_code),
             uid = case_when(
               uid == "00041" & constituency_code == "PP-246" ~ "99999",
               uid %in% c(
                   "01868", "01869", "01870", "01835", "01836", "01826", "01828",
                   "01698", "01699", "01792", "01791", "01790", "01789", "01788",
                   "01700", "01781", "01782", "01783", "01784", "01794", "01795",
                   "01796", "01797", "01798", "01799", "01800", "01801", "01805", "01806",
                   "01807", "01808", paste0("018", 10:21), "01840", "01842", "01843", "01844", "01845",
                   paste0("018", 48:57), paste0("018", 59:64), "01744", "01832", "01833",
                   paste0("018", 22:24)
                 ) ~
                 as.character(as.numeric(uid) - 1),
               uid == "02324" ~ "02335",
               uid == "01743" & candidate_name == "Bilal Hussain" ~ "01744",
               uid == "01871" & candidate_name == "Sameena Huma Meer" ~ "01870",
               uid == "01837" & candidate_name == "Amjad Ali Asif" ~ "01836",
               uid == "01827" & candidate_name == "M Kamran Khan" ~ "01826",
               uid == "01829" & candidate_name == "M Usman Razi Khan" ~ "01828",
               uid == "04048" ~ "04034",
               uid == "21035" & candidate_name == "Muneeb Ahmad" ~ "10352",
               uid == "04659" & candidate_name == "Shoukat Zareen" ~ "04660",
               uid == "10762" & candidate_name == "Mir Jawad Afsar" ~ "10802",
               key == "uuid:416eeaaa-bcf1-4ff7-87d0-2ff66068fa40" ~ "10821",
               key == "uuid:e34eeb2d-f1e7-4af1-aa73-ac5fea030acc" ~ "08773",
               key == "uuid:2881c391-c69a-4b19-9dba-bd16aba2d308" ~ "10891",
               key == "uuid:d5ba44ed-f815-46b6-97cc-4a97e1d693c1" ~ "10968",
               key == "uuid:631fb218-c618-406d-aa8b-bae4087de033" ~ "11125",
               key == "uuid:1509903d-d088-4ff6-9f7c-1677ab731dba" ~ "11509",
               key == "uuid:a9830635-b368-4563-a88b-2f1e257907d8" ~ "11533",
               key == "uuid:5ae33a35-ad79-4eca-8c25-3f48b6ebb8f9" ~ "11540",
               key == "uuid:3981c2eb-5f47-4a96-99fa-0c2cd4f5a758" ~ "11544",
               key == "uuid:d716f876-fc71-484a-9b04-bd113dfb9b21" ~ "11554",
               key == "uuid:cd055a6e-5285-4ae8-bbf2-f235dc13033f" ~ "11655",
               key == "uuid:f55db341-7893-47e7-99bd-84a41012667a" ~ "05013",
               key == "uuid:7f3de0be-44c0-4316-a13c-26835672aeb6" ~ "05531",
               key == "uuid:8a795fb4-f2e4-4122-b4d9-c69a82f93e34" ~ "05608",
               key == "uuid:b535e156-9797-4d5f-a420-c27254992b9b" ~ "05941",
               key == "uuid:f3ab974a-5780-4e27-ae1c-24f5fdc33b5a" ~ "10007",
               key == "uuid:43e7522a-2e20-48ae-8ed1-93b09c95a171" ~ "06318",
               key == "uuid:d4810176-c5de-4e93-985d-04bc75ecc6fc" ~ "06416",
               key == "uuid:59919191-14fe-4f18-8420-534dca08887f" ~ "06864",
               key == "uuid:f324490d-490d-4a5f-bb88-116a1b0a1971" ~ "07854",
               key == "uuid:5dc7c351-9fd1-4663-83c3-636d5c15ab9b" ~ "08081",
               key == "uuid:33239735-7f89-42aa-90e5-b04e6f24e4b1" ~ "08117",
               key == "uuid:c01e1c53-8ac8-4ca3-96af-b733c8667970" ~ "08391",
               key == "uuid:de14e6d5-120b-4d5c-b53d-9402b977f98e" ~ "08392",
               key == "uuid:30ea1a9c-13ad-44ee-99f8-e00b6b51cb4c" ~ "11487",
               key == "uuid:433e4259-48cc-43f6-9631-78dbaff4444e" ~ "11493",
               key == "uuid:8574eba7-1840-4d5d-852c-03eaf32ed02d" ~ "02869",
               key == "uuid:b19df50a-a90e-437d-bd4c-3262b217ce01" ~ "02940",
               key == "uuid:faee6e1f-88fc-4321-8f89-8ce57e484280" ~ "03388",
               key %in% c(
                 "uuid:9eb4da4a-2e0c-488c-b44c-e2fe822a148d",
                 "uuid:0dacdbea-61b0-406b-affc-53760f2770bc",
                 "uuid:c45b4eb8-df0d-43fc-9ce7-9fe2dee533fd",
                 "uuid:6c0d977f-7e2b-4cbb-ac9c-8e79516c0f17",
                 "uuid:3032acd3-304b-4164-9a7e-d4c395771c7f"
               ) ~ "-9999",
               TRUE ~ uid
              ),
             uid = str_pad(uid, width = 5, side = "left", pad = "0"),
             constituency_code = case_when(
               key == "uuid:09bd51bc-8a64-41a3-bfee-77d54c0a5c50" ~ "PS-25",
               key == "uuid:de3f7645-7b5e-4f26-a1b5-8bc810578b8c" ~ "NA-218",
               key == "uuid:009be942-a075-4141-a33c-c87466c15a4f" ~ "PS-64",
               key == "uuid:739588bf-b767-497e-bcad-9a670292f779" ~ "PB-24",
               key == "uuid:e8e5444d-bfd5-4246-9724-a3aa3a5ee4dc" ~ "PS-97",
               key == "uuid:8a795fb4-f2e4-4122-b4d9-c69a82f93e34" ~ "PP-105",
               key == "uuid:3a2653e3-2575-4e31-b6df-29752d74df66" ~ "PP-243",
               TRUE ~ constituency_code
             )) %>%
      # drop some duplicates!
      filter(!(key %in% c(
        "uuid:f4c076be-4ed9-44e3-a3e2-afdabebb3439",
        "uuid:20d90571-fd3c-4399-ac4c-71e3ebf736de",
        "uuid:33f6b55b-aa75-4a55-9e95-265b717390a1"
      ))) %>%
      group_by(uid) %>%
      mutate(duplicate_uid = n() > 1 & !is.na(uid) & uid != "-9999") %>%
      ungroup() 

    # check for any duplicate uids
    dup_uids <- cleaned_assets %>%
      group_by(uid) %>%
      mutate(n = n()) %>%
      filter(duplicate_uid & !any(uid_chars[!is.na(uid_chars)] < 5)) %>%
      arrange(uid) %>%
      select(key, enum_name, uid, candidate_name, constituency_code, n, entry) %>%
      mutate(new_and_old = !all(entry == "old") & n > 1,
             only_one_old_some_new = new_and_old & sum(entry=="old") == 1 & length(unique(constituency_code)) == 1)

    # Many are the same person, multiple entries? (NB: even the same person in different constituencies gets a new UID because of how I built them just on the results data)
    dup_uids %>%
      filter(length(unique(constituency_code)) > 1 | length(unique(candidate_name)) > 1) %>%
      as.data.frame

    #>                                           key enum_name   uid
    #> 1   uuid:b9733e62-8d51-4c3d-9895-67ff7c2fb1fa       101 00101
    #> 2   uuid:f2f9b235-190a-44cb-8aa5-b01934b0ffdc       150 00101
    #> 3   uuid:196ce44a-b3cc-43de-946f-ab51149563f7       127 00250
    #> 4   uuid:44cf31bf-cfbc-4eb1-b56b-eeafa0613153       127 00250
    #> 5   uuid:28a19680-6c94-4887-8df0-9930174456a8       104 00256
    #> 6   uuid:4aa68e97-075a-45a4-8b07-9d14242b706e       149 00256
    #> 7   uuid:f0b48232-e73b-48f2-99ff-5ba42baadbeb       127 00279
    #> 8   uuid:127fc669-c22e-4124-95c8-89b2c54c3f4a       156 00279
    #> 9   uuid:7db14f46-d7c8-47b4-b19e-0177c9a9dea6       137 00494
    #> 10  uuid:a98a44c0-c13e-469d-b1b2-848e406764f0       137 00494
    #> 11  uuid:beeaab01-ac84-4b66-9681-dab97cf3fb6b       137 00495
    #> 12  uuid:7063096d-0eb5-4fd7-977e-d0a8f3dcb62f       137 00495
    #> 13  uuid:b4cacdd0-19df-48f1-ac87-c3e338bce9ff       137 00499
    #> 14  uuid:4cd895a5-eff3-487f-ab26-73127e4ef681       137 00499
    #> 15  uuid:062547f1-5bb9-45b4-942f-6b426df40f7a       131 00569
    #> 16  uuid:b6cdead7-1fb8-4413-9513-c36e7f282316       131 00569
    #> 17  uuid:a673b3e6-d120-4fa8-8be1-859de6df4bab       128 00791
    #> 18  uuid:85742a64-3af7-46cc-aeac-000969c6328f       128 00791
    #> 19  uuid:764b53e1-4604-426f-b0b5-c0aa889f94e6       105 01141
    #> 20  uuid:ca7e9822-d75c-4b2e-8534-05168e329781       105 01141
    #> 21  uuid:0ec8e338-1e55-4456-ad6d-18a8df23ef8e       105 01141
    #> 22  uuid:399065a6-69b4-4551-ae09-b7a0b6962a61       105 01149
    #> 23  uuid:144bd870-6bfe-4a6a-8d3f-fab9dc30fed9       105 01149
    #> 24  uuid:eb6f316f-0994-4525-bd81-2a6bc4eaa40c       134 01340
    #> 25  uuid:c600a48b-85b3-4349-bdcc-e2d510c99efd       123 01340
    #> 26  uuid:fe691388-adff-4471-bf48-7e79d3805e85       134 01341
    #> 27  uuid:5502ffee-4386-4c5e-858c-f43b6a0dd114       123 01341
    #> 28  uuid:a26d194d-64eb-4226-b847-d46c69ca4f39       134 01342
    #> 29  uuid:8ad065f8-f383-483b-a0f4-d469e05043f1       123 01342
    #> 30  uuid:fe236480-94b0-4c71-ad1c-9b2862e176e7       134 01343
    #> 31  uuid:ee7d77a3-00ab-4337-b494-384d78b07cf6       123 01343
    #> 32  uuid:97d81fbd-11a6-460e-91c0-c565947bf299       134 01344
    #> 33  uuid:0299c40e-2843-47a1-bd7a-64037d32f018       123 01344
    #> 34  uuid:ce8139da-ffe3-4f05-a012-7f517cb0fef4       134 01345
    #> 35  uuid:2b68052c-8a2a-45be-a444-08ce3ceb7e3d       123 01345
    #> 36  uuid:c99cd433-0ab4-4ffc-b682-12d8af519a24       134 01346
    #> 37  uuid:a1a86f78-c3a2-44b8-a339-8622228ec404       123 01346
    #> 38  uuid:35927d4f-99de-4180-a505-351e457eda3d       134 01348
    #> 39  uuid:a0df9753-7ecc-43b2-bab5-82de74238514       123 01348
    #> 40  uuid:dfad3e5b-3a74-4763-9c0c-c653a4ebc5e6       134 01349
    #> 41  uuid:1c10e732-cbb4-42f8-95f7-82c3b0ccf4f3       123 01349
    #> 42  uuid:41de4d0f-63b1-4238-a336-c0011be4a852       134 01350
    #> 43  uuid:4156ecb1-ecc3-4659-b74c-04ee0f878d81       123 01350
    #> 44  uuid:041215e9-6bd6-4be1-85b6-02c6f43b85c7       134 01351
    #> 45  uuid:676ca4dd-de33-42d6-9133-ac901fb05a4b       123 01351
    #> 46  uuid:5d705b24-fd8b-4f60-af0b-3e796ded11a4       134 01352
    #> 47  uuid:fd13c1db-434b-442b-9a58-8ec1b5d9a26f       123 01352
    #> 48  uuid:7748d7cc-c0b6-43f1-9974-31430d949657       134 01353
    #> 49  uuid:4cdb0a7a-b464-49d3-bb76-e0d8983c5fae       123 01353
    #> 50  uuid:ab1e7472-16b1-44bf-8dad-28691596f799       134 01354
    #> 51  uuid:b291bd59-4a80-40a2-b09c-644107e54eb5       123 01354
    #> 52  uuid:1db18cce-a0df-46fa-8885-85ff4d0b9f9b       134 01357
    #> 53  uuid:73145ec7-136f-44db-a0f7-f2293b79e735       123 01357
    #> 54  uuid:41bef2c9-61d8-46c8-ace8-a9ba3fef7c71       134 01359
    #> 55  uuid:01e4a656-016d-4b87-87e6-127471dd5e15       123 01359
    #> 56  uuid:0ac83197-7a59-475f-96b8-6e5a24639550       134 01360
    #> 57  uuid:5e605882-9ae7-4dc1-a307-e23a441c1b31       123 01360
    #> 58  uuid:cb11d5db-3545-46eb-9678-897b46f71194       134 01361
    #> 59  uuid:edf71f14-4e95-420f-a12c-cfdea12aa9dd       123 01361
    #> 60  uuid:cec5dc02-f05d-4cad-b17e-6dbedb0a4048       134 01362
    #> 61  uuid:5bb8b648-b70b-4c90-8f87-8bb82c159ce1       123 01362
    #> 62  uuid:e46b00e3-6a94-4c36-b619-759f8e0423f1       134 01363
    #> 63  uuid:853b606f-a68b-4c78-991c-6b132a019148       123 01363
    #> 64  uuid:6d7b08ed-7ebb-47b5-ab22-ebbf92fe4099       134 01364
    #> 65  uuid:4463dec8-45b8-4239-87f3-76b8d9299e89       123 01364
    #> 66  uuid:c91474e1-68f1-4a19-b2ad-ddb18ee57b94       134 01375
    #> 67  uuid:ba5f497e-ae0b-445a-a8f0-6c6a9be24c99       123 01375
    #> 68  uuid:1f6fe5eb-7ba2-4f45-bc9d-519f18b7e421       134 01376
    #> 69  uuid:cad58e21-292c-4acc-9c66-fb3b2eff6e7e       123 01376
    #> 70  uuid:e8bb7842-0db8-4558-b436-788f76b85ce2       134 01377
    #> 71  uuid:86c7a74a-c3ea-4577-ad1b-437a04312353       123 01377
    #> 72  uuid:ca791835-cdb5-48c7-9898-226be58c5a05       134 01378
    #> 73  uuid:b438a71d-7cde-4c0d-8c2f-f107f7b5c25b       123 01378
    #> 74  uuid:fd0eaa5a-c971-4b20-9a22-d0ee9b30f784       134 01379
    #> 75  uuid:048b4295-d657-40a3-bf46-89e7ca4c4f9c       123 01379
    #> 76  uuid:0f2b102d-dbec-4da1-b035-ce86b3aeb04a       134 01380
    #> 77  uuid:f572810a-6604-4c6a-be1f-72ebbd3a24d9       123 01380
    #> 78  uuid:4f43f794-723c-41fa-9d52-a5a9ccc464bd       134 01381
    #> 79  uuid:0ddce0d2-a86d-41a1-b1a8-60e405b6035d       123 01381
    #> 80  uuid:a9b715e2-ed84-40a2-bdd9-8f5bb8d78fa1       134 01382
    #> 81  uuid:154d08aa-81a3-49ef-95d6-a1b97315f2d1       123 01382
    #> 82  uuid:5e620189-6426-4ab0-8c2f-3c194f3cb4cb       122 01493
    #> 83  uuid:bcfabb33-ecde-44b3-bbd0-cb55cc825355       150 01493
    #> 84  uuid:57e7a043-12b1-4ea9-b8bf-84217ee8c949       104 01519
    #> 85  uuid:58349bf1-37fa-4e2e-bc17-c22b99f8e723       149 01519
    #> 86  uuid:4265e1ea-e501-4f16-90b0-1bd70e1943b4       104 01522
    #> 87  uuid:73217620-ce26-45f1-8726-6ac1e86aa5f5       149 01522
    #> 88  uuid:99b3276f-495a-4fbc-9978-9c516af6fba7       104 01537
    #> 89  uuid:19bcfda9-4af2-490a-9656-7d143c5315cf       104 01537
    #> 90  uuid:9e7d4937-f2d0-4db3-bf0d-941f4e09f2fd       117 01543
    #> 91  uuid:69a992aa-1ce6-41d0-ad7a-6ac78a09dedf       117 01543
    #> 92  uuid:b0100727-1afd-491f-9771-b4739a396bc2       117 01546
    #> 93  uuid:52c689bd-2727-4b97-94fe-97d4390f7448       150 01546
    #> 94  uuid:2d306a5e-570b-4e96-a238-8bb81d9a35ff       117 01557
    #> 95  uuid:fc63e3d0-cb46-4475-835f-b02e41267f96       149 01557
    #> 96  uuid:03150d96-1796-4f65-bedd-0a26e93d50dd       117 01567
    #> 97  uuid:125aea83-07f2-4386-88da-3f67ea78e459       149 01567
    #> 98  uuid:5be24a03-04cb-4e11-b14b-8c504abd5b99       117 01570
    #> 99  uuid:67d7d8b7-81db-4c5b-bda9-e5ec88e782cf       149 01570
    #> 100 uuid:2e1b66a3-fe1e-4ccf-a46e-70ee464afde7       128 02184
    #> 101 uuid:36c73832-c0e1-4365-b205-506a30e6995f       128 02184
    #> 102 uuid:760d7200-e517-4575-bc8d-161c4af261b6       134 02371
    #> 103 uuid:188575a3-abfa-4d68-bd0a-47642f80e8b3       113 02371
    #> 104 uuid:f7138ff3-2593-4e52-b05a-62a1f3820e51       134 02372
    #> 105 uuid:3a403cc3-89d0-4650-9b2a-50e707284a97       113 02372
    #> 106 uuid:cbef5b5b-e35b-4c07-87c4-8f91bd8665fa       134 02373
    #> 107 uuid:97d8e01a-ba01-4393-af53-c346ca16fb2d       113 02373
    #> 108 uuid:5eee4fa7-c9b0-473f-a789-9ddf8b09bc32       134 02374
    #> 109 uuid:9e9264d2-99b0-4e07-9665-9a1bf8e3c248       113 02374
    #> 110 uuid:0593ae23-f9e5-482f-9bd8-4452b80242b1       134 02384
    #> 111 uuid:bc179778-13fc-4975-a782-1c33c2e1cf67       113 02384
    #> 112 uuid:e0f014ed-5a6d-4e3e-ab9b-cd9130d19914       134 02385
    #> 113 uuid:40a6017d-8316-4f1e-b6da-6e4d2d7553e8       113 02385
    #> 114 uuid:c9cc3860-2a9d-4857-a7e0-f5dbba62f612       134 02386
    #> 115 uuid:854db0ba-1aac-4cc3-a7a0-86273159f11f       113 02386
    #> 116 uuid:0669755d-4f41-40c7-995e-153db4f8354a       134 02387
    #> 117 uuid:dce4d6de-3582-4b90-bc5a-a36352427bdf       113 02387
    #> 118 uuid:a59ac673-581c-4de3-ae38-7cd6308954b9       134 02388
    #> 119 uuid:ba74c518-81a9-4c11-b861-0eda1547ad43       113 02388
    #> 120 uuid:7457d5e3-083f-43d3-8faf-a3a0e02730cb       134 02389
    #> 121 uuid:1baca951-a248-45c1-85da-efc960ebf4ce       113 02389
    #> 122 uuid:b8990d47-4519-4574-a58c-69b2945f146e       134 02390
    #> 123 uuid:9187c685-a690-4d47-a1a4-0a17de436c15       113 02390
    #> 124 uuid:79c7f4eb-0c50-4b93-91df-01b45873f506       134 02391
    #> 125 uuid:2fb36ec5-b576-4a54-967a-689bbaec5db9       113 02391
    #> 126 uuid:45aa4942-3806-442d-988d-b28ce39ca5bc       134 02392
    #> 127 uuid:cc48185c-827b-4768-8740-7d2d05c88183       113 02392
    #> 128 uuid:de499b28-168b-417f-93b3-827bcbe0fb4b       134 02393
    #> 129 uuid:80551a96-31cb-48f8-82b6-b20fd5c6f161       113 02393
    #> 130 uuid:71789030-9cfc-4967-bd7f-4009f8219b82       134 02394
    #> 131 uuid:8dbfc0c4-e93c-47a9-ab80-e930c171a0cd       113 02394
    #> 132 uuid:ccd695e6-7475-499d-a76d-5910c1d100e9       134 02395
    #> 133 uuid:f396ee81-f824-4f52-a8ac-7504cf336061       113 02395
    #> 134 uuid:ee9a6273-e646-48d4-bcbc-e744abf7af8e       134 02396
    #> 135 uuid:3926fe98-38ca-464e-8a36-024e8631e63a       113 02396
    #> 136 uuid:e8036cbe-23a6-4c1a-b69c-de9094246d5d       134 02397
    #> 137 uuid:5282054f-761c-44f7-9488-f2d869e542e6       113 02397
    #> 138 uuid:886970c1-4c4d-4b3c-9a36-d4303f4cce08       134 02398
    #> 139 uuid:b0acf07c-c988-4ab7-9f67-acf6caa675f5       113 02398
    #> 140 uuid:36a7a262-b1d9-4dc9-a0e6-36a2f434d4c1       134 02399
    #> 141 uuid:ae017b2a-8801-43cb-9c45-d91c20c69bae       113 02399
    #> 142 uuid:7016ff6a-3c4e-4e3a-97db-c026c70533b5       134 02400
    #> 143 uuid:c4d7954b-f12c-4344-bf55-a4b6d1e998a7       113 02400
    #> 144 uuid:a870848c-1e4e-4639-b6c8-bcb68a8dfadc       134 02401
    #> 145 uuid:4f5990ab-3b71-4138-93fd-bbd344f34f95       113 02401
    #> 146 uuid:470c2e39-6658-4671-9f90-d8c7111284b3       134 02402
    #> 147 uuid:67fde7fc-c584-4622-b0e3-bd27c8d711bb       113 02402
    #> 148 uuid:529cac19-1fe4-4fca-81e2-fbaa7af3419c       134 02403
    #> 149 uuid:b2c52ba4-7a93-43b0-ac51-49d49670a657       113 02403
    #> 150 uuid:ebd88759-ed10-40d8-b6b9-b08345b587f7       134 02404
    #> 151 uuid:c659f04c-f0ba-47f8-82c5-36fa8d53b2e5       113 02404
    #> 152 uuid:a64846a7-6f48-4b5e-869a-8c6e3bd9863d       134 02405
    #> 153 uuid:6695b724-94f9-40de-ac70-4bf375453719       113 02405
    #> 154 uuid:49a1cfc3-6958-4a23-b8e7-54922206adfc       134 02406
    #> 155 uuid:78932369-353e-4790-84fb-c49cdc9876d5       113 02406
    #> 156 uuid:a2810a37-ece9-4097-9f5f-0d78270bc736       134 02407
    #> 157 uuid:b7467d2f-fc09-4a68-89bf-98b5bbca5df9       113 02407
    #> 158 uuid:b77029e1-0a33-4332-bcb8-609a65c51dca       134 02408
    #> 159 uuid:af8fcdca-2bd1-4a43-8af7-23f4c3f4665c       113 02408
    #> 160 uuid:e3b8d811-df42-4f2c-bc69-5200856246a1       113 02408
    #> 161 uuid:7cce7baf-a049-4a5c-924a-ff3f16bb7d3d       134 02409
    #> 162 uuid:97c785b7-f3c8-49c9-99cd-4ab58c19baab       113 02409
    #> 163 uuid:481c7a8b-5cf8-4225-a609-2f97b6d05256       134 02410
    #> 164 uuid:56d6d138-a887-402f-9aaf-4e6ef5920eb2       113 02410
    #> 165 uuid:c95548d1-d270-4453-a79f-c0c2b8bcd9de       134 02412
    #> 166 uuid:652e0195-2144-45e9-bd46-7e870e8d23df       113 02412
    #> 167 uuid:2d9fd112-d5f2-4970-8110-2f8d03ef04d0       134 02413
    #> 168 uuid:88837605-3ffa-48d0-abf7-9b177b624f51       113 02413
    #> 169 uuid:9bb236cf-da05-492f-bf98-572fc076c16c       134 02414
    #> 170 uuid:4cd502a2-1082-4b81-acc9-b1e16121693c       113 02414
    #> 171 uuid:52e9d99c-072b-4d85-96f0-3e2d08dfd552       134 02415
    #> 172 uuid:44413641-19ef-464d-8cb6-baf03336c988       113 02415
    #> 173 uuid:79322d8c-9897-4a5d-841b-f7b00d83d0c6       134 02416
    #> 174 uuid:b1e64933-9cc1-45be-b534-44d1c801c00a       113 02416
    #> 175 uuid:8d821b4f-3305-4869-a031-1500babbc1ee       134 02417
    #> 176 uuid:9b470886-af68-4574-b46c-6be20d306846       113 02417
    #> 177 uuid:b7b8728a-638e-481e-a598-0f73c0a4beb2       134 02418
    #> 178 uuid:1c1cd6bc-ab05-4054-90b7-03b3e663bbfa       113 02418
    #> 179 uuid:ddea0c76-56f5-4886-a753-e94e78e025e1       134 02419
    #> 180 uuid:c91b38ca-bde0-444f-8263-cdea72bcbf63       113 02419
    #> 181 uuid:b0386ef2-2615-4f3c-bf2e-1d0a859c5503       134 02420
    #> 182 uuid:f275bd94-5f5d-48c0-9c58-ea50ce34be53       113 02420
    #> 183 uuid:ac0556ca-c0db-47b8-808d-491c2ecc583a       134 02421
    #> 184 uuid:0df0e70b-f19c-44aa-90f3-1746901df35e       113 02421
    #> 185 uuid:93fa40c5-5af5-4c57-8926-9537f93014ee       134 02422
    #> 186 uuid:460a5cfe-2025-4614-b3fd-61078b3942d9       113 02422
    #> 187 uuid:9925c10a-21f3-49c7-a7ac-3774d97314e3       134 02423
    #> 188 uuid:82706e5a-6bd3-401b-859a-664d99f39df4       113 02423
    #> 189 uuid:575df99c-d21e-4169-afe1-82ebce7020d7       134 02424
    #> 190 uuid:83e38751-8bb4-4b49-8ad7-9ddcb956d1bc       113 02424
    #> 191 uuid:968b98ce-e95a-47d7-86cc-a39a119200bf       134 02425
    #> 192 uuid:732effb3-8554-4d5f-9711-61ddf51768d0       113 02425
    #> 193 uuid:870c02cc-2bf6-4695-8ec6-2966e39ec019       134 02426
    #> 194 uuid:c57d8598-90a3-4f13-bb97-381d224af62e       113 02426
    #> 195 uuid:d46f85d0-b8d2-4d38-849e-04009469b072       134 02427
    #> 196 uuid:2bd4e4a7-b6fe-4232-8dd9-28f22c3c316a       113 02427
    #> 197 uuid:1bde8df3-b279-4d61-8893-997a7085fede       134 02428
    #> 198 uuid:93898f85-795b-46e4-a21b-31414e271ba2       113 02428
    #> 199 uuid:c0bcf39a-1234-40b6-8075-c4833d899e17       134 02429
    #> 200 uuid:952e45be-05e7-4b65-8893-b00bef2905f2       113 02429
    #> 201 uuid:e47042b1-7629-4c69-aa1f-d10dc87f1cd1       134 02430
    #> 202 uuid:a61d39b9-97e5-4984-97d6-cdb043ab9bc0       113 02430
    #> 203 uuid:13f07d9e-0c5a-48e9-8180-f7c3632a750b       134 02431
    #> 204 uuid:08746336-4e2c-41ea-aa3c-6d85423bcbb2       113 02431
    #> 205 uuid:473a2d91-9fe4-41a2-96b1-9122391b3679       134 02432
    #> 206 uuid:92e51e3f-ebb0-4822-b8ba-c8ec87a5c316       113 02432
    #> 207 uuid:f8c9ae7b-5427-4044-bde6-f12591b7bb0c       134 02433
    #> 208 uuid:74134ab0-c4c3-457b-8167-7dd3ecd9fd4f       113 02433
    #> 209 uuid:228ad853-745e-416d-841f-55a8fe33ec12       134 02434
    #> 210 uuid:4698fb13-c059-493f-840a-2ece9087ee07       113 02434
    #> 211 uuid:155c510d-565a-431f-bbd7-e3b03891f416       134 02435
    #> 212 uuid:8f4343c0-4786-4b6e-8853-14a6bc4a5ca8       113 02435
    #> 213 uuid:d3d1c1f5-f0a7-45aa-a32a-e2d2e38ec3cf       134 02436
    #> 214 uuid:d52e05aa-f3b3-440f-b57a-640eb4e67704       113 02436
    #> 215 uuid:fe54c03f-a03b-4d4b-b8f6-6b1b5bdd8512       134 02437
    #> 216 uuid:c98e7110-78c9-4a98-891c-34062ff11024       113 02437
    #> 217 uuid:b8071c31-69d0-477a-8c8e-83ec3a5e8355       134 02438
    #> 218 uuid:cf9291ce-2ca6-49d3-aef6-380f56021dcc       113 02438
    #> 219 uuid:58c1f4e2-eca0-45d4-9fef-2a0e8830e879       134 02440
    #> 220 uuid:e408fd35-d61b-47d2-9e2c-1244405171cc       113 02440
    #> 221 uuid:1ae1dbc5-8e48-4eaa-b1ef-2fecef470cbd       134 02441
    #> 222 uuid:66f2e612-cdfe-46e9-8d06-212b6a729f44       113 02441
    #> 223 uuid:aeda63ef-e099-460b-bd60-fe31908ea9aa       134 02442
    #> 224 uuid:69301eda-d9b7-4079-a6b4-b90bca3ee6d5       113 02442
    #> 225 uuid:f36eeca5-2af6-41bb-9de5-f4ca94e9f458       134 02443
    #> 226 uuid:770cf305-78eb-428c-8833-f3151fcd64bd       113 02443
    #> 227 uuid:2ff8ee27-4259-44d4-b8ef-7a5959efe88c       134 02444
    #> 228 uuid:e11a73c4-c32d-4d10-8974-92fe74e9ae67       113 02444
    #> 229 uuid:0e4bc0e2-eb62-42f3-9fbf-d5630c980479       134 02445
    #> 230 uuid:ca854942-7618-4615-b7e7-eed022849e70       113 02445
    #> 231 uuid:8221c29a-3cb2-4f52-858a-51e9d98537b5       134 02446
    #> 232 uuid:6075860f-edb8-40a8-a54e-adb9f489f63b       113 02446
    #> 233 uuid:056834f6-d14d-47f8-b1c9-cb4f4f2fefc7       134 02447
    #> 234 uuid:65e40162-b03f-47be-8b8a-1e467b3b9816       113 02447
    #> 235 uuid:39ed8f71-f900-463a-9188-228b553b96c1       134 02448
    #> 236 uuid:dddb5678-483d-43ae-966c-edd3d5e657aa       113 02448
    #> 237 uuid:befb6248-729e-4312-b1ae-35241e949fb1       134 02449
    #> 238 uuid:b911ce48-85cf-4270-ab09-190f641ed2fe       113 02449
    #> 239 uuid:3c664349-0b48-4edb-a1dd-dbf8c18ed0ed       134 02450
    #> 240 uuid:553a2576-904a-46cf-b2b3-e3b888953a7f       113 02450
    #> 241 uuid:9df5fd74-affc-4468-9c62-cedee7de8dbd       134 02451
    #> 242 uuid:86aef3c6-4c56-45fe-89f1-0efdfeebcdf3       113 02451
    #> 243 uuid:7b286d59-8d88-4b32-ae1c-00501ce16356       134 02452
    #> 244 uuid:1ff812ce-4c9a-4c22-a06b-dcede4cecdfe       113 02452
    #> 245 uuid:5f2a0243-0868-46ef-9922-c36ffd79877f       134 02453
    #> 246 uuid:52839a8f-0849-436b-ab4b-a8b8e648986f       113 02453
    #> 247 uuid:cb56b331-5040-4337-9537-080603dbf4c8       134 02454
    #> 248 uuid:6b1a0fc9-5ed6-4a2b-a992-4dc4501e4099       113 02454
    #> 249 uuid:19ee2095-1273-46e4-8c0e-411f2cb3ef6b       134 02455
    #> 250 uuid:5711c19b-b1e7-44ae-b422-8e7f286aa4fb       113 02455
    #> 251 uuid:3adfa9cd-1d40-4b5d-94cb-bcb2288b62ec       134 02456
    #> 252 uuid:975524bd-f34f-4a9f-ad9f-c32bce5d6579       113 02456
    #> 253 uuid:71fcf97b-f205-4ed5-81e3-8c34cab26bc5       134 02457
    #> 254 uuid:8c99e615-eef1-4919-8918-d7c7527248e8       113 02457
    #> 255 uuid:be6236ad-3712-48bb-8752-455bc5abc1d2       132 02884
    #> 256 uuid:fd81a38f-e71f-401a-935f-e7a1bb28260c       132 02884
    #> 257 uuid:82f70958-2c0f-472d-b47f-9cfe4deaac66       118 03089
    #> 258 uuid:0ebd7956-48e6-407d-8da6-bcdec782802a       104 03089
    #> 259 uuid:c80ff99d-fde6-43d9-b592-9f0f9db36472       124 03435
    #> 260 uuid:f67de0fa-5cdd-474b-8f47-f946ccfc22d7       124 03435
    #> 261 uuid:e585ef8b-110c-40f3-8d3a-39bed14b1caa       122 03514
    #> 262 uuid:9c5ff618-6d54-41f3-bfac-d1ff91964103       150 03514
    #> 263 uuid:ba5ec538-0314-4fc5-a6cf-a1b30b656598       150 03514
    #> 264 uuid:3901f039-b815-442a-9c14-1574fdcb6c06       122 03524
    #> 265 uuid:a7c1f8b7-5e79-4119-ba5d-7f54b2f93d75       150 03524
    #> 266 uuid:097cf60a-6998-4439-99eb-cc2449223e42       122 03804
    #> 267 uuid:bb55a6d5-0987-4e16-9582-1b227dd697d4       150 03804
    #> 268 uuid:80fcda23-ac73-43ad-8c4d-fbd1c06711d6       109 04440
    #> 269 uuid:a1940237-fd4f-4794-aeba-ca299ec72627       149 04440
    #> 270 uuid:d042a688-c479-4bc5-bb5f-0babaa37abf6       125 04475
    #> 271 uuid:9ff16bb9-886b-40f8-93cc-ed7b8b32b01e       150 04475
    #> 272 uuid:55b3e243-cfdc-487d-aa33-34ec96779227       125 04558
    #> 273 uuid:09547d1a-f37e-4eb7-8d86-84fa1d83a063       125 04558
    #> 274 uuid:b22822ab-3d49-4a7c-b33e-372e5692c8b5       109 04589
    #> 275 uuid:b5542f53-35b4-476b-81a7-098e431e3959       149 04589
    #> 276 uuid:929e1d7f-5f41-45fe-a457-99dda58d0ae7       109 04921
    #> 277 uuid:afeb5e0f-ba25-44ac-9fa5-7f7d0aea20b3       109 04921
    #> 278 uuid:99a7bbf1-cbde-40e1-adaa-6fecc2c6174c       103 05134
    #> 279 uuid:3c977ba7-de1f-4ec7-ab7a-f23f86ec52b9       150 05134
    #> 280 uuid:c5126dce-e795-4e9b-9774-1ba436d6f53b       113 05526
    #> 281 uuid:93ae491b-b4ba-4f01-9c82-0589c2269e52       113 05526
    #> 282 uuid:4bbf4f1f-90e9-4fef-ae64-095d185f2e67       108 05661
    #> 283 uuid:c8f954a2-cab2-4b83-ab21-bff7dd63b9dd       108 05661
    #> 284 uuid:ab69c0d8-28c5-48cf-b68d-59340a98d083       156 05686
    #> 285 uuid:daa61561-f6b8-4516-8ff2-70560780e901       113 05686
    #> 286 uuid:ce02537b-8c33-442a-b5d0-2f0e416d8290       156 05687
    #> 287 uuid:4ae5a66a-d86b-47e0-9bbd-82e8c66e9370       113 05687
    #> 288 uuid:09d611e2-b0c1-4360-8433-0b6bf2aa54e3       156 05688
    #> 289 uuid:afe0f7c0-3ddf-4b52-a784-0ee7488d226d       113 05688
    #> 290 uuid:b40aae45-c764-4bb4-acc3-28ae5a75b17b       156 05689
    #> 291 uuid:800515af-b85a-46a4-a003-5a0339b68810       113 05689
    #> 292 uuid:c2469415-457b-47ee-90d8-162398d6ef4c       156 05690
    #> 293 uuid:f1ea0a17-861a-451b-b005-1099bfc0ca56       113 05690
    #> 294 uuid:1704060c-709c-4e78-b18c-096de15e5ba9       156 05691
    #> 295 uuid:8c353d53-7fb0-47ce-b088-8f82221795c2       113 05691
    #> 296 uuid:e6d2c092-21e9-4b15-8df1-b518209d8700       156 05692
    #> 297 uuid:08863bc3-ce52-4afe-92c8-8b2e5cbb9bba       113 05692
    #> 298 uuid:14598f7a-59de-438e-b131-b9ab61ec2bfe       156 05693
    #> 299 uuid:c562ec5d-eaa9-4da0-9853-af03c8d2c338       113 05693
    #> 300 uuid:57341afc-e4ec-4b4f-a1dc-507ed4c7827f       156 05694
    #> 301 uuid:b8fce575-e145-4fbe-b6dd-5f55d9fb013b       113 05694
    #> 302 uuid:db60e2fb-6bd3-4c82-90dd-5aa68ba3319e       156 05695
    #> 303 uuid:6ad3d7c9-f6f2-4242-8575-64050c480527       113 05695
    #> 304 uuid:1df0a1b5-6d3a-4ba2-97ff-66f71d013b3d       113 05859
    #> 305 uuid:9ef406c4-14a9-415c-9a76-74e8d5b70532       150 05859
    #> 306 uuid:1598afa5-c76a-47aa-b355-0383a4096e36       106 06160
    #> 307 uuid:57e2b083-c7e3-4f1d-bb89-a0cfd5aa54ba       150 06160
    #> 308 uuid:89ab72f9-dad0-45b3-a828-77853fb2ab58       127 06178
    #> 309 uuid:7b6d79d8-20cd-4768-8f30-ec718baf3974       149 06178
    #> 310 uuid:15341f27-4267-4b92-9448-34a8e230ef2b       127 06247
    #> 311 uuid:3a9c16e4-ee4e-4203-a081-6ecdb2b429dd       127 06247
    #> 312 uuid:7ddbe0ce-cf09-4803-ab77-b75b419fa37e       106 06338
    #> 313 uuid:95e1e23e-d56b-4d72-b5c6-7bfd85f98323       106 06338
    #> 314 uuid:a6becc49-4561-4b71-9d64-8407331708d5       106 06339
    #> 315 uuid:6842380d-d7c2-43af-a0f7-9cac0fa0eba4       106 06339
    #> 316 uuid:da4815b2-2009-419f-bf9b-89a0d1f32ca3       106 06342
    #> 317 uuid:95ce78b0-c4bc-4fc0-8090-c01df62f3e15       106 06342
    #> 318 uuid:b97057e8-0264-45f7-80cd-66c682886ced       106 06344
    #> 319 uuid:0c305dc5-a6de-4fc9-9546-572dd66a2c9c       106 06344
    #> 320 uuid:f7c30b44-2253-4274-86fe-f525cf0cb89b       144 06470
    #> 321 uuid:79b797e4-94f6-444f-bb82-d56b164fb2f2       144 06470
    #> 322 uuid:b867c11b-4076-4101-bd34-21e7a5ee224e       144 06480
    #> 323 uuid:50bc41b1-a7ba-4385-bc48-6c7495a973a1       777 06480
    #> 324 uuid:8f51998c-80c2-42b3-8975-ec5e4c4cc10b       777 06480
    #> 325 uuid:2ffcf6d6-fad2-4f36-9a6c-d98e47c01bcb       151 06480
    #> 326 uuid:239c2f5d-0abd-4651-a943-d6080d85369a       777 06504
    #> 327 uuid:f94083d9-f933-49f2-bef8-db06eb3249ae       143 06504
    #> 328 uuid:1ee2192d-7125-4843-8376-792e2b13900e       137 06584
    #> 329 uuid:a90ada81-65d2-44a6-b2b6-a71422e48a4b       143 06584
    #> 330 uuid:29a0db7a-0918-434a-af3f-fc579e949db1       154 06968
    #> 331 uuid:c91cc227-99d4-4d82-a751-03fbcb9f7f06       131 06968
    #> 332 uuid:90b548a3-9a58-4bb8-bb89-483247492c60       131 07012
    #> 333 uuid:50ee75ef-39e1-4609-983f-e34b5086ca8d       131 07012
    #> 334 uuid:707f6815-36fa-4d9c-b7fa-1e1f5f41d7f5       131 07015
    #> 335 uuid:477c19a5-3260-47ad-b7c6-daba5e5392d2       106 07015
    #> 336 uuid:493b4465-b4b3-4d8a-8f3a-27088ebfca7e       131 07051
    #> 337 uuid:a4218b6c-0d12-4901-99e0-9f55dbeb219f       149 07051
    #> 338 uuid:c4d41f7f-6bbe-4f21-873d-2831f457e97d       131 07052
    #> 339 uuid:aa33804f-9dad-4937-bd1c-d3e79f4a8ffe       128 07052
    #> 340 uuid:778a72ce-14e3-49b8-ae2c-ca2c13b77a65       128 07489
    #> 341 uuid:a4e765d1-5115-4de9-b9c5-116f6e5255c0       128 07489
    #> 342 uuid:bd08e960-a04a-4257-9d6c-2f2d70ed351d       107 07891
    #> 343 uuid:aba5759c-50f9-4514-9790-7aac07a1f974       150 07891
    #> 344 uuid:86744263-800d-45f8-a279-a7c3bb7ec025       150 08325
    #> 345 uuid:162963c3-651d-4890-b518-6c2ac017f5e2       105 08325
    #> 346 uuid:867693fa-5cab-4d5e-947a-6138cc68c4c9       114 08631
    #> 347 uuid:fb57cbe2-8f9e-418c-ad89-38d9e5d58d7d       150 08631
    #> 348 uuid:daffbd7d-ecdd-4c71-be15-2b9d4c27f452       122 08924
    #> 349 uuid:02808c34-bc9b-444f-a39c-6b2893ecfe38       123 08924
    #> 350 uuid:40939c1d-47d3-49ca-b08d-996c91c07b01       143 08927
    #> 351 uuid:0a6af6cc-e2f0-482d-96f4-c86ea55916eb       143 08927
    #> 352 uuid:4bf414b3-7503-447d-b995-cb06848412be       128 09050
    #> 353 uuid:367763e0-d43b-47cf-a9a9-1c107d902a36       128 09050
    #> 354 uuid:11de2c70-af08-47ff-bb0a-67e52523c6e6       140 09194
    #> 355 uuid:0ff31a06-940a-41a1-bc69-778195bc852e       140 09194
    #> 356 uuid:bd4d4058-23e2-46d1-9b25-62105cb29e08       150 09406
    #> 357 uuid:dc851e8a-2317-45f6-8ffc-29e7c6f23d15       150 09406
    #> 358 uuid:34f33af7-d928-4eed-81ce-c6abf3f206cb       157 09557
    #> 359 uuid:90c6b127-3b37-4666-9ffd-2c523efba18f       157 09557
    #> 360 uuid:333d499b-fd6c-486d-9fd6-dd5cb69480f6       150 09627
    #> 361 uuid:7a4cffd2-fc67-4c77-b0e4-b906324c8e2c       145 09627
    #> 362 uuid:ee177327-a8f9-40cd-ba4f-db3dd249247e       145 09639
    #> 363 uuid:71ac1cfa-8670-4a74-b7e8-0d050c5a18f7       145 09639
    #> 364 uuid:c310da3f-dc16-46b4-889e-683ceba5127d       150 09657
    #> 365 uuid:20982ea0-5bd7-4548-9f18-c547dd152b74       145 09657
    #> 366 uuid:4ab601b3-467b-4e15-bfd9-a18a477a46b4       145 09671
    #> 367 uuid:b5db77a7-756a-4dbb-b3ec-2f77c4781db6       153 09671
    #> 368 uuid:7cd69f44-4223-4b5a-b68c-066bc27a1403       150 10165
    #> 369 uuid:4bbc1858-da5a-4dcc-8860-6b7a03f908a6       108 10165
    #> 370 uuid:bc092492-675b-4984-a1c7-c98b700565e9       105 10278
    #> 371 uuid:f5a1990a-0e97-4dac-93bd-57e926193b79       150 10278
    #> 372 uuid:910c3e04-38f4-46b1-8df0-49175292d7cf       105 10298
    #> 373 uuid:f8bc615d-5c8b-4562-9b4b-e64a2e7b2e80       149 10298
    #> 374 uuid:9a7982da-a703-49f6-98be-4478c0365b38       148 10469
    #> 375 uuid:f1af5832-c13f-4da5-95e3-6c038b00869f       114 10469
    #> 376 uuid:9624e0dd-ddfb-4e1d-a7b0-81616ffbc50b       159 10671
    #> 377 uuid:b1f8c502-5cb6-4680-80d9-c9ae292cc51e       150 10671
    #> 378 uuid:177d634f-6503-4e7c-b4c4-9e360a072071       159 10715
    #> 379 uuid:4151a6a8-4efd-40a2-b51f-1376927ed6f7       150 10715
    #> 380 uuid:8132b5b9-caf9-411d-81ea-b47362acdda5       150 11270
    #> 381 uuid:a55c457f-8493-41a3-8928-fad85b97cfaa       114 11270
    #> 382 uuid:107a6198-30dc-40a9-ad57-c8719d18cb0a       114 11304
    #> 383 uuid:2fdd75eb-92cf-4577-8505-a9ed8fe26939       114 11304
    #> 384 uuid:6330aa19-40a8-4157-b525-42ebe993f740       114 11316
    #> 385 uuid:fd65243c-6054-4a93-8772-b9a81b7e1c1f       114 11316
    #> 386 uuid:1afc8923-8af2-4d50-b7b3-1525b9416cf6       114 11366
    #> 387 uuid:53bc7f75-53ea-4f2b-8868-0c936b85293d       114 11366
    #> 388 uuid:961a5fbc-fc20-4d15-90c1-b038ac7465ff       150 11414
    #> 389 uuid:f10cca25-7cac-4d87-8475-136baae7c9ee       114 11414
    #> 390 uuid:e79929a9-6144-4f86-b240-3e27a4db3783       101 99999
    #> 391 uuid:24e35aec-90a5-4df2-930c-c0f0fab6a528       106 99999
    #> 392 uuid:746b8006-119f-4860-a2c2-3ad9afa8a6d7       101 99999
    #> 393 uuid:4988f9d9-04e7-41ea-92d3-f501fb12ad75       104 99999
    #> 394 uuid:5aed9e42-bf88-4a24-8461-f8caf820e4fb       101 99999
    #> 395 uuid:cda1234c-8605-465a-912a-737a83cb3f1e       101 99999
    #>                     candidate_name constituency_code n entry new_and_old
    #> 1                    Muhammad Asif            NA-107 2   old       FALSE
    #> 2                    Muhammad Asif             PS-67 2   old       FALSE
    #> 3              Rana Afzaal Hussain            NA-119 2   old       FALSE
    #> 4              Rana Afzaal Hussain            NA-120 2   old       FALSE
    #> 5                  Muhammad Yousaf             NA-12 2   old       FALSE
    #> 6                  Muhammad Yousaf             PS-49 2   old       FALSE
    #> 7              Muhammad Saeed Virk            NA-121 2   old       FALSE
    #> 8                    Muhammad Syed             PS-91 2   old       FALSE
    #> 9                           Salman            NA-137 2   old       FALSE
    #> 10                          Salman            NA-138 2   old       FALSE
    #> 11                        Asif Ali            NA-137 2   old       FALSE
    #> 12                            Asif            NA-138 2   old       FALSE
    #> 13                    Anis Qureshi            NA-138 2   old       FALSE
    #> 14             Dawood Anis Qureshi            NA-138 2   old       FALSE
    #> 15                     Raiz Ul Haq            NA-142 2   old       FALSE
    #> 16                    Sheraz Zafar            NA-142 2   old       FALSE
    #> 17            Nawab Amanullah Khan            NA-160 2   old       FALSE
    #> 18           Nawab Amanullaha Khan            NA-161 2   old       FALSE
    #> 19                    Abdul Rehman            NA-191 3   old       FALSE
    #> 20                    Abdul Rehman            NA-191 3   old       FALSE
    #> 21            Dr Mian Abdul Rehman            NA-191 3   old       FALSE
    #> 22                  Parveen Akhtar            NA-191 2   old       FALSE
    #> 23                  Parveen Akthar            NA-191 2   old       FALSE
    #> 24                       Akbar Ali            NA-207 2   old        TRUE
    #> 25                       Akbar ali            NA-207 2   new        TRUE
    #> 26                       Amanullah            NA-207 2   old        TRUE
    #> 27                 Amanullah khoso            NA-207 2   new        TRUE
    #> 28                     Ameer Buksh            NA-207 2   old        TRUE
    #> 29                       Ameer bux            NA-207 2   new        TRUE
    #> 30                       Didar Ali            NA-207 2   old        TRUE
    #> 31                       Didar ali            NA-207 2   new        TRUE
    #> 32                  Ghulam Murtaza            NA-207 2   old        TRUE
    #> 33                  Ghulam murtaza            NA-207 2   new        TRUE
    #> 34                       Gohar Ali            NA-207 2   old        TRUE
    #> 35                 Gohar ali khoso            NA-207 2   new        TRUE
    #> 36                         M.Zaman            NA-207 2   old        TRUE
    #> 37            Hafiz muhammad zaman            NA-207 2   new        TRUE
    #> 38                Khuda Dino Sangi            NA-207 2   old        TRUE
    #> 39                Khuda dino sangi            NA-207 2   new        TRUE
    #> 40                 Khurshid Afghan            NA-207 2   old        TRUE
    #> 41                 Khurshid afghan            NA-207 2   new        TRUE
    #> 42                    Mubeen Ahmad            NA-207 2   old        TRUE
    #> 43                    Mubeen ahmad            NA-207 2   new        TRUE
    #> 44                   M.Amir Sheikh            NA-207 2   old        TRUE
    #> 45             Muhammad ami sheikh            NA-207 2   new        TRUE
    #> 46                         M.Murad            NA-207 2   old        TRUE
    #> 47                  Muhammad murad            NA-207 2   new        TRUE
    #> 48                         M.Tahir            NA-207 2   old        TRUE
    #> 49                  Muhammad tahir            NA-207 2   new        TRUE
    #> 50                    Nouman Islam            NA-207 2   old        TRUE
    #> 51              Numan islam sheikh            NA-207 2   new        TRUE
    #> 52                     Saood Afzal            NA-207 2   old        TRUE
    #> 53                     Saood afzal            NA-207 2   new        TRUE
    #> 54                     Sohail Niaz            NA-207 2   old        TRUE
    #> 55                Sohail naz khoso            NA-207 2   new        TRUE
    #> 56              Syed Ali Raza Shah            NA-207 2   old        TRUE
    #> 57                   Ali raza shah            NA-207 2   new        TRUE
    #> 58                         M.Ayoub            NA-207 2   old        TRUE
    #> 59             Syed muhammad ayoub            NA-207 2   new        TRUE
    #> 60                       Wazir Ali            NA-207 2   old        TRUE
    #> 61                       Wazir ali            NA-207 2   new        TRUE
    #> 62                 Wazir Ali Jatoi            NA-207 2   old        TRUE
    #> 63                 Wazir ali jatoi            NA-207 2   new        TRUE
    #> 64                 Ziauddin Bhatti            NA-207 2   old        TRUE
    #> 65                 Ziauddin bhatti            NA-207 2   new        TRUE
    #> 66                       Abdul Haq            NA-209 2   old        TRUE
    #> 67                      Abul haque            NA-209 2   new        TRUE
    #> 68                      Ghulam Ali            NA-209 2   old        TRUE
    #> 69                Ghulam ali hajno            NA-209 2   new        TRUE
    #> 70                  Ghulam Mustafa            NA-209 2   old        TRUE
    #> 71                 Ghulam  mustafa            NA-209 2   new        TRUE
    #> 72                     Ismail Shah            NA-209 2   old        TRUE
    #> 73                     Ismail shah            NA-209 2   new        TRUE
    #> 74                      Munwar Ali            NA-209 2   old        TRUE
    #> 75               Munwar ali wassan            NA-209 2   new        TRUE
    #> 76             Peer Sadruddin Shah            NA-209 2   old        TRUE
    #> 77           Pir saddar uddin shah            NA-209 2   new        TRUE
    #> 78        Peer Syed Fazal Ali Shah            NA-209 2   old        TRUE
    #> 79                 Syed fazal shah            NA-209 2   new        TRUE
    #> 80                 Syed Ameer Umar            NA-209 2   old        TRUE
    #> 81         Syed ameer umar jillani            NA-209 2   new        TRUE
    #> 82                      Abdul Aziz            NA-217 2   old       FALSE
    #> 83                      Abdul Aziz             PS-54 2   old       FALSE
    #> 84                      Roop Chand            NA-218 2   old       FALSE
    #> 85                      Roop Chand             PS-48 2   old       FALSE
    #> 86                  Ali Nawaz Shah            NA-219 2   old       FALSE
    #> 87                  Ali Nawaz Shah             PS-48 2   old       FALSE
    #> 88                Shoaib Alam Khan             NA-22 2   old       FALSE
    #> 89                     Shoaib Khan             NA-22 2   old       FALSE
    #> 90                 M Younis Talpur            NA-220 2   old       FALSE
    #> 91           Nawab M Yousaf Talpur            NA-220 2   old       FALSE
    #> 92                    Dilawar Khan            NA-221 2   old       FALSE
    #> 93                    Dilawar Khan             PS-54 2   old       FALSE
    #> 94                 Arbab Zakaullah            NA-222 2   old       FALSE
    #> 95                 Arbab Zakaullah             PS-56 2   old       FALSE
    #> 96                  Gul Sher Sario            NA-223 2   old       FALSE
    #> 97                  Gul Sher Sario             PS-58 2   old       FALSE
    #> 98            Makhdoom Shahzad Ali            NA-223 2   old       FALSE
    #> 99            Makhdoom Shahzad Ali             PS-58 2   old       FALSE
    #> 100                Bismillah Kakar            NA-263 2   old       FALSE
    #> 101                Busmillah Kakar            NA-263 2   old       FALSE
    #> 102                     Amir Muqam             PK-29 2   old        TRUE
    #> 103                    Ameer Muqam             NA-29 2   new        TRUE
    #> 104              Arbab Kamal Ahmad             PK-29 2   old        TRUE
    #> 105              Arbab kamal ahmad             NA-29 2   new        TRUE
    #> 106                       M.Shafiq             PK-29 2   old        TRUE
    #> 107                Muhammad Shafiq             NA-29 2   new        TRUE
    #> 108                Mufti Naeem Jan             PK-29 2   old        TRUE
    #> 109                Mufti Naeem Jan             NA-29 2   new        TRUE
    #> 110             Arbab Alamgir Khan             PK-30 2   old        TRUE
    #> 111             Arbab alamgir khan             NA-30 2   new        TRUE
    #> 112             Arbab Najeeb Ullah             PK-30 2   old        TRUE
    #> 113        Arbab Najeeb Ullah Khan             NA-30 2   new        TRUE
    #> 114              Mailk Haider Khan             PK-30 2   old        TRUE
    #> 115              Malik haider khan             NA-30 2   new        TRUE
    #> 116                 Alamgir Khalil             PK-30 2   old        TRUE
    #> 117        Muhammad Alamgir Khalil             NA-30 2   new        TRUE
    #> 118                       M.Junaid             PK-30 2   old        TRUE
    #> 119                Muhammad Junaid             NA-30 2   new        TRUE
    #> 120                      M.Shoukat             PK-30 2   old        TRUE
    #> 121      Muhammad Shoukat Khorshed             NA-30 2   new        TRUE
    #> 122                 Noor Wali Khan             PK-30 2   old        TRUE
    #> 123                 Noor wali khan             NA-30 2   new        TRUE
    #> 124                       Sher Ali             PK-30 2   old        TRUE
    #> 125                 Sher ali arbab             NA-30 2   new        TRUE
    #> 126     Akhunzada Irfan Ullah Shah             PK-31 2   old        TRUE
    #> 127          Akhunzada irfan ullah             NA-31 2   new        TRUE
    #> 128                 Aurangzeb Khan             PK-31 2   old        TRUE
    #> 129                 Aurangzeb khan             NA-31 2   new        TRUE
    #> 130                     Gul Rehman             PK-31 2   old        TRUE
    #> 131                     Gul rehman             NA-31 2   new        TRUE
    #> 132               Haji Gulam Ahmad             PK-31 2   old        TRUE
    #> 133              Haji ghulam ahmad             NA-31 2   new        TRUE
    #> 134                    Ilyas Ahmad             PK-31 2   old        TRUE
    #> 135             Ilyas ahmad bilour             NA-31 2   new        TRUE
    #> 136                       M.Nadeem             PK-31 2   old        TRUE
    #> 137                Muhammad nadeem             NA-31 2   new        TRUE
    #> 138                 M.Saqqaf Yasir             PK-31 2   old        TRUE
    #> 139          Muhammad saqqaf nasir             NA-31 2   new        TRUE
    #> 140             M.Siddiq Ur Rehman             PK-31 2   old        TRUE
    #> 141      Muhammad siddiq ur rehman             NA-31 2   new        TRUE
    #> 142                   Noor Hussain             PK-31 2   old        TRUE
    #> 143                   Noor hussain             NA-31 2   new        TRUE
    #> 144                      Roohullah             PK-31 2   old        TRUE
    #> 145                      Roohullah             NA-31 2   new        TRUE
    #> 146                    Shoukat Ali             PK-31 2   old        TRUE
    #> 147 Shaukat ali s/o muhammad afzal             NA-31 2   new        TRUE
    #> 148                    Shoukat Ali             PK-31 2   old        TRUE
    #> 149     Shaukat ali s/o liaqat ali             NA-31 2   new        TRUE
    #> 150                        Yasmeen             PK-31 2   old        TRUE
    #> 151                        Yasmeen             NA-31 2   new        TRUE
    #> 152                   Abbas Afridi             PK-32 2   old        TRUE
    #> 153                   Abbas Afridi             NA-32 2   new        TRUE
    #> 154                    Asmat Ullah             PK-32 2   old        TRUE
    #> 155               Asmat Ullah Khan             NA-32 2   new        TRUE
    #> 156                 Bismillah Khan             PK-32 2   old        TRUE
    #> 157                 Bismillah Khan             NA-32 2   new        TRUE
    #> 158                   Gohar M.Khan             PK-32 3   old        TRUE
    #> 159    Gohar Muhammad Khan Bangash             NA-32 3   new        TRUE
    #> 160    Gohar Muhammad khan Bangash             NA-32 3   new        TRUE
    #> 161                   Madiha Faraz             PK-32 2   old        TRUE
    #> 162                   Madiha Faraz             NA-32 2   new        TRUE
    #> 163             Mufti Ibrar Sultan             PK-32 2   old        TRUE
    #> 164             Mufti Ibrar Sultan             NA-32 2   new        TRUE
    #> 165            Najeeb Ullah Durani             PK-32 2   old        TRUE
    #> 166           Najeeb Ullah Durrani             NA-32 2   new        TRUE
    #> 167                 Sheryar Afridi             PK-32 2   old        TRUE
    #> 168                Shehryar Afridi             NA-32 2   new        TRUE
    #> 169                  Yousaf Afridi             PK-32 2   old        TRUE
    #> 170                  Yousaf Afridi             NA-32 2   new        TRUE
    #> 171                   Akhtar Munir             PK-33 2   old        TRUE
    #> 172           Akhtar munir bangash             NA-33 2   new        TRUE
    #> 173                   Arif Hussain             PK-33 2   old        TRUE
    #> 174                   Arif hussain             NA-33 2   new        TRUE
    #> 175                 Atiq Ur Rehman             PK-33 2   old        TRUE
    #> 176                 Atiq ur rehman             NA-33 2   new        TRUE
    #> 177                 Hussain Jalali             PK-33 2   old        TRUE
    #> 178                 Hussain jalali             NA-33 2   new        TRUE
    #> 179               Khaliq Ur Rehman             PK-33 2   old        TRUE
    #> 180               Khaliq ur rehman             NA-33 2   new        TRUE
    #> 181                   Khiyal Zaman             PK-33 2   old        TRUE
    #> 182                    Khyal zaman             NA-33 2   new        TRUE
    #> 183                     Malik Riaz             PK-33 2   old        TRUE
    #> 184             Malak riaz bangash             NA-33 2   new        TRUE
    #> 185                    Mufti Imran             PK-33 2   old        TRUE
    #> 186                 Muhammad imran             NA-33 2   new        TRUE
    #> 187                   Rangeen Khan             PK-33 2   old        TRUE
    #> 188                   Rangeen khan             NA-33 2   new        TRUE
    #> 189                   Sultan Akbar             PK-33 2   old        TRUE
    #> 190                   Sultan akbar             NA-33 2   new        TRUE
    #> 191                Haider Ali Shah             PK-33 2   old        TRUE
    #> 192           Syed haider ali shah             NA-33 2   new        TRUE
    #> 193                   Taj Mohammad             PK-33 2   old        TRUE
    #> 194                   Taj muhammad             NA-33 2   new        TRUE
    #> 195                  Ulfat Hussain             PK-33 2   old        TRUE
    #> 196                  Ulfat hussain             NA-33 2   new        TRUE
    #> 197                    Altaf Qadir             PK-34 2   old        TRUE
    #> 198                    altaf qadir             NA-34 2   new        TRUE
    #> 199                      Aziz Khan             PK-34 2   old        TRUE
    #> 200                      aziz khan             NA-34 2   new        TRUE
    #> 201                 Gul Sahib Khan             PK-34 2   old        TRUE
    #> 202                 gul sahib khan             NA-34 2   new        TRUE
    #> 203                   Hameed Ullah             PK-34 2   old        TRUE
    #> 204               hameedullah khan             NA-34 2   new        TRUE
    #> 205                   Haseeb Ahmad             PK-34 2   old        TRUE
    #> 206             haseeb ahmad afaqi             NA-34 2   new        TRUE
    #> 207                    Ikhtiar Gul             PK-34 2   old        TRUE
    #> 208                   ikhtiyar gul             NA-34 2   new        TRUE
    #> 209                     Immad Azam             PK-34 2   old        TRUE
    #> 210                     immad azam             NA-34 2   new        TRUE
    #> 211                 Mir Zakim Khan             PK-34 2   old        TRUE
    #> 212                 mir zakim khan             NA-34 2   new        TRUE
    #> 213           Nawabzada Mohsin Ali             PK-34 2   old        TRUE
    #> 214      nawabzada mohsin ali khan             NA-34 2   new        TRUE
    #> 215                   Rehmat Salam             PK-34 2   old        TRUE
    #> 216              rehmat salam khan             NA-34 2   new        TRUE
    #> 217                Saad Ullah Khan             PK-34 2   old        TRUE
    #> 218         saad ullah khan khatak             NA-34 2   new        TRUE
    #> 219                   Shahid Ahmad             PK-34 2   old        TRUE
    #> 220                   shahid ahmad             NA-34 2   new        TRUE
    #> 221                 Shakir Zeeshan             PK-34 2   old        TRUE
    #> 222         shakir zeeshan khattak             NA-34 2   new        TRUE
    #> 223                Shams Ur Rehman             PK-34 2   old        TRUE
    #> 224                shams ur rehman             NA-34 2   new        TRUE
    #> 225                Tauseef Razique             PK-34 2   old        TRUE
    #> 226                touseef razique             NA-34 2   new        TRUE
    #> 227                    Usman Ghani             PK-34 2   old        TRUE
    #> 228                    usman ghani             NA-34 2   new        TRUE
    #> 229                  Zafar Mehmood             PK-34 2   old        TRUE
    #> 230                  zafar mehmood             NA-34 2   new        TRUE
    #> 231               Abdul Samad Khan             PK-35 2   old        TRUE
    #> 232               abdul samad khan             NA-35 2   new        TRUE
    #> 233                     Akram Khan             PK-35 2   old        TRUE
    #> 234              akram khan durani             NA-35 2   new        TRUE
    #> 235                     Amin Ullah             PK-35 2   old        TRUE
    #> 236                     amin ullah             NA-35 2   new        TRUE
    #> 237                     Hamid Shah             PK-35 2   old        TRUE
    #> 238                     hamid shah             NA-35 2   new        TRUE
    #> 239         Imran Ahmad Khan Niazi             PK-35 2   old        TRUE
    #> 240         imran ahmad khan niazi             NA-35 2   new        TRUE
    #> 241                     Inam Ullah             PK-35 2   old        TRUE
    #> 242                     inam ullah             NA-35 2   new        TRUE
    #> 243                     Aslam Noor             PK-35 2   old        TRUE
    #> 244                     islam noor             NA-35 2   new        TRUE
    #> 245                     Liaqat Ali             PK-35 2   old        TRUE
    #> 246                     liaqat ali             NA-35 2   new        TRUE
    #> 247                    M.Usman Ali             PK-35 2   old        TRUE
    #> 248                    M usman ali             NA-35 2   new        TRUE
    #> 249                    Sadaf Iqbal             PK-35 2   old        TRUE
    #> 250              Safdar iqbal shah             NA-35 2   new        TRUE
    #> 251                   Yasmin Sadaf             PK-35 2   old        TRUE
    #> 252            Syeda yasmin safdar             NA-35 2   new        TRUE
    #> 253                  Wali Dad Khan             PK-35 2   old        TRUE
    #> 254                  Wali dad khan             NA-35 2   new        TRUE
    #> 255                Sayed Zafar Ali             NA-53 2   old       FALSE
    #> 256                 Zaib Ur Rahman             NA-53 2   old       FALSE
    #> 257             Chaduary Abid Raza             NA-71 2   old       FALSE
    #> 258            Chaudhary Abid Raza             NA-71 2   old       FALSE
    #> 259                Jala Ahmad Khan              PB-1 2   old       FALSE
    #> 260                     Jalil Khan              PB-1 2   old       FALSE
    #> 261                     Habibullah             PB-13 3   old       FALSE
    #> 262                     Habibullah             PS-54 3   old       FALSE
    #> 263                     Habibullah             PS-63 3   old       FALSE
    #> 264                       Ali Sher             PB-14 2   old       FALSE
    #> 265                       Ali Sher             PS-59 2   old       FALSE
    #> 266                        Ibrahim             PB-27 2   old       FALSE
    #> 267                        Ibrahim             PS-54 2   old       FALSE
    #> 268                   Sajjad Ahmad             PK-15 2   old       FALSE
    #> 269                   Sajjad Ahmad             PS-50 2   old       FALSE
    #> 270                      Gul Zaman             PK-19 2   old       FALSE
    #> 271                      Gul Zaman             PS-53 2   old       FALSE
    #> 272                   Naseer U Din             PK-27 2   old       FALSE
    #> 273                  Naseer Ud Din             PK-27 2   old       FALSE
    #> 274                     Javaid Ali              PK-3 2   old       FALSE
    #> 275                     Javaid Ali             PS-48 2   old       FALSE
    #> 276                     Maaz Ullah             PK-56 2   old       FALSE
    #> 277                     Maza Ullah             PK-56 2   old       FALSE
    #> 278                   Abdul Raheem             PK-74 2   old       FALSE
    #> 279                   Abdul Raheem             PS-51 2   old       FALSE
    #> 280              Tariq bashir raja             PP-10 2   new        TRUE
    #> 281               Safadar ali khan             PP-10 2   new        TRUE
    #> 282                      Shokt Ali            PP-107 2   old       FALSE
    #> 283                         Shukat            PP-107 2   old       FALSE
    #> 284                      Rida Asim             PS-92 2   old        TRUE
    #> 285                   Babar sohail             PP-11 2   new        TRUE
    #> 286                  Shafeeq Ahmed             PS-92 2   old        TRUE
    #> 287             Ch. Muhammad adnan             PP-11 2   new        TRUE
    #> 288            Sahid Kursheed Rana             PS-92 2   old        TRUE
    #> 289                Ghulam muhammad             PP-11 2   new        TRUE
    #> 290                   Shahzad Khan             PS-92 2   old        TRUE
    #> 291       Haji malik parvez akhtar             PP-11 2   new        TRUE
    #> 292            Syed Hammad Hussain             PS-92 2   old        TRUE
    #> 293              Malik azhar aftab             PP-11 2   new        TRUE
    #> 294           Syed Khawar Ali Shah             PS-92 2   old        TRUE
    #> 295          Muhammad adnan younas             PP-11 2   new        TRUE
    #> 296            Syed Muhammad Rafiq             PS-92 2   old        TRUE
    #> 297            Musarat iqbal abasi             PP-11 2   new        TRUE
    #> 298            Zameer Waheed Jafri             PS-92 2   old        TRUE
    #> 299             Naeem aslam kiyani             PP-11 2   new        TRUE
    #> 300                      Almasazam             PS-93 2   old        TRUE
    #> 301                    Raja arshad             PP-11 2   new        TRUE
    #> 302             Asfag Ahmend Mangi             PS-93 2   old        TRUE
    #> 303            Raja qaiser mujahid             PP-11 2   new        TRUE
    #> 304                    Saeed Ahmad            PP-121 2   old       FALSE
    #> 305                    Saeed Ahmad             PS-59 2   old       FALSE
    #> 306                   Waseem Ahmad             PP-14 2   old       FALSE
    #> 307                   Waseem Ahmad             PS-67 2   old       FALSE
    #> 308                  Mukhtar Ahmad            PP-140 2   old       FALSE
    #> 309                  Mukhtar Ahmad             PS-50 2   old       FALSE
    #> 310                Muhammad Saleem            PP-142 2   old       FALSE
    #> 311                Muhammad Saleem            PP-143 2   old       FALSE
    #> 312              Abdul Kareem Khan             PP-15 2   old       FALSE
    #> 313         Malik Abdul Karim Khan             PP-15 2   old       FALSE
    #> 314                M. Javaid Iqbal             PP-15 2   old       FALSE
    #> 315           M. Javed Iqbal Malik             PP-15 2   old       FALSE
    #> 316                 M. Tahir Iqbal             PP-15 2   old       FALSE
    #> 317                    Tahir Iqbal             PP-15 2   old       FALSE
    #> 318                   Umer Tanveer             PP-15 2   old       FALSE
    #> 319                    Umer Tanvir             PP-15 2   old       FALSE
    #> 320          Khawaja Salma Rafique            PP-157 2   old       FALSE
    #> 321          Kjawaja Salma Rafique            PP-157 2   old       FALSE
    #> 322               Abdul Aleem Khan            PP-157 4   old       FALSE
    #> 323               Abdul Aleem Khan            PP-158 4   old       FALSE
    #> 324               Kiran Aleem Khan            PP-158 4   old       FALSE
    #> 325               Abdul Aleem Khan             PS-64 4   old       FALSE
    #> 326                M Mumtaz Khalid            PP-159 2   old       FALSE
    #> 327         Muhammad Mumtaz Khalid            PP-159 2   old       FALSE
    #> 328                Ghulam Muhammad            NA-138 2   old       FALSE
    #> 329                Ghulam Muhammad            PP-164 2   old       FALSE
    #> 330               Muhammad Pervaiz            PP-128 2   old       FALSE
    #> 331              Mehr Muhamd Iqbal            PP-184 2   old       FALSE
    #> 332            Muhhmad Moeen Watto            PP-186 2   old       FALSE
    #> 333            Noor Ul Ameen Watto            PP-186 2   old       FALSE
    #> 334                      Rao Fahad            PP-186 2   old       FALSE
    #> 335                 Muhammad Akram            PP-192 2   old       FALSE
    #> 336                   Ghulam Qadir            PP-189 2   old       FALSE
    #> 337                   Gjulam Qadir             PS-52 2   old       FALSE
    #> 338                   Habib Ul Haq            PP-189 2   old       FALSE
    #> 339                   Arshad Iqbal            PP-226 2   old       FALSE
    #> 340           Mian Amir Iqbal Shah            PP-224 2   old       FALSE
    #> 341            Muhammad Iqbal Shah            PP-224 2   old       FALSE
    #> 342                    Sardar Khan            PP-257 2   old       FALSE
    #> 343                    Sardar Khan             PS-55 2   old       FALSE
    #> 344                   Abdul Hafeez             PP-61 2   old       FALSE
    #> 345                   Abdul Hafeez            PP-286 2   old       FALSE
    #> 346                 Ghulam Mustafa             PP-40 2   old       FALSE
    #> 347                 Ghulam Mustafa             PS-63 2   old       FALSE
    #> 348               Dr Sohail Zaffar             PP-59 2   old       FALSE
    #> 349            Sohail Zafar Cheema             PP-59 2   old       FALSE
    #> 350               Amir raza abbasi              PP-6 2   new        TRUE
    #> 351             Amjad arbab abbasi              PP-6 2   new        TRUE
    #> 352                   Sajjid Ahmad             PP-67 2   old       FALSE
    #> 353                    Sajjid Khan             PP-67 2   old       FALSE
    #> 354                  Zulafiqar Ali             PP-77 2   old       FALSE
    #> 355                   Zulfiqar Ali             PP-77 2   old       FALSE
    #> 356                Muhammad Haroon             PS-55 2   old       FALSE
    #> 357                Muhammad Haroon             PS-67 2   old       FALSE
    #> 358                    Arsalan Taj            PS-102 2   old       FALSE
    #> 359                 Dilwer Jackson            PS-102 2   old       FALSE
    #> 360                 Abdul Razzaque             PS-54 2   old       FALSE
    #> 361           Mehmood Abdul Razzaq            PS-105 2   old       FALSE
    #> 362                Dilawar Jeckson            PS-106 2   old       FALSE
    #> 363                  Ghulam Hashim            PS-106 2   old       FALSE
    #> 364                    Abdul Latif             PS-63 2   old       FALSE
    #> 365                    Abdul Latif            PS-107 2   old       FALSE
    #> 366                Muhammad Jawaid            PS-107 2   old       FALSE
    #> 367                Syed Anwer Shah            PS-116 2   old       FALSE
    #> 368                Muhammad Mohsin             PP-61 2   old       FALSE
    #> 369                Muhammad Mohsin             PS-13 2   old       FALSE
    #> 370                    Sarang Khan             PS-19 2   old       FALSE
    #> 371                    Sarang Khan             PS-59 2   old       FALSE
    #> 372                      Ali Murad             PS-20 2   old       FALSE
    #> 373                      Ali Murad             PS-51 2   old       FALSE
    #> 374                 Ahmad Ali Shah             PS-30 2   old        TRUE
    #> 375             Ghulam Shabir Shah              PS-3 2   new        TRUE
    #> 376                        Ali Bux             PS-40 2   old       FALSE
    #> 377                        Ali Bux             PS-53 2   old       FALSE
    #> 378                  Raza Muhammad             PS-43 2   old       FALSE
    #> 379                  Raza Muhammad             PS-67 2   old       FALSE
    #> 380                   Abdul Hameed             PS-63 2   old        TRUE
    #> 381           Abdul Hameed Panhwar             PS-78 2   new        TRUE
    #> 382            Meer Hassan Panhyar             PS-80 2   new        TRUE
    #> 383     Mehmood Ur Rehman Halepoto             PS-80 2   new        TRUE
    #> 384                           Dayo             PS-81 2   new        TRUE
    #> 385                    Giyanoo Mal             PS-81 2   new        TRUE
    #> 386          Dost Muhammad Solangi             PS-84 2   new        TRUE
    #> 387              Sadaqat Ali Jatoi             PS-84 2   new        TRUE
    #> 388               Muhammad Ibrahim             PS-67 2   old        TRUE
    #> 389               Muhammad Ibrahim             PS-88 2   new        TRUE
    #> 390              Amad U Allah Awan            PP-246 6   old       FALSE
    #> 391                Amad Ullah Awan            PP-246 6   old       FALSE
    #> 392                Amad Ullha Awan            PP-246 6   old       FALSE
    #> 393             Amadeus Ullah Awam            PP-246 6   old       FALSE
    #> 394                 Amadullah Awan            PP-246 6   old       FALSE
    #> 395                  Amadullahawan            PP-246 6   old       FALSE
    #>     only_one_old_some_new
    #> 1                   FALSE
    #> 2                   FALSE
    #> 3                   FALSE
    #> 4                   FALSE
    #> 5                   FALSE
    #> 6                   FALSE
    #> 7                   FALSE
    #> 8                   FALSE
    #> 9                   FALSE
    #> 10                  FALSE
    #> 11                  FALSE
    #> 12                  FALSE
    #> 13                  FALSE
    #> 14                  FALSE
    #> 15                  FALSE
    #> 16                  FALSE
    #> 17                  FALSE
    #> 18                  FALSE
    #> 19                  FALSE
    #> 20                  FALSE
    #> 21                  FALSE
    #> 22                  FALSE
    #> 23                  FALSE
    #> 24                   TRUE
    #> 25                   TRUE
    #> 26                   TRUE
    #> 27                   TRUE
    #> 28                   TRUE
    #> 29                   TRUE
    #> 30                   TRUE
    #> 31                   TRUE
    #> 32                   TRUE
    #> 33                   TRUE
    #> 34                   TRUE
    #> 35                   TRUE
    #> 36                   TRUE
    #> 37                   TRUE
    #> 38                   TRUE
    #> 39                   TRUE
    #> 40                   TRUE
    #> 41                   TRUE
    #> 42                   TRUE
    #> 43                   TRUE
    #> 44                   TRUE
    #> 45                   TRUE
    #> 46                   TRUE
    #> 47                   TRUE
    #> 48                   TRUE
    #> 49                   TRUE
    #> 50                   TRUE
    #> 51                   TRUE
    #> 52                   TRUE
    #> 53                   TRUE
    #> 54                   TRUE
    #> 55                   TRUE
    #> 56                   TRUE
    #> 57                   TRUE
    #> 58                   TRUE
    #> 59                   TRUE
    #> 60                   TRUE
    #> 61                   TRUE
    #> 62                   TRUE
    #> 63                   TRUE
    #> 64                   TRUE
    #> 65                   TRUE
    #> 66                   TRUE
    #> 67                   TRUE
    #> 68                   TRUE
    #> 69                   TRUE
    #> 70                   TRUE
    #> 71                   TRUE
    #> 72                   TRUE
    #> 73                   TRUE
    #> 74                   TRUE
    #> 75                   TRUE
    #> 76                   TRUE
    #> 77                   TRUE
    #> 78                   TRUE
    #> 79                   TRUE
    #> 80                   TRUE
    #> 81                   TRUE
    #> 82                  FALSE
    #> 83                  FALSE
    #> 84                  FALSE
    #> 85                  FALSE
    #> 86                  FALSE
    #> 87                  FALSE
    #> 88                  FALSE
    #> 89                  FALSE
    #> 90                  FALSE
    #> 91                  FALSE
    #> 92                  FALSE
    #> 93                  FALSE
    #> 94                  FALSE
    #> 95                  FALSE
    #> 96                  FALSE
    #> 97                  FALSE
    #> 98                  FALSE
    #> 99                  FALSE
    #> 100                 FALSE
    #> 101                 FALSE
    #> 102                 FALSE
    #> 103                 FALSE
    #> 104                 FALSE
    #> 105                 FALSE
    #> 106                 FALSE
    #> 107                 FALSE
    #> 108                 FALSE
    #> 109                 FALSE
    #> 110                 FALSE
    #> 111                 FALSE
    #> 112                 FALSE
    #> 113                 FALSE
    #> 114                 FALSE
    #> 115                 FALSE
    #> 116                 FALSE
    #> 117                 FALSE
    #> 118                 FALSE
    #> 119                 FALSE
    #> 120                 FALSE
    #> 121                 FALSE
    #> 122                 FALSE
    #> 123                 FALSE
    #> 124                 FALSE
    #> 125                 FALSE
    #> 126                 FALSE
    #> 127                 FALSE
    #> 128                 FALSE
    #> 129                 FALSE
    #> 130                 FALSE
    #> 131                 FALSE
    #> 132                 FALSE
    #> 133                 FALSE
    #> 134                 FALSE
    #> 135                 FALSE
    #> 136                 FALSE
    #> 137                 FALSE
    #> 138                 FALSE
    #> 139                 FALSE
    #> 140                 FALSE
    #> 141                 FALSE
    #> 142                 FALSE
    #> 143                 FALSE
    #> 144                 FALSE
    #> 145                 FALSE
    #> 146                 FALSE
    #> 147                 FALSE
    #> 148                 FALSE
    #> 149                 FALSE
    #> 150                 FALSE
    #> 151                 FALSE
    #> 152                 FALSE
    #> 153                 FALSE
    #> 154                 FALSE
    #> 155                 FALSE
    #> 156                 FALSE
    #> 157                 FALSE
    #> 158                 FALSE
    #> 159                 FALSE
    #> 160                 FALSE
    #> 161                 FALSE
    #> 162                 FALSE
    #> 163                 FALSE
    #> 164                 FALSE
    #> 165                 FALSE
    #> 166                 FALSE
    #> 167                 FALSE
    #> 168                 FALSE
    #> 169                 FALSE
    #> 170                 FALSE
    #> 171                 FALSE
    #> 172                 FALSE
    #> 173                 FALSE
    #> 174                 FALSE
    #> 175                 FALSE
    #> 176                 FALSE
    #> 177                 FALSE
    #> 178                 FALSE
    #> 179                 FALSE
    #> 180                 FALSE
    #> 181                 FALSE
    #> 182                 FALSE
    #> 183                 FALSE
    #> 184                 FALSE
    #> 185                 FALSE
    #> 186                 FALSE
    #> 187                 FALSE
    #> 188                 FALSE
    #> 189                 FALSE
    #> 190                 FALSE
    #> 191                 FALSE
    #> 192                 FALSE
    #> 193                 FALSE
    #> 194                 FALSE
    #> 195                 FALSE
    #> 196                 FALSE
    #> 197                 FALSE
    #> 198                 FALSE
    #> 199                 FALSE
    #> 200                 FALSE
    #> 201                 FALSE
    #> 202                 FALSE
    #> 203                 FALSE
    #> 204                 FALSE
    #> 205                 FALSE
    #> 206                 FALSE
    #> 207                 FALSE
    #> 208                 FALSE
    #> 209                 FALSE
    #> 210                 FALSE
    #> 211                 FALSE
    #> 212                 FALSE
    #> 213                 FALSE
    #> 214                 FALSE
    #> 215                 FALSE
    #> 216                 FALSE
    #> 217                 FALSE
    #> 218                 FALSE
    #> 219                 FALSE
    #> 220                 FALSE
    #> 221                 FALSE
    #> 222                 FALSE
    #> 223                 FALSE
    #> 224                 FALSE
    #> 225                 FALSE
    #> 226                 FALSE
    #> 227                 FALSE
    #> 228                 FALSE
    #> 229                 FALSE
    #> 230                 FALSE
    #> 231                 FALSE
    #> 232                 FALSE
    #> 233                 FALSE
    #> 234                 FALSE
    #> 235                 FALSE
    #> 236                 FALSE
    #> 237                 FALSE
    #> 238                 FALSE
    #> 239                 FALSE
    #> 240                 FALSE
    #> 241                 FALSE
    #> 242                 FALSE
    #> 243                 FALSE
    #> 244                 FALSE
    #> 245                 FALSE
    #> 246                 FALSE
    #> 247                 FALSE
    #> 248                 FALSE
    #> 249                 FALSE
    #> 250                 FALSE
    #> 251                 FALSE
    #> 252                 FALSE
    #> 253                 FALSE
    #> 254                 FALSE
    #> 255                 FALSE
    #> 256                 FALSE
    #> 257                 FALSE
    #> 258                 FALSE
    #> 259                 FALSE
    #> 260                 FALSE
    #> 261                 FALSE
    #> 262                 FALSE
    #> 263                 FALSE
    #> 264                 FALSE
    #> 265                 FALSE
    #> 266                 FALSE
    #> 267                 FALSE
    #> 268                 FALSE
    #> 269                 FALSE
    #> 270                 FALSE
    #> 271                 FALSE
    #> 272                 FALSE
    #> 273                 FALSE
    #> 274                 FALSE
    #> 275                 FALSE
    #> 276                 FALSE
    #> 277                 FALSE
    #> 278                 FALSE
    #> 279                 FALSE
    #> 280                 FALSE
    #> 281                 FALSE
    #> 282                 FALSE
    #> 283                 FALSE
    #> 284                 FALSE
    #> 285                 FALSE
    #> 286                 FALSE
    #> 287                 FALSE
    #> 288                 FALSE
    #> 289                 FALSE
    #> 290                 FALSE
    #> 291                 FALSE
    #> 292                 FALSE
    #> 293                 FALSE
    #> 294                 FALSE
    #> 295                 FALSE
    #> 296                 FALSE
    #> 297                 FALSE
    #> 298                 FALSE
    #> 299                 FALSE
    #> 300                 FALSE
    #> 301                 FALSE
    #> 302                 FALSE
    #> 303                 FALSE
    #> 304                 FALSE
    #> 305                 FALSE
    #> 306                 FALSE
    #> 307                 FALSE
    #> 308                 FALSE
    #> 309                 FALSE
    #> 310                 FALSE
    #> 311                 FALSE
    #> 312                 FALSE
    #> 313                 FALSE
    #> 314                 FALSE
    #> 315                 FALSE
    #> 316                 FALSE
    #> 317                 FALSE
    #> 318                 FALSE
    #> 319                 FALSE
    #> 320                 FALSE
    #> 321                 FALSE
    #> 322                 FALSE
    #> 323                 FALSE
    #> 324                 FALSE
    #> 325                 FALSE
    #> 326                 FALSE
    #> 327                 FALSE
    #> 328                 FALSE
    #> 329                 FALSE
    #> 330                 FALSE
    #> 331                 FALSE
    #> 332                 FALSE
    #> 333                 FALSE
    #> 334                 FALSE
    #> 335                 FALSE
    #> 336                 FALSE
    #> 337                 FALSE
    #> 338                 FALSE
    #> 339                 FALSE
    #> 340                 FALSE
    #> 341                 FALSE
    #> 342                 FALSE
    #> 343                 FALSE
    #> 344                 FALSE
    #> 345                 FALSE
    #> 346                 FALSE
    #> 347                 FALSE
    #> 348                 FALSE
    #> 349                 FALSE
    #> 350                 FALSE
    #> 351                 FALSE
    #> 352                 FALSE
    #> 353                 FALSE
    #> 354                 FALSE
    #> 355                 FALSE
    #> 356                 FALSE
    #> 357                 FALSE
    #> 358                 FALSE
    #> 359                 FALSE
    #> 360                 FALSE
    #> 361                 FALSE
    #> 362                 FALSE
    #> 363                 FALSE
    #> 364                 FALSE
    #> 365                 FALSE
    #> 366                 FALSE
    #> 367                 FALSE
    #> 368                 FALSE
    #> 369                 FALSE
    #> 370                 FALSE
    #> 371                 FALSE
    #> 372                 FALSE
    #> 373                 FALSE
    #> 374                 FALSE
    #> 375                 FALSE
    #> 376                 FALSE
    #> 377                 FALSE
    #> 378                 FALSE
    #> 379                 FALSE
    #> 380                 FALSE
    #> 381                 FALSE
    #> 382                 FALSE
    #> 383                 FALSE
    #> 384                 FALSE
    #> 385                 FALSE
    #> 386                 FALSE
    #> 387                 FALSE
    #> 388                 FALSE
    #> 389                 FALSE
    #> 390                 FALSE
    #> 391                 FALSE
    #> 392                 FALSE
    #> 393                 FALSE
    #> 394                 FALSE
    #> 395                 FALSE

    # resolve those with only one old and some new data entry waves, all in the same constituency
    old_new_merge <- dup_uids %>%
      filter(only_one_old_some_new) %>%
      mutate(uid_dup_resolved = TRUE,
             uid_dup_notes = ifelse(entry == "old",
                                    "keeper_new_old_dup",
                                    "dropper_new_old_dup"),
             uid_dup_drop = entry != "old") %>%
      ungroup() %>%
      select(key, starts_with("uid_dup"))

Resolve those in two different constituencies, not with one old and two
new.

    # Check those in different constituencies
    two_cons <- dup_uids %>%
      filter(length(unique(constituency_code)) > 1 & !only_one_old_some_new) %>%
      select(key, uid, candidate_name, constituency_code, entry) %>%
      left_join(select(res_u, constituency_code, uid, candidate_name), by = c("uid", "constituency_code"))

    uid_found <- two_cons %>%
      filter(!is.na(candidate_name.y)) # these people we want to keep as is
    uid_notfound <- two_cons %>% 
      filter(is.na(candidate_name.y)) # these people we need to find a new UID for

    # these ones we are looking for name-constituency matches, and their new
    # uids will be appended here
    change_uid <- uid_notfound %>%
      ungroup() %>%
      mutate(candidate_name = tolower(candidate_name.x)) %>%
      rename(uid_old = uid) %>%
      select(-candidate_name.x, -candidate_name.y) %>%
      left_join(res_u %>%
                  mutate(candidate_name = tolower(candidate_name)) %>%
                  filter(!found_initially) %>%
                  select(constituency_code, uid, candidate_name),
                by = c("candidate_name", "constituency_code"))


    # mark those in the right place as resolved
    correct_uid_merge <- uid_found %>%
      ungroup() %>%
      select(key) %>%
      mutate(uid_dup_resolved = TRUE,
             uid_dup_notes = "correct_uid",
             uid_dup_drop = FALSE)

    # mark those who I could match on constituency and name as resolved
    cons_matched_uid_merge <- change_uid %>%
      filter(!is.na(uid)) %>%
      select(uid, key) %>%
      mutate(uid_dup_resolved = TRUE,
             uid_dup_notes = "matched_by_name_constituency_to_previously_unmatched_result_uid",
             uid_dup_drop = FALSE) %>%
      rename(uid_dup_new = uid)

Now we have to do some manual work to find those that are still needed
and to resolve duplicate UIDs within constituency.

    still_needed <- filter(change_uid, is.na(uid))
    table(still_needed$constituency_code, still_needed$entry)

    #>         
    #>          old
    #>   NA-120   1
    #>   NA-138   3
    #>   NA-161   1
    #>   PK-29    4
    #>   PK-30    8
    #>   PK-31   13
    #>   PK-32    9
    #>   PK-33   13
    #>   PK-34   17
    #>   PK-35   12
    #>   PP-128   1
    #>   PP-142   1
    #>   PP-157   1
    #>   PP-61    2
    #>   PS-116   1
    #>   PS-48    1
    #>   PS-49    1
    #>   PS-50    2
    #>   PS-51    1
    #>   PS-52    1
    #>   PS-53    1
    #>   PS-54    1
    #>   PS-55    1
    #>   PS-59    2
    #>   PS-63    3
    #>   PS-64    1
    #>   PS-67    4
    #>   PS-91    1
    #>   PS-92    3
    #>   PS-93    2

    # PK-30 here is actually NA-30
    still_needed %>%
      filter(constituency_code == "PK-30")

    #> # A tibble: 8 x 6
    #>   key                 uid_old constituency_co… entry candidate_name  uid  
    #>   <chr>               <chr>   <chr>            <chr> <chr>           <chr>
    #> 1 uuid:0593ae23-f9e5… 02384   PK-30            old   arbab alamgir … <NA> 
    #> 2 uuid:e0f014ed-5a6d… 02385   PK-30            old   arbab najeeb u… <NA> 
    #> 3 uuid:c9cc3860-2a9d… 02386   PK-30            old   mailk haider k… <NA> 
    #> 4 uuid:0669755d-4f41… 02387   PK-30            old   alamgir khalil  <NA> 
    #> 5 uuid:a59ac673-581c… 02388   PK-30            old   m.junaid        <NA> 
    #> 6 uuid:7457d5e3-083f… 02389   PK-30            old   m.shoukat       <NA> 
    #> 7 uuid:b8990d47-4519… 02390   PK-30            old   noor wali khan  <NA> 
    #> 8 uuid:79c7f4eb-0c50… 02391   PK-30            old   sher ali        <NA>

    # 
    # dup_uid <- map_dfr(unique(still_needed$constituency_code), ~ {
    #   m <- ""
    #   ccode <- .x
    #   while (!(m %in% c("y","n", "w"))) {
    #     still_needed %>%
    #       filter(constituency_code == .x) %>%
    #       as.data.frame %>%
    #       print
    #     filter(res_u, constituency_code == ccode) %>%
    #       arrange(uid) %>%
    #       select(constituency_code, candidate_name, uid, found_initially) %>%
    #       as.data.frame %>%
    #       print
    #     m <- readline("Match (y/n/na/w)?")
    #     if (m == "na") {
    #       ccode <- gsub("P[KFSBP]", "NA", ccode)
    #     }
    #   }
    #   if (m == "n") {
    #     data.frame(type = "no_match", constituency_code = .x)
    #   } else if (m == "w") {
    #     data.frame(type = "na_instead", constituency_code = .x)
    #   } else if (m == "y") {
    #     data.frame(type = "a_match", constituency_code = .x)
    #   } else {
    #     NULL
    #   }
    # })

    # the below is generated from the above manual checks on 2018-11-01
    dup_uid <- structure(list(type = c("no_match", "no_match", "no_match", "no_match", 
    "no_match", "na_instead", "na_instead", "na_instead", "na_instead", 
    "na_instead", "na_instead", "na_instead", "no_match", "no_match", 
    "a_match", "no_match", "a_match", "a_match", "a_match", "a_match", 
    "no_match", "no_match", "no_match", "a_match", "a_match", "no_match", 
    "no_match", "a_match", "a_match", "a_match", "no_match"), constituency_code = c("NA-120", 
    "PS-49", "PS-91", "NA-138", "NA-161", "PK-29", "PK-30", "PK-31", 
    "PK-32", "PK-33", "PK-34", "PK-35", "NA-34", "PS-54", "PS-50", 
    "PS-48", "PS-92", "PS-93", "PS-59", "PS-67", "PP-142", "PP-157", 
    "PS-64", "PP-128", "PS-52", "PP-61", "PS-63", "PS-55", "PS-116", 
    "PS-51", "PS-53")), row.names = c(NA, -31L), class = "data.frame")

    # Duplicate UIDs that were not found, so leave UID as NA but should
    # match CNICs if it is the same person
    not_found <- still_needed %>%
      filter(constituency_code %in% dup_uid$constituency_code[dup_uid$type == "no_match"])

    not_found_merge <- not_found %>%
        mutate(uid_dup_resolved = TRUE,
             uid_dup_notes = "not_found_set_uid_NA",
             uid_dup_drop = FALSE) %>%
      ungroup() %>%
      select(key, starts_with("uid_dup"))

    # Ones found that just need a manual match
    found <- still_needed %>%
      filter(constituency_code %in% dup_uid$constituency_code[dup_uid$type == "a_match"])

    # check_match <- pmap_dfr(found, function(constituency_code, ...) {
    #   
    #   ret_dat <- data.frame(constituency_code = constituency_code,
    #                         ...)
    #   
    #   print(ret_dat)
    #   val <- constituency_code
    #   filter(res_u, constituency_code == val) %>%
    #     arrange(uid) %>%
    #     select(constituency_code, candidate_name, uid, found_initially) %>%
    #     as.data.frame %>%
    #     print
    # 
    #   m <- readline("correct uid: ")
    #   if (m == "q") stop()
    #   ret_dat$uid_dup_new <- m
    #   ret_dat
    # })

    check_match <- structure(list(constituency_code = c("PS-50", "PS-92", "PS-92", 
    "PS-92", "PS-93", "PS-93", "PS-59", "PS-67", "PS-50", "PP-128", 
    "PS-52", "PS-55", "PS-67", "PS-116", "PS-59", "PS-51", "PS-67", 
    "PS-67"), key = c("uuid:a1940237-fd4f-4794-aeba-ca299ec72627", 
    "uuid:ce02537b-8c33-442a-b5d0-2f0e416d8290", "uuid:09d611e2-b0c1-4360-8433-0b6bf2aa54e3", 
    "uuid:e6d2c092-21e9-4b15-8df1-b518209d8700", "uuid:57341afc-e4ec-4b4f-a1dc-507ed4c7827f", 
    "uuid:db60e2fb-6bd3-4c82-90dd-5aa68ba3319e", "uuid:9ef406c4-14a9-415c-9a76-74e8d5b70532", 
    "uuid:57e2b083-c7e3-4f1d-bb89-a0cfd5aa54ba", "uuid:7b6d79d8-20cd-4768-8f30-ec718baf3974", 
    "uuid:29a0db7a-0918-434a-af3f-fc579e949db1", "uuid:a4218b6c-0d12-4901-99e0-9f55dbeb219f", 
    "uuid:bd4d4058-23e2-46d1-9b25-62105cb29e08", "uuid:dc851e8a-2317-45f6-8ffc-29e7c6f23d15", 
    "uuid:b5db77a7-756a-4dbb-b3ec-2f77c4781db6", "uuid:f5a1990a-0e97-4dac-93bd-57e926193b79", 
    "uuid:f8bc615d-5c8b-4562-9b4b-e64a2e7b2e80", "uuid:4151a6a8-4efd-40a2-b51f-1376927ed6f7", 
    "uuid:961a5fbc-fc20-4d15-90c1-b038ac7465ff"), uid_old = c("04440", 
    "05687", "05688", "05692", "05694", "05695", "05859", "06160", 
    "06178", "06968", "07051", "09406", "09406", "09671", "10278", 
    "10298", "10715", "11414"), entry = structure(c(1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "old", class = "factor"), 
        candidate_name = c("sajjad ahmad", "shafeeq ahmed", "sahid kursheed rana", 
        "syed muhammad rafiq", "almasazam", "asfag ahmend mangi", 
        "saeed ahmad", "waseem ahmad", "mukhtar ahmad", "muhammad pervaiz", 
        "gjulam qadir", "muhammad haroon", "muhammad haroon", "syed anwer shah", 
        "sarang khan", "ali murad", "raza muhammad", "muhammad ibrahim"
        ), uid = structure(c(NA_integer_, NA_integer_, NA_integer_, 
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, 
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, 
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_
        ), .Label = character(0), class = "factor"), uid_dup_new = c("10845", 
        "11515", "11516", "11520", "11522", "11523", "10976", "no_match", 
        "10841", "05968", "10866", "NA_cand", "no_match", "09871", 
        "10977", "NA_cand", "11126", "11122")), row.names = c(NA, -18L
    ), class = "data.frame")
    check_match

    #>    constituency_code                                       key uid_old
    #> 1              PS-50 uuid:a1940237-fd4f-4794-aeba-ca299ec72627   04440
    #> 2              PS-92 uuid:ce02537b-8c33-442a-b5d0-2f0e416d8290   05687
    #> 3              PS-92 uuid:09d611e2-b0c1-4360-8433-0b6bf2aa54e3   05688
    #> 4              PS-92 uuid:e6d2c092-21e9-4b15-8df1-b518209d8700   05692
    #> 5              PS-93 uuid:57341afc-e4ec-4b4f-a1dc-507ed4c7827f   05694
    #> 6              PS-93 uuid:db60e2fb-6bd3-4c82-90dd-5aa68ba3319e   05695
    #> 7              PS-59 uuid:9ef406c4-14a9-415c-9a76-74e8d5b70532   05859
    #> 8              PS-67 uuid:57e2b083-c7e3-4f1d-bb89-a0cfd5aa54ba   06160
    #> 9              PS-50 uuid:7b6d79d8-20cd-4768-8f30-ec718baf3974   06178
    #> 10            PP-128 uuid:29a0db7a-0918-434a-af3f-fc579e949db1   06968
    #> 11             PS-52 uuid:a4218b6c-0d12-4901-99e0-9f55dbeb219f   07051
    #> 12             PS-55 uuid:bd4d4058-23e2-46d1-9b25-62105cb29e08   09406
    #> 13             PS-67 uuid:dc851e8a-2317-45f6-8ffc-29e7c6f23d15   09406
    #> 14            PS-116 uuid:b5db77a7-756a-4dbb-b3ec-2f77c4781db6   09671
    #> 15             PS-59 uuid:f5a1990a-0e97-4dac-93bd-57e926193b79   10278
    #> 16             PS-51 uuid:f8bc615d-5c8b-4562-9b4b-e64a2e7b2e80   10298
    #> 17             PS-67 uuid:4151a6a8-4efd-40a2-b51f-1376927ed6f7   10715
    #> 18             PS-67 uuid:961a5fbc-fc20-4d15-90c1-b038ac7465ff   11414
    #>    entry      candidate_name  uid uid_dup_new
    #> 1    old        sajjad ahmad <NA>       10845
    #> 2    old       shafeeq ahmed <NA>       11515
    #> 3    old sahid kursheed rana <NA>       11516
    #> 4    old syed muhammad rafiq <NA>       11520
    #> 5    old           almasazam <NA>       11522
    #> 6    old  asfag ahmend mangi <NA>       11523
    #> 7    old         saeed ahmad <NA>       10976
    #> 8    old        waseem ahmad <NA>    no_match
    #> 9    old       mukhtar ahmad <NA>       10841
    #> 10   old    muhammad pervaiz <NA>       05968
    #> 11   old        gjulam qadir <NA>       10866
    #> 12   old     muhammad haroon <NA>     NA_cand
    #> 13   old     muhammad haroon <NA>    no_match
    #> 14   old     syed anwer shah <NA>       09871
    #> 15   old         sarang khan <NA>       10977
    #> 16   old           ali murad <NA>     NA_cand
    #> 17   old       raza muhammad <NA>       11126
    #> 18   old    muhammad ibrahim <NA>       11122

    check_cand_merge <- check_match %>%
        mutate(uid_dup_resolved = TRUE,
             uid_dup_notes = case_when(
               uid_dup_new == "no_match" ~ "no_match_found_set_UID_NA",
               uid_dup_new == "NA_cand" ~ "uid_NA_in_res",
               TRUE ~ "found_uid"
             ),
             uid_dup_drop = FALSE) %>%
      ungroup() %>%
      select(key, starts_with("uid_dup"))

    # Ones found that are actually MNA candidates
    change_constituency <- still_needed %>%
      filter(constituency_code %in% dup_uid$constituency_code[dup_uid$type == "na_instead"])
    # seems like most of these were accurately reentered as NA candidates, let's check! They all are now thanks to 
    # the one typo fix above, so we can mark as fixed
    change_constituency[!(change_constituency$uid_old %in% uid_found$uid),]

    #> # A tibble: 0 x 6
    #> # ... with 6 variables: key <chr>, uid_old <chr>, constituency_code <chr>,
    #> #   entry <chr>, candidate_name <chr>, uid <chr>

    new_cons_merge <- change_constituency %>%
        mutate(uid_dup_resolved = TRUE,
             uid_dup_notes = "old_entries_PK_instead_of_NA",
             uid_dup_drop = TRUE) %>%
      ungroup() %>%
      select(key, starts_with("uid_dup"))

    # 6 new rows from fixing uid padding issue!
    done <- bind_rows(
      not_found_merge, 
      new_cons_merge,
      check_cand_merge
    )

    need <- still_needed %>%
      filter(!(key %in% done$key))
    if (nrow(need) != 0) {
      stop("Still some needed")
    }

Now check those duplicates in one constituency.

    same_cons <- dup_uids %>%
      filter(length(unique(constituency_code)) == 1 & !only_one_old_some_new) %>%
      select(key, enum_name, uid, candidate_name, constituency_code, entry)
    same_cons

    #> # A tibble: 93 x 6
    #> # Groups:   uid [44]
    #>    key              enum_name uid   candidate_name  constituency_co… entry
    #>    <chr>                <dbl> <chr> <chr>           <chr>            <chr>
    #>  1 uuid:b4cacdd0-1…       137 00499 Anis Qureshi    NA-138           old  
    #>  2 uuid:4cd895a5-e…       137 00499 Dawood Anis Qu… NA-138           old  
    #>  3 uuid:062547f1-5…       131 00569 Raiz Ul Haq     NA-142           old  
    #>  4 uuid:b6cdead7-1…       131 00569 Sheraz Zafar    NA-142           old  
    #>  5 uuid:dd95cd3d-a…       107 01009 Muhammad Arshad NA-180           old  
    #>  6 uuid:8cfd91c8-2…       107 01009 Muhammad Arshad NA-180           old  
    #>  7 uuid:764b53e1-4…       105 01141 Abdul Rehman    NA-191           old  
    #>  8 uuid:ca7e9822-d…       105 01141 Abdul Rehman    NA-191           old  
    #>  9 uuid:0ec8e338-1…       105 01141 Dr Mian Abdul … NA-191           old  
    #> 10 uuid:399065a6-6…       105 01149 Parveen Akhtar  NA-191           old  
    #> # ... with 83 more rows

    # within_cons_dup <- map_dfr(unique(same_cons$uid), ~ {
    #   
    #   uid_dat <- filter(same_cons, uid == .x)
    #   
    #   print(uid_dat)
    #   filter(res_u, constituency_code %in% unique(uid_dat$constituency_code)) %>%
    #     arrange(uid) %>%
    #     select(constituency_code, candidate_name, uid, found_initially) %>%
    #     as.data.frame %>%
    #     print
    # 
    #   uid_dat$uid_dup_new <- NA_character_
    #   for (i in seq_len(nrow(uid_dat))) {
    #     m <- readline("correct uid: ")
    #     if (m == "q") {
    #       stop()
    #     } else if (m == "") {
    #       uid_dat$uid_dup_new[i] <- uid_dat$uid
    #     } else if (m == "n"){
    #       uid_dat$uid_dup_new[i] <- "not_found"
    #     } else {
    #       uid_dat$uid_dup_new[i] <- m
    #     }
    #   }
    #   uid_dat
    # })
    #04243 and 04244 have to be distinguished between
    #09326 and 09327
    #09764 and 09763, maybe?
    within_cons_dup <- structure(list(key = c("uuid:e79929a9-6144-4f86-b240-3e27a4db3783", 
    "uuid:24e35aec-90a5-4df2-930c-c0f0fab6a528", "uuid:746b8006-119f-4860-a2c2-3ad9afa8a6d7", 
    "uuid:4988f9d9-04e7-41ea-92d3-f501fb12ad75", "uuid:5aed9e42-bf88-4a24-8461-f8caf820e4fb", 
    "uuid:cda1234c-8605-465a-912a-737a83cb3f1e", "uuid:b4cacdd0-19df-48f1-ac87-c3e338bce9ff", 
    "uuid:4cd895a5-eff3-487f-ab26-73127e4ef681", "uuid:062547f1-5bb9-45b4-942f-6b426df40f7a", 
    "uuid:b6cdead7-1fb8-4413-9513-c36e7f282316", "uuid:dd95cd3d-a950-43f2-93d9-baf687fb9e0b", 
    "uuid:8cfd91c8-2517-4dde-8cac-0ea5fc38c8a8", "uuid:764b53e1-4604-426f-b0b5-c0aa889f94e6", 
    "uuid:ca7e9822-d75c-4b2e-8534-05168e329781", "uuid:0ec8e338-1e55-4456-ad6d-18a8df23ef8e", 
    "uuid:399065a6-69b4-4551-ae09-b7a0b6962a61", "uuid:144bd870-6bfe-4a6a-8d3f-fab9dc30fed9", 
    "uuid:99b3276f-495a-4fbc-9978-9c516af6fba7", "uuid:19bcfda9-4af2-490a-9656-7d143c5315cf", 
    "uuid:9e7d4937-f2d0-4db3-bf0d-941f4e09f2fd", "uuid:69a992aa-1ce6-41d0-ad7a-6ac78a09dedf", 
    "uuid:2e1b66a3-fe1e-4ccf-a46e-70ee464afde7", "uuid:36c73832-c0e1-4365-b205-506a30e6995f", 
    "uuid:be6236ad-3712-48bb-8752-455bc5abc1d2", "uuid:fd81a38f-e71f-401a-935f-e7a1bb28260c", 
    "uuid:82f70958-2c0f-472d-b47f-9cfe4deaac66", "uuid:0ebd7956-48e6-407d-8da6-bcdec782802a", 
    "uuid:c80ff99d-fde6-43d9-b592-9f0f9db36472", "uuid:f67de0fa-5cdd-474b-8f47-f946ccfc22d7", 
    "uuid:1ef57b7a-e5a9-44a6-96ef-59482e3fed77", "uuid:c0905220-7f76-423b-84a8-e2d2e57440d1", 
    "uuid:55b3e243-cfdc-487d-aa33-34ec96779227", "uuid:09547d1a-f37e-4eb7-8d86-84fa1d83a063", 
    "uuid:1f216579-b055-4541-820f-a7b28ea47b63", "uuid:754e322c-e51f-492a-9e3a-1d7fc5a17840", 
    "uuid:929e1d7f-5f41-45fe-a457-99dda58d0ae7", "uuid:afeb5e0f-ba25-44ac-9fa5-7f7d0aea20b3", 
    "uuid:c1e62d09-c6ee-4a73-97fa-6ae93e49c4f1", "uuid:14f33480-deeb-4969-be2b-5a24b01b6546", 
    "uuid:c5126dce-e795-4e9b-9774-1ba436d6f53b", "uuid:93ae491b-b4ba-4f01-9c82-0589c2269e52", 
    "uuid:4bbf4f1f-90e9-4fef-ae64-095d185f2e67", "uuid:c8f954a2-cab2-4b83-ab21-bff7dd63b9dd", 
    "uuid:7ddbe0ce-cf09-4803-ab77-b75b419fa37e", "uuid:95e1e23e-d56b-4d72-b5c6-7bfd85f98323", 
    "uuid:a6becc49-4561-4b71-9d64-8407331708d5", "uuid:6842380d-d7c2-43af-a0f7-9cac0fa0eba4", 
    "uuid:da4815b2-2009-419f-bf9b-89a0d1f32ca3", "uuid:95ce78b0-c4bc-4fc0-8090-c01df62f3e15", 
    "uuid:eeaa6319-35ee-48e5-8aaa-ea2578606563", "uuid:36a22575-03e7-4fd3-bd2d-4e9ec1a9fb08", 
    "uuid:b97057e8-0264-45f7-80cd-66c682886ced", "uuid:0c305dc5-a6de-4fc9-9546-572dd66a2c9c", 
    "uuid:f7c30b44-2253-4274-86fe-f525cf0cb89b", "uuid:79b797e4-94f6-444f-bb82-d56b164fb2f2", 
    "uuid:239c2f5d-0abd-4651-a943-d6080d85369a", "uuid:f94083d9-f933-49f2-bef8-db06eb3249ae", 
    "uuid:90b548a3-9a58-4bb8-bb89-483247492c60", "uuid:50ee75ef-39e1-4609-983f-e34b5086ca8d", 
    "uuid:0bde11f1-197b-416e-9310-8b7600e436c7", "uuid:4ebf0d5e-863c-4d63-b5e4-4d59d1996a3c", 
    "uuid:778a72ce-14e3-49b8-ae2c-ca2c13b77a65", "uuid:a4e765d1-5115-4de9-b9c5-116f6e5255c0", 
    "uuid:daffbd7d-ecdd-4c71-be15-2b9d4c27f452", "uuid:02808c34-bc9b-444f-a39c-6b2893ecfe38", 
    "uuid:40939c1d-47d3-49ca-b08d-996c91c07b01", "uuid:0a6af6cc-e2f0-482d-96f4-c86ea55916eb", 
    "uuid:4bf414b3-7503-447d-b995-cb06848412be", "uuid:367763e0-d43b-47cf-a9a9-1c107d902a36", 
    "uuid:11de2c70-af08-47ff-bb0a-67e52523c6e6", "uuid:0ff31a06-940a-41a1-bc69-778195bc852e", 
    "uuid:d0ed5a66-9854-4578-8570-127e0a1458a3", "uuid:8ab83e71-93e5-48cb-a6c1-c164bb5ecfdf", 
    "uuid:34f33af7-d928-4eed-81ce-c6abf3f206cb", "uuid:90c6b127-3b37-4666-9ffd-2c523efba18f", 
    "uuid:ee177327-a8f9-40cd-ba4f-db3dd249247e", "uuid:71ac1cfa-8670-4a74-b7e8-0d050c5a18f7", 
    "uuid:d6670190-0e5f-48cf-8c7d-65f8b61fd061", "uuid:aff75d29-3dc9-45c3-892f-1c2b45950b05", 
    "uuid:0c2ef92a-f727-48d1-8480-2057640b4d2c", "uuid:6fc43dc8-cafc-4ba8-8cbc-d11ebe2a5b4b", 
    "uuid:5efd3c61-4a49-4552-9e9c-b0c04846a18d", "uuid:6eac9aee-161f-40ac-923c-42fbd2f4ec13", 
    "uuid:80469606-2b9b-4e35-af8b-a534acd9d9c8", "uuid:44feceb8-ecf3-4bbc-ba92-0ca87a5e3a52", 
    "uuid:6fa2d5aa-0c24-4d69-bb9b-23281421d8b5", "uuid:8010874e-6cdb-48b2-a8c8-28dfda30f2dd", 
    "uuid:107a6198-30dc-40a9-ad57-c8719d18cb0a", "uuid:2fdd75eb-92cf-4577-8505-a9ed8fe26939", 
    "uuid:6330aa19-40a8-4157-b525-42ebe993f740", "uuid:fd65243c-6054-4a93-8772-b9a81b7e1c1f", 
    "uuid:1afc8923-8af2-4d50-b7b3-1525b9416cf6", "uuid:53bc7f75-53ea-4f2b-8868-0c936b85293d"
    ), enum_name = c(101, 106, 101, 104, 101, 101, 137, 137, 131, 
    131, 107, 107, 105, 105, 105, 105, 105, 104, 104, 117, 117, 128, 
    128, 132, 132, 118, 104, 124, 124, 124, 124, 125, 125, 130, 130, 
    109, 109, 109, 109, 113, 113, 108, 108, 106, 106, 106, 106, 106, 
    106, 106, 106, 106, 106, 144, 144, 777, 143, 131, 131, 126, 126, 
    128, 128, 122, 123, 143, 143, 128, 128, 140, 140, 142, 142, 157, 
    157, 145, 145, 124, 124, 124, 124, 107, 107, 148, 148, 148, 148, 
    114, 114, 114, 114, 114, 114), uid = c("00041", "00041", "00041", 
    "00041", "00041", "00041", "00499", "00499", "00569", "00569", 
    "01009", "01009", "01141", "01141", "01141", "01149", "01149", 
    "01537", "01537", "01543", "01543", "02184", "02184", "02884", 
    "02884", "03089", "03089", "03435", "03435", "04244", "04244", 
    "04558", "04558", "04912", "04912", "04921", "04921", "05039", 
    "05039", "05526", "05526", "05661", "05661", "06338", "06338", 
    "06339", "06339", "06342", "06342", "06343", "06343", "06344", 
    "06344", "06470", "06470", "06504", "06504", "07012", "07012", 
    "07212", "07212", "07489", "07489", "08924", "08924", "08927", 
    "08927", "09050", "09050", "09194", "09194", "09327", "09327", 
    "09557", "09557", "09639", "09639", "09763", "09763", "09820", 
    "09820", "10074", "10074", "10480", "10480", "10574", "10574", 
    "11304", "11304", "11316", "11316", "11366", "11366"), candidate_name = c("Amad U Allah Awan", 
    "Amad Ullah Awan", "Amad Ullha Awan", "Amadeus Ullah Awam", "Amadullah Awan", 
    "Amadullahawan", "Anis Qureshi", "Dawood Anis Qureshi", "Raiz Ul Haq", 
    "Sheraz Zafar", "Muhammad Arshad", "Muhammad Arshad", "Abdul Rehman", 
    "Abdul Rehman", "Dr Mian Abdul Rehman", "Parveen Akhtar", "Parveen Akthar", 
    "Shoaib Alam Khan", "Shoaib Khan", "M Younis Talpur", "Nawab M Yousaf Talpur", 
    "Bismillah Kakar", "Busmillah Kakar", "Sayed Zafar Ali", "Zaib Ur Rahman", 
    "Chaduary Abid Raza", "Chaudhary Abid Raza", "Jala Ahmad Khan", 
    "Jalil Khan", "M Rasool", "M Rasool", "Naseer U Din", "Naseer Ud Din", 
    "Nadeem Khan", "Nadeem Khan", "Maaz Ullah", "Maza Ullah", "Saif Ullah Khan", 
    "Saif Ullah Khan", "Tariq bashir raja", "Safadar ali khan", "Shokt Ali", 
    "Shukat", "Abdul Kareem Khan", "Malik Abdul Karim Khan", "M. Javaid Iqbal", 
    "M. Javed Iqbal Malik", "M. Tahir Iqbal", "Tahir Iqbal", "Tahir Mehmood", 
    "Tahir Mehmood", "Umer Tanveer", "Umer Tanvir", "Khawaja Salma Rafique", 
    "Kjawaja Salma Rafique", "M Mumtaz Khalid", "Muhammad Mumtaz Khalid", 
    "Muhhmad Moeen Watto", "Noor Ul Ameen Watto", "Waheed Asghar", 
    "Waheed Asghar", "Mian Amir Iqbal Shah", "Muhammad Iqbal Shah", 
    "Dr Sohail Zaffar", "Sohail Zafar Cheema", "Amir raza abbasi", 
    "Amjad arbab abbasi", "Sajjid Ahmad", "Sajjid Khan", "Zulafiqar Ali", 
    "Zulfiqar Ali", "Muhammad Arshad", "Muhammad Arshad", "Arsalan Taj", 
    "Dilwer Jackson", "Dilawar Jeckson", "Ghulam Hashim", "Muhammad Asif", 
    "Muhammad Asif", "Sultan Ahmad", "Sultan Ahmad", "Syed Mohammad Abbas Jafri", 
    "Syed Mohammad Abbas Jafri", "Shahnawaz", "Shahnawaz", "Zulfiqar Ali", 
    "Zulfiqar Ali", "Meer Hassan Panhyar", "Mehmood Ur Rehman Halepoto", 
    "Dayo", "Giyanoo Mal", "Dost Muhammad Solangi", "Sadaqat Ali Jatoi"
    ), constituency_code = c("PP-246", "PP-246", "PP-246", "PP-246", 
    "PP-246", "PP-246", "NA-138", "NA-138", "NA-142", "NA-142", "NA-180", 
    "NA-180", "NA-191", "NA-191", "NA-191", "NA-191", "NA-191", "NA-22", 
    "NA-22", "NA-220", "NA-220", "NA-263", "NA-263", "NA-53", "NA-53", 
    "NA-71", "NA-71", "PB-1", "PB-1", "PB-5", "PB-5", "PK-27", "PK-27", 
    "PK-55", "PK-55", "PK-56", "PK-56", "PK-66", "PK-66", "PP-10", 
    "PP-10", "PP-107", "PP-107", "PP-15", "PP-15", "PP-15", "PP-15", 
    "PP-15", "PP-15", "PP-15", "PP-15", "PP-15", "PP-15", "PP-157", 
    "PP-157", "PP-159", "PP-159", "PP-186", "PP-186", "PP-200", "PP-200", 
    "PP-224", "PP-224", "PP-59", "PP-59", "PP-6", "PP-6", "PP-67", 
    "PP-67", "PP-77", "PP-77", "PP-89", "PP-89", "PS-102", "PS-102", 
    "PS-106", "PS-106", "PS-111", "PS-111", "PS-113", "PS-113", "PS-125", 
    "PS-125", "PS-30", "PS-30", "PS-35", "PS-35", "PS-80", "PS-80", 
    "PS-81", "PS-81", "PS-84", "PS-84"), entry = c("old", "old", 
    "old", "old", "old", "old", "old", "old", "old", "old", "old", 
    "old", "old", "old", "old", "old", "old", "old", "old", "old", 
    "old", "old", "old", "old", "old", "old", "old", "old", "old", 
    "old", "old", "old", "old", "old", "old", "old", "old", "old", 
    "old", "new", "new", "old", "old", "old", "old", "old", "old", 
    "old", "old", "old", "old", "old", "old", "old", "old", "old", 
    "old", "old", "old", "old", "old", "old", "old", "old", "old", 
    "new", "new", "old", "old", "old", "old", "old", "old", "old", 
    "old", "old", "old", "old", "old", "old", "old", "old", "old", 
    "old", "old", "old", "old", "new", "new", "new", "new", "new", 
    "new"), uid_dup_new = c("not_found", "not_found", "not_found", 
    "not_found", "not_found", "not_found", "00499", "00499", "00569", 
    "00570", "01009", "01009", "01141", "01141", "01141", "01149", 
    "01149", "01537", "01537", "not_found", "01543", "02184", "02184", 
    "02883", "02884", "03089", "03089", "03435", "03435", "04243", 
    "04244", "04558", "04558", "04912", "04912", "04921", "04921", 
    "05039", "05039", "05527", "05526", "05661", "05661", "06338", 
    "06338", "06339", "06339", "06342", "06342", "06343", "06343", 
    "06344", "06344", "06470", "06470", "06504", "06504", "07012", 
    "07014", "07212", "07212", "07489", "07489", "08924", "08924", 
    "08927", "08927", "09050", "09050", "09194", "09194", "09326", 
    "09327", "09556", "09557", "09639", "09640", "09763", "09763", 
    "09820", "09820", "10074", "10074", "10480", "10480", "10574", 
    "10574", "11305", "11304", "11315", "11316", "11356", "11366"
    )), row.names = c(NA, -93L), class = c("grouped_df", "tbl_df", 
    "tbl", "data.frame"), vars = "uid", indices = list(0:5, 6:7, 
        8:9, 10:11, 12:14, 15:16, 17:18, 19:20, 21:22, 23:24, 25:26, 
        27:28, 29:30, 31:32, 33:34, 35:36, 37:38, 39:40, 41:42, 43:44, 
        45:46, 47:48, 49:50, 51:52, 53:54, 55:56, 57:58, 59:60, 61:62, 
        63:64, 65:66, 67:68, 69:70, 71:72, 73:74, 75:76, 77:78, 79:80, 
        81:82, 83:84, 85:86, 87:88, 89:90, 91:92), group_sizes = c(6L, 
    2L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), biggest_group_size = 6L, labels = structure(list(
        uid = c("00041", "00499", "00569", "01009", "01141", "01149", 
        "01537", "01543", "02184", "02884", "03089", "03435", "04244", 
        "04558", "04912", "04921", "05039", "05526", "05661", "06338", 
        "06339", "06342", "06343", "06344", "06470", "06504", "07012", 
        "07212", "07489", "08924", "08927", "09050", "09194", "09327", 
        "09557", "09639", "09763", "09820", "10074", "10480", "10574", 
        "11304", "11316", "11366")), row.names = c(NA, -44L), class = "data.frame", vars = "uid"))

    if (nrow(within_cons_dup) != nrow(same_cons)) {
      stop("new same cons dupes")
    }
    table(within_cons_dup$uid_dup_new)

    #> 
    #>     00499     00569     00570     01009     01141     01149     01537 
    #>         2         1         1         2         3         2         2 
    #>     01543     02184     02883     02884     03089     03435     04243 
    #>         1         2         1         1         2         2         1 
    #>     04244     04558     04912     04921     05039     05526     05527 
    #>         1         2         2         2         2         1         1 
    #>     05661     06338     06339     06342     06343     06344     06470 
    #>         2         2         2         2         2         2         2 
    #>     06504     07012     07014     07212     07489     08924     08927 
    #>         2         1         1         2         2         2         2 
    #>     09050     09194     09326     09327     09556     09557     09639 
    #>         2         2         1         1         1         1         1 
    #>     09640     09763     09820     10074     10480     10574     11304 
    #>         1         2         2         2         2         2         1 
    #>     11305     11315     11316     11356     11366 not_found 
    #>         1         1         1         1         1         7

    within_cons_dup_merge <- within_cons_dup %>%
        mutate(uid_dup_resolved = TRUE,
             uid_dup_notes = case_when(
               uid_dup_new == "not_found" ~ "no_match_in_constituency_duplicate",
               TRUE ~ "found_uid"
             ),
             uid_dup_drop = uid_dup_new == "not_found") %>%
      ungroup() %>%
      select(key, starts_with("uid_dup"))
    # TODO: figure out NAs in res from uid merge
    # TODO: handle these appropriately!

    # Merging in what to do
    clean_uids <- bind_rows(
      old_new_merge,
      correct_uid_merge,
      cons_matched_uid_merge,
      not_found_merge,
      new_cons_merge,
      check_cand_merge,
      within_cons_dup_merge
    )

    if (nrow(clean_uids) != 421) {
      stop("dupes number changed")
    }
    if (nrow(clean_uids) != nrow(dup_uids)) {
      stop("new dupes!")
    }
    table(duplicated(clean_uids$key))

    #> 
    #> FALSE 
    #>   421

    clean_uids %>%
      filter(!uid_dup_drop) %>%
      select(uid_dup_new, uid_dup_notes)  %>%
      as.data.frame

    #>     uid_dup_new
    #> 1          <NA>
    #> 2          <NA>
    #> 3          <NA>
    #> 4          <NA>
    #> 5          <NA>
    #> 6          <NA>
    #> 7          <NA>
    #> 8          <NA>
    #> 9          <NA>
    #> 10         <NA>
    #> 11         <NA>
    #> 12         <NA>
    #> 13         <NA>
    #> 14         <NA>
    #> 15         <NA>
    #> 16         <NA>
    #> 17         <NA>
    #> 18         <NA>
    #> 19         <NA>
    #> 20         <NA>
    #> 21         <NA>
    #> 22         <NA>
    #> 23         <NA>
    #> 24         <NA>
    #> 25         <NA>
    #> 26         <NA>
    #> 27         <NA>
    #> 28         <NA>
    #> 29         <NA>
    #> 30         <NA>
    #> 31         <NA>
    #> 32         <NA>
    #> 33         <NA>
    #> 34         <NA>
    #> 35         <NA>
    #> 36         <NA>
    #> 37         <NA>
    #> 38         <NA>
    #> 39         <NA>
    #> 40         <NA>
    #> 41         <NA>
    #> 42         <NA>
    #> 43         <NA>
    #> 44         <NA>
    #> 45         <NA>
    #> 46         <NA>
    #> 47         <NA>
    #> 48         <NA>
    #> 49         <NA>
    #> 50         <NA>
    #> 51         <NA>
    #> 52         <NA>
    #> 53         <NA>
    #> 54         <NA>
    #> 55         <NA>
    #> 56         <NA>
    #> 57         <NA>
    #> 58         <NA>
    #> 59         <NA>
    #> 60         <NA>
    #> 61         <NA>
    #> 62         <NA>
    #> 63         <NA>
    #> 64         <NA>
    #> 65         <NA>
    #> 66         <NA>
    #> 67         <NA>
    #> 68         <NA>
    #> 69         <NA>
    #> 70         <NA>
    #> 71         <NA>
    #> 72         <NA>
    #> 73         <NA>
    #> 74         <NA>
    #> 75         <NA>
    #> 76         <NA>
    #> 77         <NA>
    #> 78         <NA>
    #> 79         <NA>
    #> 80         <NA>
    #> 81         <NA>
    #> 82         <NA>
    #> 83         <NA>
    #> 84         <NA>
    #> 85         <NA>
    #> 86         <NA>
    #> 87         <NA>
    #> 88         <NA>
    #> 89         <NA>
    #> 90         <NA>
    #> 91         <NA>
    #> 92         <NA>
    #> 93         <NA>
    #> 94         <NA>
    #> 95         <NA>
    #> 96         <NA>
    #> 97         <NA>
    #> 98         <NA>
    #> 99         <NA>
    #> 100        <NA>
    #> 101        <NA>
    #> 102        <NA>
    #> 103        <NA>
    #> 104        <NA>
    #> 105        <NA>
    #> 106        <NA>
    #> 107        <NA>
    #> 108        <NA>
    #> 109        <NA>
    #> 110        <NA>
    #> 111        <NA>
    #> 112        <NA>
    #> 113        <NA>
    #> 114        <NA>
    #> 115        <NA>
    #> 116        <NA>
    #> 117        <NA>
    #> 118        <NA>
    #> 119        <NA>
    #> 120        <NA>
    #> 121        <NA>
    #> 122        <NA>
    #> 123        <NA>
    #> 124        <NA>
    #> 125        <NA>
    #> 126        <NA>
    #> 127        <NA>
    #> 128        <NA>
    #> 129        <NA>
    #> 130        <NA>
    #> 131        <NA>
    #> 132        <NA>
    #> 133        <NA>
    #> 134        <NA>
    #> 135        <NA>
    #> 136        <NA>
    #> 137        <NA>
    #> 138        <NA>
    #> 139        <NA>
    #> 140        <NA>
    #> 141        <NA>
    #> 142        <NA>
    #> 143        <NA>
    #> 144        <NA>
    #> 145        <NA>
    #> 146        <NA>
    #> 147        <NA>
    #> 148        <NA>
    #> 149        <NA>
    #> 150        <NA>
    #> 151        <NA>
    #> 152        <NA>
    #> 153        <NA>
    #> 154        <NA>
    #> 155        <NA>
    #> 156        <NA>
    #> 157        <NA>
    #> 158        <NA>
    #> 159        <NA>
    #> 160        <NA>
    #> 161        <NA>
    #> 162        <NA>
    #> 163        <NA>
    #> 164       11119
    #> 165       10895
    #> 166       10806
    #> 167       10797
    #> 168       10898
    #> 169       10924
    #> 170       10957
    #> 171       10960
    #> 172       10900
    #> 173       11033
    #> 174       10967
    #> 175       10882
    #> 176       10847
    #> 177       11514
    #> 178       11517
    #> 179       11518
    #> 180       11519
    #> 181       11521
    #> 182       07105
    #> 183       07502
    #> 184       10919
    #> 185       10896
    #> 186       10459
    #> 187        <NA>
    #> 188        <NA>
    #> 189        <NA>
    #> 190        <NA>
    #> 191        <NA>
    #> 192        <NA>
    #> 193        <NA>
    #> 194        <NA>
    #> 195        <NA>
    #> 196        <NA>
    #> 197        <NA>
    #> 198        <NA>
    #> 199        <NA>
    #> 200        <NA>
    #> 201        <NA>
    #> 202        <NA>
    #> 203        <NA>
    #> 204        <NA>
    #> 205       10845
    #> 206       11515
    #> 207       11516
    #> 208       11520
    #> 209       11522
    #> 210       11523
    #> 211       10976
    #> 212    no_match
    #> 213       10841
    #> 214       05968
    #> 215       10866
    #> 216     NA_cand
    #> 217    no_match
    #> 218       09871
    #> 219       10977
    #> 220     NA_cand
    #> 221       11126
    #> 222       11122
    #> 223       00499
    #> 224       00499
    #> 225       00569
    #> 226       00570
    #> 227       01009
    #> 228       01009
    #> 229       01141
    #> 230       01141
    #> 231       01141
    #> 232       01149
    #> 233       01149
    #> 234       01537
    #> 235       01537
    #> 236       01543
    #> 237       02184
    #> 238       02184
    #> 239       02883
    #> 240       02884
    #> 241       03089
    #> 242       03089
    #> 243       03435
    #> 244       03435
    #> 245       04243
    #> 246       04244
    #> 247       04558
    #> 248       04558
    #> 249       04912
    #> 250       04912
    #> 251       04921
    #> 252       04921
    #> 253       05039
    #> 254       05039
    #> 255       05527
    #> 256       05526
    #> 257       05661
    #> 258       05661
    #> 259       06338
    #> 260       06338
    #> 261       06339
    #> 262       06339
    #> 263       06342
    #> 264       06342
    #> 265       06343
    #> 266       06343
    #> 267       06344
    #> 268       06344
    #> 269       06470
    #> 270       06470
    #> 271       06504
    #> 272       06504
    #> 273       07012
    #> 274       07014
    #> 275       07212
    #> 276       07212
    #> 277       07489
    #> 278       07489
    #> 279       08924
    #> 280       08924
    #> 281       08927
    #> 282       08927
    #> 283       09050
    #> 284       09050
    #> 285       09194
    #> 286       09194
    #> 287       09326
    #> 288       09327
    #> 289       09556
    #> 290       09557
    #> 291       09639
    #> 292       09640
    #> 293       09763
    #> 294       09763
    #> 295       09820
    #> 296       09820
    #> 297       10074
    #> 298       10074
    #> 299       10480
    #> 300       10480
    #> 301       10574
    #> 302       10574
    #> 303       11305
    #> 304       11304
    #> 305       11315
    #> 306       11316
    #> 307       11356
    #> 308       11366
    #>                                                       uid_dup_notes
    #> 1                                                keeper_new_old_dup
    #> 2                                                keeper_new_old_dup
    #> 3                                                keeper_new_old_dup
    #> 4                                                keeper_new_old_dup
    #> 5                                                keeper_new_old_dup
    #> 6                                                keeper_new_old_dup
    #> 7                                                keeper_new_old_dup
    #> 8                                                keeper_new_old_dup
    #> 9                                                keeper_new_old_dup
    #> 10                                               keeper_new_old_dup
    #> 11                                               keeper_new_old_dup
    #> 12                                               keeper_new_old_dup
    #> 13                                               keeper_new_old_dup
    #> 14                                               keeper_new_old_dup
    #> 15                                               keeper_new_old_dup
    #> 16                                               keeper_new_old_dup
    #> 17                                               keeper_new_old_dup
    #> 18                                               keeper_new_old_dup
    #> 19                                               keeper_new_old_dup
    #> 20                                               keeper_new_old_dup
    #> 21                                               keeper_new_old_dup
    #> 22                                               keeper_new_old_dup
    #> 23                                               keeper_new_old_dup
    #> 24                                               keeper_new_old_dup
    #> 25                                               keeper_new_old_dup
    #> 26                                               keeper_new_old_dup
    #> 27                                               keeper_new_old_dup
    #> 28                                               keeper_new_old_dup
    #> 29                                               keeper_new_old_dup
    #> 30                                               keeper_new_old_dup
    #> 31                                                      correct_uid
    #> 32                                                      correct_uid
    #> 33                                                      correct_uid
    #> 34                                                      correct_uid
    #> 35                                                      correct_uid
    #> 36                                                      correct_uid
    #> 37                                                      correct_uid
    #> 38                                                      correct_uid
    #> 39                                                      correct_uid
    #> 40                                                      correct_uid
    #> 41                                                      correct_uid
    #> 42                                                      correct_uid
    #> 43                                                      correct_uid
    #> 44                                                      correct_uid
    #> 45                                                      correct_uid
    #> 46                                                      correct_uid
    #> 47                                                      correct_uid
    #> 48                                                      correct_uid
    #> 49                                                      correct_uid
    #> 50                                                      correct_uid
    #> 51                                                      correct_uid
    #> 52                                                      correct_uid
    #> 53                                                      correct_uid
    #> 54                                                      correct_uid
    #> 55                                                      correct_uid
    #> 56                                                      correct_uid
    #> 57                                                      correct_uid
    #> 58                                                      correct_uid
    #> 59                                                      correct_uid
    #> 60                                                      correct_uid
    #> 61                                                      correct_uid
    #> 62                                                      correct_uid
    #> 63                                                      correct_uid
    #> 64                                                      correct_uid
    #> 65                                                      correct_uid
    #> 66                                                      correct_uid
    #> 67                                                      correct_uid
    #> 68                                                      correct_uid
    #> 69                                                      correct_uid
    #> 70                                                      correct_uid
    #> 71                                                      correct_uid
    #> 72                                                      correct_uid
    #> 73                                                      correct_uid
    #> 74                                                      correct_uid
    #> 75                                                      correct_uid
    #> 76                                                      correct_uid
    #> 77                                                      correct_uid
    #> 78                                                      correct_uid
    #> 79                                                      correct_uid
    #> 80                                                      correct_uid
    #> 81                                                      correct_uid
    #> 82                                                      correct_uid
    #> 83                                                      correct_uid
    #> 84                                                      correct_uid
    #> 85                                                      correct_uid
    #> 86                                                      correct_uid
    #> 87                                                      correct_uid
    #> 88                                                      correct_uid
    #> 89                                                      correct_uid
    #> 90                                                      correct_uid
    #> 91                                                      correct_uid
    #> 92                                                      correct_uid
    #> 93                                                      correct_uid
    #> 94                                                      correct_uid
    #> 95                                                      correct_uid
    #> 96                                                      correct_uid
    #> 97                                                      correct_uid
    #> 98                                                      correct_uid
    #> 99                                                      correct_uid
    #> 100                                                     correct_uid
    #> 101                                                     correct_uid
    #> 102                                                     correct_uid
    #> 103                                                     correct_uid
    #> 104                                                     correct_uid
    #> 105                                                     correct_uid
    #> 106                                                     correct_uid
    #> 107                                                     correct_uid
    #> 108                                                     correct_uid
    #> 109                                                     correct_uid
    #> 110                                                     correct_uid
    #> 111                                                     correct_uid
    #> 112                                                     correct_uid
    #> 113                                                     correct_uid
    #> 114                                                     correct_uid
    #> 115                                                     correct_uid
    #> 116                                                     correct_uid
    #> 117                                                     correct_uid
    #> 118                                                     correct_uid
    #> 119                                                     correct_uid
    #> 120                                                     correct_uid
    #> 121                                                     correct_uid
    #> 122                                                     correct_uid
    #> 123                                                     correct_uid
    #> 124                                                     correct_uid
    #> 125                                                     correct_uid
    #> 126                                                     correct_uid
    #> 127                                                     correct_uid
    #> 128                                                     correct_uid
    #> 129                                                     correct_uid
    #> 130                                                     correct_uid
    #> 131                                                     correct_uid
    #> 132                                                     correct_uid
    #> 133                                                     correct_uid
    #> 134                                                     correct_uid
    #> 135                                                     correct_uid
    #> 136                                                     correct_uid
    #> 137                                                     correct_uid
    #> 138                                                     correct_uid
    #> 139                                                     correct_uid
    #> 140                                                     correct_uid
    #> 141                                                     correct_uid
    #> 142                                                     correct_uid
    #> 143                                                     correct_uid
    #> 144                                                     correct_uid
    #> 145                                                     correct_uid
    #> 146                                                     correct_uid
    #> 147                                                     correct_uid
    #> 148                                                     correct_uid
    #> 149                                                     correct_uid
    #> 150                                                     correct_uid
    #> 151                                                     correct_uid
    #> 152                                                     correct_uid
    #> 153                                                     correct_uid
    #> 154                                                     correct_uid
    #> 155                                                     correct_uid
    #> 156                                                     correct_uid
    #> 157                                                     correct_uid
    #> 158                                                     correct_uid
    #> 159                                                     correct_uid
    #> 160                                                     correct_uid
    #> 161                                                     correct_uid
    #> 162                                                     correct_uid
    #> 163                                                     correct_uid
    #> 164 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 165 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 166 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 167 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 168 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 169 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 170 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 171 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 172 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 173 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 174 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 175 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 176 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 177 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 178 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 179 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 180 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 181 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 182 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 183 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 184 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 185 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 186 matched_by_name_constituency_to_previously_unmatched_result_uid
    #> 187                                            not_found_set_uid_NA
    #> 188                                            not_found_set_uid_NA
    #> 189                                            not_found_set_uid_NA
    #> 190                                            not_found_set_uid_NA
    #> 191                                            not_found_set_uid_NA
    #> 192                                            not_found_set_uid_NA
    #> 193                                            not_found_set_uid_NA
    #> 194                                            not_found_set_uid_NA
    #> 195                                            not_found_set_uid_NA
    #> 196                                            not_found_set_uid_NA
    #> 197                                            not_found_set_uid_NA
    #> 198                                            not_found_set_uid_NA
    #> 199                                            not_found_set_uid_NA
    #> 200                                            not_found_set_uid_NA
    #> 201                                            not_found_set_uid_NA
    #> 202                                            not_found_set_uid_NA
    #> 203                                            not_found_set_uid_NA
    #> 204                                            not_found_set_uid_NA
    #> 205                                                       found_uid
    #> 206                                                       found_uid
    #> 207                                                       found_uid
    #> 208                                                       found_uid
    #> 209                                                       found_uid
    #> 210                                                       found_uid
    #> 211                                                       found_uid
    #> 212                                       no_match_found_set_UID_NA
    #> 213                                                       found_uid
    #> 214                                                       found_uid
    #> 215                                                       found_uid
    #> 216                                                   uid_NA_in_res
    #> 217                                       no_match_found_set_UID_NA
    #> 218                                                       found_uid
    #> 219                                                       found_uid
    #> 220                                                   uid_NA_in_res
    #> 221                                                       found_uid
    #> 222                                                       found_uid
    #> 223                                                       found_uid
    #> 224                                                       found_uid
    #> 225                                                       found_uid
    #> 226                                                       found_uid
    #> 227                                                       found_uid
    #> 228                                                       found_uid
    #> 229                                                       found_uid
    #> 230                                                       found_uid
    #> 231                                                       found_uid
    #> 232                                                       found_uid
    #> 233                                                       found_uid
    #> 234                                                       found_uid
    #> 235                                                       found_uid
    #> 236                                                       found_uid
    #> 237                                                       found_uid
    #> 238                                                       found_uid
    #> 239                                                       found_uid
    #> 240                                                       found_uid
    #> 241                                                       found_uid
    #> 242                                                       found_uid
    #> 243                                                       found_uid
    #> 244                                                       found_uid
    #> 245                                                       found_uid
    #> 246                                                       found_uid
    #> 247                                                       found_uid
    #> 248                                                       found_uid
    #> 249                                                       found_uid
    #> 250                                                       found_uid
    #> 251                                                       found_uid
    #> 252                                                       found_uid
    #> 253                                                       found_uid
    #> 254                                                       found_uid
    #> 255                                                       found_uid
    #> 256                                                       found_uid
    #> 257                                                       found_uid
    #> 258                                                       found_uid
    #> 259                                                       found_uid
    #> 260                                                       found_uid
    #> 261                                                       found_uid
    #> 262                                                       found_uid
    #> 263                                                       found_uid
    #> 264                                                       found_uid
    #> 265                                                       found_uid
    #> 266                                                       found_uid
    #> 267                                                       found_uid
    #> 268                                                       found_uid
    #> 269                                                       found_uid
    #> 270                                                       found_uid
    #> 271                                                       found_uid
    #> 272                                                       found_uid
    #> 273                                                       found_uid
    #> 274                                                       found_uid
    #> 275                                                       found_uid
    #> 276                                                       found_uid
    #> 277                                                       found_uid
    #> 278                                                       found_uid
    #> 279                                                       found_uid
    #> 280                                                       found_uid
    #> 281                                                       found_uid
    #> 282                                                       found_uid
    #> 283                                                       found_uid
    #> 284                                                       found_uid
    #> 285                                                       found_uid
    #> 286                                                       found_uid
    #> 287                                                       found_uid
    #> 288                                                       found_uid
    #> 289                                                       found_uid
    #> 290                                                       found_uid
    #> 291                                                       found_uid
    #> 292                                                       found_uid
    #> 293                                                       found_uid
    #> 294                                                       found_uid
    #> 295                                                       found_uid
    #> 296                                                       found_uid
    #> 297                                                       found_uid
    #> 298                                                       found_uid
    #> 299                                                       found_uid
    #> 300                                                       found_uid
    #> 301                                                       found_uid
    #> 302                                                       found_uid
    #> 303                                                       found_uid
    #> 304                                                       found_uid
    #> 305                                                       found_uid
    #> 306                                                       found_uid
    #> 307                                                       found_uid
    #> 308                                                       found_uid

    table(clean_uids$uid_dup_notes[!clean_uids$uid_dup_drop])

    #> 
    #>                                                     correct_uid 
    #>                                                             133 
    #>                                                       found_uid 
    #>                                                             100 
    #>                                              keeper_new_old_dup 
    #>                                                              30 
    #> matched_by_name_constituency_to_previously_unmatched_result_uid 
    #>                                                              23 
    #>                                       no_match_found_set_UID_NA 
    #>                                                               2 
    #>                                            not_found_set_uid_NA 
    #>                                                              18 
    #>                                                   uid_NA_in_res 
    #>                                                               2

    # Drop duplicates for dropping!
    cleaned_assets <- cleaned_assets %>%
      left_join(clean_uids) %>%
      filter(!uid_dup_drop | is.na(uid_dup_drop)) %>%
      mutate(uid_before_duplicate_correction = uid) %>%
      mutate(uid = case_when(
        uid_dup_notes %in% c("keeper_new_old_dup", "correct_uid") ~ uid,
        grepl("set_uid_na", tolower(uid_dup_notes)) ~ "-9999",
        uid_dup_notes == "uid_NA_in_res" ~ NA_character_,
        uid_dup_notes %in% c(
          "matched_by_name_constituency_to_previously_unmatched_result_uid",
          "found_uid"
          ) ~ uid_dup_new,
        TRUE ~ uid
      ))

    #> Joining, by = "key"

    table(cleaned_assets$duplicate_uid, cleaned_assets$uid_dup_resolved)

    #>        
    #>         TRUE
    #>   FALSE    0
    #>   TRUE   308

    cleaned_assets$uid

    #>     [1] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00001"
    #>     [9] "00002" "00003" "00004" "00005" "00006" "00007" "00008" "00009"
    #>    [17] "00010" "00011" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>    [25] "-9999" "-9999" "-9999" "-9999" "-9999" "01239" "01240" "01241"
    #>    [33] "01243" "01245" "01246" "01247" "01248" "-9999" "-9999" "-9999"
    #>    [41] "-9999" "-9999" "-9999" "-9999" "-9999" "02376" "02377" "02378"
    #>    [49] "02379" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>    [57] "-9999" "02519" "02522" "02524" "02525" "02526" "02527" "02528"
    #>    [65] "02529" "02520" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>    [73] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "02787" "02788"
    #>    [81] "02789" "02793" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>    [89] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "02955"
    #>    [97] "02957" "02958" "02959" "02960" "02961" "02962" "03074" "-9999"
    #>   [105] "-9999" "-9999" "-9999" "-9999" "03069" "03070" "03071" "03072"
    #>   [113] "03073" "03075" "03076" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [121] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [129] "-9999" "-9999" "-9999" "-9999" "03187" "03189" "03190" "03192"
    #>   [137] "03193" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [145] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [153] "03319" "03320" "03321" "03322" "03323" "03324" "-9999" "-9999"
    #>   [161] "-9999" "-9999" "-9999" "-9999" "-9999" "00012" "00013" "00014"
    #>   [169] "00015" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [177] "-9999" "-9999" "-9999" "00125" "00126" "00127" "00128" "00129"
    #>   [185] "00130" "00131" "00132" "00133" "00134" "00135" "00136" "00137"
    #>   [193] "00138" "00139" "00140" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [201] "-9999" "-9999" "00254" "00256" "00257" "00258" "00259" "00260"
    #>   [209] "00261" "00255" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [217] "-9999" "-9999" "-9999" "00379" "00380" "00381" "00382" "00383"
    #>   [225] "00385" "00386" "00387" "00388" "00389" "00390" "00391" "00392"
    #>   [233] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00529"
    #>   [241] "00530" "00532" "00533" "00534" "00535" "00536" "00537" "00538"
    #>   [249] "-9999" "-9999" "00663" "00664" "00665" "00666" "00667" "00893"
    #>   [257] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [265] "-9999" "-9999" "00770" "00771" "00776" "00777" "00780" "00781"
    #>   [273] "00782" "00783" "00784" "00785" "00897" "00898" "00899" "00901"
    #>   [281] "-9999" "-9999" "-9999" "00889" "00890" "00891" "00892" "00894"
    #>   [289] "00895" "00896" "00900" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [297] "-9999" "-9999" "-9999" "00994" "00995" "00996" "00997" "00998"
    #>   [305] "00999" "01000" "01001" "01002" "01003" "01004" "01005" "-9999"
    #>   [313] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [321] "-9999" "-9999" "-9999" "01132" "01127" "01129" "01130" "01131"
    #>   [329] "01133" "01134" "01135" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [337] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01249"
    #>   [345] "01250" "01251" "01252" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [353] "01383" "01384" "01385" "01386" "01387" "01388" "-9999" "-9999"
    #>   [361] "-9999" "-9999" "-9999" "01531" "01532" "01533" "01534" "01535"
    #>   [369] "01536" "01537" "01537" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [377] "-9999" "-9999" "-9999" "01640" "01641" "01642" "01643" "01644"
    #>   [385] "01645" "01646" "-9999" "-9999" "-9999" "-9999" "-9999" "01763"
    #>   [393] "01764" "01765" "01766" "01767" "01768" "01769" "01770" "-9999"
    #>   [401] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01938"
    #>   [409] "01939" "01940" "01941" "01943" "01944" "01945" "01946" "-9999"
    #>   [417] "-9999" "-9999" "02117" "02118" "02119" "02120" "02121" "02122"
    #>   [425] "02123" "02124" "02125" "-9999" "-9999" "-9999" "02311" "02312"
    #>   [433] "02313" "02314" "02315" "02316" "02317" "02318" "02319" "02320"
    #>   [441] "-9999" "-9999" "-9999" "02365" "02366" "02367" "02368" "02369"
    #>   [449] "02370" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [457] "-9999" "-9999" "02459" "02460" "02461" "02462" "02463" "02464"
    #>   [465] "02465" "02466" "02467" "02468" "02469" "02470" "02471" "02472"
    #>   [473] "02473" "02474" "02478" "-9999" "02476" "02477" "02479" "02480"
    #>   [481] "02481" "02482" "02483" "02484" "02485" "02486" "02487" "02488"
    #>   [489] "-9999" "-9999" "-9999" "-9999" "-9999" "02489" "02490" "02491"
    #>   [497] "02492" "02493" "02494" "02495" "02496" "02497" "02498" "02499"
    #>   [505] "02500" "02501" "02502" "02503" "02505" "-9999" "-9999" "-9999"
    #>   [513] "02506" "02507" "02508" "02509" "02510" "02511" "02512" "02513"
    #>   [521] "02514" "02515" "02516" "02517" "02518" "-9999" "-9999" "-9999"
    #>   [529] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [537] "02530" "02531" "02532" "02533" "02534" "02535" "02536" "02537"
    #>   [545] "02538" "02539" "02540" "02541" "02543" "-9999" "-9999" "-9999"
    #>   [553] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [561] "-9999" "02544" "02545" "02547" "02548" "02549" "02550" "02551"
    #>   [569] "02552" "02553" "02554" "02555" "-9999" "-9999" "-9999" "-9999"
    #>   [577] "02556" "02557" "02558" "02559" "02561" "02562" "02564" "02565"
    #>   [585] "02566" "02567" "02568" "02569" "02570" "02571" "02572" "02573"
    #>   [593] "-9999" "02574" "02575" "02576" "02577" "02578" "02579" "02580"
    #>   [601] "02581" "02582" "02583" "02584" "02585" "02586" "02587" "02588"
    #>   [609] "02589" "02590" "02591" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [617] "-9999" "-9999" "-9999" "-9999" "-9999" "02592" "02593" "02594"
    #>   [625] "02595" "02597" "02598" "02599" "02600" "02601" "02602" "02603"
    #>   [633] "02604" "02605" "02606" "02607" "02608" "02609" "02610" "02611"
    #>   [641] "02612" "02614" "02615" "02616" "02617" "02618" "02619" "02620"
    #>   [649] "02622" "02623" "02624" "02625" "02626" "02627" "02628" "02629"
    #>   [657] "02630" "02631" "-9999" "-9999" "-9999" "-9999" "02632" "02638"
    #>   [665] "02639" "02644" "02645" "02647" "02649" "02655" "02665" "02666"
    #>   [673] "02668" "-9999" "-9999" "-9999" "-9999" "02670" "02671" "02672"
    #>   [681] "02673" "02674" "02675" "02676" "02677" "02679" "02680" "02681"
    #>   [689] "02682" "02683" "02684" "02685" "02686" "02687" "02688" "02689"
    #>   [697] "02690" "02691" "02692" "02693" "02694" "-9999" "-9999" "-9999"
    #>   [705] "-9999" "-9999" "02695" "02696" "02697" "02698" "02699" "02700"
    #>   [713] "02701" "02702" "02703" "02704" "02705" "02706" "02707" "02709"
    #>   [721] "02710" "02711" "02712" "02713" "02714" "02715" "02716" "02717"
    #>   [729] "02718" "02719" "02720" "02721" "02722" "02723" "02724" "02725"
    #>   [737] "-9999" "02843" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [745] "-9999" "-9999" "-9999" "-9999" "02847" "-9999" "-9999" "-9999"
    #>   [753] "-9999" "-9999" "-9999" "-9999" "02848" "02849" "-9999" "02850"
    #>   [761] "-9999" "-9999" "-9999" "02851" "02852" "-9999" "-9999" "-9999"
    #>   [769] "-9999" "02854" "02855" "-9999" "02856" "02858" "-9999" "-9999"
    #>   [777] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "02860"
    #>   [785] "02861" "02871" "02873" "02874" "-9999" "02875" "02876" "-9999"
    #>   [793] "-9999" "02877" "-9999" "-9999" "-9999" "-9999" "-9999" "02882"
    #>   [801] "02883" "02878" "02880" "02879" "-9999" "02884" "-9999" "02869"
    #>   [809] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "02891" "-9999"
    #>   [817] "-9999" "-9999" "02901" "-9999" "02912" "-9999" "-9999" "-9999"
    #>   [825] "-9999" "-9999" "-9999" "02918" "-9999" "02913" "02914" "02915"
    #>   [833] "02916" "02917" "02919" "-9999" "-9999" "-9999" "-9999" "02921"
    #>   [841] "-9999" "02926" "-9999" "02920" "-9999" "02922" "02924" "02925"
    #>   [849] "02929" "02930" "02931" "02932" "02934" "02935" "02936" "02938"
    #>   [857] "02939" "02941" "02942" "02943" "02944" "02940" "02945" "02946"
    #>   [865] "02947" "02948" "02949" "02950" "02951" "02952" "02953" "02954"
    #>   [873] "-9999" "-9999" "02963" "02964" "02965" "02966" "02967" "02968"
    #>   [881] "02969" "02970" "02971" "02972" "02973" "02974" "02975" "02976"
    #>   [889] "02977" "02978" "02979" "02980" "02981" "02984" "-9999" "02982"
    #>   [897] "02983" "02985" "02986" "02987" "02988" "02989" "02990" "02991"
    #>   [905] "02992" "02993" "02994" "02995" "02996" "02997" "02998" "02999"
    #>   [913] "03000" "03001" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [921] "03005" "-9999" "03006" "03007" "-9999" "-9999" "-9999" "03008"
    #>   [929] "03010" "03011" "-9999" "-9999" "-9999" "-9999" "03013" "03014"
    #>   [937] "03015" "03016" "03017" "03019" "-9999" "-9999" "-9999" "-9999"
    #>   [945] "-9999" "-9999" "-9999" "-9999" "03021" "03022" "03023" "03024"
    #>   [953] "03025" "03026" "03027" "03028" "03029" "-9999" "-9999" "-9999"
    #>   [961] "-9999" "-9999" "03030" "03031" "03032" "03033" "03034" "03035"
    #>   [969] "03036" "03037" "03038" "03039" "03040" "03041" "03042" "03043"
    #>   [977] "03044" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>   [985] "03045" "03046" "03047" "03048" "03049" "03050" "03051" "03052"
    #>   [993] "03053" "03054" "03055" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1001] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "03056" "03057"
    #>  [1009] "03058" "03059" "03060" "03062" "03063" "03064" "03065" "03067"
    #>  [1017] "03068" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "03077"
    #>  [1025] "03078" "03079" "03080" "03081" "03082" "03083" "03084" "03085"
    #>  [1033] "03096" "-9999" "-9999" "-9999" "03086" "03087" "03088" "-9999"
    #>  [1041] "03089" "03089" "03098" "03090" "03091" "03092" "03093" "03094"
    #>  [1049] "03095" "03097" "03099" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1057] "-9999" "-9999" "03100" "03101" "03103" "03104" "03105" "03106"
    #>  [1065] "03107" "03108" "03109" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1073] "-9999" "-9999" "-9999" "-9999" "-9999" "03110" "03111" "03112"
    #>  [1081] "03113" "03114" "03115" "03116" "03117" "03118" "03119" "-9999"
    #>  [1089] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1097] "-9999" "-9999" "03120" "03121" "03122" "03123" "03124" "03125"
    #>  [1105] "03126" "03127" "03128" "03129" "03130" "-9999" "-9999" "-9999"
    #>  [1113] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "03131" "03132"
    #>  [1121] "03133" "03134" "03135" "03136" "03137" "03138" "03139" "03140"
    #>  [1129] "03141" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1137] "-9999" "-9999" "03142" "03143" "03144" "03145" "03147" "03148"
    #>  [1145] "03149" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1153] "-9999" "-9999" "-9999" "-9999" "03150" "03151" "03152" "03154"
    #>  [1161] "03155" "03156" "03157" "03158" "03159" "03160" "-9999" "-9999"
    #>  [1169] "-9999" "-9999" "-9999" "03161" "03162" "03163" "03164" "03165"
    #>  [1177] "03166" "03167" "03168" "03169" "03170" "03171" "03172" "-9999"
    #>  [1185] "-9999" "-9999" "-9999" "-9999" "-9999" "03173" "03175" "03176"
    #>  [1193] "03177" "03178" "03179" "03180" "03181" "03182" "03183" "03184"
    #>  [1201] "-9999" "03185" "03186" "-9999" "-9999" "03200" "03201" "03202"
    #>  [1209] "03203" "03204" "03205" "03206" "03207" "03208" "03209" "03210"
    #>  [1217] "03211" "03212" "03213" "03214" "03215" "03216" "-9999" "-9999"
    #>  [1225] "-9999" "-9999" "-9999" "-9999" "03217" "03218" "03219" "03220"
    #>  [1233] "03221" "03222" "03223" "03224" "03225" "03226" "03227" "03228"
    #>  [1241] "03229" "-9999" "-9999" "-9999" "-9999" "-9999" "03230" "03231"
    #>  [1249] "03233" "03235" "03236" "03237" "03238" "03239" "03240" "03241"
    #>  [1257] "03242" "03243" "03244" "03245" "03247" "03248" "-9999" "-9999"
    #>  [1265] "-9999" "03250" "03252" "03253" "03255" "03256" "03257" "03258"
    #>  [1273] "-9999" "03249" "03259" "03260" "03261" "03262" "03263" "03264"
    #>  [1281] "03265" "03266" "03267" "03268" "03269" "-9999" "-9999" "-9999"
    #>  [1289] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "03271"
    #>  [1297] "03272" "03273" "03274" "03275" "03276" "03277" "03278" "03279"
    #>  [1305] "03280" "03281" "03283" "03282" "-9999" "-9999" "-9999" "-9999"
    #>  [1313] "-9999" "-9999" "-9999" "03284" "03285" "03287" "03288" "03289"
    #>  [1321] "03291" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1329] "-9999" "-9999" "-9999" "03292" "03293" "03294" "03295" "03297"
    #>  [1337] "03298" "03299" "03301" "03302" "03303" "03304" "03305" "03306"
    #>  [1345] "-9999" "-9999" "03307" "-9999" "-9999" "03308" "-9999" "-9999"
    #>  [1353] "03309" "-9999" "03310" "03311" "-9999" "-9999" "-9999" "03312"
    #>  [1361] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1369] "-9999" "03332" "-9999" "-9999" "-9999" "-9999" "-9999" "03326"
    #>  [1377] "03327" "03328" "03329" "03330" "03331" "03333" "03334" "03335"
    #>  [1385] "03340" "-9999" "-9999" "-9999" "03341" "03342" "03343" "03344"
    #>  [1393] "03345" "03346" "03347" "03348" "03349" "03350" "03351" "03352"
    #>  [1401] "03353" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1409] "-9999" "03364" "-9999" "-9999" "03355" "03356" "03357" "03358"
    #>  [1417] "03360" "03363" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1425] "-9999" "-9999" "03365" "03366" "03367" "03368" "03369" "03370"
    #>  [1433] "03371" "03372" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1441] "-9999" "-9999" "-9999" "03373" "03374" "03378" "03379" "-9999"
    #>  [1449] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1457] "-9999" "-9999" "-9999" "03382" "03385" "03388" "03391" "-9999"
    #>  [1465] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "03392" "03393"
    #>  [1473] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1481] "03398" "03400" "03401" "03402" "03403" "03405" "03406" "03408"
    #>  [1489] "03409" "09434" "-9999" "-9999" "-9999" "-9999" "-9999" "03410"
    #>  [1497] "03411" "03412" "03413" "03414" "03415" "-9999" "03416" "03417"
    #>  [1505] "03418" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "03419"
    #>  [1513] "03420" "03421" "03422" "03423" "03424" "03425" "03426" "03427"
    #>  [1521] "03428" "-9999" "-9999" "00021" "00022" "00023" "00024" "00025"
    #>  [1529] "00026" "00027" "00028" "00029" "00030" "00031" "00032" "00033"
    #>  [1537] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00034" "00035"
    #>  [1545] "00036" "00037" "00038" "00039" "00040" "00041" "00042" "00047"
    #>  [1553] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1561] "00048" "00049" "00050" "00051" "00052" "00053" "00054" "00055"
    #>  [1569] "00056" "00057" "00058" "00059" "00060" "00061" "00062" "00063"
    #>  [1577] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1585] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1593] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1601] "-9999" "-9999" "-9999" "00067" "00068" "00069" "00070" "00071"
    #>  [1609] "00072" "00073" "00074" "00075" "00077" "-9999" "-9999" "-9999"
    #>  [1617] "-9999" "-9999" "-9999" "-9999" "00080" "00081" "00082" "00084"
    #>  [1625] "00085" "00087" "00088" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1633] "-9999" "00091" "00092" "00093" "00094" "00095" "00096" "00097"
    #>  [1641] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1649] "-9999" "00099" "00100" "00101" "00102" "00103" "00105" "-9999"
    #>  [1657] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00106" "00107"
    #>  [1665] "00108" "00109" "00110" "00111" "00112" "00113" "00114" "-9999"
    #>  [1673] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00115"
    #>  [1681] "00116" "00117" "00118" "00119" "00120" "00121" "00122" "00123"
    #>  [1689] "00124" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1697] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1705] "-9999" "-9999" "-9999" "-9999" "00142" "00143" "00144" "00146"
    #>  [1713] "00147" "00148" "-9999" "-9999" "-9999" "00153" "00154" "00155"
    #>  [1721] "00156" "00157" "00158" "00159" "00160" "00161" "00162" "00163"
    #>  [1729] "-9999" "-9999" "-9999" "00164" "00165" "00166" "00167" "00168"
    #>  [1737] "00169" "00170" "00172" "00173" "00174" "00176" "00177" "00178"
    #>  [1745] "-9999" "-9999" "-9999" "-9999" "-9999" "00179" "00180" "00181"
    #>  [1753] "00182" "00183" "00184" "00185" "00187" "00188" "00189" "00190"
    #>  [1761] "00191" "00192" "00193" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1769] "00194" "00195" "00196" "00197" "00198" "00199" "00200" "00201"
    #>  [1777] "00202" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1785] "-9999" "00203" "00204" "00205" "00206" "00207" "00208" "00209"
    #>  [1793] "00210" "00212" "00213" "-9999" "-9999" "00214" "00215" "00216"
    #>  [1801] "00217" "00218" "00219" "00220" "00221" "00222" "00224" "00225"
    #>  [1809] "00226" "-9999" "-9999" "-9999" "-9999" "00227" "00228" "00229"
    #>  [1817] "00231" "00232" "00233" "00234" "00235" "00236" "00237" "-9999"
    #>  [1825] "-9999" "-9999" "-9999" "00238" "00239" "00240" "00242" "00243"
    #>  [1833] "00244" "00245" "00246" "00247" "-9999" "-9999" "-9999" "-9999"
    #>  [1841] "-9999" "00248" "00249" "00250" "00251" "00252" "00253" "-9999"
    #>  [1849] "-9999" "00262" "00263" "00264" "00265" "00267" "00268" "00269"
    #>  [1857] "00270" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1865] "00271" "00272" "00273" "00274" "00275" "00276" "00277" "00278"
    #>  [1873] "00279" "00280" "00281" "00282" "00283" "00284" "00285" "-9999"
    #>  [1881] "-9999" "-9999" "-9999" "-9999" "00288" "00289" "00290" "00291"
    #>  [1889] "00293" "00294" "00295" "00296" "00297" "00298" "00299" "00300"
    #>  [1897] "00301" "00302" "00303" "00304" "00305" "00306" "-9999" "-9999"
    #>  [1905] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1913] "00307" "00308" "00309" "00310" "00311" "00312" "00313" "00314"
    #>  [1921] "00315" "00316" "00317" "00318" "00319" "-9999" "-9999" "-9999"
    #>  [1929] "00320" "00321" "00322" "00323" "00324" "00325" "00326" "-9999"
    #>  [1937] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1945] "-9999" "-9999" "-9999" "00327" "00328" "00329" "00330" "00331"
    #>  [1953] "00332" "00333" "00335" "00336" "00337" "00338" "00339" "00351"
    #>  [1961] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [1969] "-9999" "00334" "00340" "00341" "00342" "00343" "00347" "-9999"
    #>  [1977] "-9999" "00346" "00349" "00350" "00352" "00353" "-9999" "-9999"
    #>  [1985] "00362" "00354" "00355" "00358" "00359" "00361" "00365" "00366"
    #>  [1993] "00367" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2001] "-9999" "01370" "00370" "00371" "00372" "00373" "00374" "00375"
    #>  [2009] "00376" "00377" "00378" "01411" "-9999" "-9999" "-9999" "-9999"
    #>  [2017] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2025] "-9999" "-9999" "-9999" "-9999" "00393" "00394" "00395" "00396"
    #>  [2033] "00397" "00399" "00400" "00401" "00402" "00403" "00404" "00405"
    #>  [2041] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00411"
    #>  [2049] "00412" "00413" "00414" "00415" "00416" "00417" "-9999" "00407"
    #>  [2057] "00408" "00409" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2065] "-9999" "-9999" "-9999" "-9999" "-9999" "00418" "00419" "00421"
    #>  [2073] "00422" "00424" "00426" "00427" "00428" "00429" "00430" "00431"
    #>  [2081] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2089] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00432" "00433"
    #>  [2097] "00434" "00435" "00437" "00438" "00439" "00440" "00441" "00442"
    #>  [2105] "00443" "00444" "00445" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2113] "-9999" "-9999" "-9999" "-9999" "-9999" "00446" "00447" "00448"
    #>  [2121] "00449" "00450" "00451" "00452" "00453" "00454" "00455" "00456"
    #>  [2129] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00457"
    #>  [2137] "00458" "00459" "00460" "00461" "00462" "00463" "00464" "-9999"
    #>  [2145] "-9999" "-9999" "-9999" "-9999" "00470" "00471" "00472" "00473"
    #>  [2153] "00474" "00475" "00476" "00477" "00478" "00479" "-9999" "-9999"
    #>  [2161] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2169] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2177] "-9999" "-9999" "-9999" "-9999" "-9999" "00481" "00482" "00488"
    #>  [2185] "00493" "00494" "00495" "-9999" "-9999" "00480" "-9999" "00490"
    #>  [2193] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2201] "-9999" "00499" "00499" "00502" "00507" "00511" "-9999" "06945"
    #>  [2209] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2217] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2225] "-9999" "00518" "00528" "06135" "06136" "-9999" "-9999" "-9999"
    #>  [2233] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00541"
    #>  [2241] "00543" "00547" "00550" "06175" "-9999" "-9999" "00552" "-9999"
    #>  [2249] "-9999" "00559" "00555" "-9999" "-9999" "-9999" "-9999" "00560"
    #>  [2257] "00562" "00563" "00564" "00566" "00568" "00569" "00570" "00570"
    #>  [2265] "00571" "-9999" "-9999" "00572" "00577" "00578" "00580" "00581"
    #>  [2273] "00582" "00584" "00585" "00583" "-9999" "00586" "00587" "00588"
    #>  [2281] "00589" "00590" "00591" "00595" "00596" "00597" "00598" "-9999"
    #>  [2289] "-9999" "-9999" "-9999" "-9999" "00599" "00600" "00601" "00602"
    #>  [2297] "00603" "00604" "00605" "00606" "00607" "-9999" "-9999" "-9999"
    #>  [2305] "-9999" "-9999" "00608" "00609" "00610" "00611" "00612" "00613"
    #>  [2313] "00614" "00615" "00616" "00617" "00618" "00619" "00620" "00621"
    #>  [2321] "06294" "-9999" "-9999" "00622" "00623" "00624" "00625" "00626"
    #>  [2329] "00627" "00628" "00629" "00630" "00631" "00632" "00633" "00634"
    #>  [2337] "00635" "00636" "00637" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2345] "-9999" "-9999" "00638" "00639" "00640" "00641" "00642" "00643"
    #>  [2353] "00644" "00645" "00646" "00647" "00659" "-9999" "-9999" "00648"
    #>  [2361] "00649" "00650" "00651" "00652" "00653" "00654" "00655" "00656"
    #>  [2369] "00657" "00658" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2377] "00673" "00674" "00675" "00676" "00677" "00678" "00679" "00680"
    #>  [2385] "-9999" "-9999" "00681" "00682" "00683" "00684" "00685" "00686"
    #>  [2393] "00687" "-9999" "-9999" "-9999" "-9999" "-9999" "00688" "00689"
    #>  [2401] "00690" "00691" "00692" "00693" "00694" "-9999" "00695" "00696"
    #>  [2409] "00698" "00699" "00700" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2417] "-9999" "-9999" "00701" "00702" "00703" "00704" "00705" "00706"
    #>  [2425] "00707" "00708" "00709" "00710" "00711" "00712" "00713" "-9999"
    #>  [2433] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2441] "00714" "00715" "00716" "00717" "00718" "00719" "00720" "00721"
    #>  [2449] "00722" "00723" "00724" "00725" "00726" "00727" "00728" "00729"
    #>  [2457] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2465] "-9999" "00730" "00731" "00732" "00733" "00734" "00736" "00737"
    #>  [2473] "00738" "00739" "00740" "00741" "-9999" "-9999" "00749" "00742"
    #>  [2481] "00743" "00744" "00745" "00746" "00747" "00748" "-9999" "00750"
    #>  [2489] "00751" "00752" "00753" "00754" "00755" "00756" "-9999" "-9999"
    #>  [2497] "-9999" "-9999" "-9999" "00757" "00758" "00759" "00760" "00761"
    #>  [2505] "00762" "00763" "00764" "00765" "00766" "00767" "00769" "-9999"
    #>  [2513] "-9999" "-9999" "-9999" "00788" "00789" "00791" "00792" "-9999"
    #>  [2521] "-9999" "-9999" "-9999" "-9999" "00793" "00794" "00795" "00796"
    #>  [2529] "00797" "00798" "00799" "00800" "00801" "00802" "00803" "00804"
    #>  [2537] "00805" "00806" "00807" "-9999" "-9999" "-9999" "-9999" "00808"
    #>  [2545] "00809" "00810" "00811" "00812" "00813" "00814" "00815" "00816"
    #>  [2553] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00817" "00818"
    #>  [2561] "00819" "00820" "00822" "00823" "00824" "00825" "-9999" "-9999"
    #>  [2569] "00826" "00827" "00828" "00829" "00830" "00831" "00832" "00833"
    #>  [2577] "00834" "00835" "00836" "00837" "00838" "00839" "-9999" "-9999"
    #>  [2585] "00841" "00843" "00844" "00845" "00846" "00847" "00848" "-9999"
    #>  [2593] "-9999" "-9999" "-9999" "-9999" "00849" "00851" "00852" "00853"
    #>  [2601] "00854" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00855"
    #>  [2609] "00856" "00857" "00860" "00861" "00862" "00863" "00864" "00865"
    #>  [2617] "00866" "00867" "00868" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2625] "-9999" "-9999" "00869" "00870" "00871" "00872" "00873" "00874"
    #>  [2633] "00875" "00876" "00877" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2641] "-9999" "-9999" "00878" "00879" "00880" "00882" "00883" "00884"
    #>  [2649] "00885" "00886" "00887" "00888" "-9999" "-9999" "-9999" "-9999"
    #>  [2657] "-9999" "-9999" "-9999" "-9999" "-9999" "00903" "00904" "00905"
    #>  [2665] "00906" "00907" "00908" "00909" "00910" "00911" "00912" "00913"
    #>  [2673] "00914" "00916" "00917" "00918" "00919" "00920" "00921" "00922"
    #>  [2681] "-9999" "-9999" "00928" "00929" "00930" "00931" "00932" "00933"
    #>  [2689] "00934" "-9999" "-9999" "-9999" "-9999" "00935" "00936" "00937"
    #>  [2697] "00938" "00939" "00940" "00941" "00943" "-9999" "-9999" "-9999"
    #>  [2705] "-9999" "00944" "00945" "00946" "00947" "00948" "00949" "00950"
    #>  [2713] "00951" "00952" "-9999" "00953" "00954" "00955" "00956" "00957"
    #>  [2721] "00959" "00961" "00962" "-9999" "-9999" "-9999" "-9999" "00965"
    #>  [2729] "00966" "00967" "00968" "00969" "00970" "00971" "00972" "00973"
    #>  [2737] "00974" "00975" "00976" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2745] "-9999" "00980" "-9999" "-9999" "-9999" "-9999" "-9999" "00982"
    #>  [2753] "00983" "00986" "00987" "00989" "00990" "00991" "00992" "-9999"
    #>  [2761] "-9999" "-9999" "-9999" "-9999" "00984" "01006" "01009" "01009"
    #>  [2769] "01011" "01012" "01014" "01015" "01016" "01017" "-9999" "-9999"
    #>  [2777] "-9999" "-9999" "-9999" "01018" "01019" "01020" "01021" "01022"
    #>  [2785] "01023" "01024" "01025" "01026" "01027" "01028" "01044" "-9999"
    #>  [2793] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01029" "01030"
    #>  [2801] "01031" "01032" "01033" "01034" "01035" "01036" "01037" "01038"
    #>  [2809] "01039" "01040" "01041" "01042" "01043" "-9999" "-9999" "-9999"
    #>  [2817] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2825] "-9999" "-9999" "01061" "01062" "01063" "01065" "01068" "01069"
    #>  [2833] "01074" "01075" "-9999" "-9999" "-9999" "01077" "01079" "01080"
    #>  [2841] "01081" "01082" "01083" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2849] "01084" "01085" "01086" "01089" "01091" "01092" "01093" "01094"
    #>  [2857] "01095" "01096" "01098" "01099" "-9999" "-9999" "-9999" "01100"
    #>  [2865] "01102" "01103" "01104" "01105" "01106" "01107" "01108" "01110"
    #>  [2873] "-9999" "-9999" "01111" "01112" "01113" "01114" "01115" "01116"
    #>  [2881] "01117" "01118" "01119" "01120" "01121" "01123" "01125" "01126"
    #>  [2889] "-9999" "-9999" "-9999" "-9999" "01136" "01138" "01140" "-9999"
    #>  [2897] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2905] "-9999" "01141" "01141" "01141" "01143" "01144" "01145" "01147"
    #>  [2913] "01148" "01149" "01149" "01150" "01151" "01152" "01153" "01146"
    #>  [2921] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [2929] "-9999" "-9999" "01154" "01155" "01156" "01158" "01159" "-9999"
    #>  [2937] "-9999" "01160" "01161" "01162" "01163" "01164" "01166" "01170"
    #>  [2945] "01171" "01172" "01173" "01174" "01175" "01176" "01177" "01178"
    #>  [2953] "01179" "01180" "01181" "01182" "-9999" "-9999" "01183" "01184"
    #>  [2961] "01185" "01186" "01187" "01188" "01189" "-9999" "-9999" "-9999"
    #>  [2969] "-9999" "01190" "01191" "01192" "01193" "01194" "01195" "01196"
    #>  [2977] "01197" "01198" "01199" "01200" "01201" "01202" "01203" "01204"
    #>  [2985] "01205" "01206" "01207" "01208" "01209" "01210" "01211" "01212"
    #>  [2993] "01213" "01214" "01215" "01216" "01217" "01218" "01219" "-9999"
    #>  [3001] "-9999" "01220" "01222" "01224" "01225" "01226" "01227" "01228"
    #>  [3009] "01229" "01230" "-9999" "-9999" "-9999" "-9999" "-9999" "01233"
    #>  [3017] "01234" "01235" "01236" "01237" "01238" "-9999" "-9999" "-9999"
    #>  [3025] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01254"
    #>  [3033] "01255" "01257" "01258" "01259" "01265" "01266" "01267" "01268"
    #>  [3041] "01269" "01270" "-9999" "-9999" "-9999" "01272" "-9999" "-9999"
    #>  [3049] "-9999" "-9999" "01280" "01281" "01282" "01283" "01284" "01285"
    #>  [3057] "01286" "01287" "01288" "01289" "01290" "-9999" "-9999" "-9999"
    #>  [3065] "-9999" "01293" "01294" "01295" "01296" "01297" "01298" "01300"
    #>  [3073] "01301" "01302" "01303" "01305" "-9999" "-9999" "01304" "01306"
    #>  [3081] "01307" "01308" "01309" "01310" "01311" "01312" "01313" "-9999"
    #>  [3089] "01314" "01315" "01316" "01317" "01318" "01319" "01320" "01321"
    #>  [3097] "01322" "-9999" "-9999" "-9999" "-9999" "01324" "01325" "01326"
    #>  [3105] "01327" "01328" "01329" "01330" "01331" "01332" "01333" "01334"
    #>  [3113] "01335" "01336" "01337" "01338" "01339" "-9999" "01366" "01367"
    #>  [3121] "01369" "01371" "01372" "01373" "01374" "01542" "07318" "-9999"
    #>  [3129] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01482" "01483"
    #>  [3137] "01484" "01485" "01486" "01487" "01489" "01490" "01491" "01492"
    #>  [3145] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01481" "01493"
    #>  [3153] "01494" "01495" "01496" "01497" "01498" "01499" "01500" "01501"
    #>  [3161] "01502" "01503" "01504" "01505" "01506" "-9999" "-9999" "-9999"
    #>  [3169] "-9999" "-9999" "-9999" "-9999" "-9999" "01507" "01508" "01509"
    #>  [3177] "01510" "01511" "01512" "01513" "01514" "01515" "01516" "01517"
    #>  [3185] "01518" "01519" "01520" "-9999" "-9999" "01522" "01523" "01524"
    #>  [3193] "01525" "01526" "01527" "01528" "01529" "-9999" "-9999" "-9999"
    #>  [3201] "-9999" "-9999" "-9999" "-9999" "-9999" "01538" "01539" "01540"
    #>  [3209] "01543" "01544" "-9999" "-9999" "-9999" "01545" "01546" "01547"
    #>  [3217] "01548" "01549" "01550" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3225] "-9999" "-9999" "-9999" "01551" "01552" "01553" "01563" "01554"
    #>  [3233] "01555" "01556" "01557" "01558" "01560" "01561" "01562" "-9999"
    #>  [3241] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3249] "01565" "01566" "01567" "01568" "01569" "01570" "01571" "01572"
    #>  [3257] "01573" "01574" "01575" "01576" "-9999" "-9999" "-9999" "-9999"
    #>  [3265] "-9999" "-9999" "-9999" "01577" "01579" "01580" "01581" "01582"
    #>  [3273] "01583" "01584" "01585" "01586" "01588" "01590" "01591" "01592"
    #>  [3281] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3289] "-9999" "01593" "01594" "01595" "01596" "01597" "01598" "-9999"
    #>  [3297] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3305] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01599" "01600"
    #>  [3313] "01602" "01603" "01604" "01605" "01606" "01607" "01608" "01609"
    #>  [3321] "01610" "01612" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3329] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3337] "-9999" "-9999" "01613" "01614" "01615" "01616" "01617" "01618"
    #>  [3345] "01619" "01620" "01622" "01623" "01624" "-9999" "-9999" "-9999"
    #>  [3353] "-9999" "01625" "01626" "01627" "01628" "01629" "01630" "01631"
    #>  [3361] "01632" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01633"
    #>  [3369] "01634" "01635" "01636" "01638" "01639" "-9999" "-9999" "-9999"
    #>  [3377] "-9999" "-9999" "01648" "01649" "01650" "01651" "01652" "01653"
    #>  [3385] "-9999" "-9999" "-9999" "-9999" "01654" "01655" "01656" "01657"
    #>  [3393] "01658" "01659" "01660" "01661" "-9999" "-9999" "-9999" "-9999"
    #>  [3401] "-9999" "-9999" "-9999" "-9999" "-9999" "01662" "01663" "01664"
    #>  [3409] "01665" "01666" "01667" "01668" "-9999" "-9999" "-9999" "01669"
    #>  [3417] "01670" "01671" "01672" "01673" "01674" "01675" "01677" "01678"
    #>  [3425] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3433] "-9999" "01679" "01680" "01681" "01682" "01683" "-9999" "-9999"
    #>  [3441] "-9999" "-9999" "01696" "01697" "01698" "01699" "-9999" "-9999"
    #>  [3449] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01708"
    #>  [3457] "01709" "01710" "01712" "01714" "01715" "01716" "01717" "01718"
    #>  [3465] "01719" "01720" "01721" "-9999" "01722" "01723" "-9999" "01743"
    #>  [3473] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3481] "-9999" "-9999" "-9999" "-9999" "01744" "01745" "01746" "01747"
    #>  [3489] "01748" "01749" "01750" "01751" "01752" "01753" "01754" "01755"
    #>  [3497] "01756" "01757" "01758" "01759" "01760" "01761" "01762" "-9999"
    #>  [3505] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3513] "-9999" "-9999" "01771" "01772" "01773" "01775" "01777" "01778"
    #>  [3521] "01779" "01780" "01781" "01782" "01783" "01785" "01786" "-9999"
    #>  [3529] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3537] "-9999" "01787" "01788" "01789" "01790" "01791" "01793" "01794"
    #>  [3545] "01795" "01796" "01797" "01798" "01800" "-9999" "-9999" "-9999"
    #>  [3553] "-9999" "-9999" "-9999" "-9999" "01803" "01804" "01805" "01806"
    #>  [3561] "01807" "01809" "01810" "01811" "01812" "01813" "01814" "01815"
    #>  [3569] "01816" "01817" "01818" "01819" "01820" "-9999" "-9999" "-9999"
    #>  [3577] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3585] "-9999" "-9999" "-9999" "-9999" "-9999" "01821" "01822" "01823"
    #>  [3593] "01825" "01826" "01827" "01828" "01829" "01831" "01832" "01834"
    #>  [3601] "01835" "01843" "01844" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3609] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3617] "-9999" "01837" "01836" "01839" "01841" "01842" "01848" "01849"
    #>  [3625] "01850" "01851" "01852" "01853" "01854" "01855" "01856" "01858"
    #>  [3633] "01859" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3641] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3649] "01860" "01861" "01862" "01863" "01865" "01866" "01867" "01868"
    #>  [3657] "01869" "01870" "01871" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3665] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01872" "01873"
    #>  [3673] "01875" "01876" "01877" "01878" "01879" "01880" "01881" "01882"
    #>  [3681] "01883" "01884" "01886" "01887" "02112" "-9999" "-9999" "-9999"
    #>  [3689] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01888" "01889"
    #>  [3697] "01890" "01891" "01892" "01893" "01894" "01895" "01896" "01897"
    #>  [3705] "01898" "01899" "01900" "01902" "01903" "01904" "01905" "01906"
    #>  [3713] "01907" "01908" "01909" "01910" "-9999" "-9999" "-9999" "-9999"
    #>  [3721] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01911" "01912"
    #>  [3729] "01913" "01914" "01915" "01916" "01917" "01918" "01919" "01920"
    #>  [3737] "01921" "01922" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3745] "-9999" "-9999" "-9999" "-9999" "01947" "01948" "01949" "01950"
    #>  [3753] "01951" "01952" "01953" "01954" "01955" "01956" "01957" "01958"
    #>  [3761] "01959" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3769] "-9999" "01960" "01961" "01962" "01963" "01964" "01965" "01966"
    #>  [3777] "01967" "01968" "01969" "01972" "-9999" "-9999" "-9999" "-9999"
    #>  [3785] "-9999" "-9999" "-9999" "01973" "01974" "01975" "01977" "01979"
    #>  [3793] "01980" "01981" "01982" "01984" "01985" "01986" "01987" "-9999"
    #>  [3801] "-9999" "-9999" "-9999" "-9999" "01989" "01990" "01991" "01992"
    #>  [3809] "01993" "01994" "01995" "01996" "01997" "01998" "01999" "02000"
    #>  [3817] "02001" "02002" "02003" "02004" "02005" "02006" "02007" "-9999"
    #>  [3825] "-9999" "-9999" "-9999" "-9999" "02008" "02009" "02010" "02011"
    #>  [3833] "02012" "02013" "02014" "02015" "02016" "02017" "02018" "02019"
    #>  [3841] "02020" "02021" "02022" "02023" "02024" "-9999" "-9999" "-9999"
    #>  [3849] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3857] "-9999" "-9999" "-9999" "-9999" "02027" "02028" "02029" "02030"
    #>  [3865] "02031" "02032" "02033" "02034" "02036" "02038" "02039" "-9999"
    #>  [3873] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3881] "02025" "02040" "02041" "02042" "02043" "02044" "02045" "02046"
    #>  [3889] "02047" "02049" "02050" "02051" "02053" "02054" "02055" "-9999"
    #>  [3897] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3905] "-9999" "02056" "02058" "02059" "02060" "02061" "02062" "02063"
    #>  [3913] "02065" "02066" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3921] "02067" "02068" "02069" "02070" "02071" "02073" "02074" "02075"
    #>  [3929] "02076" "02077" "02079" "02080" "02082" "02083" "02084" "02085"
    #>  [3937] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3945] "-9999" "-9999" "02086" "02087" "02088" "02089" "02090" "02091"
    #>  [3953] "02095" "02096" "02097" "02098" "02099" "02100" "02101" "02102"
    #>  [3961] "02103" "02104" "02108" "02109" "02110" "02111" "02113" "02114"
    #>  [3969] "02116" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3977] "-9999" "-9999" "02152" "02153" "02155" "-9999" "02156" "02157"
    #>  [3985] "02158" "02159" "02161" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [3993] "-9999" "-9999" "-9999" "02177" "02178" "02179" "02180" "02182"
    #>  [4001] "02183" "02184" "02184" "02185" "02186" "02187" "02188" "02189"
    #>  [4009] "02190" "02191" "02192" "02193" "02194" "02195" "-9999" "-9999"
    #>  [4017] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4025] "-9999" "-9999" "-9999" "02197" "02198" "02199" "02200" "02201"
    #>  [4033] "02204" "02205" "02206" "02207" "02208" "02209" "-9999" "-9999"
    #>  [4041] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4049] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "02210" "02212"
    #>  [4057] "02213" "02214" "02215" "02217" "02218" "02219" "02221" "02222"
    #>  [4065] "02224" "02225" "02227" "02228" "02229" "02230" "02232" "02233"
    #>  [4073] "02234" "-9999" "-9999" "-9999" "-9999" "02235" "02236" "02237"
    #>  [4081] "02238" "02239" "02240" "02241" "02242" "02244" "02245" "02246"
    #>  [4089] "02247" "02248" "02249" "02250" "02251" "02252" "02253" "02254"
    #>  [4097] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4105] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4113] "-9999" "02298" "02299" "02300" "02301" "02302" "02303" "02304"
    #>  [4121] "02305" "02307" "02309" "02310" "-9999" "-9999" "-9999" "-9999"
    #>  [4129] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4137] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "02322" "02335"
    #>  [4145] "02325" "02326" "02327" "02331" "02332" "02333" "02334" "02336"
    #>  [4153] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4161] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4169] "-9999" "03429" "03430" "03431" "03432" "-9999" "03433" "03434"
    #>  [4177] "03435" "03435" "03436" "03437" "03438" "03440" "03441" "03442"
    #>  [4185] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4193] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "03618" "03619"
    #>  [4201] "03620" "03621" "03622" "03623" "03624" "03625" "03626" "03627"
    #>  [4209] "03628" "03630" "03631" "03632" "03633" "03634" "03635" "03636"
    #>  [4217] "03637" "03638" "03639" "03640" "-9999" "-9999" "-9999" "-9999"
    #>  [4225] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4233] "-9999" "03891" "03892" "03893" "03894" "03895" "03896" "03897"
    #>  [4241] "03898" "03899" "03900" "03901" "-9999" "-9999" "-9999" "-9999"
    #>  [4249] "-9999" "-9999" "04059" "04060" "04061" "04062" "04063" "04064"
    #>  [4257] "04065" "04066" "04067" "04068" "04069" "04070" "04071" "04072"
    #>  [4265] "04073" "04074" "04075" "04076" "04077" "04078" "04079" "04080"
    #>  [4273] "04081" "-9999" "-9999" "-9999" "-9999" "-9999" "04228" "04229"
    #>  [4281] "04230" "04231" "04232" "04235" "04236" "04238" "04239" "04242"
    #>  [4289] "04243" "04244" "04245" "-9999" "04248" "04251" "04252" "04253"
    #>  [4297] "04254" "04255" "04256" "04257" "-9999" "-9999" "-9999" "-9999"
    #>  [4305] "-9999" "-9999" "-9999" "-9999" "-9999" "04282" "04283" "04284"
    #>  [4313] "04285" "04286" "04287" "04289" "04290" "04291" "04292" "04293"
    #>  [4321] "04294" "04295" "04296" "04297" "04298" "04299" "-9999" "-9999"
    #>  [4329] "-9999" "-9999" "-9999" "-9999" "04331" "04332" "04333" "04334"
    #>  [4337] "04335" "04336" "04337" "04338" "04339" "04340" "04341" "04342"
    #>  [4345] "04343" "-9999" "-9999" "04344" "04345" "04346" "04347" "04349"
    #>  [4353] "04351" "04352" "04353" "04354" "04356" "04357" "04358" "04361"
    #>  [4361] "04363" "04365" "04366" "04367" "04368" "04370" "04372" "04374"
    #>  [4369] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4377] "-9999" "03507" "03508" "03509" "03510" "03511" "03512" "03513"
    #>  [4385] "03514" "03515" "03516" "03517" "03518" "03519" "03520" "-9999"
    #>  [4393] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "03521" "03522"
    #>  [4401] "03523" "03524" "03526" "03528" "03529" "03530" "03531" "03532"
    #>  [4409] "03533" "03534" "03535" "03536" "03537" "03538" "-9999" "-9999"
    #>  [4417] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4425] "-9999" "03539" "03540" "03541" "03542" "03543" "03544" "03545"
    #>  [4433] "03547" "03548" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4441] "-9999" "-9999" "-9999" "-9999" "03553" "03554" "-9999" "-9999"
    #>  [4449] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4457] "-9999" "-9999" "-9999" "03689" "03690" "03691" "03693" "03694"
    #>  [4465] "03695" "03696" "03697" "03698" "03699" "03700" "03701" "03702"
    #>  [4473] "03703" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4481] "-9999" "03715" "03719" "03728" "-9999" "-9999" "-9999" "-9999"
    #>  [4489] "-9999" "-9999" "-9999" "03758" "03759" "03760" "-9999" "03761"
    #>  [4497] "03762" "03763" "03764" "03766" "03767" "03768" "03769" "03770"
    #>  [4505] "03772" "03773" "03774" "03775" "03776" "-9999" "03777" "-9999"
    #>  [4513] "03779" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4521] "-9999" "-9999" "-9999" "-9999" "03780" "03781" "03782" "03783"
    #>  [4529] "03784" "03785" "03786" "03787" "03788" "03789" "03790" "03791"
    #>  [4537] "03792" "03793" "03794" "03795" "03796" "03797" "03798" "03799"
    #>  [4545] "03800" "03801" "03802" "03803" "03804" "03805" "03806" "03807"
    #>  [4553] "03808" "03809" "03810" "03811" "03812" "03813" "03814" "03815"
    #>  [4561] "03816" "03817" "03818" "03819" "03820" "03914" "-9999" "-9999"
    #>  [4569] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "03902" "03903"
    #>  [4577] "03905" "03906" "03907" "03908" "03909" "03910" "03911" "03912"
    #>  [4585] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4593] "03997" "03998" "03999" "04000" "04001" "04002" "04004" "04005"
    #>  [4601] "04369" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4609] "-9999" "04007" "04008" "04009" "04010" "04011" "04012" "04013"
    #>  [4617] "04014" "04015" "04016" "04017" "04018" "04019" "04020" "04021"
    #>  [4625] "04022" "04023" "04024" "04025" "04026" "04027" "04028" "-9999"
    #>  [4633] "-9999" "-9999" "-9999" "04031" "04032" "04033" "04044" "04034"
    #>  [4641] "10624" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4649] "-9999" "-9999" "-9999" "04045" "04046" "04047" "04049" "04050"
    #>  [4657] "04051" "04052" "04053" "04054" "04055" "04056" "04058" "-9999"
    #>  [4665] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04082" "04083"
    #>  [4673] "04084" "04085" "04086" "04087" "04088" "-9999" "-9999" "04089"
    #>  [4681] "04090" "04091" "04092" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4689] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04160" "04162"
    #>  [4697] "04163" "04164" "04165" "04166" "04167" "04168" "04169" "04170"
    #>  [4705] "04171" "04173" "04174" "-9999" "-9999" "-9999" "04175" "04176"
    #>  [4713] "04177" "04178" "04179" "04180" "04181" "04182" "04183" "04184"
    #>  [4721] "04185" "-9999" "-9999" "-9999" "04186" "04187" "04188" "04189"
    #>  [4729] "04190" "04191" "-9999" "04193" "04194" "04195" "04196" "04197"
    #>  [4737] "04198" "04199" "04200" "04201" "-9999" "-9999" "-9999" "-9999"
    #>  [4745] "-9999" "04203" "04204" "04205" "04206" "04208" "04209" "04210"
    #>  [4753] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4761] "-9999" "04212" "04213" "04214" "04215" "04216" "04217" "04218"
    #>  [4769] "04219" "04220" "04221" "04222" "04223" "04224" "04226" "04227"
    #>  [4777] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4785] "-9999" "-9999" "04268" "04269" "04270" "04271" "04273" "04274"
    #>  [4793] "04277" "04278" "04279" "04280" "04281" "-9999" "-9999" "-9999"
    #>  [4801] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4809] "04375" "04376" "04377" "04378" "04379" "04380" "04381" "04382"
    #>  [4817] "04383" "04384" "04385" "04386" "04387" "04388" "04389" "04390"
    #>  [4825] "-9999" "-9999" "-9999" "-9999" "-9999" "04483" "04484" "04485"
    #>  [4833] "04486" "04487" "04488" "04585" "-9999" "-9999" "-9999" "-9999"
    #>  [4841] "-9999" "04584" "04586" "04587" "04588" "04589" "04590" "04591"
    #>  [4849] "04592" "04593" "04594" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4857] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4865] "-9999" "-9999" "-9999" "04734" "04735" "04736" "04737" "04738"
    #>  [4873] "04739" "04740" "04741" "04742" "04743" "04744" "04745" "04746"
    #>  [4881] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4889] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4897] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4905] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4913] "-9999" "-9999" "04955" "04956" "04957" "04958" "04959" "04960"
    #>  [4921] "04961" "04963" "04964" "04965" "04966" "04968" "-9999" "-9999"
    #>  [4929] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05076" "05077"
    #>  [4937] "05078" "05079" "05080" "05081" "05082" "05083" "05084" "05085"
    #>  [4945] "05086" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4953] "-9999" "05202" "05203" "05204" "05205" "05206" "05207" "05208"
    #>  [4961] "05209" "05210" "05212" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4969] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4977] "05352" "05353" "05354" "05355" "05356" "05357" "-9999" "-9999"
    #>  [4985] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [4993] "-9999" "-9999" "-9999" "-9999" "04392" "04393" "-9999" "04394"
    #>  [5001] "04395" "04396" "04397" "04399" "04400" "04401" "-9999" "-9999"
    #>  [5009] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5017] "-9999" "-9999" "-9999" "04402" "04403" "04404" "04405" "04406"
    #>  [5025] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04407"
    #>  [5033] "04408" "04409" "04410" "04411" "04412" "04414" "04415" "-9999"
    #>  [5041] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5049] "-9999" "04417" "04418" "04419" "04420" "04421" "04422" "-9999"
    #>  [5057] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5065] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04424" "04425"
    #>  [5073] "04426" "04427" "04428" "04429" "04430" "04431" "04432" "04433"
    #>  [5081] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5089] "-9999" "-9999" "-9999" "04435" "04436" "04437" "04438" "04439"
    #>  [5097] "04440" "04441" "04442" "04443" "04444" "-9999" "-9999" "-9999"
    #>  [5105] "-9999" "-9999" "04445" "04446" "04447" "04448" "04449" "04450"
    #>  [5113] "04451" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04452"
    #>  [5121] "04453" "04454" "04455" "04456" "04457" "04458" "04459" "04460"
    #>  [5129] "-9999" "-9999" "-9999" "-9999" "-9999" "04461" "04462" "04463"
    #>  [5137] "04464" "04465" "04466" "04467" "04468" "04469" "04470" "04471"
    #>  [5145] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5153] "-9999" "04472" "04473" "04474" "04475" "04476" "04477" "04478"
    #>  [5161] "04479" "04480" "04481" "04482" "-9999" "-9999" "-9999" "-9999"
    #>  [5169] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5177] "-9999" "04489" "04490" "04491" "-9999" "04492" "04493" "04494"
    #>  [5185] "04495" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5193] "-9999" "-9999" "-9999" "04497" "04498" "-9999" "-9999" "-9999"
    #>  [5201] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5209] "-9999" "04503" "04504" "04505" "04507" "04508" "-9999" "-9999"
    #>  [5217] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5225] "-9999" "-9999" "04150" "04509" "04511" "04512" "04513" "04514"
    #>  [5233] "04515" "04516" "04517" "-9999" "04519" "04521" "-9999" "-9999"
    #>  [5241] "-9999" "-9999" "-9999" "-9999" "04522" "04523" "04524" "04525"
    #>  [5249] "04526" "04527" "04528" "04529" "04530" "04531" "-9999" "-9999"
    #>  [5257] "04550" "04551" "04552" "04553" "-9999" "-9999" "04554" "04555"
    #>  [5265] "04556" "04557" "04558" "04558" "04559" "04560" "04561" "04562"
    #>  [5273] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04563" "04564"
    #>  [5281] "04565" "04566" "04567" "04568" "04569" "04570" "-9999" "-9999"
    #>  [5289] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5297] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5305] "02375" "04571" "04573" "04574" "04575" "04576" "04577" "04578"
    #>  [5313] "04580" "04581" "04582" "04583" "-9999" "-9999" "-9999" "-9999"
    #>  [5321] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5329] "-9999" "-9999" "-9999" "-9999" "-9999" "04572" "04595" "04596"
    #>  [5337] "04597" "04599" "04600" "04601" "04603" "04604" "04605" "04606"
    #>  [5345] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5353] "-9999" "-9999" "-9999" "04607" "04608" "04609" "04610" "04611"
    #>  [5361] "04612" "04613" "04614" "04615" "04616" "04617" "04618" "04619"
    #>  [5369] "04620" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5377] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04211"
    #>  [5385] "04621" "04622" "04623" "04624" "04625" "04626" "04627" "04628"
    #>  [5393] "04629" "04630" "04631" "04632" "-9999" "-9999" "-9999" "-9999"
    #>  [5401] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5409] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5417] "04633" "04634" "04635" "04636" "04637" "04638" "04639" "04640"
    #>  [5425] "04641" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5433] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5441] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5449] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04646" "04647"
    #>  [5457] "04648" "04649" "04650" "04651" "04652" "04653" "04654" "-9999"
    #>  [5465] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5473] "-9999" "-9999" "04657" "04658" "04660" "04661" "04662" "04663"
    #>  [5481] "04664" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5489] "-9999" "-9999" "04665" "04666" "04667" "04668" "04669" "04670"
    #>  [5497] "04671" "04672" "04673" "04674" "04675" "04676" "04677" "04678"
    #>  [5505] "04679" "04680" "04681" "-9999" "-9999" "-9999" "-9999" "04682"
    #>  [5513] "04683" "04684" "04685" "04686" "04687" "04688" "04689" "04690"
    #>  [5521] "04691" "04693" "04694" "04695" "04696" "04697" "04698" "04699"
    #>  [5529] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5537] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5545] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5553] "-9999" "04700" "04701" "04702" "04703" "04717" "04718" "-9999"
    #>  [5561] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5569] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5577] "-9999" "04720" "04721" "04726" "04728" "04733" "-9999" "-9999"
    #>  [5585] "-9999" "-9999" "-9999" "04747" "04748" "04749" "04750" "04751"
    #>  [5593] "04752" "04753" "04754" "04755" "04756" "-9999" "-9999" "-9999"
    #>  [5601] "-9999" "-9999" "-9999" "04758" "04759" "04760" "04762" "04765"
    #>  [5609] "04766" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5617] "04767" "04768" "04769" "04770" "04771" "04772" "04773" "04774"
    #>  [5625] "04777" "04779" "04780" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5633] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5641] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04783"
    #>  [5649] "04787" "04789" "04791" "04792" "04794" "04795" "04796" "04797"
    #>  [5657] "04798" "04799" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5665] "-9999" "-9999" "-9999" "-9999" "-9999" "04800" "04801" "04802"
    #>  [5673] "04803" "04804" "04805" "04806" "04807" "04808" "04809" "04810"
    #>  [5681] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04812"
    #>  [5689] "04813" "04814" "04815" "04816" "04817" "04818" "04819" "04820"
    #>  [5697] "04821" "04822" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5705] "-9999" "-9999" "-9999" "04823" "04824" "04827" "04829" "-9999"
    #>  [5713] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5721] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5729] "04833" "04838" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5737] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5745] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5753] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04848" "04850"
    #>  [5761] "04852" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5769] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "04859" "04860"
    #>  [5777] "04862" "04863" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5785] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5793] "-9999" "-9999" "04872" "04873" "04875" "04876" "04877" "04925"
    #>  [5801] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5809] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5817] "-9999" "-9999" "04879" "04880" "04881" "04883" "-9999" "-9999"
    #>  [5825] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5833] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5841] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5849] "-9999" "-9999" "-9999" "04896" "04898" "-9999" "-9999" "-9999"
    #>  [5857] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5865] "04904" "04905" "04909" "04910" "04911" "04912" "04912" "04913"
    #>  [5873] "04914" "04915" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5881] "-9999" "-9999" "-9999" "-9999" "04916" "04917" "04918" "04919"
    #>  [5889] "04920" "04921" "04921" "04922" "04923" "04924" "04926" "04927"
    #>  [5897] "04928" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5905] "-9999" "-9999" "-9999" "-9999" "-9999" "04929" "04930" "04931"
    #>  [5913] "04933" "04934" "04935" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5921] "04936" "04937" "04938" "04939" "04940" "04941" "04942" "04943"
    #>  [5929] "04944" "04945" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5937] "-9999" "-9999" "04946" "04948" "04949" "04950" "04951" "04952"
    #>  [5945] "04953" "04954" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5953] "-9999" "-9999" "-9999" "-9999" "04969" "04971" "04972" "04973"
    #>  [5961] "04975" "04976" "04977" "04978" "04979" "04980" "04981" "-9999"
    #>  [5969] "-9999" "-9999" "-9999" "-9999" "-9999" "04982" "04983" "04984"
    #>  [5977] "04985" "04986" "04987" "04988" "04989" "04990" "04991" "04992"
    #>  [5985] "04993" "04994" "04995" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [5993] "-9999" "04996" "04997" "04998" "04999" "05000" "05001" "05002"
    #>  [6001] "05003" "05004" "05005" "05006" "05007" "-9999" "-9999" "-9999"
    #>  [6009] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05008" "05009"
    #>  [6017] "05010" "05011" "05012" "05014" "05015" "05016" "05013" "05022"
    #>  [6025] "-9999" "-9999" "-9999" "-9999" "-9999" "05017" "05020" "05021"
    #>  [6033] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6041] "05023" "05024" "05025" "05026" "05027" "05028" "05029" "05030"
    #>  [6049] "05031" "05032" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6057] "-9999" "05033" "05034" "05035" "05036" "05037" "05038" "05039"
    #>  [6065] "05039" "05040" "05042" "-9999" "05043" "05044" "05045" "05046"
    #>  [6073] "05047" "05048" "05049" "05050" "05051" "05052" "05053" "05054"
    #>  [6081] "05055" "05056" "05057" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6089] "-9999" "05058" "05059" "05060" "05061" "05062" "05063" "05064"
    #>  [6097] "05065" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05067"
    #>  [6105] "05068" "05069" "05070" "05071" "05072" "05073" "05074" "-9999"
    #>  [6113] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05087" "05088"
    #>  [6121] "05089" "05090" "05091" "05092" "05093" "05094" "05095" "-9999"
    #>  [6129] "-9999" "-9999" "-9999" "-9999" "-9999" "05096" "05097" "05098"
    #>  [6137] "05099" "05100" "05101" "05102" "05103" "05104" "05105" "-9999"
    #>  [6145] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6153] "-9999" "05106" "05107" "05108" "05109" "05111" "05112" "05113"
    #>  [6161] "05114" "05115" "05116" "05117" "05118" "-9999" "-9999" "-9999"
    #>  [6169] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6177] "-9999" "-9999" "-9999" "-9999" "05119" "05120" "05122" "05123"
    #>  [6185] "05124" "05125" "05126" "05127" "05128" "05130" "05131" "05132"
    #>  [6193] "05133" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6201] "-9999" "-9999" "05134" "05135" "-9999" "05136" "05137" "05139"
    #>  [6209] "05140" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6217] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05141" "05142"
    #>  [6225] "05143" "05144" "05145" "05146" "05147" "05148" "05149" "05150"
    #>  [6233] "05151" "05152" "05153" "05154" "05155" "05156" "-9999" "-9999"
    #>  [6241] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6249] "-9999" "-9999" "-9999" "-9999" "-9999" "05157" "05158" "05159"
    #>  [6257] "-9999" "05160" "05161" "05162" "05163" "05164" "05165" "05166"
    #>  [6265] "05167" "05168" "05169" "05170" "05171" "05173" "-9999" "-9999"
    #>  [6273] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6281] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6289] "05174" "05175" "05176" "05177" "05178" "05179" "05180" "05181"
    #>  [6297] "05184" "05185" "05186" "05187" "-9999" "-9999" "-9999" "-9999"
    #>  [6305] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6313] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6321] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6329] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05188" "05189"
    #>  [6337] "05190" "05191" "05194" "05195" "05196" "05197" "05198" "05199"
    #>  [6345] "05200" "05201" "-9999" "-9999" "-9999" "-9999" "05213" "05214"
    #>  [6353] "05215" "05216" "05217" "05218" "05219" "05220" "05221" "-9999"
    #>  [6361] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6369] "-9999" "-9999" "05222" "05223" "05224" "05225" "05226" "05227"
    #>  [6377] "05228" "05229" "05230" "05231" "05232" "05233" "05234" "05235"
    #>  [6385] "05236" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6393] "-9999" "05237" "05238" "05239" "05241" "05242" "05243" "05244"
    #>  [6401] "05245" "05246" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6409] "-9999" "-9999" "-9999" "-9999" "05247" "05248" "05249" "05250"
    #>  [6417] "05251" "05252" "05253" "05254" "05255" "05256" "05257" "05258"
    #>  [6425] "05259" "05260" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6433] "-9999" "-9999" "-9999" "-9999" "-9999" "05261" "05262" "05263"
    #>  [6441] "05264" "05265" "05266" "05267" "05268" "05269" "05270" "05271"
    #>  [6449] "05272" "05273" "05274" "05275" "05276" "05277" "05278" "05279"
    #>  [6457] "05280" "05281" "05282" "05283" "-9999" "-9999" "-9999" "-9999"
    #>  [6465] "-9999" "-9999" "-9999" "-9999" "05284" "05285" "05286" "05287"
    #>  [6473] "05289" "05290" "05291" "05292" "05293" "05294" "-9999" "-9999"
    #>  [6481] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6489] "-9999" "-9999" "-9999" "-9999" "05295" "05296" "05297" "05298"
    #>  [6497] "05299" "05300" "05301" "05302" "05303" "05304" "05305" "05306"
    #>  [6505] "05307" "05308" "05309" "05310" "05311" "05312" "-9999" "-9999"
    #>  [6513] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05313"
    #>  [6521] "05314" "05316" "05317" "05318" "05319" "05320" "05321" "-9999"
    #>  [6529] "-9999" "-9999" "05322" "05323" "05324" "05325" "05326" "05327"
    #>  [6537] "05328" "05329" "05330" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6545] "-9999" "-9999" "-9999" "05331" "05332" "05333" "05334" "05335"
    #>  [6553] "05336" "05337" "05338" "05339" "05340" "05341" "05343" "05344"
    #>  [6561] "05345" "05346" "05347" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6569] "-9999" "-9999" "-9999" "05358" "05359" "05360" "05361" "05362"
    #>  [6577] "05363" "05364" "05365" "05366" "05367" "-9999" "-9999" "-9999"
    #>  [6585] "-9999" "-9999" "-9999" "-9999" "05368" "05369" "05370" "05371"
    #>  [6593] "05372" "05373" "05374" "05375" "05376" "05377" "05378" "05379"
    #>  [6601] "05380" "05381" "05382" "05383" "05384" "-9999" "-9999" "05385"
    #>  [6609] "05386" "05387" "05388" "05389" "05390" "05391" "05392" "05393"
    #>  [6617] "05394" "05395" "05396" "05397" "05398" "05399" "05400" "05401"
    #>  [6625] "05402" "05403" "05404" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6633] "-9999" "05406" "05407" "05408" "05409" "05410" "05411" "05412"
    #>  [6641] "05413" "05414" "05415" "05416" "05417" "05418" "05419" "05420"
    #>  [6649] "05421" "05422" "05423" "05424" "05425" "-9999" "05426" "05427"
    #>  [6657] "05428" "05429" "05430" "05431" "05432" "05433" "05434" "05435"
    #>  [6665] "05436" "05437" "05438" "05439" "05440" "05441" "05442" "-9999"
    #>  [6673] "05443" "05444" "05445" "05446" "05447" "05448" "05449" "05450"
    #>  [6681] "05451" "05452" "05453" "05454" "05455" "05456" "05457" "05458"
    #>  [6689] "05459" "-9999" "-9999" "-9999" "05460" "05461" "05462" "05463"
    #>  [6697] "05464" "05465" "05466" "05467" "05468" "05469" "05470" "05471"
    #>  [6705] "05472" "05473" "05474" "05475" "-9999" "-9999" "-9999" "05476"
    #>  [6713] "05477" "05478" "05480" "05481" "05482" "05483" "05484" "05485"
    #>  [6721] "05486" "05487" "05488" "05489" "05490" "05491" "-9999" "-9999"
    #>  [6729] "-9999" "05492" "05494" "05495" "05496" "05497" "05498" "05499"
    #>  [6737] "05500" "05501" "05502" "05503" "05504" "-9999" "-9999" "-9999"
    #>  [6745] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6753] "-9999" "-9999" "-9999" "-9999" "05505" "05506" "05507" "05508"
    #>  [6761] "05509" "-9999" "05511" "-9999" "05512" "-9999" "07187" "07188"
    #>  [6769] "07189" "07190" "07191" "07192" "07193" "-9999" "-9999" "-9999"
    #>  [6777] "-9999" "08472" "-9999" "-9999" "08475" "-9999" "-9999" "-9999"
    #>  [6785] "-9999" "08474" "-9999" "-9999" "-9999" "08476" "08477" "-9999"
    #>  [6793] "08622" "-9999" "08623" "-9999" "-9999" "-9999" "08624" "-9999"
    #>  [6801] "08625" "-9999" "-9999" "-9999" "-9999" "-9999" "08626" "08753"
    #>  [6809] "-9999" "-9999" "08754" "-9999" "08761" "08755" "08756" "08757"
    #>  [6817] "08758" "-9999" "08759" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6825] "08762" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6833] "09091" "09092" "09093" "09094" "09095" "09096" "-9999" "-9999"
    #>  [6841] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6849] "-9999" "-9999" "-9999" "05821" "05822" "05823" "05824" "05826"
    #>  [6857] "05828" "05829" "05830" "05831" "05832" "05833" "05834" "05835"
    #>  [6865] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6873] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05825" "05984"
    #>  [6881] "05986" "05987" "05988" "05989" "05990" "05991" "05992" "05993"
    #>  [6889] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6897] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6905] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6913] "-9999" "-9999" "-9999" "-9999" "06146" "06147" "06148" "06149"
    #>  [6921] "06150" "06151" "06152" "06153" "06154" "06155" "06156" "06157"
    #>  [6929] "06158" "06159" "06160" "06161" "-9999" "-9999" "-9999" "-9999"
    #>  [6937] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6945] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6953] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00668" "06335"
    #>  [6961] "06336" "06337" "06338" "06338" "06339" "06339" "06340" "06341"
    #>  [6969] "06342" "06342" "06343" "06343" "06344" "06344" "-9999" "-9999"
    #>  [6977] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6985] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [6993] "06510" "06511" "06512" "06513" "06514" "06515" "06516" "-9999"
    #>  [7001] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7009] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7017] "-9999" "-9999" "06678" "06679" "06680" "06681" "06682" "06683"
    #>  [7025] "06684" "06685" "06686" "06687" "06688" "06689" "-9999" "-9999"
    #>  [7033] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7041] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7049] "-9999" "-9999" "-9999" "06887" "06888" "06889" "06890" "06891"
    #>  [7057] "06893" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7065] "-9999" "07063" "07064" "07066" "07067" "07069" "07071" "-9999"
    #>  [7073] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7081] "07197" "07198" "07199" "07200" "07201" "07202" "07203" "07431"
    #>  [7089] "07433" "07434" "-9999" "-9999" "07435" "-9999" "-9999" "07436"
    #>  [7097] "-9999" "-9999" "07437" "07438" "07432" "-9999" "07550" "-9999"
    #>  [7105] "-9999" "07551" "-9999" "-9999" "-9999" "-9999" "07553" "07554"
    #>  [7113] "07555" "07556" "-9999" "-9999" "07558" "07559" "-9999" "07695"
    #>  [7121] "07696" "07697" "07698" "-9999" "-9999" "07699" "-9999" "-9999"
    #>  [7129] "-9999" "07700" "-9999" "07701" "-9999" "-9999" "-9999" "-9999"
    #>  [7137] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7145] "-9999" "-9999" "-9999" "-9999" "07806" "07807" "07809" "07810"
    #>  [7153] "07811" "07812" "07813" "07814" "07815" "-9999" "-9999" "-9999"
    #>  [7161] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7169] "-9999" "07927" "07928" "07929" "07930" "07931" "07932" "07933"
    #>  [7177] "07934" "07935" "07936" "07937" "07938" "07939" "07940" "-9999"
    #>  [7185] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7193] "-9999" "-9999" "-9999" "-9999" "08067" "08069" "08070" "08071"
    #>  [7201] "08072" "08073" "08074" "08075" "08076" "08077" "08078" "08079"
    #>  [7209] "08080" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08219"
    #>  [7217] "08220" "08221" "08222" "08224" "08226" "08227" "08228" "08229"
    #>  [7225] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7233] "-9999" "08375" "08376" "08377" "08378" "08379" "08380" "08381"
    #>  [7241] "08382" "08383" "08384" "08385" "08386" "08388" "08389" "08390"
    #>  [7249] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08481" "08482"
    #>  [7257] "08483" "08484" "08485" "08486" "08487" "08488" "-9999" "-9999"
    #>  [7265] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7273] "-9999" "-9999" "08478" "08479" "08480" "08489" "08490" "08491"
    #>  [7281] "08492" "08493" "08494" "08495" "08496" "08497" "08498" "08499"
    #>  [7289] "08500" "08501" "08502" "08503" "08504" "08505" "08506" "08507"
    #>  [7297] "08508" "08509" "08510" "08511" "08512" "-9999" "-9999" "-9999"
    #>  [7305] "08513" "08514" "08515" "08517" "08518" "08519" "08520" "08521"
    #>  [7313] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7321] "-9999" "-9999" "-9999" "-9999" "08522" "08523" "08524" "08525"
    #>  [7329] "08526" "08528" "08529" "08530" "08531" "08532" "08533" "08534"
    #>  [7337] "08535" "08536" "08537" "08538" "08539" "08540" "08541" "08542"
    #>  [7345] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08543" "08544"
    #>  [7353] "08545" "08546" "08547" "08548" "08550" "08551" "08552" "08553"
    #>  [7361] "08554" "08555" "08556" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7369] "-9999" "-9999" "-9999" "-9999" "-9999" "08557" "08558" "08559"
    #>  [7377] "08560" "08561" "08562" "08563" "08564" "08565" "08566" "08567"
    #>  [7385] "08568" "08569" "08570" "08571" "-9999" "-9999" "-9999" "-9999"
    #>  [7393] "-9999" "-9999" "-9999" "08572" "08573" "08574" "08575" "08576"
    #>  [7401] "08577" "08578" "08579" "08580" "08581" "-9999" "-9999" "-9999"
    #>  [7409] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7417] "-9999" "08582" "08583" "08584" "08585" "08586" "08587" "08588"
    #>  [7425] "08589" "08590" "08591" "08592" "08593" "08594" "08596" "08597"
    #>  [7433] "08598" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7441] "-9999" "-9999" "-9999" "-9999" "08599" "08600" "08601" "08602"
    #>  [7449] "08603" "08604" "08605" "08606" "-9999" "-9999" "-9999" "-9999"
    #>  [7457] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7465] "-9999" "-9999" "-9999" "-9999" "-9999" "08610" "08611" "08612"
    #>  [7473] "08613" "08614" "08615" "08616" "08617" "08618" "08619" "08620"
    #>  [7481] "08621" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7489] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7497] "08627" "08628" "08629" "08630" "08631" "08632" "08633" "08634"
    #>  [7505] "08635" "08636" "08637" "08638" "08639" "-9999" "-9999" "-9999"
    #>  [7513] "-9999" "08640" "08641" "08642" "08643" "08644" "08645" "-9999"
    #>  [7521] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7529] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7537] "08663" "08664" "08666" "08667" "08668" "08669" "08670" "08671"
    #>  [7545] "08672" "08673" "08674" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7553] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7561] "-9999" "08675" "08676" "08677" "08678" "08679" "08680" "08681"
    #>  [7569] "08682" "08683" "08684" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7577] "-9999" "08685" "08687" "08688" "08690" "08691" "08693" "08695"
    #>  [7585] "08696" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7593] "08697" "08698" "08699" "08700" "08701" "08702" "08703" "-9999"
    #>  [7601] "-9999" "-9999" "-9999" "-9999" "08704" "08705" "08706" "08707"
    #>  [7609] "08709" "08710" "08711" "08712" "08714" "-9999" "-9999" "-9999"
    #>  [7617] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7625] "-9999" "-9999" "-9999" "-9999" "08713" "08715" "08716" "08717"
    #>  [7633] "08718" "08719" "08721" "08722" "08723" "08724" "08725" "08726"
    #>  [7641] "08727" "08728" "08729" "08730" "08731" "08732" "-9999" "-9999"
    #>  [7649] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08733" "08734"
    #>  [7657] "08735" "08736" "08738" "08739" "08740" "-9999" "-9999" "-9999"
    #>  [7665] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08743"
    #>  [7673] "08744" "08745" "08746" "08747" "08748" "08749" "08750" "08751"
    #>  [7681] "08752" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7689] "-9999" "-9999" "-9999" "08763" "08764" "08765" "08766" "08767"
    #>  [7697] "08768" "08769" "08770" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7705] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08771"
    #>  [7713] "08772" "08774" "08775" "08776" "08777" "08778" "08779" "08773"
    #>  [7721] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7729] "-9999" "-9999" "-9999" "08781" "08782" "08783" "08784" "08785"
    #>  [7737] "08786" "08787" "08788" "08789" "08790" "08791" "08792" "08793"
    #>  [7745] "08794" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7753] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7761] "-9999" "-9999" "-9999" "08795" "08796" "08797" "08799" "08800"
    #>  [7769] "08801" "08802" "08803" "-9999" "08804" "08805" "08806" "08807"
    #>  [7777] "08808" "08809" "08810" "08811" "08812" "-9999" "-9999" "-9999"
    #>  [7785] "-9999" "-9999" "-9999" "-9999" "-9999" "08813" "08814" "08815"
    #>  [7793] "08816" "08817" "08818" "08819" "08820" "08821" "08822" "08823"
    #>  [7801] "08824" "08825" "08826" "08827" "08828" "-9999" "-9999" "-9999"
    #>  [7809] "-9999" "-9999" "-9999" "-9999" "-9999" "08829" "08830" "08831"
    #>  [7817] "08832" "08833" "08834" "08835" "08836" "08837" "-9999" "08838"
    #>  [7825] "08839" "08840" "08841" "08842" "08843" "08844" "08845" "08846"
    #>  [7833] "08847" "08848" "-9999" "-9999" "-9999" "-9999" "08849" "08850"
    #>  [7841] "08853" "08855" "08856" "08857" "08858" "08859" "-9999" "08860"
    #>  [7849] "08861" "08862" "08863" "08864" "08865" "08866" "-9999" "-9999"
    #>  [7857] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7865] "08867" "08868" "08869" "08870" "08871" "08872" "08873" "08874"
    #>  [7873] "08875" "08876" "08877" "08878" "08879" "08880" "08881" "08882"
    #>  [7881] "08883" "08884" "08885" "08886" "08887" "08888" "08889" "-9999"
    #>  [7889] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7897] "-9999" "-9999" "-9999" "-9999" "-9999" "08890" "08892" "08893"
    #>  [7905] "08894" "08895" "08896" "08897" "08898" "08899" "08900" "08901"
    #>  [7913] "08902" "08903" "08904" "08905" "08906" "-9999" "-9999" "-9999"
    #>  [7921] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08908" "08909"
    #>  [7929] "08910" "08911" "08913" "08914" "08915" "08916" "08917" "08918"
    #>  [7937] "08919" "08920" "08921" "08922" "08923" "08924" "08924" "08925"
    #>  [7945] "08926" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08940"
    #>  [7953] "08942" "08944" "08945" "08946" "08947" "08949" "08950" "08951"
    #>  [7961] "08953" "08954" "08955" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7969] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [7977] "-9999" "01264" "-9999" "-9999" "-9999" "-9999" "-9999" "08956"
    #>  [7985] "08957" "08958" "08961" "08962" "08963" "08964" "08965" "08966"
    #>  [7993] "-9999" "-9999" "-9999" "-9999" "11009" "11011" "11491" "-9999"
    #>  [8001] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08967" "08968"
    #>  [8009] "08969" "08970" "08971" "08972" "08973" "08974" "08975" "08976"
    #>  [8017] "08977" "08978" "08980" "08981" "08982" "08983" "08984" "-9999"
    #>  [8025] "-9999" "-9999" "-9999" "-9999" "08985" "08986" "08987" "08988"
    #>  [8033] "08989" "08990" "08991" "08992" "08993" "08994" "08995" "08996"
    #>  [8041] "08997" "08999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8049] "-9999" "-9999" "09000" "09001" "09002" "09003" "09004" "09005"
    #>  [8057] "09006" "09007" "09008" "09009" "09010" "09011" "09013" "09014"
    #>  [8065] "09015" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8073] "-9999" "-9999" "06612" "09018" "09019" "09020" "-9999" "09021"
    #>  [8081] "09022" "09023" "09024" "09025" "09026" "09027" "-9999" "-9999"
    #>  [8089] "-9999" "-9999" "-9999" "-9999" "-9999" "09028" "09029" "09030"
    #>  [8097] "09031" "09032" "09033" "09034" "09035" "09036" "09038" "09039"
    #>  [8105] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8113] "08050" "08122" "09040" "09041" "09042" "09043" "09045" "09046"
    #>  [8121] "09047" "09048" "09050" "09050" "09051" "09052" "-9999" "-9999"
    #>  [8129] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00512"
    #>  [8137] "09054" "09055" "09056" "09057" "09059" "09060" "09062" "09063"
    #>  [8145] "09064" "09065" "09067" "09068" "09069" "09071" "09072" "09073"
    #>  [8153] "-9999" "-9999" "-9999" "09075" "09077" "09078" "09079" "09080"
    #>  [8161] "09081" "09082" "09083" "09084" "09085" "09086" "09087" "09088"
    #>  [8169] "09089" "09090" "-9999" "09098" "09099" "09100" "09101" "09103"
    #>  [8177] "09104" "09105" "09106" "09107" "-9999" "-9999" "-9999" "-9999"
    #>  [8185] "-9999" "-9999" "09109" "09110" "09111" "09112" "-9999" "09115"
    #>  [8193] "09116" "09117" "09118" "09119" "09120" "09121" "09122" "09123"
    #>  [8201] "09124" "09126" "-9999" "-9999" "09128" "09129" "09130" "-9999"
    #>  [8209] "-9999" "09131" "09132" "-9999" "09133" "09135" "-9999" "-9999"
    #>  [8217] "-9999" "09136" "09137" "09138" "09139" "09140" "09141" "-9999"
    #>  [8225] "-9999" "09143" "09144" "-9999" "09145" "-9999" "09146" "09142"
    #>  [8233] "-9999" "-9999" "09147" "09148" "09149" "09150" "09151" "-9999"
    #>  [8241] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8249] "-9999" "-9999" "-9999" "-9999" "09159" "09160" "09161" "-9999"
    #>  [8257] "09162" "-9999" "09163" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8265] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8273] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "09166" "-9999"
    #>  [8281] "09167" "09175" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8289] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8297] "-9999" "-9999" "-9999" "-9999" "-9999" "03153" "09176" "09177"
    #>  [8305] "09178" "09179" "09180" "09181" "09182" "09183" "09184" "-9999"
    #>  [8313] "09185" "09186" "09187" "09188" "09189" "09190" "09192" "09194"
    #>  [8321] "09194" "09195" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8329] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8337] "-9999" "-9999" "-9999" "-9999" "09196" "09197" "09198" "09199"
    #>  [8345] "09200" "09201" "09202" "09206" "09207" "09208" "-9999" "-9999"
    #>  [8353] "-9999" "-9999" "-9999" "09209" "09210" "09212" "09213" "09214"
    #>  [8361] "09215" "09216" "09217" "09218" "09219" "09220" "09221" "09222"
    #>  [8369] "09223" "09224" "09225" "09226" "09227" "09228" "09229" "09230"
    #>  [8377] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8385] "09241" "09242" "09243" "09244" "09245" "09246" "09247" "09248"
    #>  [8393] "09249" "09250" "09251" "09252" "-9999" "-9999" "-9999" "-9999"
    #>  [8401] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8409] "-9999" "-9999" "-9999" "-9999" "-9999" "09253" "-9999" "-9999"
    #>  [8417] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8425] "-9999" "09262" "09263" "09264" "09267" "-9999" "-9999" "-9999"
    #>  [8433] "-9999" "-9999" "-9999" "-9999" "-9999" "09268" "09269" "09270"
    #>  [8441] "09271" "09272" "09276" "09277" "09278" "-9999" "-9999" "-9999"
    #>  [8449] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "09279" "09286"
    #>  [8457] "09287" "-9999" "-9999" "-9999" "09293" "-9999" "09295" "09296"
    #>  [8465] "-9999" "09288" "09289" "09290" "09291" "09292" "09294" "09297"
    #>  [8473] "09302" "09298" "09300" "-9999" "-9999" "-9999" "09303" "09306"
    #>  [8481] "-9999" "-9999" "-9999" "-9999" "-9999" "09301" "09304" "-9999"
    #>  [8489] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8497] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8505] "-9999" "-9999" "-9999" "-9999" "-9999" "09308" "-9999" "-9999"
    #>  [8513] "09309" "09310" "09312" "09313" "09315" "09316" "09317" "09318"
    #>  [8521] "-9999" "-9999" "-9999" "09320" "09321" "09322" "09323" "09324"
    #>  [8529] "09325" "09326" "09327" "09328" "09329" "09330" "09331" "-9999"
    #>  [8537] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8545] "09343" "09345" "09346" "09347" "09348" "09349" "09350" "09351"
    #>  [8553] "09352" "09353" "09355" "09356" "-9999" "-9999" "-9999" "-9999"
    #>  [8561] "09344" "09357" "09358" "09359" "-9999" "09360" "09361" "-9999"
    #>  [8569] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8577] "-9999" "-9999" "09362" "09363" "09364" "09365" "09366" "09372"
    #>  [8585] "-9999" "09374" "09375" "09376" "09377" "09378" "09379" "09380"
    #>  [8593] "09381" "09382" "09383" "09384" "09385" "09386" "09387" "-9999"
    #>  [8601] "-9999" "-9999" "-9999" "09388" "09389" "09390" "09391" "09392"
    #>  [8609] "09393" "09394" "09395" "09396" "09397" "09398" "09399" "09400"
    #>  [8617] "09401" "09402" "09403" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8625] "-9999" "-9999" "09407" "09410" "09411" "09412" "-9999" "-9999"
    #>  [8633] "-9999" "-9999" "09413" "09414" "09415" "09416" "09417" "09418"
    #>  [8641] "09419" "09420" "09421" "09422" "09424" "09425" "09426" "09427"
    #>  [8649] "09428" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8657] "-9999" "-9999" "-9999" "09430" "09431" "09432" "09433" "09435"
    #>  [8665] "09436" "09437" "09438" "09441" "-9999" "-9999" "-9999" "09442"
    #>  [8673] "09443" "09449" "09450" "09451" "09452" "09453" "09454" "09462"
    #>  [8681] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8689] "-9999" "-9999" "-9999" "-9999" "09463" "09464" "09465" "09466"
    #>  [8697] "09467" "09468" "09469" "09470" "09471" "09472" "09473" "09474"
    #>  [8705] "09476" "09477" "09479" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8713] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05528" "05529"
    #>  [8721] "05530" "05531" "-9999" "05533" "05534" "05535" "05536" "05537"
    #>  [8729] "05538" "05539" "05540" "05541" "05542" "05543" "05544" "05545"
    #>  [8737] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8745] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8753] "-9999" "-9999" "-9999" "-9999" "-9999" "00043" "00045" "00046"
    #>  [8761] "05546" "05547" "05549" "05550" "05551" "05553" "05555" "-9999"
    #>  [8769] "05556" "05557" "05558" "05559" "05560" "05561" "05562" "05563"
    #>  [8777] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8785] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8793] "-9999" "-9999" "-9999" "-9999" "-9999" "05564" "05565" "05566"
    #>  [8801] "05569" "05570" "05571" "05572" "05573" "05574" "05575" "05577"
    #>  [8809] "05578" "05579" "05580" "05582" "05583" "05584" "05585" "05586"
    #>  [8817] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8825] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8833] "-9999" "-9999" "-9999" "05554" "-9999" "-9999" "-9999" "-9999"
    #>  [8841] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8849] "-9999" "-9999" "05588" "05589" "05590" "05591" "05592" "05594"
    #>  [8857] "05595" "05596" "05597" "05598" "05599" "05600" "05603" "05604"
    #>  [8865] "05605" "05619" "-9999" "05608" "-9999" "-9999" "-9999" "-9999"
    #>  [8873] "-9999" "-9999" "-9999" "-9999" "05606" "05607" "05609" "05611"
    #>  [8881] "05612" "-9999" "05614" "05617" "05618" "05620" "-9999" "-9999"
    #>  [8889] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8897] "-9999" "05622" "05623" "05624" "05625" "05626" "05627" "05628"
    #>  [8905] "05629" "05630" "05631" "05632" "05634" "05635" "05636" "05637"
    #>  [8913] "05638" "05639" "05640" "05641" "-9999" "-9999" "-9999" "-9999"
    #>  [8921] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05642" "05643"
    #>  [8929] "05644" "05645" "05647" "05648" "05649" "05650" "05652" "05654"
    #>  [8937] "05655" "05656" "05657" "05658" "05660" "05661" "05661" "05662"
    #>  [8945] "05663" "05672" "-9999" "-9999" "-9999" "05664" "05666" "05667"
    #>  [8953] "05668" "05669" "-9999" "05670" "05671" "05673" "-9999" "-9999"
    #>  [8961] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8969] "05676" "05678" "05679" "05680" "05682" "05683" "05684" "05685"
    #>  [8977] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [8985] "-9999" "-9999" "-9999" "-9999" "-9999" "05696" "-9999" "05697"
    #>  [8993] "05698" "05700" "05701" "05702" "05703" "05704" "-9999" "05706"
    #>  [9001] "05707" "05708" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9009] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9017] "-9999" "-9999" "-9999" "-9999" "05713" "05714" "05715" "05717"
    #>  [9025] "05718" "05719" "05720" "05721" "05723" "05724" "05725" "-9999"
    #>  [9033] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9041] "-9999" "-9999" "05728" "05729" "05730" "05731" "05732" "05733"
    #>  [9049] "05734" "05735" "05736" "05737" "05739" "05740" "05741" "05742"
    #>  [9057] "05743" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9065] "-9999" "05744" "05745" "05746" "05747" "05749" "05750" "05751"
    #>  [9073] "05752" "05753" "05754" "05755" "05756" "05757" "05758" "05759"
    #>  [9081] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9089] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9097] "-9999" "-9999" "-9999" "-9999" "-9999" "05763" "05764" "-9999"
    #>  [9105] "05766" "05767" "05768" "05769" "05787" "06263" "-9999" "-9999"
    #>  [9113] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9121] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9129] "-9999" "-9999" "-9999" "-9999" "-9999" "05770" "05771" "05772"
    #>  [9137] "05773" "05774" "05775" "05776" "05777" "-9999" "-9999" "-9999"
    #>  [9145] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9153] "-9999" "-9999" "05779" "05780" "05781" "05782" "05783" "05784"
    #>  [9161] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9169] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9177] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9185] "-9999" "-9999" "05785" "05786" "05788" "05789" "05790" "05791"
    #>  [9193] "05792" "05793" "05794" "05795" "05796" "05797" "05798" "05799"
    #>  [9201] "05800" "05801" "05802" "-9999" "-9999" "-9999" "-9999" "05803"
    #>  [9209] "05805" "05806" "05808" "05809" "-9999" "-9999" "-9999" "-9999"
    #>  [9217] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05810" "05811"
    #>  [9225] "05812" "05813" "05814" "05815" "05816" "05817" "05818" "05819"
    #>  [9233] "05820" "-9999" "-9999" "-9999" "00266" "05836" "05837" "05838"
    #>  [9241] "05839" "05840" "05841" "05842" "05843" "05844" "05845" "-9999"
    #>  [9249] "-9999" "-9999" "-9999" "05846" "05847" "05848" "05849" "05850"
    #>  [9257] "05851" "05852" "05853" "05854" "05855" "05856" "05857" "05858"
    #>  [9265] "05859" "05860" "05861" "05862" "05863" "-9999" "-9999" "-9999"
    #>  [9273] "-9999" "05864" "05865" "05866" "05867" "05868" "05869" "05870"
    #>  [9281] "05871" "05872" "05873" "05874" "05875" "05876" "05877" "05878"
    #>  [9289] "05879" "05880" "05881" "05882" "05883" "05884" "05885" "05886"
    #>  [9297] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9305] "05887" "05888" "05889" "05890" "05891" "05892" "05893" "05894"
    #>  [9313] "05895" "05896" "05898" "05899" "05900" "05901" "05902" "05903"
    #>  [9321] "05904" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9329] "05905" "05906" "05907" "05908" "05909" "05910" "05911" "05912"
    #>  [9337] "05913" "05914" "05915" "-9999" "-9999" "-9999" "-9999" "05917"
    #>  [9345] "05918" "05920" "05921" "05922" "05923" "05924" "05925" "05926"
    #>  [9353] "05927" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05928"
    #>  [9361] "05930" "05931" "05932" "05934" "05935" "05936" "05937" "05938"
    #>  [9369] "05939" "05940" "05942" "05943" "05944" "05945" "05941" "-9999"
    #>  [9377] "-9999" "-9999" "05946" "05947" "05948" "-9999" "05950" "05951"
    #>  [9385] "05952" "05953" "05954" "05955" "05956" "05957" "05958" "05959"
    #>  [9393] "05960" "05961" "05962" "05963" "05964" "05965" "05966" "05967"
    #>  [9401] "05969" "05970" "05971" "05968" "-9999" "-9999" "-9999" "-9999"
    #>  [9409] "05929" "05972" "05973" "05974" "05975" "05976" "05977" "05978"
    #>  [9417] "05979" "05980" "05981" "05982" "05983" "-9999" "-9999" "-9999"
    #>  [9425] "05994" "05995" "05996" "05997" "05998" "05999" "06000" "-9999"
    #>  [9433] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9441] "-9999" "-9999" "-9999" "-9999" "-9999" "06009" "06010" "06011"
    #>  [9449] "06012" "-9999" "06013" "06014" "06023" "-9999" "-9999" "-9999"
    #>  [9457] "-9999" "-9999" "-9999" "-9999" "06015" "06016" "06018" "06019"
    #>  [9465] "06020" "06021" "06022" "06025" "06026" "06027" "06028" "06029"
    #>  [9473] "06030" "-9999" "-9999" "-9999" "06031" "06032" "06034" "06035"
    #>  [9481] "06036" "-9999" "06037" "06038" "06039" "06040" "06041" "06042"
    #>  [9489] "06043" "06044" "06045" "06046" "06047" "06048" "06049" "06050"
    #>  [9497] "06051" "06052" "06053" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9505] "-9999" "-9999" "-9999" "-9999" "-9999" "06055" "06056" "06058"
    #>  [9513] "06059" "06060" "06066" "06061" "06062" "06063" "06064" "06068"
    #>  [9521] "06069" "-9999" "06070" "06071" "06072" "06074" "06075" "06076"
    #>  [9529] "06077" "06078" "06079" "06080" "06083" "-9999" "-9999" "-9999"
    #>  [9537] "06081" "06082" "06084" "06085" "06086" "06087" "06088" "06089"
    #>  [9545] "06090" "06091" "06092" "06093" "06094" "06095" "06096" "-9999"
    #>  [9553] "-9999" "06097" "06098" "06099" "06100" "06101" "06102" "06103"
    #>  [9561] "06104" "06105" "06106" "06107" "-9999" "-9999" "-9999" "-9999"
    #>  [9569] "-9999" "00492" "06108" "06110" "06111" "06112" "06113" "06114"
    #>  [9577] "06115" "06116" "06117" "06118" "06119" "06120" "06121" "06122"
    #>  [9585] "06123" "06124" "06125" "06126" "06127" "06128" "-9999" "-9999"
    #>  [9593] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06129" "06130"
    #>  [9601] "06131" "06132" "06133" "06134" "06137" "06138" "06139" "06140"
    #>  [9609] "06141" "06142" "06143" "06144" "06145" "-9999" "-9999" "-9999"
    #>  [9617] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9625] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "00539" "06164"
    #>  [9633] "06165" "06166" "06167" "06168" "06169" "06170" "06171" "06172"
    #>  [9641] "06173" "06174" "06176" "06177" "06178" "06181" "06182" "06183"
    #>  [9649] "06184" "06185" "06186" "06187" "06188" "06194" "06199" "-9999"
    #>  [9657] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9665] "-9999" "-9999" "-9999" "06189" "06190" "06191" "06192" "06193"
    #>  [9673] "06196" "06197" "06198" "06200" "06202" "06203" "06204" "06205"
    #>  [9681] "06206" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9689] "-9999" "-9999" "-9999" "-9999" "-9999" "06207" "06208" "06209"
    #>  [9697] "06210" "06213" "06214" "06216" "06217" "06219" "06221" "06223"
    #>  [9705] "06224" "06225" "06226" "06227" "06228" "06229" "06230" "06231"
    #>  [9713] "06232" "06233" "06234" "06235" "06236" "06237" "-9999" "-9999"
    #>  [9721] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9729] "-9999" "-9999" "-9999" "-9999" "-9999" "06238" "06239" "06240"
    #>  [9737] "06241" "06242" "06243" "06244" "06245" "06246" "06247" "06248"
    #>  [9745] "06250" "06251" "06252" "06256" "-9999" "06258" "-9999" "-9999"
    #>  [9753] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9761] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9769] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9777] "-9999" "-9999" "-9999" "-9999" "06259" "06260" "06261" "06262"
    #>  [9785] "06264" "06265" "06266" "06267" "06269" "06270" "06271" "06272"
    #>  [9793] "06273" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9801] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9809] "-9999" "-9999" "06274" "06275" "06276" "06277" "06278" "06279"
    #>  [9817] "06280" "06281" "06283" "06284" "06287" "06288" "-9999" "-9999"
    #>  [9825] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9833] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9841] "-9999" "06282" "06285" "06289" "06290" "06291" "06292" "06293"
    #>  [9849] "-9999" "06295" "06296" "06297" "06298" "06299" "-9999" "-9999"
    #>  [9857] "-9999" "-9999" "-9999" "-9999" "-9999" "06300" "06301" "06302"
    #>  [9865] "06303" "06304" "06305" "06306" "-9999" "-9999" "-9999" "-9999"
    #>  [9873] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9881] "-9999" "-9999" "-9999" "-9999" "06318" "06307" "06308" "06309"
    #>  [9889] "06310" "06311" "-9999" "06313" "06314" "06315" "06316" "06317"
    #>  [9897] "06319" "06320" "06321" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9905] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9913] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9921] "-9999" "-9999" "06322" "06323" "06324" "06325" "06326" "06328"
    #>  [9929] "06329" "06330" "06331" "06332" "06333" "06334" "-9999" "-9999"
    #>  [9937] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06345" "06346"
    #>  [9945] "06347" "06348" "06349" "-9999" "06351" "06352" "06353" "06355"
    #>  [9953] "06356" "06357" "06358" "06359" "-9999" "-9999" "-9999" "-9999"
    #>  [9961] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06360" "06361"
    #>  [9969] "06362" "06363" "06364" "06366" "-9999" "06367" "06368" "06370"
    #>  [9977] "06371" "06372" "06377" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9985] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #>  [9993] "-9999" "-9999" "06374" "06375" "06376" "06378" "06380" "06381"
    #> [10001] "06383" "06384" "06385" "06386" "06387" "-9999" "-9999" "-9999"
    #> [10009] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10017] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10025] "06390" "06391" "06392" "06393" "06394" "06396" "06397" "06399"
    #> [10033] "06400" "06401" "06402" "-9999" "06404" "06405" "-9999" "-9999"
    #> [10041] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10049] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10057] "06407" "06408" "06410" "06411" "06412" "06413" "06416" "06417"
    #> [10065] "06418" "06419" "06420" "06421" "06422" "06424" "06425" "06426"
    #> [10073] "06427" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10081] "-9999" "-9999" "06428" "06429" "06430" "06431" "06432" "06433"
    #> [10089] "06434" "06435" "06436" "06437" "06438" "06439" "06441" "06442"
    #> [10097] "06443" "06444" "06445" "06446" "06448" "-9999" "-9999" "-9999"
    #> [10105] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10113] "-9999" "-9999" "06447" "06449" "06450" "06451" "06452" "06453"
    #> [10121] "06454" "06455" "06456" "06457" "06458" "06459" "06460" "06461"
    #> [10129] "06462" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10137] "-9999" "-9999" "-9999" "-9999" "06463" "06464" "06465" "06466"
    #> [10145] "06467" "06468" "06469" "06470" "06470" "06471" "06472" "06473"
    #> [10153] "06475" "06477" "06478" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10161] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10169] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10177] "06480" "06480" "06481" "06482" "06483" "06484" "06486" "06487"
    #> [10185] "06488" "06489" "06490" "06491" "06492" "-9999" "-9999" "-9999"
    #> [10193] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10201] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10209] "-9999" "-9999" "-9999" "-9999" "06493" "06494" "06495" "06496"
    #> [10217] "06497" "06498" "06499" "06500" "06501" "06503" "06504" "06504"
    #> [10225] "06507" "06508" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10233] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06517"
    #> [10241] "06518" "06519" "06520" "06521" "06522" "06523" "06524" "06525"
    #> [10249] "06526" "06527" "06528" "06529" "-9999" "-9999" "-9999" "-9999"
    #> [10257] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06530"
    #> [10265] "06531" "06532" "06533" "06534" "06535" "06536" "06537" "06538"
    #> [10273] "06539" "06540" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10281] "-9999" "-9999" "-9999" "-9999" "-9999" "06542" "06543" "06545"
    #> [10289] "06546" "06547" "06548" "06549" "06552" "06553" "06554" "06555"
    #> [10297] "06557" "06558" "06563" "06564" "-9999" "-9999" "-9999" "-9999"
    #> [10305] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06559"
    #> [10313] "06560" "06562" "06565" "06566" "06567" "06568" "06569" "06570"
    #> [10321] "06571" "06573" "06574" "06575" "06576" "06577" "06585" "-9999"
    #> [10329] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10337] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06578" "06579"
    #> [10345] "06580" "06581" "06582" "06583" "06584" "06586" "06587" "06588"
    #> [10353] "06589" "06590" "06592" "06593" "06594" "06595" "06596" "06597"
    #> [10361] "06598" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10369] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10377] "-9999" "-9999" "06600" "06601" "06602" "06605" "06607" "06608"
    #> [10385] "06609" "06610" "06611" "06613" "06614" "06615" "-9999" "-9999"
    #> [10393] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10401] "-9999" "-9999" "06616" "06617" "06618" "06619" "06620" "06622"
    #> [10409] "06623" "06625" "06626" "06627" "06644" "06645" "-9999" "-9999"
    #> [10417] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10425] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10433] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10441] "-9999" "-9999" "-9999" "06476" "06479" "06638" "06639" "06640"
    #> [10449] "06641" "06643" "06646" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10457] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10465] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10473] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06647" "06648"
    #> [10481] "06649" "06650" "06651" "06652" "06655" "06656" "06657" "06658"
    #> [10489] "06659" "06660" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10497] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10505] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06662" "06663"
    #> [10513] "06664" "06665" "06666" "06667" "06668" "06670" "06671" "06672"
    #> [10521] "06673" "06674" "06675" "06676" "06677" "-9999" "-9999" "-9999"
    #> [10529] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10537] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10545] "-9999" "-9999" "06690" "06691" "06693" "06694" "06695" "06696"
    #> [10553] "06697" "06698" "06699" "06700" "06701" "06702" "06703" "06733"
    #> [10561] "06750" "06759" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10569] "-9999" "06704" "06705" "06706" "06707" "06708" "06709" "06710"
    #> [10577] "06711" "06713" "06714" "06715" "06716" "06717" "06718" "06719"
    #> [10585] "06720" "06721" "06722" "06723" "06724" "06725" "06726" "06727"
    #> [10593] "06728" "06729" "06730" "06731" "-9999" "-9999" "-9999" "-9999"
    #> [10601] "-9999" "-9999" "06732" "06734" "06735" "06737" "06738" "06739"
    #> [10609] "06740" "06741" "06742" "06743" "06744" "06745" "06746" "06747"
    #> [10617] "06748" "06749" "06751" "06752" "06753" "06754" "06755" "06756"
    #> [10625] "06757" "06758" "06760" "06761" "06762" "06763" "06764" "-9999"
    #> [10633] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10641] "-9999" "06765" "06766" "06767" "06768" "06769" "06770" "06771"
    #> [10649] "06772" "06773" "06774" "06775" "06776" "-9999" "-9999" "-9999"
    #> [10657] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10665] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10673] "-9999" "06777" "06779" "06786" "06788" "06791" "06794" "-9999"
    #> [10681] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10689] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10697] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10705] "-9999" "-9999" "06796" "06798" "06803" "06806" "06815" "06819"
    #> [10713] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10721] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06824" "-9999"
    #> [10729] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10737] "-9999" "-9999" "-9999" "-9999" "-9999" "06834" "06835" "06848"
    #> [10745] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10753] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06850" "06856"
    #> [10761] "06857" "06858" "06862" "06864" "-9999" "-9999" "-9999" "-9999"
    #> [10769] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10777] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [10785] "06867" "06868" "06869" "06871" "06873" "06875" "-9999" "-9999"
    #> [10793] "-9999" "-9999" "-9999" "-9999" "-9999" "01008" "06895" "06896"
    #> [10801] "06899" "06903" "06904" "06906" "-9999" "-9999" "-9999" "-9999"
    #> [10809] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "06908"
    #> [10817] "06916" "06917" "06918" "06921" "06923" "06925" "-9999" "-9999"
    #> [10825] "-9999" "-9999" "-9999" "-9999" "-9999" "06926" "06927" "06928"
    #> [10833] "06929" "06930" "06931" "06932" "06933" "06934" "06935" "06936"
    #> [10841] "06937" "06938" "06939" "06940" "06941" "06942" "06943" "-9999"
    #> [10849] "-9999" "-9999" "06951" "06950" "06953" "-9999" "-9999" "06955"
    #> [10857] "06957" "06960" "-9999" "-9999" "-9999" "06963" "-9999" "06961"
    #> [10865] "06965" "06966" "-9999" "-9999" "-9999" "-9999" "06968" "-9999"
    #> [10873] "06967" "06969" "06970" "06971" "-9999" "-9999" "06972" "06973"
    #> [10881] "06974" "-9999" "-9999" "06976" "06977" "06978" "06979" "06980"
    #> [10889] "06981" "06982" "06983" "06984" "06985" "06986" "06987" "06992"
    #> [10897] "06989" "06988" "06990" "06991" "06993" "06994" "06995" "06996"
    #> [10905] "06997" "06998" "06999" "07000" "-9999" "07001" "07003" "07002"
    #> [10913] "07004" "07007" "07006" "-9999" "07009" "07010" "07012" "07011"
    #> [10921] "07014" "07015" "07016" "07017" "-9999" "07018" "07019" "07013"
    #> [10929] "01087" "07020" "07021" "07022" "-9999" "07023" "07024" "-9999"
    #> [10937] "07025" "-9999" "07027" "07029" "-9999" "07030" "-9999" "-9999"
    #> [10945] "-9999" "07032" "07033" "07034" "-9999" "-9999" "07038" "07039"
    #> [10953] "07042" "07043" "07044" "07045" "07046" "07048" "-9999" "07051"
    #> [10961] "07052" "-9999" "-9999" "-9999" "07055" "-9999" "07057" "07058"
    #> [10969] "07054" "07059" "07060" "07061" "07062" "07078" "-9999" "-9999"
    #> [10977] "07072" "07073" "07075" "-9999" "-9999" "-9999" "07080" "07081"
    #> [10985] "07077" "-9999" "07082" "07084" "07074" "-9999" "07085" "-9999"
    #> [10993] "-9999" "-9999" "-9999" "07086" "07087" "07088" "07089" "07090"
    #> [11001] "07091" "07092" "07093" "07094" "07095" "07096" "07097" "07098"
    #> [11009] "07099" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11017] "-9999" "07105" "07100" "07101" "07102" "07103" "07104" "07106"
    #> [11025] "07107" "07108" "07109" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11033] "07110" "07111" "07112" "07113" "07114" "07115" "07116" "07117"
    #> [11041] "07118" "07119" "07120" "-9999" "-9999" "-9999" "07121" "07122"
    #> [11049] "07123" "07124" "07125" "07126" "07127" "07128" "07129" "07130"
    #> [11057] "07131" "-9999" "-9999" "07132" "07133" "07134" "07135" "07136"
    #> [11065] "07137" "07138" "07139" "07140" "07141" "-9999" "-9999" "-9999"
    #> [11073] "07142" "07143" "07144" "07145" "07146" "07147" "07148" "07149"
    #> [11081] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07150"
    #> [11089] "07151" "07153" "07155" "07156" "07157" "07158" "07159" "-9999"
    #> [11097] "07161" "07162" "07163" "07164" "07166" "07167" "07168" "07169"
    #> [11105] "07170" "07171" "07172" "07173" "07174" "-9999" "-9999" "-9999"
    #> [11113] "-9999" "07175" "07176" "07177" "07178" "07179" "07180" "07181"
    #> [11121] "07182" "07183" "07184" "-9999" "07185" "07186" "-9999" "-9999"
    #> [11129] "-9999" "07206" "07207" "07208" "07209" "07210" "-9999" "07212"
    #> [11137] "07212" "-9999" "-9999" "07213" "07214" "07215" "07216" "07217"
    #> [11145] "07218" "07219" "07220" "07221" "07222" "07225" "-9999" "-9999"
    #> [11153] "-9999" "-9999" "07224" "07226" "07227" "-9999" "-9999" "-9999"
    #> [11161] "-9999" "-9999" "-9999" "07230" "07231" "07232" "07233" "07234"
    #> [11169] "07235" "07237" "07238" "07239" "-9999" "-9999" "-9999" "-9999"
    #> [11177] "-9999" "-9999" "07240" "07241" "07242" "07243" "07244" "07245"
    #> [11185] "07246" "07247" "07248" "07249" "07250" "07251" "07252" "07253"
    #> [11193] "07254" "-9999" "-9999" "-9999" "-9999" "07255" "07256" "07257"
    #> [11201] "07258" "07259" "07260" "07261" "-9999" "-9999" "-9999" "-9999"
    #> [11209] "-9999" "-9999" "-9999" "07262" "07264" "07265" "07266" "07267"
    #> [11217] "07268" "07269" "07270" "07271" "07272" "07273" "07274" "07275"
    #> [11225] "-9999" "07276" "07277" "07278" "07279" "07280" "07281" "07282"
    #> [11233] "07283" "07284" "07285" "07286" "07287" "07288" "-9999" "-9999"
    #> [11241] "-9999" "-9999" "07289" "07290" "07291" "07292" "07293" "07295"
    #> [11249] "07296" "-9999" "-9999" "07297" "07298" "07300" "07301" "07302"
    #> [11257] "07303" "07304" "07305" "07306" "-9999" "07315" "07316" "07317"
    #> [11265] "07319" "07320" "07321" "07322" "07323" "-9999" "-9999" "-9999"
    #> [11273] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11281] "-9999" "-9999" "-9999" "-9999" "07324" "07325" "07326" "07327"
    #> [11289] "07328" "07329" "07330" "07331" "07332" "07333" "07334" "07335"
    #> [11297] "07336" "07337" "07338" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11305] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07339" "07340"
    #> [11313] "-9999" "07341" "07342" "07343" "07345" "07346" "07347" "07348"
    #> [11321] "07349" "07350" "07351" "07352" "07353" "07354" "-9999" "-9999"
    #> [11329] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11337] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11345] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07355" "07356"
    #> [11353] "07357" "07358" "07359" "07360" "07361" "07362" "07363" "07364"
    #> [11361] "07365" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11369] "-9999" "-9999" "07367" "07369" "07370" "07371" "07372" "07373"
    #> [11377] "07374" "07375" "07376" "07377" "07378" "-9999" "-9999" "-9999"
    #> [11385] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07380" "07382"
    #> [11393] "07385" "07387" "07388" "07389" "-9999" "-9999" "-9999" "-9999"
    #> [11401] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11409] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11417] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07391" "07392"
    #> [11425] "07393" "07395" "07397" "07398" "07399" "07400" "07401" "-9999"
    #> [11433] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11441] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07366" "07368"
    #> [11449] "07402" "07403" "07404" "07405" "07406" "07407" "07409" "07410"
    #> [11457] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07411" "07412"
    #> [11465] "07413" "07414" "07415" "07416" "07417" "07418" "-9999" "-9999"
    #> [11473] "-9999" "-9999" "07440" "07441" "07442" "07443" "07444" "07445"
    #> [11481] "07446" "07447" "07448" "07449" "07451" "07452" "-9999" "-9999"
    #> [11489] "-9999" "07453" "07454" "07455" "07456" "07457" "07459" "07461"
    #> [11497] "-9999" "-9999" "-9999" "07463" "07464" "07465" "07466" "07467"
    #> [11505] "07468" "07469" "07470" "07471" "07472" "07473" "-9999" "07474"
    #> [11513] "07475" "07476" "07477" "07478" "07479" "07480" "07481" "07482"
    #> [11521] "07483" "07484" "07485" "-9999" "-9999" "-9999" "-9999" "07486"
    #> [11529] "07487" "07488" "07489" "07489" "07490" "07491" "07492" "07493"
    #> [11537] "-9999" "-9999" "-9999" "-9999" "-9999" "07494" "07495" "07496"
    #> [11545] "07497" "07498" "07499" "07500" "-9999" "-9999" "-9999" "-9999"
    #> [11553] "-9999" "-9999" "07502" "07503" "07504" "07506" "07508" "07509"
    #> [11561] "07510" "07511" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11569] "-9999" "-9999" "-9999" "07512" "07513" "07514" "07515" "07516"
    #> [11577] "07517" "07518" "07519" "07520" "07521" "07522" "-9999" "-9999"
    #> [11585] "07523" "07524" "07525" "07526" "07527" "07528" "-9999" "07529"
    #> [11593] "07530" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11601] "-9999" "07531" "07532" "07534" "07535" "07536" "07537" "07538"
    #> [11609] "07539" "07540" "07541" "07542" "07543" "07544" "07545" "-9999"
    #> [11617] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07560" "07561"
    #> [11625] "07562" "07563" "07564" "07565" "07566" "07567" "07568" "07569"
    #> [11633] "07570" "07571" "07572" "07573" "07574" "-9999" "-9999" "-9999"
    #> [11641] "-9999" "-9999" "-9999" "07575" "07576" "07577" "07578" "07579"
    #> [11649] "07580" "07581" "07582" "07583" "07584" "07585" "07586" "07587"
    #> [11657] "07588" "-9999" "-9999" "-9999" "-9999" "-9999" "07589" "07590"
    #> [11665] "07591" "07592" "07593" "07594" "07595" "07596" "07598" "07599"
    #> [11673] "07600" "07601" "07607" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11681] "-9999" "-9999" "-9999" "07602" "07603" "07604" "07605" "07606"
    #> [11689] "07608" "07609" "07610" "07612" "07613" "07614" "07615" "07616"
    #> [11697] "-9999" "-9999" "-9999" "07617" "07618" "07619" "07620" "07621"
    #> [11705] "07622" "07623" "07624" "07625" "07626" "07627" "07628" "07629"
    #> [11713] "07630" "07631" "07741" "-9999" "-9999" "07632" "07633" "07634"
    #> [11721] "07635" "07636" "07639" "07640" "07641" "07642" "07643" "-9999"
    #> [11729] "07645" "07646" "07647" "07648" "07649" "07650" "07651" "07652"
    #> [11737] "07653" "07654" "07655" "07656" "07657" "07658" "-9999" "-9999"
    #> [11745] "-9999" "-9999" "-9999" "07659" "07660" "07662" "07663" "07664"
    #> [11753] "07665" "-9999" "-9999" "07666" "07667" "07668" "07669" "07670"
    #> [11761] "07671" "07672" "07673" "07674" "07675" "07676" "-9999" "-9999"
    #> [11769] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11777] "-9999" "-9999" "-9999" "07677" "07678" "07679" "07680" "07682"
    #> [11785] "07683" "07684" "07685" "07686" "07687" "07688" "07690" "07691"
    #> [11793] "07692" "07693" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11801] "-9999" "-9999" "07702" "07703" "07704" "07705" "07706" "07707"
    #> [11809] "07708" "07709" "07710" "07711" "07712" "07713" "07714" "07715"
    #> [11817] "07716" "07717" "07718" "07719" "07720" "-9999" "-9999" "-9999"
    #> [11825] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01799"
    #> [11833] "07721" "07722" "07723" "07724" "07725" "07726" "07727" "07728"
    #> [11841] "07729" "07730" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11849] "-9999" "-9999" "07731" "07732" "07733" "07735" "07736" "07737"
    #> [11857] "07738" "07739" "07740" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11865] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07742"
    #> [11873] "07744" "07745" "07746" "07747" "07748" "07749" "07750" "07751"
    #> [11881] "07752" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11889] "-9999" "-9999" "07753" "07754" "07755" "07756" "07758" "07759"
    #> [11897] "07760" "07761" "07762" "07763" "07764" "07765" "07766" "07767"
    #> [11905] "07768" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11913] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07769" "07770"
    #> [11921] "07771" "07772" "07773" "07774" "-9999" "-9999" "-9999" "-9999"
    #> [11929] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11937] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11945] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11953] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [11961] "-9999" "-9999" "-9999" "-9999" "07775" "07776" "07777" "07779"
    #> [11969] "07780" "07781" "07782" "07784" "07785" "-9999" "-9999" "-9999"
    #> [11977] "07786" "07787" "07788" "07789" "07790" "-9999" "07791" "07792"
    #> [11985] "07793" "07794" "07795" "07796" "07797" "-9999" "-9999" "-9999"
    #> [11993] "-9999" "07798" "07799" "07800" "07801" "07802" "07803" "07804"
    #> [12001] "07805" "-9999" "-9999" "-9999" "-9999" "07816" "07817" "07818"
    #> [12009] "07819" "07820" "07822" "07823" "07824" "07825" "-9999" "-9999"
    #> [12017] "-9999" "07826" "07827" "07828" "07829" "07831" "-9999" "-9999"
    #> [12025] "-9999" "-9999" "-9999" "07832" "07833" "07834" "07835" "07836"
    #> [12033] "07837" "07838" "07839" "07840" "07841" "-9999" "-9999" "-9999"
    #> [12041] "07842" "07843" "07844" "07847" "07848" "07849" "07850" "07851"
    #> [12049] "07852" "-9999" "-9999" "07853" "07855" "07856" "07857" "07858"
    #> [12057] "07859" "07860" "07854" "-9999" "07861" "07862" "07865" "-9999"
    #> [12065] "07866" "07867" "07868" "07869" "-9999" "-9999" "07871" "07872"
    #> [12073] "07873" "07874" "07875" "07876" "07877" "07878" "07880" "-9999"
    #> [12081] "-9999" "-9999" "07870" "07881" "07882" "07883" "07884" "07885"
    #> [12089] "07886" "07887" "07888" "07889" "07890" "07891" "07892" "-9999"
    #> [12097] "-9999" "-9999" "-9999" "07893" "07894" "07895" "07896" "07897"
    #> [12105] "07902" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12113] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12121] "-9999" "-9999" "-9999" "-9999" "07913" "07914" "07915" "07917"
    #> [12129] "07918" "07920" "07921" "07922" "07923" "07925" "-9999" "-9999"
    #> [12137] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "07942"
    #> [12145] "07943" "-9999" "07945" "07946" "07947" "07948" "07949" "07950"
    #> [12153] "07951" "07953" "-9999" "-9999" "-9999" "-9999" "-9999" "07941"
    #> [12161] "07954" "07955" "07956" "07957" "07958" "07959" "07960" "07961"
    #> [12169] "07962" "07964" "07965" "07966" "-9999" "-9999" "-9999" "-9999"
    #> [12177] "-9999" "-9999" "07967" "07968" "07969" "07970" "07971" "07972"
    #> [12185] "07973" "07974" "07975" "07976" "07977" "07978" "-9999" "-9999"
    #> [12193] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12201] "07979" "07981" "07982" "07983" "07984" "07987" "07991" "-9999"
    #> [12209] "-9999" "-9999" "-9999" "07988" "07989" "07990" "-9999" "-9999"
    #> [12217] "-9999" "-9999" "-9999" "07992" "07993" "07995" "07997" "07998"
    #> [12225] "07999" "08000" "08002" "08003" "-9999" "-9999" "-9999" "-9999"
    #> [12233] "-9999" "-9999" "07996" "08004" "08005" "08006" "08007" "08008"
    #> [12241] "08009" "08010" "08011" "-9999" "08013" "08014" "08015" "08016"
    #> [12249] "08017" "08018" "08019" "08020" "08021" "08022" "08023" "08024"
    #> [12257] "08025" "08026" "08027" "08028" "08029" "08030" "08031" "08032"
    #> [12265] "08033" "08034" "08035" "08036" "08037" "08038" "-9999" "-9999"
    #> [12273] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12281] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08081" "08082"
    #> [12289] "08083" "08085" "08086" "08087" "08090" "08091" "08092" "-9999"
    #> [12297] "08094" "08095" "08096" "08097" "-9999" "08099" "-9999" "08101"
    #> [12305] "08102" "08103" "08104" "08106" "-9999" "-9999" "-9999" "-9999"
    #> [12313] "08108" "08109" "08110" "08111" "08112" "08113" "08115" "08117"
    #> [12321] "08118" "08120" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12329] "-9999" "08124" "08125" "08126" "08128" "08130" "08131" "08132"
    #> [12337] "08133" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08134"
    #> [12345] "-9999" "08135" "08137" "08138" "08139" "08140" "08142" "08143"
    #> [12353] "08144" "-9999" "-9999" "-9999" "-9999" "08145" "08146" "08147"
    #> [12361] "08148" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12369] "-9999" "08149" "08150" "08151" "08152" "08153" "08154" "08156"
    #> [12377] "08157" "08158" "08159" "08161" "-9999" "-9999" "-9999" "-9999"
    #> [12385] "-9999" "-9999" "-9999" "-9999" "-9999" "08163" "08164" "08165"
    #> [12393] "08166" "08167" "08169" "08170" "08171" "08172" "08173" "08174"
    #> [12401] "08176" "08177" "08178" "08192" "-9999" "-9999" "-9999" "-9999"
    #> [12409] "-9999" "08179" "08180" "08181" "08182" "08183" "08184" "08185"
    #> [12417] "08186" "08187" "08188" "08189" "08190" "08191" "-9999" "-9999"
    #> [12425] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08193" "08195"
    #> [12433] "08197" "08198" "08199" "08200" "08201" "08202" "08203" "08205"
    #> [12441] "08206" "08207" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12449] "-9999" "-9999" "-9999" "-9999" "08208" "08209" "08210" "08212"
    #> [12457] "08213" "08214" "08215" "08216" "08217" "08218" "08204" "08230"
    #> [12465] "08231" "08232" "08233" "08234" "08235" "08236" "08237" "08238"
    #> [12473] "08239" "08240" "08241" "08242" "-9999" "-9999" "-9999" "-9999"
    #> [12481] "-9999" "-9999" "-9999" "08243" "08244" "08245" "08246" "08247"
    #> [12489] "08248" "08249" "08250" "08251" "08252" "08253" "08254" "08255"
    #> [12497] "-9999" "-9999" "-9999" "08256" "08257" "08258" "08259" "08260"
    #> [12505] "08261" "08262" "08263" "08264" "08265" "08266" "08268" "08269"
    #> [12513] "08271" "08272" "08275" "-9999" "08273" "08274" "08276" "08277"
    #> [12521] "08278" "08279" "08280" "08281" "08282" "08283" "-9999" "-9999"
    #> [12529] "-9999" "-9999" "-9999" "-9999" "08284" "08285" "08286" "08287"
    #> [12537] "08288" "08289" "08290" "08291" "08292" "08293" "08294" "08295"
    #> [12545] "08296" "08297" "08298" "08299" "-9999" "-9999" "-9999" "-9999"
    #> [12553] "-9999" "08300" "08301" "08304" "08305" "08306" "08307" "08308"
    #> [12561] "08309" "08310" "08311" "08313" "08314" "08315" "08316" "08317"
    #> [12569] "08318" "08319" "08320" "-9999" "08322" "08323" "08324" "-9999"
    #> [12577] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12585] "-9999" "-9999" "08325" "08327" "08328" "08329" "08330" "08331"
    #> [12593] "08334" "08335" "08336" "08338" "08339" "08340" "-9999" "-9999"
    #> [12601] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08341"
    #> [12609] "08342" "08343" "08344" "08345" "08347" "08348" "-9999" "-9999"
    #> [12617] "-9999" "-9999" "-9999" "-9999" "-9999" "08349" "08350" "08351"
    #> [12625] "08352" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12633] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12641] "-9999" "-9999" "-9999" "-9999" "-9999" "08355" "08357" "08358"
    #> [12649] "08359" "08361" "08362" "08365" "08369" "08371" "08372" "08373"
    #> [12657] "08374" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12665] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08394"
    #> [12673] "08395" "08396" "08397" "08399" "08391" "08392" "-9999" "-9999"
    #> [12681] "-9999" "-9999" "-9999" "-9999" "08400" "-9999" "08402" "08403"
    #> [12689] "08404" "08405" "08406" "08407" "08408" "08409" "-9999" "-9999"
    #> [12697] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "08411" "08412"
    #> [12705] "08413" "08414" "-9999" "-9999" "-9999" "08416" "08417" "08419"
    #> [12713] "08420" "08421" "08422" "08423" "08424" "-9999" "08425" "08426"
    #> [12721] "08427" "08428" "08429" "08430" "08431" "08432" "08433" "-9999"
    #> [12729] "-9999" "08436" "08437" "08438" "08439" "08440" "08441" "08442"
    #> [12737] "08443" "08444" "08445" "08446" "08447" "08448" "08449" "08450"
    #> [12745] "08451" "08452" "-9999" "-9999" "-9999" "-9999" "08453" "08454"
    #> [12753] "08455" "08456" "08457" "08458" "08459" "08460" "08461" "08462"
    #> [12761] "08463" "08464" "-9999" "-9999" "08465" "08466" "08467" "08468"
    #> [12769] "08469" "08470" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12777] "-9999" "09481" "09482" "09483" "09485" "-9999" "09487" "09488"
    #> [12785] "09489" "09490" "09491" "-9999" "-9999" "-9999" "-9999" "10281"
    #> [12793] "10282" "10284" "10285" "10286" "10287" "10288" "10289" "10290"
    #> [12801] "10292" "10293" "10294" "-9999" "-9999" "-9999" "-9999" "10655"
    #> [12809] "10657" "10658" "10659" "10660" "10663" "10664" "-9999" "-9999"
    #> [12817] "-9999" "-9999" "-9999" "-9999" "10823" "-9999" "10824" "10825"
    #> [12825] "10826" "10828" "10829" "10831" "10833" "-9999" "-9999" "-9999"
    #> [12833] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "11156"
    #> [12841] "11157" "11158" "11159" "11160" "11161" "11163" "11164" "11165"
    #> [12849] "-9999" "11167" "11168" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12857] "-9999" "11284" "11285" "11286" "11287" "11288" "11289" "11290"
    #> [12865] "11291" "11292" "11293" "11295" "11449" "-9999" "-9999" "11446"
    #> [12873] "11447" "11448" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12881] "-9999" "-9999" "-9999" "09492" "09493" "09494" "09495" "09496"
    #> [12889] "09497" "09498" "09501" "09502" "09503" "09504" "09505" "-9999"
    #> [12897] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12905] "-9999" "-9999" "-9999" "09720" "09722" "09724" "09725" "-9999"
    #> [12913] "09727" "09728" "09729" "09730" "09732" "09733" "09734" "-9999"
    #> [12921] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12929] "-9999" "09940" "09941" "09942" "09943" "09944" "09945" "09948"
    #> [12937] "09951" "09953" "-9999" "-9999" "-9999" "-9999" "-9999" "10155"
    #> [12945] "10156" "10157" "10158" "10160" "10161" "10162" "10164" "10165"
    #> [12953] "10166" "10167" "10169" "10170" "10171" "-9999" "-9999" "-9999"
    #> [12961] "10194" "10195" "10196" "10197" "10198" "10199" "10200" "10201"
    #> [12969] "10204" "10205" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12977] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [12985] "-9999" "-9999" "-9999" "10207" "10209" "10210" "10211" "10212"
    #> [12993] "10213" "10215" "10217" "10218" "10219" "10220" "10232" "-9999"
    #> [13001] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13009] "-9999" "-9999" "10221" "10222" "10223" "10225" "10227" "10228"
    #> [13017] "10229" "10230" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13025] "-9999" "-9999" "10233" "10234" "10235" "10236" "10237" "10238"
    #> [13033] "10239" "10240" "10241" "10242" "10243" "10244" "10245" "10246"
    #> [13041] "10247" "10248" "10249" "10250" "10251" "10252" "10253" "10254"
    #> [13049] "10255" "10256" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13057] "-9999" "-9999" "10266" "10267" "10268" "10269" "10270" "10271"
    #> [13065] "10272" "10274" "10275" "10276" "10277" "10278" "10279" "10280"
    #> [13073] "-9999" "-9999" "10296" "10297" "10298" "10299" "10300" "10301"
    #> [13081] "10302" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13089] "10303" "10304" "10305" "10306" "10307" "10308" "10309" "10310"
    #> [13097] "10311" "10312" "10313" "10314" "10315" "10316" "10335" "-9999"
    #> [13105] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13113] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13121] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13129] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13137] "10334" "10336" "10337" "10338" "10339" "10340" "-9999" "10342"
    #> [13145] "10344" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10345"
    #> [13153] "10346" "10347" "10349" "10351" "10353" "10354" "10355" "10356"
    #> [13161] "10357" "10359" "10361" "10362" "10364" "10365" "10366" "10367"
    #> [13169] "10368" "10369" "10370" "10371" "10372" "10373" "10376" "10352"
    #> [13177] "-9999" "-9999" "-9999" "-9999" "-9999" "10374" "10375" "10377"
    #> [13185] "10378" "10379" "10380" "10381" "10382" "10383" "10385" "10386"
    #> [13193] "10387" "10388" "10389" "10390" "-9999" "10391" "10393" "10394"
    #> [13201] "10395" "10396" "10397" "10398" "10399" "10401" "-9999" "-9999"
    #> [13209] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13217] "-9999" "10405" "10407" "10408" "10410" "10411" "10412" "-9999"
    #> [13225] "10415" "-9999" "10416" "10418" "10419" "-9999" "-9999" "-9999"
    #> [13233] "-9999" "-9999" "-9999" "10429" "10430" "10431" "10432" "10433"
    #> [13241] "10434" "10436" "10437" "10438" "10440" "10441" "10443" "-9999"
    #> [13249] "-9999" "-9999" "-9999" "-9999" "10446" "10447" "10448" "10449"
    #> [13257] "10450" "10451" "10452" "10454" "10455" "-9999" "-9999" "-9999"
    #> [13265] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10468" "10469"
    #> [13273] "10470" "10471" "10472" "10473" "10474" "10475" "10476" "10477"
    #> [13281] "10478" "10479" "10480" "10480" "10481" "10482" "10483" "10484"
    #> [13289] "10485" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10486"
    #> [13297] "10487" "10488" "10489" "10490" "10491" "10492" "10493" "10494"
    #> [13305] "10495" "10496" "-9999" "-9999" "-9999" "10497" "10498" "10500"
    #> [13313] "10501" "10502" "10503" "10504" "10505" "10506" "10507" "10508"
    #> [13321] "10509" "10510" "-9999" "-9999" "-9999" "-9999" "-9999" "10511"
    #> [13329] "10512" "10513" "10514" "10515" "10516" "10517" "10518" "10519"
    #> [13337] "10520" "10521" "10522" "10523" "10524" "10525" "10526" "10527"
    #> [13345] "10529" "10530" "10531" "10532" "10533" "10535" "-9999" "-9999"
    #> [13353] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10536"
    #> [13361] "10537" "10538" "10539" "10540" "10541" "10542" "10543" "10544"
    #> [13369] "10545" "10546" "10547" "10548" "10549" "10550" "10551" "10552"
    #> [13377] "10553" "10554" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13385] "-9999" "-9999" "10555" "10556" "10557" "10561" "10562" "10563"
    #> [13393] "10564" "10565" "10566" "10567" "10568" "10569" "10570" "10571"
    #> [13401] "10572" "10573" "10574" "10574" "10575" "10576" "10578" "10580"
    #> [13409] "-9999" "-9999" "-9999" "-9999" "-9999" "10581" "10582" "10584"
    #> [13417] "10585" "10586" "10587" "10588" "10589" "10590" "10591" "10592"
    #> [13425] "10593" "10594" "10595" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13433] "-9999" "-9999" "-9999" "-9999" "-9999" "10596" "10597" "10598"
    #> [13441] "10599" "10600" "10601" "10602" "10603" "10607" "10608" "10610"
    #> [13449] "10611" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13457] "10614" "10615" "10616" "10617" "10618" "10619" "10620" "10621"
    #> [13465] "10622" "10623" "10625" "10626" "10627" "10628" "10629" "10630"
    #> [13473] "10631" "10632" "10633" "10634" "10635" "10636" "10637" "10638"
    #> [13481] "-9999" "-9999" "-9999" "-9999" "10643" "10644" "10646" "10647"
    #> [13489] "10648" "10651" "10652" "10653" "-9999" "-9999" "-9999" "10666"
    #> [13497] "10667" "10668" "-9999" "10669" "10670" "10671" "10672" "10673"
    #> [13505] "10674" "10675" "10676" "10677" "10679" "10680" "10681" "10682"
    #> [13513] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10683" "10684"
    #> [13521] "10685" "10686" "10687" "10688" "10689" "10690" "10691" "10692"
    #> [13529] "10693" "-9999" "-9999" "-9999" "10694" "10695" "10696" "10697"
    #> [13537] "10698" "10699" "10700" "10701" "10702" "-9999" "-9999" "-9999"
    #> [13545] "-9999" "-9999" "-9999" "10703" "10705" "10706" "10707" "10708"
    #> [13553] "10709" "10710" "10711" "10712" "10713" "10714" "10715" "-9999"
    #> [13561] "-9999" "-9999" "-9999" "-9999" "-9999" "10716" "10717" "10718"
    #> [13569] "10719" "10720" "10721" "10722" "10723" "10724" "10725" "10726"
    #> [13577] "10727" "10728" "10729" "10730" "10731" "10732" "10733" "10734"
    #> [13585] "10735" "10736" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13593] "-9999" "-9999" "10737" "10738" "10739" "10740" "10741" "10743"
    #> [13601] "10744" "10745" "10746" "10747" "10748" "10749" "10750" "10751"
    #> [13609] "10752" "10753" "10754" "10755" "-9999" "-9999" "-9999" "-9999"
    #> [13617] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10756" "10757"
    #> [13625] "10758" "10759" "10760" "10761" "10763" "10764" "10765" "10766"
    #> [13633] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13641] "-9999" "10767" "10768" "10769" "10770" "10771" "10772" "10773"
    #> [13649] "10774" "10775" "10776" "10777" "10778" "10779" "10780" "10781"
    #> [13657] "10782" "10783" "10784" "10785" "10786" "10787" "10789" "10790"
    #> [13665] "10791" "10792" "10793" "10794" "10795" "10796" "-9999" "-9999"
    #> [13673] "-9999" "-9999" "10799" "-9999" "10806" "10797" "-9999" "10802"
    #> [13681] "-9999" "10800" "10804" "10805" "10822" "-9999" "-9999" "-9999"
    #> [13689] "-9999" "-9999" "-9999" "-9999" "-9999" "01521" "-9999" "10821"
    #> [13697] "-9999" "10810" "10812" "10813" "10814" "10815" "10816" "10818"
    #> [13705] "10820" "-9999" "-9999" "-9999" "-9999" "10837" "-9999" "10845"
    #> [13713] "10841" "-9999" "-9999" "10835" "10836" "10839" "-9999" "-9999"
    #> [13721] "10847" "-9999" NA      "-9999" "10848" "10850" "10851" "10852"
    #> [13729] "10853" "10854" "10855" "10856" "10857" "10858" "10859" "10860"
    #> [13737] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10866"
    #> [13745] "10834" "10861" "10864" "10865" "10869" "10870" "10871" "10872"
    #> [13753] "10874" "10875" "10876" "-9999" "10891" "10882" "-9999" "-9999"
    #> [13761] "-9999" "10878" "10879" "10883" "10884" "10885" "10886" "10887"
    #> [13769] "10888" "10889" "10890" "10892" "10893" "-9999" "-9999" "-9999"
    #> [13777] "-9999" "10895" "10898" "10900" "-9999" "07734" "08367" "10896"
    #> [13785] "10901" "10902" "10905" "10906" "-9999" "-9999" "11332" "-9999"
    #> [13793] "-9999" "-9999" "-9999" "-9999" "10921" "10909" "01559" "03473"
    #> [13801] "-9999" "-9999" "10919" NA      "-9999" "10907" "10908" "10910"
    #> [13809] "10911" "10913" "10916" "10917" "-9999" "-9999" "-9999" "-9999"
    #> [13817] "-9999" "-9999" "10923" "10924" "10922" "10925" "10926" "10927"
    #> [13825] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13833] "-9999" "10940" "10949" "-9999" "-9999" "-9999" "10937" "10938"
    #> [13841] "10942" "10943" "10944" "10945" "10946" "10947" "10948" "10950"
    #> [13849] "10952" "11028" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13857] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10957"
    #> [13865] "10958" "-9999" "10960" "-9999" "-9999" "10959" "10964" "-9999"
    #> [13873] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13881] "10967" "10968" "-9999" "10976" "07533" "10977" "10969" "10971"
    #> [13889] "10972" "10973" "10975" "10978" "-9999" "10980" "10981" "-9999"
    #> [13897] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13905] "10983" "10986" "10988" "10989" "10991" "10992" "10993" "10994"
    #> [13913] "10995" "10997" "10999" "11001" "11002" "-9999" "08363" "-9999"
    #> [13921] "06002" "11020" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13929] "-9999" "11033" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13937] "-9999" "11034" "11035" "11036" "-9999" "-9999" "-9999" "-9999"
    #> [13945] "11048" "11049" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13953] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13961] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [13969] "-9999" "-9999" "11068" "11069" "11070" "11071" "11072" "11073"
    #> [13977] "11074" "11075" "11076" "-9999" "11077" "11078" "11079" "11080"
    #> [13985] "11081" "11082" "11083" "11084" "-9999" "-9999" "-9999" "-9999"
    #> [13993] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14001] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14009] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "11093"
    #> [14017] "11094" "11095" "11096" "11097" "11098" "11099" "11100" "11101"
    #> [14025] "11102" "11103" "11104" "11105" "11106" "11108" "11109" "11110"
    #> [14033] "11111" "11112" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14041] "-9999" "-9999" "-9999" "11119" "-9999" "-9999" "-9999" "03724"
    #> [14049] "-9999" "11125" "-9999" "-9999" "-9999" "09266" "-9999" "-9999"
    #> [14057] "-9999" "-9999" "-9999" "-9999" "-9999" "11126" "11067" "11115"
    #> [14065] "11117" "11120" "11124" "11127" "11128" "11129" "11130" "11122"
    #> [14073] "11607" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "11131"
    #> [14081] "11132" "11133" "11134" "11135" "11136" "11137" "-9999" "11138"
    #> [14089] "11139" "11140" "11141" "11142" "11144" "11145" "-9999" "-9999"
    #> [14097] "-9999" "-9999" "-9999" "-9999" "11148" "11149" "11150" "11151"
    #> [14105] "11152" "11153" "11154" "11155" "-9999" "-9999" "-9999" "-9999"
    #> [14113] "11169" "11170" "11171" "11172" "11173" "11174" "11175" "11176"
    #> [14121] "11177" "11179" "11180" "-9999" "11184" "11186" "11189" "11191"
    #> [14129] "-9999" "-9999" "-9999" "11197" "11198" "11199" "11200" "11201"
    #> [14137] "11202" "11203" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14145] "-9999" "11204" "11205" "11206" "11207" "11208" "11209" "11210"
    #> [14153] "11211" "11212" "11213" "11214" "11215" "11216" "11217" "11218"
    #> [14161] "-9999" "-9999" "-9999" "-9999" "11219" "11220" "11221" "11222"
    #> [14169] "11223" "11224" "11225" "11226" "11228" "11229" "-9999" "-9999"
    #> [14177] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14185] "-9999" "11230" "11231" "11232" "11233" "11234" "11235" "11236"
    #> [14193] "11237" "11238" "11239" "11240" "-9999" "-9999" "-9999" "-9999"
    #> [14201] "-9999" "11241" "11242" "11243" "11244" "11245" "11246" "11249"
    #> [14209] "11250" "11251" "11252" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14217] "-9999" "-9999" "-9999" "11253" "11254" "11255" "11256" "11257"
    #> [14225] "11258" "11259" "11260" "11261" "11262" "11263" "11264" "11265"
    #> [14233] "11266" "11267" "11268" "11269" "-9999" "-9999" "-9999" "-9999"
    #> [14241] "-9999" "-9999" "-9999" "11481" "-9999" "-9999" "-9999" "-9999"
    #> [14249] "-9999" "11487" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14257] "11493" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14265] "-9999" "-9999" "-9999" "05674" "11503" "-9999" "-9999" "-9999"
    #> [14273] "-9999" "11509" "-9999" "-9999" "-9999" "-9999" "11514" "11515"
    #> [14281] "11516" "11517" "11518" "11519" "11520" "11521" "-9999" "-9999"
    #> [14289] "-9999" "-9999" "-9999" "-9999" "11522" "11523" "-9999" "05699"
    #> [14297] "-9999" "-9999" "-9999" "11533" "-9999" "-9999" "-9999" "05709"
    #> [14305] "-9999" "-9999" "-9999" "11540" "-9999" "-9999" "11544" "-9999"
    #> [14313] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14321] "-9999" "11554" "05727" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14329] "-9999" "-9999" "-9999" "11573" "-9999" "-9999" "05738" "-9999"
    #> [14337] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14345] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "11585" "-9999"
    #> [14353] "-9999" "11576" "-9999" "11579" "-9999" "-9999" "-9999" "11586"
    #> [14361] "11587" "11588" "05761" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14369] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14377] "-9999" "-9999" "-9999" "05765" "-9999" "-9999" "-9999" "-9999"
    #> [14385] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14393] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14401] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14409] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14417] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14425] "-9999" "05804" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14433] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14441] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "05807" "-9999"
    #> [14449] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14457] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14465] "11655" "-9999" "09507" "-9999" "09510" "-9999" "09512" "09513"
    #> [14473] "-9999" "-9999" "09516" "09517" "09518" "-9999" "09519" "09520"
    #> [14481] "-9999" "09522" "-9999" "09523" "09524" "09525" "09526" "09527"
    #> [14489] "09528" "09529" "-9999" "09530" "09531" "-9999" "-9999" "09532"
    #> [14497] "-9999" "-9999" "-9999" "09534" "-9999" "09535" "-9999" "09536"
    #> [14505] "09538" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14513] "-9999" "09539" "09540" "-9999" "09541" "-9999" "-9999" "-9999"
    #> [14521] "09542" "09543" "-9999" "09544" "-9999" "09545" "09546" "-9999"
    #> [14529] "-9999" "-9999" "09547" "09548" "-9999" "-9999" "09549" "09550"
    #> [14537] "09551" "-9999" "-9999" "-9999" "-9999" "09553" "-9999" "09554"
    #> [14545] "-9999" "09555" "-9999" "09556" "-9999" "09557" "-9999" "09558"
    #> [14553] "09560" "09561" "-9999" "-9999" "-9999" "09562" "-9999" "-9999"
    #> [14561] "-9999" "09564" "-9999" "09566" "09567" "09568" "-9999" "-9999"
    #> [14569] "09570" "09571" "09572" "09573" "09574" "09575" "-9999" "-9999"
    #> [14577] "-9999" "09576" "-9999" "09578" "-9999" "-9999" "09579" "-9999"
    #> [14585] "09581" "-9999" "-9999" "-9999" "09582" "-9999" "09583" "09585"
    #> [14593] "09584" "-9999" "09586" "-9999" "-9999" "09588" "-9999" "-9999"
    #> [14601] "09590" "-9999" "-9999" "-9999" "-9999" "09591" "-9999" "-9999"
    #> [14609] "-9999" "09592" "09593" "-9999" "-9999" "09594" "09595" "-9999"
    #> [14617] "09596" "09597" "-9999" "09602" "09604" "09605" "-9999" "-9999"
    #> [14625] "09615" "-9999" "-9999" "09621" "-9999" "-9999" "-9999" "-9999"
    #> [14633] "-9999" "09623" "-9999" "09624" "-9999" "-9999" "09625" "-9999"
    #> [14641] "09626" "-9999" "09627" "09628" "-9999" "-9999" "-9999" "-9999"
    #> [14649] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "09632"
    #> [14657] "-9999" "09633" "-9999" "09634" "-9999" "-9999" "-9999" "09636"
    #> [14665] "-9999" "-9999" "09637" "-9999" "-9999" "-9999" "09638" "-9999"
    #> [14673] "-9999" "-9999" "09639" "-9999" "09640" "09641" "09643" "09642"
    #> [14681] "-9999" "09645" "09646" "09647" "-9999" "-9999" "-9999" "-9999"
    #> [14689] "-9999" "-9999" "09651" "-9999" "-9999" "-9999" "-9999" "09652"
    #> [14697] "09653" "-9999" "-9999" "09654" "-9999" "-9999" "-9999" "-9999"
    #> [14705] "-9999" "-9999" "09656" "-9999" "-9999" "09657" "09658" "09659"
    #> [14713] "09660" "09661" "09662" "09663" "09664" "-9999" "-9999" "09666"
    #> [14721] "-9999" "-9999" "09667" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14729] "-9999" "-9999" "09669" "-9999" "09671" "09672" "09673" "09674"
    #> [14737] "-9999" "09677" "-9999" "-9999" "-9999" "-9999" "09679" "-9999"
    #> [14745] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14753] "-9999" "-9999" "-9999" "-9999" "09683" "09684" "09685" "09687"
    #> [14761] "09688" "09689" "09690" "09692" "09693" "09694" "09695" "09696"
    #> [14769] "09697" "09698" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14777] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14785] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14793] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14801] "-9999" "-9999" "09700" "09701" "09705" "09706" "-9999" "-9999"
    #> [14809] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14817] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14825] "09735" "09736" "09737" "09738" "09739" "09740" "09741" "09742"
    #> [14833] "09743" "09744" "09745" "09747" "09749" "09750" "09751" "09752"
    #> [14841] "-9999" "09754" "09755" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14849] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14857] "-9999" "-9999" "-9999" "-9999" "09756" "09757" "09758" "09759"
    #> [14865] "09760" "09761" "09762" "09763" "09763" "09765" "09766" "09767"
    #> [14873] "09768" "09769" "09771" "09772" "09773" "09774" "09775" "09776"
    #> [14881] "09777" "09779" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14889] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "09780"
    #> [14897] "09781" "09782" "09783" "09784" "09785" "09786" "09787" "09788"
    #> [14905] "09789" "09790" "09791" "09792" "09793" "09794" "09795" "09796"
    #> [14913] "09797" "09798" "09799" "09801" "09802" "09803" "-9999" "-9999"
    #> [14921] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14929] "09804" "09805" "09806" "09807" "09808" "09809" "09811" "09812"
    #> [14937] "09813" "09814" "09815" "09816" "09817" "09818" "09819" "09820"
    #> [14945] "09820" "09821" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14953] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14961] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14969] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [14977] "-9999" "-9999" "09822" "09823" "09826" "09833" "-9999" "-9999"
    #> [14985] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "09871"
    #> [14993] "09856" "09857" "09859" "09860" "09861" "09862" "09863" "09864"
    #> [15001] "09865" "09866" "09867" "09868" "09869" "09870" "09872" "09873"
    #> [15009] "09874" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15017] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15025] "09875" "09876" "09877" "09878" "09879" "09880" "09882" "09883"
    #> [15033] "09884" "09885" "09886" "09887" "-9999" "09888" "09889" "09890"
    #> [15041] "09891" "09892" "09893" "09894" "09895" "09897" "-9999" "09903"
    #> [15049] "09912" "09913" "09916" "09918" "-9999" "-9999" "-9999" "-9999"
    #> [15057] "-9999" "-9999" "-9999" "-9999" "-9999" "09898" "09899" "09900"
    #> [15065] "09901" "09902" "09904" "09905" "09906" "09907" "09908" "09909"
    #> [15073] "09910" "09911" "09914" "09915" "09917" "-9999" "-9999" "-9999"
    #> [15081] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "09954" "09955"
    #> [15089] "09956" "09957" "09958" "09959" "09960" "09961" "09962" "09963"
    #> [15097] "09964" "-9999" "09965" "09966" "09967" "09968" "09969" "09970"
    #> [15105] "09971" "09972" "09973" "09974" "09975" "09978" "-9999" "-9999"
    #> [15113] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "09976"
    #> [15121] "09977" "09979" "09980" "09981" "09983" "09984" "09985" "09986"
    #> [15129] "09987" "09988" "09989" "-9999" "09991" "09992" "-9999" "-9999"
    #> [15137] "-9999" "-9999" "10007" "09994" "09995" "09996" "09997" "09998"
    #> [15145] "09999" "10000" "10001" "10002" "10003" "10004" "10005" "10006"
    #> [15153] "10008" "10009" "10010" "10011" "10012" "10013" "10014" "10015"
    #> [15161] "10017" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15169] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10018"
    #> [15177] "10019" "10020" "10021" "10022" "10023" "10024" "10025" "10026"
    #> [15185] "10027" "10028" "10029" "10030" "10031" "10033" "10034" "10035"
    #> [15193] "10036" "10037" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15201] "-9999" "10038" "10039" "10040" "10041" "10042" "-9999" "10044"
    #> [15209] "10045" "10046" "10047" "10048" "10049" "10050" "10051" "10052"
    #> [15217] "10053" "10054" "10055" "10056" "10057" "10058" "10059" "-9999"
    #> [15225] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15233] "-9999" "10060" "10061" "10062" "10063" "10064" "10065" "10066"
    #> [15241] "10067" "10068" "10069" "10070" "10071" "10072" "10073" "10074"
    #> [15249] "10074" "10075" "10076" "10077" "-9999" "-9999" "-9999" "-9999"
    #> [15257] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15265] "-9999" "-9999" "-9999" "-9999" "-9999" "10078" "10080" "10081"
    #> [15273] "10082" "10084" "10086" "10087" "10088" "10090" "10091" "10092"
    #> [15281] "10093" "10094" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15289] "-9999" "10096" "10097" "10098" "10099" "10105" "10108" "-9999"
    #> [15297] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15305] "-9999" "-9999" "-9999" "-9999" "-9999" "10116" "10117" "10118"
    #> [15313] "10119" "10120" "10121" "10122" "10124" "-9999" "10125" "10126"
    #> [15321] "10127" "10128" "10129" "10131" "10132" "10134" "-9999" "-9999"
    #> [15329] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15337] "-9999" "-9999" "10135" "10136" "10137" "10138" "10139" "10140"
    #> [15345] "10141" "10142" "10145" "10146" "10147" "10148" "10149" "10150"
    #> [15353] "10151" "10152" "10153" "10154" "-9999" "-9999" "-9999" "-9999"
    #> [15361] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "10172"
    #> [15369] "10173" "10174" "10176" "10177" "10180" "-9999" "10181" "10182"
    #> [15377] "10183" "10185" "10186" "10187" "10188" "10189" "10191" "10192"
    #> [15385] "10193" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15393] "01340" "01341" "01342" "01343" "01344" "01345" "01346" "-9999"
    #> [15401] "01348" "01349" "01350" "01351" "01352" "01353" "01354" "01357"
    #> [15409] "01358" "01359" "01360" "01361" "01362" "01363" "01364" "-9999"
    #> [15417] "01375" "01376" "01377" "01378" "01379" "01380" "01381" "01382"
    #> [15425] "-9999" "-9999" "01389" "-9999" "01391" "01392" "01393" "-9999"
    #> [15433] "-9999" "-9999" "-9999" "-9999" "01399" "-9999" "-9999" "-9999"
    #> [15441] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15449] "01407" "-9999" "-9999" "-9999" "01412" "01413" "01414" "01415"
    #> [15457] "01416" "01417" "01418" "01420" "01421" "01422" "-9999" "-9999"
    #> [15465] "01423" "01424" "01425" "01426" "01427" "01428" "01429" "01430"
    #> [15473] "01431" "01432" "01433" "01434" "01436" "-9999" "-9999" "01437"
    #> [15481] "01438" "01439" "01440" "-9999" "01442" "01443" "01444" "01445"
    #> [15489] "-9999" "01447" "01448" "01449" "01450" "01451" "01452" "01453"
    #> [15497] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "01454" "01455"
    #> [15505] "01456" "01457" "01459" "01460" "01461" "01462" "01463" "01464"
    #> [15513] "01465" "01466" "01467" "01469" "01470" "-9999" "-9999" "-9999"
    #> [15521] "-9999" "-9999" "01471" "01472" "01473" "-9999" "-9999" "01476"
    #> [15529] "01477" "01478" "01479" "01480" "-9999" "-9999" "-9999" "-9999"
    #> [15537] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15545] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15553] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15561] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15569] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15577] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15585] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15593] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15601] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15609] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15617] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15625] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15633] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15641] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15649] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15657] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15665] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15673] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15681] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15689] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15697] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15705] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15713] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15721] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15729] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15737] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15745] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15753] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15761] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15769] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15777] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15785] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15793] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15801] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15809] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15817] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15825] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15833] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15841] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15849] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15857] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15865] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15873] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15881] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15889] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15897] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15905] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15913] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15921] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15929] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15937] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15945] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15953] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15961] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15969] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15977] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15985] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [15993] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16001] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16009] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16017] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16025] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16033] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16041] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16049] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16057] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16065] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16073] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16081] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16089] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16097] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16105] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16113] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16121] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16129] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16137] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16145] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16153] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16161] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16169] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16177] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16185] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16193] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16201] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16209] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16217] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16225] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16233] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16241] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16249] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16257] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16265] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16273] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16281] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16289] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16297] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16305] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16313] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16321] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16329] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16337] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16345] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16353] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16361] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16369] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16377] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16385] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16393] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16401] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16409] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16417] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16425] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16433] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16441] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16449] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16457] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16465] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16473] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16481] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16489] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16497] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16505] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16513] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16521] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16529] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16537] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16545] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16553] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16561] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16569] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16577] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16585] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16593] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16601] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16609] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16617] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16625] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16633] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16641] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16649] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16657] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16665] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16673] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16681] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16689] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16697] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16705] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16713] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16721] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16729] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16737] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16745] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16753] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16761] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16769] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16777] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16785] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16793] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16801] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16809] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16817] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16825] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16833] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16841] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16849] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16857] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16865] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16873] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16881] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16889] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16897] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16905] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16913] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16921] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16929] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16937] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16945] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16953] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16961] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16969] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16977] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16985] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [16993] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17001] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17009] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17017] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17025] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17033] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17041] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17049] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17057] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17065] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17073] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17081] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17089] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17097] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17105] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17113] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17121] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17129] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17137] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17145] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17153] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17161] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17169] "-9999" "00175" "00551" "00558" "02937" "-9999" "-9999" "10881"
    #> [17177] "11178" "11181" "11227" "-9999" "02428" "02429" "-9999" "02430"
    #> [17185] "02431" "02432" "02433" "02434" "-9999" "-9999" "02439" "02435"
    #> [17193] "-9999" "-9999" "-9999" "-9999" "02436" "02437" "02438" "01923"
    #> [17201] "01924" "01925" "01926" "01927" "-9999" "01937" "-9999" "-9999"
    #> [17209] "10456" "-9999" "10457" "10458" "10459" "-9999" "10460" "-9999"
    #> [17217] "10461" "10462" "10464" "10465" "10466" "10467" "11270" "11271"
    #> [17225] "-9999" "11272" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17233] "-9999" "-9999" "-9999" "11273" "11274" "-9999" "01935" "-9999"
    #> [17241] "11275" "-9999" "-9999" "11276" "-9999" "11278" "11277" "11279"
    #> [17249] "-9999" "11280" "-9999" "11281" "11282" "-9999" "11283" "-9999"
    #> [17257] "11296" "-9999" "11297" "11298" "11299" "11300" "-9999" "11301"
    #> [17265] "11302" "-9999" "11303" "11305" "11304" "11306" "11307" "11308"
    #> [17273] "11309" "11312" "11310" "11313" "11311" "-9999" "11314" "-9999"
    #> [17281] "11319" "11315" "11316" "11317" "11318" "11320" "11321" "-9999"
    #> [17289] "11322" "11323" "-9999" "-9999" "11334" "11335" "11336" "11324"
    #> [17297] "-9999" "-9999" "-9999" "02440" "02441" "02442" "-9999" "02443"
    #> [17305] "02444" "02445" "-9999" "01928" "-9999" "-9999" "01929" "01930"
    #> [17313] "01931" "01932" "-9999" "-9999" "-9999" "01933" "-9999" "-9999"
    #> [17321] "-9999" "-9999" "01936" "-9999" "01934" "-9999" "-9999" "02446"
    #> [17329] "-9999" "02447" "02448" "-9999" "02449" "02450" "02451" "02452"
    #> [17337] "02453" "-9999" "-9999" "02726" "02727" "02729" "02730" "02731"
    #> [17345] "-9999" "02733" "02736" "02737" "02735" "02739" "02740" "02741"
    #> [17353] "-9999" "02745" "02742" "02747" "02748" "02743" "02744" "02746"
    #> [17361] "02749" "02750" "02751" "02752" "02753" "-9999" "02754" "-9999"
    #> [17369] "02755" "-9999" "02756" "-9999" "02757" "02758" "02759" "02760"
    #> [17377] "02761" "02784" "02765" "02766" "02767" "-9999" "02768" "-9999"
    #> [17385] "02769" "02770" "02771" "02772" "02773" "02774" "02777" "02778"
    #> [17393] "02779" "02780" "02781" "02782" "02783" "02785" "-9999" "02762"
    #> [17401] "02763" "02764" "-9999" "02776" "02796" "02798" "02797" "02799"
    #> [17409] "02800" "02801" "02802" "02803" "02804" "02805" "-9999" "02806"
    #> [17417] "02807" "02808" "02809" "02813" "02810" "02811" "02812" "02814"
    #> [17425] "02815" "02816" "02817" "02818" "02819" "02820" "02821" "02822"
    #> [17433] "02823" "-9999" "02825" "02826" "02827" "02828" "-9999" "02829"
    #> [17441] "-9999" "02830" "02831" "02832" "02834" "02835" "02836" "-9999"
    #> [17449] "-9999" "-9999" "11425" "11424" "-9999" "-9999" "-9999" "-9999"
    #> [17457] "-9999" "11426" "11430" "11434" "-9999" "11435" "-9999" "-9999"
    #> [17465] "-9999" "11440" "-9999" "11444" "11437" "11392" "11393" "11394"
    #> [17473] "-9999" "-9999" "11395" "11396" "-9999" "11397" "11399" "11398"
    #> [17481] "11400" "-9999" "11401" "-9999" "11402" "11404" "-9999" "11405"
    #> [17489] "11403" "-9999" "-9999" "11455" "11456" "11457" "11458" "11459"
    #> [17497] "11460" "11461" "11462" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17505] "-9999" "11463" "11464" "11465" "-9999" "-9999" "-9999" "11466"
    #> [17513] "-9999" "11467" "-9999" "-9999" "-9999" "-9999" "02454" "02455"
    #> [17521] "-9999" "-9999" "02456" "02457" "-9999" "02415" "-9999" "02416"
    #> [17529] "02417" "02419" "02420" "02421" "02418" "02422" "-9999" "-9999"
    #> [17537] "02425" "02423" "-9999" "02424" "-9999" "-9999" "02426" "02427"
    #> [17545] "-9999" "-9999" "02838" "02839" "02840" "-9999" "02841" "02842"
    #> [17553] "03313" "03314" "03315" "03316" "03317" "03318" "-9999" "-9999"
    #> [17561] "-9999" "-9999" "01347" "-9999" "-9999" "-9999" "02393" "02394"
    #> [17569] "02395" "-9999" "02396" "02392" "-9999" "01692" "01691" "01685"
    #> [17577] "01686" "01687" "01688" "01690" "-9999" "-9999" "02414" "-9999"
    #> [17585] "02408" "-9999" "02410" "-9999" "02412" "02408" "02407" "-9999"
    #> [17593] "-9999" "02411" "02413" "-9999" "02405" "-9999" "02409" "02406"
    #> [17601] "-9999" "11468" "-9999" "-9999" "-9999" "11469" "11470" "-9999"
    #> [17609] "11471" "11472" "11473" "11474" "-9999" "11475" "11476" "-9999"
    #> [17617] "11477" "11478" "-9999" "-9999" "-9999" "11479" "11480" "11355"
    #> [17625] "11354" "-9999" "-9999" "11361" "-9999" "11356" "11357" "-9999"
    #> [17633] "11358" "-9999" "-9999" "11359" "11360" "11362" "11364" "11365"
    #> [17641] "11366" "-9999" "11367" "-9999" "-9999" "11368" "11369" "-9999"
    #> [17649] "11344" "-9999" "-9999" "11338" "11340" "11339" "11337" "11341"
    #> [17657] "11342" "11343" "02371" "-9999" "-9999" "02373" "02374" "-9999"
    #> [17665] "-9999" "-9999" "-9999" "-9999" "02372" "-9999" "11345" "11346"
    #> [17673] "11347" "-9999" "11348" "11350" "11351" "11349" "-9999" "-9999"
    #> [17681] "-9999" "-9999" "-9999" "02398" "02399" "02400" "02401" "02403"
    #> [17689] "02402" "02404" "-9999" "-9999" "02397" "-9999" "02384" "02385"
    #> [17697] "-9999" "-9999" "-9999" "02386" "02387" "02388" "02389" "-9999"
    #> [17705] "02390" "02391" "-9999" "07307" "-9999" "-9999" "-9999" "-9999"
    #> [17713] "07308" "07309" "07310" "04532" "-9999" "04533" "-9999" "-9999"
    #> [17721] "-9999" "-9999" "04534" "-9999" "04535" "-9999" "04537" "04536"
    #> [17729] "-9999" "04539" "04540" "04541" "04542" "04543" "04544" "-9999"
    #> [17737] "04545" "04546" "-9999" "04547" "04548" "-9999" "04549" "-9999"
    #> [17745] "-9999" "-9999" "-9999" "-9999" "-9999" "08927" "08927" "-9999"
    #> [17753] "-9999" "-9999" "08928" "-9999" "-9999" "08930" "-9999" "-9999"
    #> [17761] "-9999" "08932" "-9999" "-9999" "-9999" "08936" "08937" "-9999"
    #> [17769] "08933" "08934" "-9999" "-9999" "08938" "-9999" "-9999" "-9999"
    #> [17777] "-9999" "-9999" "-9999" "07421" "07422" "-9999" "-9999" "-9999"
    #> [17785] "07425" "07423" "-9999" "07427" "07428" "07429" "-9999" "07430"
    #> [17793] "-9999" "-9999" NA      "-9999" "11352" "-9999" "11353" "-9999"
    #> [17801] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17809] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17817] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" NA      "-9999"
    #> [17825] "-9999" "-9999" "05519" "-9999" "-9999" "05527" "05521" "-9999"
    #> [17833] "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999" "-9999"
    #> [17841] "-9999" "-9999" "11370" "-9999" "11371" "11372" "11374" "11375"
    #> [17849] "11376" "11378" "11379" "11380" "11381" "11382" "11383" "11386"
    #> [17857] "11391" "11389" "11377" "-9999" "-9999" "-9999" "-9999" "07312"
    #> [17865] "07311" "-9999" "07313" "07314" "-9999" "-9999" "05686" "05687"
    #> [17873] "-9999" "05688" "05689" "-9999" "05690" "11406" "11407" "-9999"
    #> [17881] "11408" "11409" "-9999" "11410" "11411" "-9999" "11412" "-9999"
    #> [17889] "11413" "11414" "11415" "11416" "11417" "11418" "-9999" "-9999"
    #> [17897] "-9999" "11420" "11421" "11422" "11423" "-9999" "-9999" "-9999"
    #> [17905] "-9999" "-9999" "09231" "09240" "-9999" "09239" "09238" "-9999"
    #> [17913] "-9999" "09237" "-9999" "-9999" "09236" "09235" "09234" "-9999"
    #> [17921] "09233" "09232" "-9999" "05691" "-9999" "-9999" "-9999" "05692"
    #> [17929] "05693" "05695" "-9999" "05694" "-9999" "-9999" "-9999" "-9999"
    #> [17937] "-9999" "-9999" "-9999" "05525" "-9999" "05515" "05524" "05522"
    #> [17945] "05523" "-9999" "05526" "05520" "05514" "05513" "05517" "-9999"
    #> [17953] "09336" "09333" "09335" "-9999" "09338" "-9999" "-9999" "05516"
    #> [17961] "05518" "09332"

    # =======
    # BEGIN OLD CODE
    # =======
    # cleaned_assets <- cleaned_assets %>%
    #   ungroup() %>%
    #   left_join(change_uid %>%
    #               filter(!is.na(uid)) %>%
    #               select(uid, key) %>%
    #               mutate(uid_dup_resolved = TRUE,
    #                      uid_dup_notes = "matched_by_name_constituency_to_previously_unmatched_result_uid"),
    #             by = "key",
    #             suffix = c("", "_new")) %>%
    #   mutate(uid = ifelse(is.na(uid_new), uid, uid_new))
      
    # Now we are going to remove some duplicated uid rows that are unnecessary
    # cleaned_assets <- cleaned_assets %>%
    #   group_by(uid) %>%
    #   mutate(remove = case_when(
    #     uid == "-9999" | is.na(uid) ~ FALSE,
    #     n() > 1 & any(entry == "new") & entry == "old" ~ TRUE,
    #     TRUE ~ FALSE
    #   ))


    not_found

    #> # A tibble: 18 x 6
    #>    key               uid_old constituency_co… entry candidate_name   uid  
    #>    <chr>             <chr>   <chr>            <chr> <chr>            <chr>
    #>  1 uuid:44cf31bf-cf… 00250   NA-120           old   rana afzaal hus… <NA> 
    #>  2 uuid:4aa68e97-07… 00256   PS-49            old   muhammad yousaf  <NA> 
    #>  3 uuid:127fc669-c2… 00279   PS-91            old   muhammad syed    <NA> 
    #>  4 uuid:a98a44c0-c1… 00494   NA-138           old   salman           <NA> 
    #>  5 uuid:7063096d-0e… 00495   NA-138           old   asif             <NA> 
    #>  6 uuid:85742a64-3a… 00791   NA-161           old   nawab amanullah… <NA> 
    #>  7 uuid:bb55a6d5-09… 03804   PS-54            old   ibrahim          <NA> 
    #>  8 uuid:b5542f53-35… 04589   PS-48            old   javaid ali       <NA> 
    #>  9 uuid:15341f27-42… 06247   PP-142           old   muhammad saleem  <NA> 
    #> 10 uuid:b867c11b-40… 06480   PP-157           old   abdul aleem khan <NA> 
    #> 11 uuid:2ffcf6d6-fa… 06480   PS-64            old   abdul aleem khan <NA> 
    #> 12 uuid:1ee2192d-71… 06584   NA-138           old   ghulam muhammad  <NA> 
    #> 13 uuid:86744263-80… 08325   PP-61            old   abdul hafeez     <NA> 
    #> 14 uuid:fb57cbe2-8f… 08631   PS-63            old   ghulam mustafa   <NA> 
    #> 15 uuid:c310da3f-dc… 09657   PS-63            old   abdul latif      <NA> 
    #> 16 uuid:7cd69f44-42… 10165   PP-61            old   muhammad mohsin  <NA> 
    #> 17 uuid:b1f8c502-5c… 10671   PS-53            old   ali bux          <NA> 
    #> 18 uuid:8132b5b9-ca… 11270   PS-63            old   abdul hameed     <NA>

    cleaned_assets %>%
      filter(constituency_code == "PK-30") %>%
      select(1:10)

    #> # A tibble: 28 x 10
    #>    enum_name enum_other type_seat const_number uid_initial candidate_name 
    #>        <dbl> <chr>      <chr>            <dbl> <chr>       <chr>          
    #>  1       125 <NA>       PK                  30 -9999       Abdul Qadoos K…
    #>  2       134 <NA>       PK                  30 -9999       Aman Ullah     
    #>  3       134 <NA>       PK                  30 -9999       Arbab Zarak Kh…
    #>  4       125 <NA>       PK                  30 -9999       Asad Ali       
    #>  5       125 <NA>       PK                  30 -9999       Atta Ullah     
    #>  6       134 <NA>       PK                  30 -9999       Fahad Ali      
    #>  7       134 <NA>       PK                  30 -9999       Khalid Masood  
    #>  8       125 <NA>       PK                  30 -9999       M Bashir       
    #>  9       125 <NA>       PK                  30 -9999       M Ijaz Khan    
    #> 10       134 <NA>       PK                  30 -9999       M.Tariq        
    #> # ... with 18 more rows, and 4 more variables: im_prop_pak <dbl>,
    #> #   im_prop_pak_g_count <dbl>, im_prop_pak_p_1 <dbl>,
    #> #   im_prop_pak_t_1 <dbl>

    filter(res_u, constituency_code == "PS-67") %>%
      arrange(uid) %>%
      select(constituency_code, candidate_name, uid, found_initially)

    #> # A tibble: 18 x 4
    #>    constituency_code candidate_name          uid   found_initially
    #>    <chr>             <chr>                   <chr> <lgl>          
    #>  1 PS-67             Abdul Jabbar            11114 FALSE          
    #>  2 PS-67             Irfan Anjum             11115 TRUE           
    #>  3 PS-67             Javed Baloch            11116 FALSE          
    #>  4 PS-67             Manthar Ali Jatoi       11117 TRUE           
    #>  5 PS-67             Mir Atiq Ullah Talpur   11118 FALSE          
    #>  6 PS-67             Muhammad Asif           11119 FALSE          
    #>  7 PS-67             Muhammad Fahad Abbasi   11120 TRUE           
    #>  8 PS-67             Muhammad Fareed Qureshi 11121 FALSE          
    #>  9 PS-67             Muhammad Ibrahim Azam   11122 FALSE          
    #> 10 PS-67             Muhammad Sagheer        11123 FALSE          
    #> 11 PS-67             Nasir Hussain Qureshi   11124 TRUE           
    #> 12 PS-67             Rashid Ahmed            11125 FALSE          
    #> 13 PS-67             Raza Muhammad Abbasi    11126 FALSE          
    #> 14 PS-67             Syed Ameer Shah Bukhari 11127 TRUE           
    #> 15 PS-67             Syed Farhad Ali         11128 TRUE           
    #> 16 PS-67             Syed Sajid Ali          11129 TRUE           
    #> 17 PS-67             Umaid Ali Junejo        11130 TRUE           
    #> 18 PS-67             Abdul Hameed Qami Arain <NA>  TRUE

    # What about those duplicates in the same constituency?å
    dup_uids %>%
      filter(length(unique(constituency_code)) > 1 | length(unique(candidate_name)) > 1) %>%
      as.data.frame

    #>                                           key enum_name   uid
    #> 1   uuid:b9733e62-8d51-4c3d-9895-67ff7c2fb1fa       101 00101
    #> 2   uuid:f2f9b235-190a-44cb-8aa5-b01934b0ffdc       150 00101
    #> 3   uuid:196ce44a-b3cc-43de-946f-ab51149563f7       127 00250
    #> 4   uuid:44cf31bf-cfbc-4eb1-b56b-eeafa0613153       127 00250
    #> 5   uuid:28a19680-6c94-4887-8df0-9930174456a8       104 00256
    #> 6   uuid:4aa68e97-075a-45a4-8b07-9d14242b706e       149 00256
    #> 7   uuid:f0b48232-e73b-48f2-99ff-5ba42baadbeb       127 00279
    #> 8   uuid:127fc669-c22e-4124-95c8-89b2c54c3f4a       156 00279
    #> 9   uuid:7db14f46-d7c8-47b4-b19e-0177c9a9dea6       137 00494
    #> 10  uuid:a98a44c0-c13e-469d-b1b2-848e406764f0       137 00494
    #> 11  uuid:beeaab01-ac84-4b66-9681-dab97cf3fb6b       137 00495
    #> 12  uuid:7063096d-0eb5-4fd7-977e-d0a8f3dcb62f       137 00495
    #> 13  uuid:b4cacdd0-19df-48f1-ac87-c3e338bce9ff       137 00499
    #> 14  uuid:4cd895a5-eff3-487f-ab26-73127e4ef681       137 00499
    #> 15  uuid:062547f1-5bb9-45b4-942f-6b426df40f7a       131 00569
    #> 16  uuid:b6cdead7-1fb8-4413-9513-c36e7f282316       131 00569
    #> 17  uuid:a673b3e6-d120-4fa8-8be1-859de6df4bab       128 00791
    #> 18  uuid:85742a64-3af7-46cc-aeac-000969c6328f       128 00791
    #> 19  uuid:764b53e1-4604-426f-b0b5-c0aa889f94e6       105 01141
    #> 20  uuid:ca7e9822-d75c-4b2e-8534-05168e329781       105 01141
    #> 21  uuid:0ec8e338-1e55-4456-ad6d-18a8df23ef8e       105 01141
    #> 22  uuid:399065a6-69b4-4551-ae09-b7a0b6962a61       105 01149
    #> 23  uuid:144bd870-6bfe-4a6a-8d3f-fab9dc30fed9       105 01149
    #> 24  uuid:eb6f316f-0994-4525-bd81-2a6bc4eaa40c       134 01340
    #> 25  uuid:c600a48b-85b3-4349-bdcc-e2d510c99efd       123 01340
    #> 26  uuid:fe691388-adff-4471-bf48-7e79d3805e85       134 01341
    #> 27  uuid:5502ffee-4386-4c5e-858c-f43b6a0dd114       123 01341
    #> 28  uuid:a26d194d-64eb-4226-b847-d46c69ca4f39       134 01342
    #> 29  uuid:8ad065f8-f383-483b-a0f4-d469e05043f1       123 01342
    #> 30  uuid:fe236480-94b0-4c71-ad1c-9b2862e176e7       134 01343
    #> 31  uuid:ee7d77a3-00ab-4337-b494-384d78b07cf6       123 01343
    #> 32  uuid:97d81fbd-11a6-460e-91c0-c565947bf299       134 01344
    #> 33  uuid:0299c40e-2843-47a1-bd7a-64037d32f018       123 01344
    #> 34  uuid:ce8139da-ffe3-4f05-a012-7f517cb0fef4       134 01345
    #> 35  uuid:2b68052c-8a2a-45be-a444-08ce3ceb7e3d       123 01345
    #> 36  uuid:c99cd433-0ab4-4ffc-b682-12d8af519a24       134 01346
    #> 37  uuid:a1a86f78-c3a2-44b8-a339-8622228ec404       123 01346
    #> 38  uuid:35927d4f-99de-4180-a505-351e457eda3d       134 01348
    #> 39  uuid:a0df9753-7ecc-43b2-bab5-82de74238514       123 01348
    #> 40  uuid:dfad3e5b-3a74-4763-9c0c-c653a4ebc5e6       134 01349
    #> 41  uuid:1c10e732-cbb4-42f8-95f7-82c3b0ccf4f3       123 01349
    #> 42  uuid:41de4d0f-63b1-4238-a336-c0011be4a852       134 01350
    #> 43  uuid:4156ecb1-ecc3-4659-b74c-04ee0f878d81       123 01350
    #> 44  uuid:041215e9-6bd6-4be1-85b6-02c6f43b85c7       134 01351
    #> 45  uuid:676ca4dd-de33-42d6-9133-ac901fb05a4b       123 01351
    #> 46  uuid:5d705b24-fd8b-4f60-af0b-3e796ded11a4       134 01352
    #> 47  uuid:fd13c1db-434b-442b-9a58-8ec1b5d9a26f       123 01352
    #> 48  uuid:7748d7cc-c0b6-43f1-9974-31430d949657       134 01353
    #> 49  uuid:4cdb0a7a-b464-49d3-bb76-e0d8983c5fae       123 01353
    #> 50  uuid:ab1e7472-16b1-44bf-8dad-28691596f799       134 01354
    #> 51  uuid:b291bd59-4a80-40a2-b09c-644107e54eb5       123 01354
    #> 52  uuid:1db18cce-a0df-46fa-8885-85ff4d0b9f9b       134 01357
    #> 53  uuid:73145ec7-136f-44db-a0f7-f2293b79e735       123 01357
    #> 54  uuid:41bef2c9-61d8-46c8-ace8-a9ba3fef7c71       134 01359
    #> 55  uuid:01e4a656-016d-4b87-87e6-127471dd5e15       123 01359
    #> 56  uuid:0ac83197-7a59-475f-96b8-6e5a24639550       134 01360
    #> 57  uuid:5e605882-9ae7-4dc1-a307-e23a441c1b31       123 01360
    #> 58  uuid:cb11d5db-3545-46eb-9678-897b46f71194       134 01361
    #> 59  uuid:edf71f14-4e95-420f-a12c-cfdea12aa9dd       123 01361
    #> 60  uuid:cec5dc02-f05d-4cad-b17e-6dbedb0a4048       134 01362
    #> 61  uuid:5bb8b648-b70b-4c90-8f87-8bb82c159ce1       123 01362
    #> 62  uuid:e46b00e3-6a94-4c36-b619-759f8e0423f1       134 01363
    #> 63  uuid:853b606f-a68b-4c78-991c-6b132a019148       123 01363
    #> 64  uuid:6d7b08ed-7ebb-47b5-ab22-ebbf92fe4099       134 01364
    #> 65  uuid:4463dec8-45b8-4239-87f3-76b8d9299e89       123 01364
    #> 66  uuid:c91474e1-68f1-4a19-b2ad-ddb18ee57b94       134 01375
    #> 67  uuid:ba5f497e-ae0b-445a-a8f0-6c6a9be24c99       123 01375
    #> 68  uuid:1f6fe5eb-7ba2-4f45-bc9d-519f18b7e421       134 01376
    #> 69  uuid:cad58e21-292c-4acc-9c66-fb3b2eff6e7e       123 01376
    #> 70  uuid:e8bb7842-0db8-4558-b436-788f76b85ce2       134 01377
    #> 71  uuid:86c7a74a-c3ea-4577-ad1b-437a04312353       123 01377
    #> 72  uuid:ca791835-cdb5-48c7-9898-226be58c5a05       134 01378
    #> 73  uuid:b438a71d-7cde-4c0d-8c2f-f107f7b5c25b       123 01378
    #> 74  uuid:fd0eaa5a-c971-4b20-9a22-d0ee9b30f784       134 01379
    #> 75  uuid:048b4295-d657-40a3-bf46-89e7ca4c4f9c       123 01379
    #> 76  uuid:0f2b102d-dbec-4da1-b035-ce86b3aeb04a       134 01380
    #> 77  uuid:f572810a-6604-4c6a-be1f-72ebbd3a24d9       123 01380
    #> 78  uuid:4f43f794-723c-41fa-9d52-a5a9ccc464bd       134 01381
    #> 79  uuid:0ddce0d2-a86d-41a1-b1a8-60e405b6035d       123 01381
    #> 80  uuid:a9b715e2-ed84-40a2-bdd9-8f5bb8d78fa1       134 01382
    #> 81  uuid:154d08aa-81a3-49ef-95d6-a1b97315f2d1       123 01382
    #> 82  uuid:5e620189-6426-4ab0-8c2f-3c194f3cb4cb       122 01493
    #> 83  uuid:bcfabb33-ecde-44b3-bbd0-cb55cc825355       150 01493
    #> 84  uuid:57e7a043-12b1-4ea9-b8bf-84217ee8c949       104 01519
    #> 85  uuid:58349bf1-37fa-4e2e-bc17-c22b99f8e723       149 01519
    #> 86  uuid:4265e1ea-e501-4f16-90b0-1bd70e1943b4       104 01522
    #> 87  uuid:73217620-ce26-45f1-8726-6ac1e86aa5f5       149 01522
    #> 88  uuid:99b3276f-495a-4fbc-9978-9c516af6fba7       104 01537
    #> 89  uuid:19bcfda9-4af2-490a-9656-7d143c5315cf       104 01537
    #> 90  uuid:9e7d4937-f2d0-4db3-bf0d-941f4e09f2fd       117 01543
    #> 91  uuid:69a992aa-1ce6-41d0-ad7a-6ac78a09dedf       117 01543
    #> 92  uuid:b0100727-1afd-491f-9771-b4739a396bc2       117 01546
    #> 93  uuid:52c689bd-2727-4b97-94fe-97d4390f7448       150 01546
    #> 94  uuid:2d306a5e-570b-4e96-a238-8bb81d9a35ff       117 01557
    #> 95  uuid:fc63e3d0-cb46-4475-835f-b02e41267f96       149 01557
    #> 96  uuid:03150d96-1796-4f65-bedd-0a26e93d50dd       117 01567
    #> 97  uuid:125aea83-07f2-4386-88da-3f67ea78e459       149 01567
    #> 98  uuid:5be24a03-04cb-4e11-b14b-8c504abd5b99       117 01570
    #> 99  uuid:67d7d8b7-81db-4c5b-bda9-e5ec88e782cf       149 01570
    #> 100 uuid:2e1b66a3-fe1e-4ccf-a46e-70ee464afde7       128 02184
    #> 101 uuid:36c73832-c0e1-4365-b205-506a30e6995f       128 02184
    #> 102 uuid:760d7200-e517-4575-bc8d-161c4af261b6       134 02371
    #> 103 uuid:188575a3-abfa-4d68-bd0a-47642f80e8b3       113 02371
    #> 104 uuid:f7138ff3-2593-4e52-b05a-62a1f3820e51       134 02372
    #> 105 uuid:3a403cc3-89d0-4650-9b2a-50e707284a97       113 02372
    #> 106 uuid:cbef5b5b-e35b-4c07-87c4-8f91bd8665fa       134 02373
    #> 107 uuid:97d8e01a-ba01-4393-af53-c346ca16fb2d       113 02373
    #> 108 uuid:5eee4fa7-c9b0-473f-a789-9ddf8b09bc32       134 02374
    #> 109 uuid:9e9264d2-99b0-4e07-9665-9a1bf8e3c248       113 02374
    #> 110 uuid:0593ae23-f9e5-482f-9bd8-4452b80242b1       134 02384
    #> 111 uuid:bc179778-13fc-4975-a782-1c33c2e1cf67       113 02384
    #> 112 uuid:e0f014ed-5a6d-4e3e-ab9b-cd9130d19914       134 02385
    #> 113 uuid:40a6017d-8316-4f1e-b6da-6e4d2d7553e8       113 02385
    #> 114 uuid:c9cc3860-2a9d-4857-a7e0-f5dbba62f612       134 02386
    #> 115 uuid:854db0ba-1aac-4cc3-a7a0-86273159f11f       113 02386
    #> 116 uuid:0669755d-4f41-40c7-995e-153db4f8354a       134 02387
    #> 117 uuid:dce4d6de-3582-4b90-bc5a-a36352427bdf       113 02387
    #> 118 uuid:a59ac673-581c-4de3-ae38-7cd6308954b9       134 02388
    #> 119 uuid:ba74c518-81a9-4c11-b861-0eda1547ad43       113 02388
    #> 120 uuid:7457d5e3-083f-43d3-8faf-a3a0e02730cb       134 02389
    #> 121 uuid:1baca951-a248-45c1-85da-efc960ebf4ce       113 02389
    #> 122 uuid:b8990d47-4519-4574-a58c-69b2945f146e       134 02390
    #> 123 uuid:9187c685-a690-4d47-a1a4-0a17de436c15       113 02390
    #> 124 uuid:79c7f4eb-0c50-4b93-91df-01b45873f506       134 02391
    #> 125 uuid:2fb36ec5-b576-4a54-967a-689bbaec5db9       113 02391
    #> 126 uuid:45aa4942-3806-442d-988d-b28ce39ca5bc       134 02392
    #> 127 uuid:cc48185c-827b-4768-8740-7d2d05c88183       113 02392
    #> 128 uuid:de499b28-168b-417f-93b3-827bcbe0fb4b       134 02393
    #> 129 uuid:80551a96-31cb-48f8-82b6-b20fd5c6f161       113 02393
    #> 130 uuid:71789030-9cfc-4967-bd7f-4009f8219b82       134 02394
    #> 131 uuid:8dbfc0c4-e93c-47a9-ab80-e930c171a0cd       113 02394
    #> 132 uuid:ccd695e6-7475-499d-a76d-5910c1d100e9       134 02395
    #> 133 uuid:f396ee81-f824-4f52-a8ac-7504cf336061       113 02395
    #> 134 uuid:ee9a6273-e646-48d4-bcbc-e744abf7af8e       134 02396
    #> 135 uuid:3926fe98-38ca-464e-8a36-024e8631e63a       113 02396
    #> 136 uuid:e8036cbe-23a6-4c1a-b69c-de9094246d5d       134 02397
    #> 137 uuid:5282054f-761c-44f7-9488-f2d869e542e6       113 02397
    #> 138 uuid:886970c1-4c4d-4b3c-9a36-d4303f4cce08       134 02398
    #> 139 uuid:b0acf07c-c988-4ab7-9f67-acf6caa675f5       113 02398
    #> 140 uuid:36a7a262-b1d9-4dc9-a0e6-36a2f434d4c1       134 02399
    #> 141 uuid:ae017b2a-8801-43cb-9c45-d91c20c69bae       113 02399
    #> 142 uuid:7016ff6a-3c4e-4e3a-97db-c026c70533b5       134 02400
    #> 143 uuid:c4d7954b-f12c-4344-bf55-a4b6d1e998a7       113 02400
    #> 144 uuid:a870848c-1e4e-4639-b6c8-bcb68a8dfadc       134 02401
    #> 145 uuid:4f5990ab-3b71-4138-93fd-bbd344f34f95       113 02401
    #> 146 uuid:470c2e39-6658-4671-9f90-d8c7111284b3       134 02402
    #> 147 uuid:67fde7fc-c584-4622-b0e3-bd27c8d711bb       113 02402
    #> 148 uuid:529cac19-1fe4-4fca-81e2-fbaa7af3419c       134 02403
    #> 149 uuid:b2c52ba4-7a93-43b0-ac51-49d49670a657       113 02403
    #> 150 uuid:ebd88759-ed10-40d8-b6b9-b08345b587f7       134 02404
    #> 151 uuid:c659f04c-f0ba-47f8-82c5-36fa8d53b2e5       113 02404
    #> 152 uuid:a64846a7-6f48-4b5e-869a-8c6e3bd9863d       134 02405
    #> 153 uuid:6695b724-94f9-40de-ac70-4bf375453719       113 02405
    #> 154 uuid:49a1cfc3-6958-4a23-b8e7-54922206adfc       134 02406
    #> 155 uuid:78932369-353e-4790-84fb-c49cdc9876d5       113 02406
    #> 156 uuid:a2810a37-ece9-4097-9f5f-0d78270bc736       134 02407
    #> 157 uuid:b7467d2f-fc09-4a68-89bf-98b5bbca5df9       113 02407
    #> 158 uuid:b77029e1-0a33-4332-bcb8-609a65c51dca       134 02408
    #> 159 uuid:af8fcdca-2bd1-4a43-8af7-23f4c3f4665c       113 02408
    #> 160 uuid:e3b8d811-df42-4f2c-bc69-5200856246a1       113 02408
    #> 161 uuid:7cce7baf-a049-4a5c-924a-ff3f16bb7d3d       134 02409
    #> 162 uuid:97c785b7-f3c8-49c9-99cd-4ab58c19baab       113 02409
    #> 163 uuid:481c7a8b-5cf8-4225-a609-2f97b6d05256       134 02410
    #> 164 uuid:56d6d138-a887-402f-9aaf-4e6ef5920eb2       113 02410
    #> 165 uuid:c95548d1-d270-4453-a79f-c0c2b8bcd9de       134 02412
    #> 166 uuid:652e0195-2144-45e9-bd46-7e870e8d23df       113 02412
    #> 167 uuid:2d9fd112-d5f2-4970-8110-2f8d03ef04d0       134 02413
    #> 168 uuid:88837605-3ffa-48d0-abf7-9b177b624f51       113 02413
    #> 169 uuid:9bb236cf-da05-492f-bf98-572fc076c16c       134 02414
    #> 170 uuid:4cd502a2-1082-4b81-acc9-b1e16121693c       113 02414
    #> 171 uuid:52e9d99c-072b-4d85-96f0-3e2d08dfd552       134 02415
    #> 172 uuid:44413641-19ef-464d-8cb6-baf03336c988       113 02415
    #> 173 uuid:79322d8c-9897-4a5d-841b-f7b00d83d0c6       134 02416
    #> 174 uuid:b1e64933-9cc1-45be-b534-44d1c801c00a       113 02416
    #> 175 uuid:8d821b4f-3305-4869-a031-1500babbc1ee       134 02417
    #> 176 uuid:9b470886-af68-4574-b46c-6be20d306846       113 02417
    #> 177 uuid:b7b8728a-638e-481e-a598-0f73c0a4beb2       134 02418
    #> 178 uuid:1c1cd6bc-ab05-4054-90b7-03b3e663bbfa       113 02418
    #> 179 uuid:ddea0c76-56f5-4886-a753-e94e78e025e1       134 02419
    #> 180 uuid:c91b38ca-bde0-444f-8263-cdea72bcbf63       113 02419
    #> 181 uuid:b0386ef2-2615-4f3c-bf2e-1d0a859c5503       134 02420
    #> 182 uuid:f275bd94-5f5d-48c0-9c58-ea50ce34be53       113 02420
    #> 183 uuid:ac0556ca-c0db-47b8-808d-491c2ecc583a       134 02421
    #> 184 uuid:0df0e70b-f19c-44aa-90f3-1746901df35e       113 02421
    #> 185 uuid:93fa40c5-5af5-4c57-8926-9537f93014ee       134 02422
    #> 186 uuid:460a5cfe-2025-4614-b3fd-61078b3942d9       113 02422
    #> 187 uuid:9925c10a-21f3-49c7-a7ac-3774d97314e3       134 02423
    #> 188 uuid:82706e5a-6bd3-401b-859a-664d99f39df4       113 02423
    #> 189 uuid:575df99c-d21e-4169-afe1-82ebce7020d7       134 02424
    #> 190 uuid:83e38751-8bb4-4b49-8ad7-9ddcb956d1bc       113 02424
    #> 191 uuid:968b98ce-e95a-47d7-86cc-a39a119200bf       134 02425
    #> 192 uuid:732effb3-8554-4d5f-9711-61ddf51768d0       113 02425
    #> 193 uuid:870c02cc-2bf6-4695-8ec6-2966e39ec019       134 02426
    #> 194 uuid:c57d8598-90a3-4f13-bb97-381d224af62e       113 02426
    #> 195 uuid:d46f85d0-b8d2-4d38-849e-04009469b072       134 02427
    #> 196 uuid:2bd4e4a7-b6fe-4232-8dd9-28f22c3c316a       113 02427
    #> 197 uuid:1bde8df3-b279-4d61-8893-997a7085fede       134 02428
    #> 198 uuid:93898f85-795b-46e4-a21b-31414e271ba2       113 02428
    #> 199 uuid:c0bcf39a-1234-40b6-8075-c4833d899e17       134 02429
    #> 200 uuid:952e45be-05e7-4b65-8893-b00bef2905f2       113 02429
    #> 201 uuid:e47042b1-7629-4c69-aa1f-d10dc87f1cd1       134 02430
    #> 202 uuid:a61d39b9-97e5-4984-97d6-cdb043ab9bc0       113 02430
    #> 203 uuid:13f07d9e-0c5a-48e9-8180-f7c3632a750b       134 02431
    #> 204 uuid:08746336-4e2c-41ea-aa3c-6d85423bcbb2       113 02431
    #> 205 uuid:473a2d91-9fe4-41a2-96b1-9122391b3679       134 02432
    #> 206 uuid:92e51e3f-ebb0-4822-b8ba-c8ec87a5c316       113 02432
    #> 207 uuid:f8c9ae7b-5427-4044-bde6-f12591b7bb0c       134 02433
    #> 208 uuid:74134ab0-c4c3-457b-8167-7dd3ecd9fd4f       113 02433
    #> 209 uuid:228ad853-745e-416d-841f-55a8fe33ec12       134 02434
    #> 210 uuid:4698fb13-c059-493f-840a-2ece9087ee07       113 02434
    #> 211 uuid:155c510d-565a-431f-bbd7-e3b03891f416       134 02435
    #> 212 uuid:8f4343c0-4786-4b6e-8853-14a6bc4a5ca8       113 02435
    #> 213 uuid:d3d1c1f5-f0a7-45aa-a32a-e2d2e38ec3cf       134 02436
    #> 214 uuid:d52e05aa-f3b3-440f-b57a-640eb4e67704       113 02436
    #> 215 uuid:fe54c03f-a03b-4d4b-b8f6-6b1b5bdd8512       134 02437
    #> 216 uuid:c98e7110-78c9-4a98-891c-34062ff11024       113 02437
    #> 217 uuid:b8071c31-69d0-477a-8c8e-83ec3a5e8355       134 02438
    #> 218 uuid:cf9291ce-2ca6-49d3-aef6-380f56021dcc       113 02438
    #> 219 uuid:58c1f4e2-eca0-45d4-9fef-2a0e8830e879       134 02440
    #> 220 uuid:e408fd35-d61b-47d2-9e2c-1244405171cc       113 02440
    #> 221 uuid:1ae1dbc5-8e48-4eaa-b1ef-2fecef470cbd       134 02441
    #> 222 uuid:66f2e612-cdfe-46e9-8d06-212b6a729f44       113 02441
    #> 223 uuid:aeda63ef-e099-460b-bd60-fe31908ea9aa       134 02442
    #> 224 uuid:69301eda-d9b7-4079-a6b4-b90bca3ee6d5       113 02442
    #> 225 uuid:f36eeca5-2af6-41bb-9de5-f4ca94e9f458       134 02443
    #> 226 uuid:770cf305-78eb-428c-8833-f3151fcd64bd       113 02443
    #> 227 uuid:2ff8ee27-4259-44d4-b8ef-7a5959efe88c       134 02444
    #> 228 uuid:e11a73c4-c32d-4d10-8974-92fe74e9ae67       113 02444
    #> 229 uuid:0e4bc0e2-eb62-42f3-9fbf-d5630c980479       134 02445
    #> 230 uuid:ca854942-7618-4615-b7e7-eed022849e70       113 02445
    #> 231 uuid:8221c29a-3cb2-4f52-858a-51e9d98537b5       134 02446
    #> 232 uuid:6075860f-edb8-40a8-a54e-adb9f489f63b       113 02446
    #> 233 uuid:056834f6-d14d-47f8-b1c9-cb4f4f2fefc7       134 02447
    #> 234 uuid:65e40162-b03f-47be-8b8a-1e467b3b9816       113 02447
    #> 235 uuid:39ed8f71-f900-463a-9188-228b553b96c1       134 02448
    #> 236 uuid:dddb5678-483d-43ae-966c-edd3d5e657aa       113 02448
    #> 237 uuid:befb6248-729e-4312-b1ae-35241e949fb1       134 02449
    #> 238 uuid:b911ce48-85cf-4270-ab09-190f641ed2fe       113 02449
    #> 239 uuid:3c664349-0b48-4edb-a1dd-dbf8c18ed0ed       134 02450
    #> 240 uuid:553a2576-904a-46cf-b2b3-e3b888953a7f       113 02450
    #> 241 uuid:9df5fd74-affc-4468-9c62-cedee7de8dbd       134 02451
    #> 242 uuid:86aef3c6-4c56-45fe-89f1-0efdfeebcdf3       113 02451
    #> 243 uuid:7b286d59-8d88-4b32-ae1c-00501ce16356       134 02452
    #> 244 uuid:1ff812ce-4c9a-4c22-a06b-dcede4cecdfe       113 02452
    #> 245 uuid:5f2a0243-0868-46ef-9922-c36ffd79877f       134 02453
    #> 246 uuid:52839a8f-0849-436b-ab4b-a8b8e648986f       113 02453
    #> 247 uuid:cb56b331-5040-4337-9537-080603dbf4c8       134 02454
    #> 248 uuid:6b1a0fc9-5ed6-4a2b-a992-4dc4501e4099       113 02454
    #> 249 uuid:19ee2095-1273-46e4-8c0e-411f2cb3ef6b       134 02455
    #> 250 uuid:5711c19b-b1e7-44ae-b422-8e7f286aa4fb       113 02455
    #> 251 uuid:3adfa9cd-1d40-4b5d-94cb-bcb2288b62ec       134 02456
    #> 252 uuid:975524bd-f34f-4a9f-ad9f-c32bce5d6579       113 02456
    #> 253 uuid:71fcf97b-f205-4ed5-81e3-8c34cab26bc5       134 02457
    #> 254 uuid:8c99e615-eef1-4919-8918-d7c7527248e8       113 02457
    #> 255 uuid:be6236ad-3712-48bb-8752-455bc5abc1d2       132 02884
    #> 256 uuid:fd81a38f-e71f-401a-935f-e7a1bb28260c       132 02884
    #> 257 uuid:82f70958-2c0f-472d-b47f-9cfe4deaac66       118 03089
    #> 258 uuid:0ebd7956-48e6-407d-8da6-bcdec782802a       104 03089
    #> 259 uuid:c80ff99d-fde6-43d9-b592-9f0f9db36472       124 03435
    #> 260 uuid:f67de0fa-5cdd-474b-8f47-f946ccfc22d7       124 03435
    #> 261 uuid:e585ef8b-110c-40f3-8d3a-39bed14b1caa       122 03514
    #> 262 uuid:9c5ff618-6d54-41f3-bfac-d1ff91964103       150 03514
    #> 263 uuid:ba5ec538-0314-4fc5-a6cf-a1b30b656598       150 03514
    #> 264 uuid:3901f039-b815-442a-9c14-1574fdcb6c06       122 03524
    #> 265 uuid:a7c1f8b7-5e79-4119-ba5d-7f54b2f93d75       150 03524
    #> 266 uuid:097cf60a-6998-4439-99eb-cc2449223e42       122 03804
    #> 267 uuid:bb55a6d5-0987-4e16-9582-1b227dd697d4       150 03804
    #> 268 uuid:80fcda23-ac73-43ad-8c4d-fbd1c06711d6       109 04440
    #> 269 uuid:a1940237-fd4f-4794-aeba-ca299ec72627       149 04440
    #> 270 uuid:d042a688-c479-4bc5-bb5f-0babaa37abf6       125 04475
    #> 271 uuid:9ff16bb9-886b-40f8-93cc-ed7b8b32b01e       150 04475
    #> 272 uuid:55b3e243-cfdc-487d-aa33-34ec96779227       125 04558
    #> 273 uuid:09547d1a-f37e-4eb7-8d86-84fa1d83a063       125 04558
    #> 274 uuid:b22822ab-3d49-4a7c-b33e-372e5692c8b5       109 04589
    #> 275 uuid:b5542f53-35b4-476b-81a7-098e431e3959       149 04589
    #> 276 uuid:929e1d7f-5f41-45fe-a457-99dda58d0ae7       109 04921
    #> 277 uuid:afeb5e0f-ba25-44ac-9fa5-7f7d0aea20b3       109 04921
    #> 278 uuid:99a7bbf1-cbde-40e1-adaa-6fecc2c6174c       103 05134
    #> 279 uuid:3c977ba7-de1f-4ec7-ab7a-f23f86ec52b9       150 05134
    #> 280 uuid:c5126dce-e795-4e9b-9774-1ba436d6f53b       113 05526
    #> 281 uuid:93ae491b-b4ba-4f01-9c82-0589c2269e52       113 05526
    #> 282 uuid:4bbf4f1f-90e9-4fef-ae64-095d185f2e67       108 05661
    #> 283 uuid:c8f954a2-cab2-4b83-ab21-bff7dd63b9dd       108 05661
    #> 284 uuid:ab69c0d8-28c5-48cf-b68d-59340a98d083       156 05686
    #> 285 uuid:daa61561-f6b8-4516-8ff2-70560780e901       113 05686
    #> 286 uuid:ce02537b-8c33-442a-b5d0-2f0e416d8290       156 05687
    #> 287 uuid:4ae5a66a-d86b-47e0-9bbd-82e8c66e9370       113 05687
    #> 288 uuid:09d611e2-b0c1-4360-8433-0b6bf2aa54e3       156 05688
    #> 289 uuid:afe0f7c0-3ddf-4b52-a784-0ee7488d226d       113 05688
    #> 290 uuid:b40aae45-c764-4bb4-acc3-28ae5a75b17b       156 05689
    #> 291 uuid:800515af-b85a-46a4-a003-5a0339b68810       113 05689
    #> 292 uuid:c2469415-457b-47ee-90d8-162398d6ef4c       156 05690
    #> 293 uuid:f1ea0a17-861a-451b-b005-1099bfc0ca56       113 05690
    #> 294 uuid:1704060c-709c-4e78-b18c-096de15e5ba9       156 05691
    #> 295 uuid:8c353d53-7fb0-47ce-b088-8f82221795c2       113 05691
    #> 296 uuid:e6d2c092-21e9-4b15-8df1-b518209d8700       156 05692
    #> 297 uuid:08863bc3-ce52-4afe-92c8-8b2e5cbb9bba       113 05692
    #> 298 uuid:14598f7a-59de-438e-b131-b9ab61ec2bfe       156 05693
    #> 299 uuid:c562ec5d-eaa9-4da0-9853-af03c8d2c338       113 05693
    #> 300 uuid:57341afc-e4ec-4b4f-a1dc-507ed4c7827f       156 05694
    #> 301 uuid:b8fce575-e145-4fbe-b6dd-5f55d9fb013b       113 05694
    #> 302 uuid:db60e2fb-6bd3-4c82-90dd-5aa68ba3319e       156 05695
    #> 303 uuid:6ad3d7c9-f6f2-4242-8575-64050c480527       113 05695
    #> 304 uuid:1df0a1b5-6d3a-4ba2-97ff-66f71d013b3d       113 05859
    #> 305 uuid:9ef406c4-14a9-415c-9a76-74e8d5b70532       150 05859
    #> 306 uuid:1598afa5-c76a-47aa-b355-0383a4096e36       106 06160
    #> 307 uuid:57e2b083-c7e3-4f1d-bb89-a0cfd5aa54ba       150 06160
    #> 308 uuid:89ab72f9-dad0-45b3-a828-77853fb2ab58       127 06178
    #> 309 uuid:7b6d79d8-20cd-4768-8f30-ec718baf3974       149 06178
    #> 310 uuid:15341f27-4267-4b92-9448-34a8e230ef2b       127 06247
    #> 311 uuid:3a9c16e4-ee4e-4203-a081-6ecdb2b429dd       127 06247
    #> 312 uuid:7ddbe0ce-cf09-4803-ab77-b75b419fa37e       106 06338
    #> 313 uuid:95e1e23e-d56b-4d72-b5c6-7bfd85f98323       106 06338
    #> 314 uuid:a6becc49-4561-4b71-9d64-8407331708d5       106 06339
    #> 315 uuid:6842380d-d7c2-43af-a0f7-9cac0fa0eba4       106 06339
    #> 316 uuid:da4815b2-2009-419f-bf9b-89a0d1f32ca3       106 06342
    #> 317 uuid:95ce78b0-c4bc-4fc0-8090-c01df62f3e15       106 06342
    #> 318 uuid:b97057e8-0264-45f7-80cd-66c682886ced       106 06344
    #> 319 uuid:0c305dc5-a6de-4fc9-9546-572dd66a2c9c       106 06344
    #> 320 uuid:f7c30b44-2253-4274-86fe-f525cf0cb89b       144 06470
    #> 321 uuid:79b797e4-94f6-444f-bb82-d56b164fb2f2       144 06470
    #> 322 uuid:b867c11b-4076-4101-bd34-21e7a5ee224e       144 06480
    #> 323 uuid:50bc41b1-a7ba-4385-bc48-6c7495a973a1       777 06480
    #> 324 uuid:8f51998c-80c2-42b3-8975-ec5e4c4cc10b       777 06480
    #> 325 uuid:2ffcf6d6-fad2-4f36-9a6c-d98e47c01bcb       151 06480
    #> 326 uuid:239c2f5d-0abd-4651-a943-d6080d85369a       777 06504
    #> 327 uuid:f94083d9-f933-49f2-bef8-db06eb3249ae       143 06504
    #> 328 uuid:1ee2192d-7125-4843-8376-792e2b13900e       137 06584
    #> 329 uuid:a90ada81-65d2-44a6-b2b6-a71422e48a4b       143 06584
    #> 330 uuid:29a0db7a-0918-434a-af3f-fc579e949db1       154 06968
    #> 331 uuid:c91cc227-99d4-4d82-a751-03fbcb9f7f06       131 06968
    #> 332 uuid:90b548a3-9a58-4bb8-bb89-483247492c60       131 07012
    #> 333 uuid:50ee75ef-39e1-4609-983f-e34b5086ca8d       131 07012
    #> 334 uuid:707f6815-36fa-4d9c-b7fa-1e1f5f41d7f5       131 07015
    #> 335 uuid:477c19a5-3260-47ad-b7c6-daba5e5392d2       106 07015
    #> 336 uuid:493b4465-b4b3-4d8a-8f3a-27088ebfca7e       131 07051
    #> 337 uuid:a4218b6c-0d12-4901-99e0-9f55dbeb219f       149 07051
    #> 338 uuid:c4d41f7f-6bbe-4f21-873d-2831f457e97d       131 07052
    #> 339 uuid:aa33804f-9dad-4937-bd1c-d3e79f4a8ffe       128 07052
    #> 340 uuid:778a72ce-14e3-49b8-ae2c-ca2c13b77a65       128 07489
    #> 341 uuid:a4e765d1-5115-4de9-b9c5-116f6e5255c0       128 07489
    #> 342 uuid:bd08e960-a04a-4257-9d6c-2f2d70ed351d       107 07891
    #> 343 uuid:aba5759c-50f9-4514-9790-7aac07a1f974       150 07891
    #> 344 uuid:86744263-800d-45f8-a279-a7c3bb7ec025       150 08325
    #> 345 uuid:162963c3-651d-4890-b518-6c2ac017f5e2       105 08325
    #> 346 uuid:867693fa-5cab-4d5e-947a-6138cc68c4c9       114 08631
    #> 347 uuid:fb57cbe2-8f9e-418c-ad89-38d9e5d58d7d       150 08631
    #> 348 uuid:daffbd7d-ecdd-4c71-be15-2b9d4c27f452       122 08924
    #> 349 uuid:02808c34-bc9b-444f-a39c-6b2893ecfe38       123 08924
    #> 350 uuid:40939c1d-47d3-49ca-b08d-996c91c07b01       143 08927
    #> 351 uuid:0a6af6cc-e2f0-482d-96f4-c86ea55916eb       143 08927
    #> 352 uuid:4bf414b3-7503-447d-b995-cb06848412be       128 09050
    #> 353 uuid:367763e0-d43b-47cf-a9a9-1c107d902a36       128 09050
    #> 354 uuid:11de2c70-af08-47ff-bb0a-67e52523c6e6       140 09194
    #> 355 uuid:0ff31a06-940a-41a1-bc69-778195bc852e       140 09194
    #> 356 uuid:bd4d4058-23e2-46d1-9b25-62105cb29e08       150 09406
    #> 357 uuid:dc851e8a-2317-45f6-8ffc-29e7c6f23d15       150 09406
    #> 358 uuid:34f33af7-d928-4eed-81ce-c6abf3f206cb       157 09557
    #> 359 uuid:90c6b127-3b37-4666-9ffd-2c523efba18f       157 09557
    #> 360 uuid:333d499b-fd6c-486d-9fd6-dd5cb69480f6       150 09627
    #> 361 uuid:7a4cffd2-fc67-4c77-b0e4-b906324c8e2c       145 09627
    #> 362 uuid:ee177327-a8f9-40cd-ba4f-db3dd249247e       145 09639
    #> 363 uuid:71ac1cfa-8670-4a74-b7e8-0d050c5a18f7       145 09639
    #> 364 uuid:c310da3f-dc16-46b4-889e-683ceba5127d       150 09657
    #> 365 uuid:20982ea0-5bd7-4548-9f18-c547dd152b74       145 09657
    #> 366 uuid:4ab601b3-467b-4e15-bfd9-a18a477a46b4       145 09671
    #> 367 uuid:b5db77a7-756a-4dbb-b3ec-2f77c4781db6       153 09671
    #> 368 uuid:7cd69f44-4223-4b5a-b68c-066bc27a1403       150 10165
    #> 369 uuid:4bbc1858-da5a-4dcc-8860-6b7a03f908a6       108 10165
    #> 370 uuid:bc092492-675b-4984-a1c7-c98b700565e9       105 10278
    #> 371 uuid:f5a1990a-0e97-4dac-93bd-57e926193b79       150 10278
    #> 372 uuid:910c3e04-38f4-46b1-8df0-49175292d7cf       105 10298
    #> 373 uuid:f8bc615d-5c8b-4562-9b4b-e64a2e7b2e80       149 10298
    #> 374 uuid:9a7982da-a703-49f6-98be-4478c0365b38       148 10469
    #> 375 uuid:f1af5832-c13f-4da5-95e3-6c038b00869f       114 10469
    #> 376 uuid:9624e0dd-ddfb-4e1d-a7b0-81616ffbc50b       159 10671
    #> 377 uuid:b1f8c502-5cb6-4680-80d9-c9ae292cc51e       150 10671
    #> 378 uuid:177d634f-6503-4e7c-b4c4-9e360a072071       159 10715
    #> 379 uuid:4151a6a8-4efd-40a2-b51f-1376927ed6f7       150 10715
    #> 380 uuid:8132b5b9-caf9-411d-81ea-b47362acdda5       150 11270
    #> 381 uuid:a55c457f-8493-41a3-8928-fad85b97cfaa       114 11270
    #> 382 uuid:107a6198-30dc-40a9-ad57-c8719d18cb0a       114 11304
    #> 383 uuid:2fdd75eb-92cf-4577-8505-a9ed8fe26939       114 11304
    #> 384 uuid:6330aa19-40a8-4157-b525-42ebe993f740       114 11316
    #> 385 uuid:fd65243c-6054-4a93-8772-b9a81b7e1c1f       114 11316
    #> 386 uuid:1afc8923-8af2-4d50-b7b3-1525b9416cf6       114 11366
    #> 387 uuid:53bc7f75-53ea-4f2b-8868-0c936b85293d       114 11366
    #> 388 uuid:961a5fbc-fc20-4d15-90c1-b038ac7465ff       150 11414
    #> 389 uuid:f10cca25-7cac-4d87-8475-136baae7c9ee       114 11414
    #> 390 uuid:e79929a9-6144-4f86-b240-3e27a4db3783       101 99999
    #> 391 uuid:24e35aec-90a5-4df2-930c-c0f0fab6a528       106 99999
    #> 392 uuid:746b8006-119f-4860-a2c2-3ad9afa8a6d7       101 99999
    #> 393 uuid:4988f9d9-04e7-41ea-92d3-f501fb12ad75       104 99999
    #> 394 uuid:5aed9e42-bf88-4a24-8461-f8caf820e4fb       101 99999
    #> 395 uuid:cda1234c-8605-465a-912a-737a83cb3f1e       101 99999
    #>                     candidate_name constituency_code n entry new_and_old
    #> 1                    Muhammad Asif            NA-107 2   old       FALSE
    #> 2                    Muhammad Asif             PS-67 2   old       FALSE
    #> 3              Rana Afzaal Hussain            NA-119 2   old       FALSE
    #> 4              Rana Afzaal Hussain            NA-120 2   old       FALSE
    #> 5                  Muhammad Yousaf             NA-12 2   old       FALSE
    #> 6                  Muhammad Yousaf             PS-49 2   old       FALSE
    #> 7              Muhammad Saeed Virk            NA-121 2   old       FALSE
    #> 8                    Muhammad Syed             PS-91 2   old       FALSE
    #> 9                           Salman            NA-137 2   old       FALSE
    #> 10                          Salman            NA-138 2   old       FALSE
    #> 11                        Asif Ali            NA-137 2   old       FALSE
    #> 12                            Asif            NA-138 2   old       FALSE
    #> 13                    Anis Qureshi            NA-138 2   old       FALSE
    #> 14             Dawood Anis Qureshi            NA-138 2   old       FALSE
    #> 15                     Raiz Ul Haq            NA-142 2   old       FALSE
    #> 16                    Sheraz Zafar            NA-142 2   old       FALSE
    #> 17            Nawab Amanullah Khan            NA-160 2   old       FALSE
    #> 18           Nawab Amanullaha Khan            NA-161 2   old       FALSE
    #> 19                    Abdul Rehman            NA-191 3   old       FALSE
    #> 20                    Abdul Rehman            NA-191 3   old       FALSE
    #> 21            Dr Mian Abdul Rehman            NA-191 3   old       FALSE
    #> 22                  Parveen Akhtar            NA-191 2   old       FALSE
    #> 23                  Parveen Akthar            NA-191 2   old       FALSE
    #> 24                       Akbar Ali            NA-207 2   old        TRUE
    #> 25                       Akbar ali            NA-207 2   new        TRUE
    #> 26                       Amanullah            NA-207 2   old        TRUE
    #> 27                 Amanullah khoso            NA-207 2   new        TRUE
    #> 28                     Ameer Buksh            NA-207 2   old        TRUE
    #> 29                       Ameer bux            NA-207 2   new        TRUE
    #> 30                       Didar Ali            NA-207 2   old        TRUE
    #> 31                       Didar ali            NA-207 2   new        TRUE
    #> 32                  Ghulam Murtaza            NA-207 2   old        TRUE
    #> 33                  Ghulam murtaza            NA-207 2   new        TRUE
    #> 34                       Gohar Ali            NA-207 2   old        TRUE
    #> 35                 Gohar ali khoso            NA-207 2   new        TRUE
    #> 36                         M.Zaman            NA-207 2   old        TRUE
    #> 37            Hafiz muhammad zaman            NA-207 2   new        TRUE
    #> 38                Khuda Dino Sangi            NA-207 2   old        TRUE
    #> 39                Khuda dino sangi            NA-207 2   new        TRUE
    #> 40                 Khurshid Afghan            NA-207 2   old        TRUE
    #> 41                 Khurshid afghan            NA-207 2   new        TRUE
    #> 42                    Mubeen Ahmad            NA-207 2   old        TRUE
    #> 43                    Mubeen ahmad            NA-207 2   new        TRUE
    #> 44                   M.Amir Sheikh            NA-207 2   old        TRUE
    #> 45             Muhammad ami sheikh            NA-207 2   new        TRUE
    #> 46                         M.Murad            NA-207 2   old        TRUE
    #> 47                  Muhammad murad            NA-207 2   new        TRUE
    #> 48                         M.Tahir            NA-207 2   old        TRUE
    #> 49                  Muhammad tahir            NA-207 2   new        TRUE
    #> 50                    Nouman Islam            NA-207 2   old        TRUE
    #> 51              Numan islam sheikh            NA-207 2   new        TRUE
    #> 52                     Saood Afzal            NA-207 2   old        TRUE
    #> 53                     Saood afzal            NA-207 2   new        TRUE
    #> 54                     Sohail Niaz            NA-207 2   old        TRUE
    #> 55                Sohail naz khoso            NA-207 2   new        TRUE
    #> 56              Syed Ali Raza Shah            NA-207 2   old        TRUE
    #> 57                   Ali raza shah            NA-207 2   new        TRUE
    #> 58                         M.Ayoub            NA-207 2   old        TRUE
    #> 59             Syed muhammad ayoub            NA-207 2   new        TRUE
    #> 60                       Wazir Ali            NA-207 2   old        TRUE
    #> 61                       Wazir ali            NA-207 2   new        TRUE
    #> 62                 Wazir Ali Jatoi            NA-207 2   old        TRUE
    #> 63                 Wazir ali jatoi            NA-207 2   new        TRUE
    #> 64                 Ziauddin Bhatti            NA-207 2   old        TRUE
    #> 65                 Ziauddin bhatti            NA-207 2   new        TRUE
    #> 66                       Abdul Haq            NA-209 2   old        TRUE
    #> 67                      Abul haque            NA-209 2   new        TRUE
    #> 68                      Ghulam Ali            NA-209 2   old        TRUE
    #> 69                Ghulam ali hajno            NA-209 2   new        TRUE
    #> 70                  Ghulam Mustafa            NA-209 2   old        TRUE
    #> 71                 Ghulam  mustafa            NA-209 2   new        TRUE
    #> 72                     Ismail Shah            NA-209 2   old        TRUE
    #> 73                     Ismail shah            NA-209 2   new        TRUE
    #> 74                      Munwar Ali            NA-209 2   old        TRUE
    #> 75               Munwar ali wassan            NA-209 2   new        TRUE
    #> 76             Peer Sadruddin Shah            NA-209 2   old        TRUE
    #> 77           Pir saddar uddin shah            NA-209 2   new        TRUE
    #> 78        Peer Syed Fazal Ali Shah            NA-209 2   old        TRUE
    #> 79                 Syed fazal shah            NA-209 2   new        TRUE
    #> 80                 Syed Ameer Umar            NA-209 2   old        TRUE
    #> 81         Syed ameer umar jillani            NA-209 2   new        TRUE
    #> 82                      Abdul Aziz            NA-217 2   old       FALSE
    #> 83                      Abdul Aziz             PS-54 2   old       FALSE
    #> 84                      Roop Chand            NA-218 2   old       FALSE
    #> 85                      Roop Chand             PS-48 2   old       FALSE
    #> 86                  Ali Nawaz Shah            NA-219 2   old       FALSE
    #> 87                  Ali Nawaz Shah             PS-48 2   old       FALSE
    #> 88                Shoaib Alam Khan             NA-22 2   old       FALSE
    #> 89                     Shoaib Khan             NA-22 2   old       FALSE
    #> 90                 M Younis Talpur            NA-220 2   old       FALSE
    #> 91           Nawab M Yousaf Talpur            NA-220 2   old       FALSE
    #> 92                    Dilawar Khan            NA-221 2   old       FALSE
    #> 93                    Dilawar Khan             PS-54 2   old       FALSE
    #> 94                 Arbab Zakaullah            NA-222 2   old       FALSE
    #> 95                 Arbab Zakaullah             PS-56 2   old       FALSE
    #> 96                  Gul Sher Sario            NA-223 2   old       FALSE
    #> 97                  Gul Sher Sario             PS-58 2   old       FALSE
    #> 98            Makhdoom Shahzad Ali            NA-223 2   old       FALSE
    #> 99            Makhdoom Shahzad Ali             PS-58 2   old       FALSE
    #> 100                Bismillah Kakar            NA-263 2   old       FALSE
    #> 101                Busmillah Kakar            NA-263 2   old       FALSE
    #> 102                     Amir Muqam             PK-29 2   old        TRUE
    #> 103                    Ameer Muqam             NA-29 2   new        TRUE
    #> 104              Arbab Kamal Ahmad             PK-29 2   old        TRUE
    #> 105              Arbab kamal ahmad             NA-29 2   new        TRUE
    #> 106                       M.Shafiq             PK-29 2   old        TRUE
    #> 107                Muhammad Shafiq             NA-29 2   new        TRUE
    #> 108                Mufti Naeem Jan             PK-29 2   old        TRUE
    #> 109                Mufti Naeem Jan             NA-29 2   new        TRUE
    #> 110             Arbab Alamgir Khan             PK-30 2   old        TRUE
    #> 111             Arbab alamgir khan             NA-30 2   new        TRUE
    #> 112             Arbab Najeeb Ullah             PK-30 2   old        TRUE
    #> 113        Arbab Najeeb Ullah Khan             NA-30 2   new        TRUE
    #> 114              Mailk Haider Khan             PK-30 2   old        TRUE
    #> 115              Malik haider khan             NA-30 2   new        TRUE
    #> 116                 Alamgir Khalil             PK-30 2   old        TRUE
    #> 117        Muhammad Alamgir Khalil             NA-30 2   new        TRUE
    #> 118                       M.Junaid             PK-30 2   old        TRUE
    #> 119                Muhammad Junaid             NA-30 2   new        TRUE
    #> 120                      M.Shoukat             PK-30 2   old        TRUE
    #> 121      Muhammad Shoukat Khorshed             NA-30 2   new        TRUE
    #> 122                 Noor Wali Khan             PK-30 2   old        TRUE
    #> 123                 Noor wali khan             NA-30 2   new        TRUE
    #> 124                       Sher Ali             PK-30 2   old        TRUE
    #> 125                 Sher ali arbab             NA-30 2   new        TRUE
    #> 126     Akhunzada Irfan Ullah Shah             PK-31 2   old        TRUE
    #> 127          Akhunzada irfan ullah             NA-31 2   new        TRUE
    #> 128                 Aurangzeb Khan             PK-31 2   old        TRUE
    #> 129                 Aurangzeb khan             NA-31 2   new        TRUE
    #> 130                     Gul Rehman             PK-31 2   old        TRUE
    #> 131                     Gul rehman             NA-31 2   new        TRUE
    #> 132               Haji Gulam Ahmad             PK-31 2   old        TRUE
    #> 133              Haji ghulam ahmad             NA-31 2   new        TRUE
    #> 134                    Ilyas Ahmad             PK-31 2   old        TRUE
    #> 135             Ilyas ahmad bilour             NA-31 2   new        TRUE
    #> 136                       M.Nadeem             PK-31 2   old        TRUE
    #> 137                Muhammad nadeem             NA-31 2   new        TRUE
    #> 138                 M.Saqqaf Yasir             PK-31 2   old        TRUE
    #> 139          Muhammad saqqaf nasir             NA-31 2   new        TRUE
    #> 140             M.Siddiq Ur Rehman             PK-31 2   old        TRUE
    #> 141      Muhammad siddiq ur rehman             NA-31 2   new        TRUE
    #> 142                   Noor Hussain             PK-31 2   old        TRUE
    #> 143                   Noor hussain             NA-31 2   new        TRUE
    #> 144                      Roohullah             PK-31 2   old        TRUE
    #> 145                      Roohullah             NA-31 2   new        TRUE
    #> 146                    Shoukat Ali             PK-31 2   old        TRUE
    #> 147 Shaukat ali s/o muhammad afzal             NA-31 2   new        TRUE
    #> 148                    Shoukat Ali             PK-31 2   old        TRUE
    #> 149     Shaukat ali s/o liaqat ali             NA-31 2   new        TRUE
    #> 150                        Yasmeen             PK-31 2   old        TRUE
    #> 151                        Yasmeen             NA-31 2   new        TRUE
    #> 152                   Abbas Afridi             PK-32 2   old        TRUE
    #> 153                   Abbas Afridi             NA-32 2   new        TRUE
    #> 154                    Asmat Ullah             PK-32 2   old        TRUE
    #> 155               Asmat Ullah Khan             NA-32 2   new        TRUE
    #> 156                 Bismillah Khan             PK-32 2   old        TRUE
    #> 157                 Bismillah Khan             NA-32 2   new        TRUE
    #> 158                   Gohar M.Khan             PK-32 3   old        TRUE
    #> 159    Gohar Muhammad Khan Bangash             NA-32 3   new        TRUE
    #> 160    Gohar Muhammad khan Bangash             NA-32 3   new        TRUE
    #> 161                   Madiha Faraz             PK-32 2   old        TRUE
    #> 162                   Madiha Faraz             NA-32 2   new        TRUE
    #> 163             Mufti Ibrar Sultan             PK-32 2   old        TRUE
    #> 164             Mufti Ibrar Sultan             NA-32 2   new        TRUE
    #> 165            Najeeb Ullah Durani             PK-32 2   old        TRUE
    #> 166           Najeeb Ullah Durrani             NA-32 2   new        TRUE
    #> 167                 Sheryar Afridi             PK-32 2   old        TRUE
    #> 168                Shehryar Afridi             NA-32 2   new        TRUE
    #> 169                  Yousaf Afridi             PK-32 2   old        TRUE
    #> 170                  Yousaf Afridi             NA-32 2   new        TRUE
    #> 171                   Akhtar Munir             PK-33 2   old        TRUE
    #> 172           Akhtar munir bangash             NA-33 2   new        TRUE
    #> 173                   Arif Hussain             PK-33 2   old        TRUE
    #> 174                   Arif hussain             NA-33 2   new        TRUE
    #> 175                 Atiq Ur Rehman             PK-33 2   old        TRUE
    #> 176                 Atiq ur rehman             NA-33 2   new        TRUE
    #> 177                 Hussain Jalali             PK-33 2   old        TRUE
    #> 178                 Hussain jalali             NA-33 2   new        TRUE
    #> 179               Khaliq Ur Rehman             PK-33 2   old        TRUE
    #> 180               Khaliq ur rehman             NA-33 2   new        TRUE
    #> 181                   Khiyal Zaman             PK-33 2   old        TRUE
    #> 182                    Khyal zaman             NA-33 2   new        TRUE
    #> 183                     Malik Riaz             PK-33 2   old        TRUE
    #> 184             Malak riaz bangash             NA-33 2   new        TRUE
    #> 185                    Mufti Imran             PK-33 2   old        TRUE
    #> 186                 Muhammad imran             NA-33 2   new        TRUE
    #> 187                   Rangeen Khan             PK-33 2   old        TRUE
    #> 188                   Rangeen khan             NA-33 2   new        TRUE
    #> 189                   Sultan Akbar             PK-33 2   old        TRUE
    #> 190                   Sultan akbar             NA-33 2   new        TRUE
    #> 191                Haider Ali Shah             PK-33 2   old        TRUE
    #> 192           Syed haider ali shah             NA-33 2   new        TRUE
    #> 193                   Taj Mohammad             PK-33 2   old        TRUE
    #> 194                   Taj muhammad             NA-33 2   new        TRUE
    #> 195                  Ulfat Hussain             PK-33 2   old        TRUE
    #> 196                  Ulfat hussain             NA-33 2   new        TRUE
    #> 197                    Altaf Qadir             PK-34 2   old        TRUE
    #> 198                    altaf qadir             NA-34 2   new        TRUE
    #> 199                      Aziz Khan             PK-34 2   old        TRUE
    #> 200                      aziz khan             NA-34 2   new        TRUE
    #> 201                 Gul Sahib Khan             PK-34 2   old        TRUE
    #> 202                 gul sahib khan             NA-34 2   new        TRUE
    #> 203                   Hameed Ullah             PK-34 2   old        TRUE
    #> 204               hameedullah khan             NA-34 2   new        TRUE
    #> 205                   Haseeb Ahmad             PK-34 2   old        TRUE
    #> 206             haseeb ahmad afaqi             NA-34 2   new        TRUE
    #> 207                    Ikhtiar Gul             PK-34 2   old        TRUE
    #> 208                   ikhtiyar gul             NA-34 2   new        TRUE
    #> 209                     Immad Azam             PK-34 2   old        TRUE
    #> 210                     immad azam             NA-34 2   new        TRUE
    #> 211                 Mir Zakim Khan             PK-34 2   old        TRUE
    #> 212                 mir zakim khan             NA-34 2   new        TRUE
    #> 213           Nawabzada Mohsin Ali             PK-34 2   old        TRUE
    #> 214      nawabzada mohsin ali khan             NA-34 2   new        TRUE
    #> 215                   Rehmat Salam             PK-34 2   old        TRUE
    #> 216              rehmat salam khan             NA-34 2   new        TRUE
    #> 217                Saad Ullah Khan             PK-34 2   old        TRUE
    #> 218         saad ullah khan khatak             NA-34 2   new        TRUE
    #> 219                   Shahid Ahmad             PK-34 2   old        TRUE
    #> 220                   shahid ahmad             NA-34 2   new        TRUE
    #> 221                 Shakir Zeeshan             PK-34 2   old        TRUE
    #> 222         shakir zeeshan khattak             NA-34 2   new        TRUE
    #> 223                Shams Ur Rehman             PK-34 2   old        TRUE
    #> 224                shams ur rehman             NA-34 2   new        TRUE
    #> 225                Tauseef Razique             PK-34 2   old        TRUE
    #> 226                touseef razique             NA-34 2   new        TRUE
    #> 227                    Usman Ghani             PK-34 2   old        TRUE
    #> 228                    usman ghani             NA-34 2   new        TRUE
    #> 229                  Zafar Mehmood             PK-34 2   old        TRUE
    #> 230                  zafar mehmood             NA-34 2   new        TRUE
    #> 231               Abdul Samad Khan             PK-35 2   old        TRUE
    #> 232               abdul samad khan             NA-35 2   new        TRUE
    #> 233                     Akram Khan             PK-35 2   old        TRUE
    #> 234              akram khan durani             NA-35 2   new        TRUE
    #> 235                     Amin Ullah             PK-35 2   old        TRUE
    #> 236                     amin ullah             NA-35 2   new        TRUE
    #> 237                     Hamid Shah             PK-35 2   old        TRUE
    #> 238                     hamid shah             NA-35 2   new        TRUE
    #> 239         Imran Ahmad Khan Niazi             PK-35 2   old        TRUE
    #> 240         imran ahmad khan niazi             NA-35 2   new        TRUE
    #> 241                     Inam Ullah             PK-35 2   old        TRUE
    #> 242                     inam ullah             NA-35 2   new        TRUE
    #> 243                     Aslam Noor             PK-35 2   old        TRUE
    #> 244                     islam noor             NA-35 2   new        TRUE
    #> 245                     Liaqat Ali             PK-35 2   old        TRUE
    #> 246                     liaqat ali             NA-35 2   new        TRUE
    #> 247                    M.Usman Ali             PK-35 2   old        TRUE
    #> 248                    M usman ali             NA-35 2   new        TRUE
    #> 249                    Sadaf Iqbal             PK-35 2   old        TRUE
    #> 250              Safdar iqbal shah             NA-35 2   new        TRUE
    #> 251                   Yasmin Sadaf             PK-35 2   old        TRUE
    #> 252            Syeda yasmin safdar             NA-35 2   new        TRUE
    #> 253                  Wali Dad Khan             PK-35 2   old        TRUE
    #> 254                  Wali dad khan             NA-35 2   new        TRUE
    #> 255                Sayed Zafar Ali             NA-53 2   old       FALSE
    #> 256                 Zaib Ur Rahman             NA-53 2   old       FALSE
    #> 257             Chaduary Abid Raza             NA-71 2   old       FALSE
    #> 258            Chaudhary Abid Raza             NA-71 2   old       FALSE
    #> 259                Jala Ahmad Khan              PB-1 2   old       FALSE
    #> 260                     Jalil Khan              PB-1 2   old       FALSE
    #> 261                     Habibullah             PB-13 3   old       FALSE
    #> 262                     Habibullah             PS-54 3   old       FALSE
    #> 263                     Habibullah             PS-63 3   old       FALSE
    #> 264                       Ali Sher             PB-14 2   old       FALSE
    #> 265                       Ali Sher             PS-59 2   old       FALSE
    #> 266                        Ibrahim             PB-27 2   old       FALSE
    #> 267                        Ibrahim             PS-54 2   old       FALSE
    #> 268                   Sajjad Ahmad             PK-15 2   old       FALSE
    #> 269                   Sajjad Ahmad             PS-50 2   old       FALSE
    #> 270                      Gul Zaman             PK-19 2   old       FALSE
    #> 271                      Gul Zaman             PS-53 2   old       FALSE
    #> 272                   Naseer U Din             PK-27 2   old       FALSE
    #> 273                  Naseer Ud Din             PK-27 2   old       FALSE
    #> 274                     Javaid Ali              PK-3 2   old       FALSE
    #> 275                     Javaid Ali             PS-48 2   old       FALSE
    #> 276                     Maaz Ullah             PK-56 2   old       FALSE
    #> 277                     Maza Ullah             PK-56 2   old       FALSE
    #> 278                   Abdul Raheem             PK-74 2   old       FALSE
    #> 279                   Abdul Raheem             PS-51 2   old       FALSE
    #> 280              Tariq bashir raja             PP-10 2   new        TRUE
    #> 281               Safadar ali khan             PP-10 2   new        TRUE
    #> 282                      Shokt Ali            PP-107 2   old       FALSE
    #> 283                         Shukat            PP-107 2   old       FALSE
    #> 284                      Rida Asim             PS-92 2   old        TRUE
    #> 285                   Babar sohail             PP-11 2   new        TRUE
    #> 286                  Shafeeq Ahmed             PS-92 2   old        TRUE
    #> 287             Ch. Muhammad adnan             PP-11 2   new        TRUE
    #> 288            Sahid Kursheed Rana             PS-92 2   old        TRUE
    #> 289                Ghulam muhammad             PP-11 2   new        TRUE
    #> 290                   Shahzad Khan             PS-92 2   old        TRUE
    #> 291       Haji malik parvez akhtar             PP-11 2   new        TRUE
    #> 292            Syed Hammad Hussain             PS-92 2   old        TRUE
    #> 293              Malik azhar aftab             PP-11 2   new        TRUE
    #> 294           Syed Khawar Ali Shah             PS-92 2   old        TRUE
    #> 295          Muhammad adnan younas             PP-11 2   new        TRUE
    #> 296            Syed Muhammad Rafiq             PS-92 2   old        TRUE
    #> 297            Musarat iqbal abasi             PP-11 2   new        TRUE
    #> 298            Zameer Waheed Jafri             PS-92 2   old        TRUE
    #> 299             Naeem aslam kiyani             PP-11 2   new        TRUE
    #> 300                      Almasazam             PS-93 2   old        TRUE
    #> 301                    Raja arshad             PP-11 2   new        TRUE
    #> 302             Asfag Ahmend Mangi             PS-93 2   old        TRUE
    #> 303            Raja qaiser mujahid             PP-11 2   new        TRUE
    #> 304                    Saeed Ahmad            PP-121 2   old       FALSE
    #> 305                    Saeed Ahmad             PS-59 2   old       FALSE
    #> 306                   Waseem Ahmad             PP-14 2   old       FALSE
    #> 307                   Waseem Ahmad             PS-67 2   old       FALSE
    #> 308                  Mukhtar Ahmad            PP-140 2   old       FALSE
    #> 309                  Mukhtar Ahmad             PS-50 2   old       FALSE
    #> 310                Muhammad Saleem            PP-142 2   old       FALSE
    #> 311                Muhammad Saleem            PP-143 2   old       FALSE
    #> 312              Abdul Kareem Khan             PP-15 2   old       FALSE
    #> 313         Malik Abdul Karim Khan             PP-15 2   old       FALSE
    #> 314                M. Javaid Iqbal             PP-15 2   old       FALSE
    #> 315           M. Javed Iqbal Malik             PP-15 2   old       FALSE
    #> 316                 M. Tahir Iqbal             PP-15 2   old       FALSE
    #> 317                    Tahir Iqbal             PP-15 2   old       FALSE
    #> 318                   Umer Tanveer             PP-15 2   old       FALSE
    #> 319                    Umer Tanvir             PP-15 2   old       FALSE
    #> 320          Khawaja Salma Rafique            PP-157 2   old       FALSE
    #> 321          Kjawaja Salma Rafique            PP-157 2   old       FALSE
    #> 322               Abdul Aleem Khan            PP-157 4   old       FALSE
    #> 323               Abdul Aleem Khan            PP-158 4   old       FALSE
    #> 324               Kiran Aleem Khan            PP-158 4   old       FALSE
    #> 325               Abdul Aleem Khan             PS-64 4   old       FALSE
    #> 326                M Mumtaz Khalid            PP-159 2   old       FALSE
    #> 327         Muhammad Mumtaz Khalid            PP-159 2   old       FALSE
    #> 328                Ghulam Muhammad            NA-138 2   old       FALSE
    #> 329                Ghulam Muhammad            PP-164 2   old       FALSE
    #> 330               Muhammad Pervaiz            PP-128 2   old       FALSE
    #> 331              Mehr Muhamd Iqbal            PP-184 2   old       FALSE
    #> 332            Muhhmad Moeen Watto            PP-186 2   old       FALSE
    #> 333            Noor Ul Ameen Watto            PP-186 2   old       FALSE
    #> 334                      Rao Fahad            PP-186 2   old       FALSE
    #> 335                 Muhammad Akram            PP-192 2   old       FALSE
    #> 336                   Ghulam Qadir            PP-189 2   old       FALSE
    #> 337                   Gjulam Qadir             PS-52 2   old       FALSE
    #> 338                   Habib Ul Haq            PP-189 2   old       FALSE
    #> 339                   Arshad Iqbal            PP-226 2   old       FALSE
    #> 340           Mian Amir Iqbal Shah            PP-224 2   old       FALSE
    #> 341            Muhammad Iqbal Shah            PP-224 2   old       FALSE
    #> 342                    Sardar Khan            PP-257 2   old       FALSE
    #> 343                    Sardar Khan             PS-55 2   old       FALSE
    #> 344                   Abdul Hafeez             PP-61 2   old       FALSE
    #> 345                   Abdul Hafeez            PP-286 2   old       FALSE
    #> 346                 Ghulam Mustafa             PP-40 2   old       FALSE
    #> 347                 Ghulam Mustafa             PS-63 2   old       FALSE
    #> 348               Dr Sohail Zaffar             PP-59 2   old       FALSE
    #> 349            Sohail Zafar Cheema             PP-59 2   old       FALSE
    #> 350               Amir raza abbasi              PP-6 2   new        TRUE
    #> 351             Amjad arbab abbasi              PP-6 2   new        TRUE
    #> 352                   Sajjid Ahmad             PP-67 2   old       FALSE
    #> 353                    Sajjid Khan             PP-67 2   old       FALSE
    #> 354                  Zulafiqar Ali             PP-77 2   old       FALSE
    #> 355                   Zulfiqar Ali             PP-77 2   old       FALSE
    #> 356                Muhammad Haroon             PS-55 2   old       FALSE
    #> 357                Muhammad Haroon             PS-67 2   old       FALSE
    #> 358                    Arsalan Taj            PS-102 2   old       FALSE
    #> 359                 Dilwer Jackson            PS-102 2   old       FALSE
    #> 360                 Abdul Razzaque             PS-54 2   old       FALSE
    #> 361           Mehmood Abdul Razzaq            PS-105 2   old       FALSE
    #> 362                Dilawar Jeckson            PS-106 2   old       FALSE
    #> 363                  Ghulam Hashim            PS-106 2   old       FALSE
    #> 364                    Abdul Latif             PS-63 2   old       FALSE
    #> 365                    Abdul Latif            PS-107 2   old       FALSE
    #> 366                Muhammad Jawaid            PS-107 2   old       FALSE
    #> 367                Syed Anwer Shah            PS-116 2   old       FALSE
    #> 368                Muhammad Mohsin             PP-61 2   old       FALSE
    #> 369                Muhammad Mohsin             PS-13 2   old       FALSE
    #> 370                    Sarang Khan             PS-19 2   old       FALSE
    #> 371                    Sarang Khan             PS-59 2   old       FALSE
    #> 372                      Ali Murad             PS-20 2   old       FALSE
    #> 373                      Ali Murad             PS-51 2   old       FALSE
    #> 374                 Ahmad Ali Shah             PS-30 2   old        TRUE
    #> 375             Ghulam Shabir Shah              PS-3 2   new        TRUE
    #> 376                        Ali Bux             PS-40 2   old       FALSE
    #> 377                        Ali Bux             PS-53 2   old       FALSE
    #> 378                  Raza Muhammad             PS-43 2   old       FALSE
    #> 379                  Raza Muhammad             PS-67 2   old       FALSE
    #> 380                   Abdul Hameed             PS-63 2   old        TRUE
    #> 381           Abdul Hameed Panhwar             PS-78 2   new        TRUE
    #> 382            Meer Hassan Panhyar             PS-80 2   new        TRUE
    #> 383     Mehmood Ur Rehman Halepoto             PS-80 2   new        TRUE
    #> 384                           Dayo             PS-81 2   new        TRUE
    #> 385                    Giyanoo Mal             PS-81 2   new        TRUE
    #> 386          Dost Muhammad Solangi             PS-84 2   new        TRUE
    #> 387              Sadaqat Ali Jatoi             PS-84 2   new        TRUE
    #> 388               Muhammad Ibrahim             PS-67 2   old        TRUE
    #> 389               Muhammad Ibrahim             PS-88 2   new        TRUE
    #> 390              Amad U Allah Awan            PP-246 6   old       FALSE
    #> 391                Amad Ullah Awan            PP-246 6   old       FALSE
    #> 392                Amad Ullha Awan            PP-246 6   old       FALSE
    #> 393             Amadeus Ullah Awam            PP-246 6   old       FALSE
    #> 394                 Amadullah Awan            PP-246 6   old       FALSE
    #> 395                  Amadullahawan            PP-246 6   old       FALSE
    #>     only_one_old_some_new
    #> 1                   FALSE
    #> 2                   FALSE
    #> 3                   FALSE
    #> 4                   FALSE
    #> 5                   FALSE
    #> 6                   FALSE
    #> 7                   FALSE
    #> 8                   FALSE
    #> 9                   FALSE
    #> 10                  FALSE
    #> 11                  FALSE
    #> 12                  FALSE
    #> 13                  FALSE
    #> 14                  FALSE
    #> 15                  FALSE
    #> 16                  FALSE
    #> 17                  FALSE
    #> 18                  FALSE
    #> 19                  FALSE
    #> 20                  FALSE
    #> 21                  FALSE
    #> 22                  FALSE
    #> 23                  FALSE
    #> 24                   TRUE
    #> 25                   TRUE
    #> 26                   TRUE
    #> 27                   TRUE
    #> 28                   TRUE
    #> 29                   TRUE
    #> 30                   TRUE
    #> 31                   TRUE
    #> 32                   TRUE
    #> 33                   TRUE
    #> 34                   TRUE
    #> 35                   TRUE
    #> 36                   TRUE
    #> 37                   TRUE
    #> 38                   TRUE
    #> 39                   TRUE
    #> 40                   TRUE
    #> 41                   TRUE
    #> 42                   TRUE
    #> 43                   TRUE
    #> 44                   TRUE
    #> 45                   TRUE
    #> 46                   TRUE
    #> 47                   TRUE
    #> 48                   TRUE
    #> 49                   TRUE
    #> 50                   TRUE
    #> 51                   TRUE
    #> 52                   TRUE
    #> 53                   TRUE
    #> 54                   TRUE
    #> 55                   TRUE
    #> 56                   TRUE
    #> 57                   TRUE
    #> 58                   TRUE
    #> 59                   TRUE
    #> 60                   TRUE
    #> 61                   TRUE
    #> 62                   TRUE
    #> 63                   TRUE
    #> 64                   TRUE
    #> 65                   TRUE
    #> 66                   TRUE
    #> 67                   TRUE
    #> 68                   TRUE
    #> 69                   TRUE
    #> 70                   TRUE
    #> 71                   TRUE
    #> 72                   TRUE
    #> 73                   TRUE
    #> 74                   TRUE
    #> 75                   TRUE
    #> 76                   TRUE
    #> 77                   TRUE
    #> 78                   TRUE
    #> 79                   TRUE
    #> 80                   TRUE
    #> 81                   TRUE
    #> 82                  FALSE
    #> 83                  FALSE
    #> 84                  FALSE
    #> 85                  FALSE
    #> 86                  FALSE
    #> 87                  FALSE
    #> 88                  FALSE
    #> 89                  FALSE
    #> 90                  FALSE
    #> 91                  FALSE
    #> 92                  FALSE
    #> 93                  FALSE
    #> 94                  FALSE
    #> 95                  FALSE
    #> 96                  FALSE
    #> 97                  FALSE
    #> 98                  FALSE
    #> 99                  FALSE
    #> 100                 FALSE
    #> 101                 FALSE
    #> 102                 FALSE
    #> 103                 FALSE
    #> 104                 FALSE
    #> 105                 FALSE
    #> 106                 FALSE
    #> 107                 FALSE
    #> 108                 FALSE
    #> 109                 FALSE
    #> 110                 FALSE
    #> 111                 FALSE
    #> 112                 FALSE
    #> 113                 FALSE
    #> 114                 FALSE
    #> 115                 FALSE
    #> 116                 FALSE
    #> 117                 FALSE
    #> 118                 FALSE
    #> 119                 FALSE
    #> 120                 FALSE
    #> 121                 FALSE
    #> 122                 FALSE
    #> 123                 FALSE
    #> 124                 FALSE
    #> 125                 FALSE
    #> 126                 FALSE
    #> 127                 FALSE
    #> 128                 FALSE
    #> 129                 FALSE
    #> 130                 FALSE
    #> 131                 FALSE
    #> 132                 FALSE
    #> 133                 FALSE
    #> 134                 FALSE
    #> 135                 FALSE
    #> 136                 FALSE
    #> 137                 FALSE
    #> 138                 FALSE
    #> 139                 FALSE
    #> 140                 FALSE
    #> 141                 FALSE
    #> 142                 FALSE
    #> 143                 FALSE
    #> 144                 FALSE
    #> 145                 FALSE
    #> 146                 FALSE
    #> 147                 FALSE
    #> 148                 FALSE
    #> 149                 FALSE
    #> 150                 FALSE
    #> 151                 FALSE
    #> 152                 FALSE
    #> 153                 FALSE
    #> 154                 FALSE
    #> 155                 FALSE
    #> 156                 FALSE
    #> 157                 FALSE
    #> 158                 FALSE
    #> 159                 FALSE
    #> 160                 FALSE
    #> 161                 FALSE
    #> 162                 FALSE
    #> 163                 FALSE
    #> 164                 FALSE
    #> 165                 FALSE
    #> 166                 FALSE
    #> 167                 FALSE
    #> 168                 FALSE
    #> 169                 FALSE
    #> 170                 FALSE
    #> 171                 FALSE
    #> 172                 FALSE
    #> 173                 FALSE
    #> 174                 FALSE
    #> 175                 FALSE
    #> 176                 FALSE
    #> 177                 FALSE
    #> 178                 FALSE
    #> 179                 FALSE
    #> 180                 FALSE
    #> 181                 FALSE
    #> 182                 FALSE
    #> 183                 FALSE
    #> 184                 FALSE
    #> 185                 FALSE
    #> 186                 FALSE
    #> 187                 FALSE
    #> 188                 FALSE
    #> 189                 FALSE
    #> 190                 FALSE
    #> 191                 FALSE
    #> 192                 FALSE
    #> 193                 FALSE
    #> 194                 FALSE
    #> 195                 FALSE
    #> 196                 FALSE
    #> 197                 FALSE
    #> 198                 FALSE
    #> 199                 FALSE
    #> 200                 FALSE
    #> 201                 FALSE
    #> 202                 FALSE
    #> 203                 FALSE
    #> 204                 FALSE
    #> 205                 FALSE
    #> 206                 FALSE
    #> 207                 FALSE
    #> 208                 FALSE
    #> 209                 FALSE
    #> 210                 FALSE
    #> 211                 FALSE
    #> 212                 FALSE
    #> 213                 FALSE
    #> 214                 FALSE
    #> 215                 FALSE
    #> 216                 FALSE
    #> 217                 FALSE
    #> 218                 FALSE
    #> 219                 FALSE
    #> 220                 FALSE
    #> 221                 FALSE
    #> 222                 FALSE
    #> 223                 FALSE
    #> 224                 FALSE
    #> 225                 FALSE
    #> 226                 FALSE
    #> 227                 FALSE
    #> 228                 FALSE
    #> 229                 FALSE
    #> 230                 FALSE
    #> 231                 FALSE
    #> 232                 FALSE
    #> 233                 FALSE
    #> 234                 FALSE
    #> 235                 FALSE
    #> 236                 FALSE
    #> 237                 FALSE
    #> 238                 FALSE
    #> 239                 FALSE
    #> 240                 FALSE
    #> 241                 FALSE
    #> 242                 FALSE
    #> 243                 FALSE
    #> 244                 FALSE
    #> 245                 FALSE
    #> 246                 FALSE
    #> 247                 FALSE
    #> 248                 FALSE
    #> 249                 FALSE
    #> 250                 FALSE
    #> 251                 FALSE
    #> 252                 FALSE
    #> 253                 FALSE
    #> 254                 FALSE
    #> 255                 FALSE
    #> 256                 FALSE
    #> 257                 FALSE
    #> 258                 FALSE
    #> 259                 FALSE
    #> 260                 FALSE
    #> 261                 FALSE
    #> 262                 FALSE
    #> 263                 FALSE
    #> 264                 FALSE
    #> 265                 FALSE
    #> 266                 FALSE
    #> 267                 FALSE
    #> 268                 FALSE
    #> 269                 FALSE
    #> 270                 FALSE
    #> 271                 FALSE
    #> 272                 FALSE
    #> 273                 FALSE
    #> 274                 FALSE
    #> 275                 FALSE
    #> 276                 FALSE
    #> 277                 FALSE
    #> 278                 FALSE
    #> 279                 FALSE
    #> 280                 FALSE
    #> 281                 FALSE
    #> 282                 FALSE
    #> 283                 FALSE
    #> 284                 FALSE
    #> 285                 FALSE
    #> 286                 FALSE
    #> 287                 FALSE
    #> 288                 FALSE
    #> 289                 FALSE
    #> 290                 FALSE
    #> 291                 FALSE
    #> 292                 FALSE
    #> 293                 FALSE
    #> 294                 FALSE
    #> 295                 FALSE
    #> 296                 FALSE
    #> 297                 FALSE
    #> 298                 FALSE
    #> 299                 FALSE
    #> 300                 FALSE
    #> 301                 FALSE
    #> 302                 FALSE
    #> 303                 FALSE
    #> 304                 FALSE
    #> 305                 FALSE
    #> 306                 FALSE
    #> 307                 FALSE
    #> 308                 FALSE
    #> 309                 FALSE
    #> 310                 FALSE
    #> 311                 FALSE
    #> 312                 FALSE
    #> 313                 FALSE
    #> 314                 FALSE
    #> 315                 FALSE
    #> 316                 FALSE
    #> 317                 FALSE
    #> 318                 FALSE
    #> 319                 FALSE
    #> 320                 FALSE
    #> 321                 FALSE
    #> 322                 FALSE
    #> 323                 FALSE
    #> 324                 FALSE
    #> 325                 FALSE
    #> 326                 FALSE
    #> 327                 FALSE
    #> 328                 FALSE
    #> 329                 FALSE
    #> 330                 FALSE
    #> 331                 FALSE
    #> 332                 FALSE
    #> 333                 FALSE
    #> 334                 FALSE
    #> 335                 FALSE
    #> 336                 FALSE
    #> 337                 FALSE
    #> 338                 FALSE
    #> 339                 FALSE
    #> 340                 FALSE
    #> 341                 FALSE
    #> 342                 FALSE
    #> 343                 FALSE
    #> 344                 FALSE
    #> 345                 FALSE
    #> 346                 FALSE
    #> 347                 FALSE
    #> 348                 FALSE
    #> 349                 FALSE
    #> 350                 FALSE
    #> 351                 FALSE
    #> 352                 FALSE
    #> 353                 FALSE
    #> 354                 FALSE
    #> 355                 FALSE
    #> 356                 FALSE
    #> 357                 FALSE
    #> 358                 FALSE
    #> 359                 FALSE
    #> 360                 FALSE
    #> 361                 FALSE
    #> 362                 FALSE
    #> 363                 FALSE
    #> 364                 FALSE
    #> 365                 FALSE
    #> 366                 FALSE
    #> 367                 FALSE
    #> 368                 FALSE
    #> 369                 FALSE
    #> 370                 FALSE
    #> 371                 FALSE
    #> 372                 FALSE
    #> 373                 FALSE
    #> 374                 FALSE
    #> 375                 FALSE
    #> 376                 FALSE
    #> 377                 FALSE
    #> 378                 FALSE
    #> 379                 FALSE
    #> 380                 FALSE
    #> 381                 FALSE
    #> 382                 FALSE
    #> 383                 FALSE
    #> 384                 FALSE
    #> 385                 FALSE
    #> 386                 FALSE
    #> 387                 FALSE
    #> 388                 FALSE
    #> 389                 FALSE
    #> 390                 FALSE
    #> 391                 FALSE
    #> 392                 FALSE
    #> 393                 FALSE
    #> 394                 FALSE
    #> 395                 FALSE

    # some are spelling discrepancies, some are the same uid in different constituencies!
    # there seems to be a double entry of the PK and NA rows!
    # TODO: For these see which columns are different!
    filter(cleaned_assets, uid == "02371") %>%
      select(entry, contains("year_income"), starts_with("intern")) %>% as.data.frame()

    #>   entry year_income year_income_1 year_income_2 year_income_3
    #> 1   new       1 2 3             1             1             1
    #>   year_income_4 intern_trips_no intern_trips_cost
    #> 1             0             -99                NA

    filter(cleaned_assets, uid == "02371") %>%
      summarize_all(funs(diff = length(unique(.))>1)) %>%
      select_if(. == TRUE)

    #> # A tibble: 1 x 0

    filter(cleaned_assets, uid == "02448") %>%
      select(entry, 1:10,contains("year_income"), starts_with("intern")) %>% as.data.frame()

    #>   entry enum_name enum_other type_seat const_number uid_initial
    #> 1   new       113       <NA>        NA           35       02448
    #>   candidate_name im_prop_pak im_prop_pak_g_count im_prop_pak_p_1
    #> 1     amin ullah           1                   1         3000000
    #>   im_prop_pak_t_1 year_income year_income_1 year_income_2 year_income_3
    #> 1               1           4             0             0             0
    #>   year_income_4 intern_trips_no intern_trips_cost
    #> 1             1               0                NA

    # if old and new, for now just take new
    # =======
    # END OLD CODE
    # =======

    dup_uids$uid

    #>   [1] "00101" "00101" "00250" "00250" "00256" "00256" "00279" "00279"
    #>   [9] "00494" "00494" "00495" "00495" "00499" "00499" "00569" "00569"
    #>  [17] "00791" "00791" "01009" "01009" "01141" "01141" "01141" "01149"
    #>  [25] "01149" "01340" "01340" "01341" "01341" "01342" "01342" "01343"
    #>  [33] "01343" "01344" "01344" "01345" "01345" "01346" "01346" "01348"
    #>  [41] "01348" "01349" "01349" "01350" "01350" "01351" "01351" "01352"
    #>  [49] "01352" "01353" "01353" "01354" "01354" "01357" "01357" "01358"
    #>  [57] "01358" "01359" "01359" "01360" "01360" "01361" "01361" "01362"
    #>  [65] "01362" "01363" "01363" "01364" "01364" "01375" "01375" "01376"
    #>  [73] "01376" "01377" "01377" "01378" "01378" "01379" "01379" "01380"
    #>  [81] "01380" "01381" "01381" "01382" "01382" "01493" "01493" "01519"
    #>  [89] "01519" "01522" "01522" "01537" "01537" "01543" "01543" "01546"
    #>  [97] "01546" "01557" "01557" "01567" "01567" "01570" "01570" "02184"
    #> [105] "02184" "02371" "02371" "02372" "02372" "02373" "02373" "02374"
    #> [113] "02374" "02384" "02384" "02385" "02385" "02386" "02386" "02387"
    #> [121] "02387" "02388" "02388" "02389" "02389" "02390" "02390" "02391"
    #> [129] "02391" "02392" "02392" "02393" "02393" "02394" "02394" "02395"
    #> [137] "02395" "02396" "02396" "02397" "02397" "02398" "02398" "02399"
    #> [145] "02399" "02400" "02400" "02401" "02401" "02402" "02402" "02403"
    #> [153] "02403" "02404" "02404" "02405" "02405" "02406" "02406" "02407"
    #> [161] "02407" "02408" "02408" "02408" "02409" "02409" "02410" "02410"
    #> [169] "02412" "02412" "02413" "02413" "02414" "02414" "02415" "02415"
    #> [177] "02416" "02416" "02417" "02417" "02418" "02418" "02419" "02419"
    #> [185] "02420" "02420" "02421" "02421" "02422" "02422" "02423" "02423"
    #> [193] "02424" "02424" "02425" "02425" "02426" "02426" "02427" "02427"
    #> [201] "02428" "02428" "02429" "02429" "02430" "02430" "02431" "02431"
    #> [209] "02432" "02432" "02433" "02433" "02434" "02434" "02435" "02435"
    #> [217] "02436" "02436" "02437" "02437" "02438" "02438" "02440" "02440"
    #> [225] "02441" "02441" "02442" "02442" "02443" "02443" "02444" "02444"
    #> [233] "02445" "02445" "02446" "02446" "02447" "02447" "02448" "02448"
    #> [241] "02449" "02449" "02450" "02450" "02451" "02451" "02452" "02452"
    #> [249] "02453" "02453" "02454" "02454" "02455" "02455" "02456" "02456"
    #> [257] "02457" "02457" "02884" "02884" "03089" "03089" "03435" "03435"
    #> [265] "03514" "03514" "03514" "03524" "03524" "03804" "03804" "04244"
    #> [273] "04244" "04440" "04440" "04475" "04475" "04558" "04558" "04589"
    #> [281] "04589" "04912" "04912" "04921" "04921" "05039" "05039" "05134"
    #> [289] "05134" "05526" "05526" "05661" "05661" "05686" "05686" "05687"
    #> [297] "05687" "05688" "05688" "05689" "05689" "05690" "05690" "05691"
    #> [305] "05691" "05692" "05692" "05693" "05693" "05694" "05694" "05695"
    #> [313] "05695" "05859" "05859" "06160" "06160" "06178" "06178" "06247"
    #> [321] "06247" "06338" "06338" "06339" "06339" "06342" "06342" "06343"
    #> [329] "06343" "06344" "06344" "06470" "06470" "06480" "06480" "06480"
    #> [337] "06480" "06504" "06504" "06584" "06584" "06968" "06968" "07012"
    #> [345] "07012" "07015" "07015" "07051" "07051" "07052" "07052" "07212"
    #> [353] "07212" "07489" "07489" "07891" "07891" "08325" "08325" "08631"
    #> [361] "08631" "08924" "08924" "08927" "08927" "09050" "09050" "09194"
    #> [369] "09194" "09327" "09327" "09406" "09406" "09557" "09557" "09627"
    #> [377] "09627" "09639" "09639" "09657" "09657" "09671" "09671" "09763"
    #> [385] "09763" "09820" "09820" "10074" "10074" "10165" "10165" "10278"
    #> [393] "10278" "10298" "10298" "10469" "10469" "10480" "10480" "10574"
    #> [401] "10574" "10671" "10671" "10715" "10715" "11270" "11270" "11304"
    #> [409] "11304" "11316" "11316" "11366" "11366" "11414" "11414" "99999"
    #> [417] "99999" "99999" "99999" "99999" "99999"

    clean_uids

    #> # A tibble: 421 x 5
    #>    key            uid_dup_resolved uid_dup_notes  uid_dup_drop uid_dup_new
    #>    <chr>          <lgl>            <chr>          <lgl>        <chr>      
    #>  1 uuid:eb6f316f… TRUE             keeper_new_ol… FALSE        <NA>       
    #>  2 uuid:c600a48b… TRUE             dropper_new_o… TRUE         <NA>       
    #>  3 uuid:fe691388… TRUE             keeper_new_ol… FALSE        <NA>       
    #>  4 uuid:5502ffee… TRUE             dropper_new_o… TRUE         <NA>       
    #>  5 uuid:a26d194d… TRUE             keeper_new_ol… FALSE        <NA>       
    #>  6 uuid:8ad065f8… TRUE             dropper_new_o… TRUE         <NA>       
    #>  7 uuid:fe236480… TRUE             keeper_new_ol… FALSE        <NA>       
    #>  8 uuid:ee7d77a3… TRUE             dropper_new_o… TRUE         <NA>       
    #>  9 uuid:97d81fbd… TRUE             keeper_new_ol… FALSE        <NA>       
    #> 10 uuid:0299c40e… TRUE             dropper_new_o… TRUE         <NA>       
    #> # ... with 411 more rows

    # TODO: figure out data quality across old and new

    table(duplicated(cleaned_assets))

    #> 
    #> FALSE 
    #> 17962

    table(cleaned_assets$remove)

    #> Warning: Unknown or uninitialised column: 'remove'.

    #> < table of extent 0 >

    table(new_assets$cons_type, new_assets$const_number)

    #>           
    #>            3 6 8 9 10 11 21 25 29 30 31 32 33 34 35 48 49 50 51 78 79 80
    #>   NA       0 0 0 0  3  0  0  0  0  0  1  0  3  2  2  9  8  6  3  0  0  0
    #>   PK       0 0 0 0  0  0  0  3  1  1  1  0  0  2  1  0  0  0  1  0  0  0
    #>   PP       0 3 1 0  1  1  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    #>   PS       2 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  2
    #>   Reserved 0 0 0 0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0
    #>           
    #>            81 82 83 84 85 86 87 88 89 90 207 209 219 235 249
    #>   NA        0  0  0  0  0  0  2  0  1  0   2   2   0   1   3
    #>   PK        0  0  0  0  0  0  0  0  0  0   0   0   0   0   0
    #>   PP        0  0  0  0  0  0  0  0  1  0   0   0   2   0   2
    #>   PS        0  1  1  2  0  0  1  2  0  0   0   0   0   1   0
    #>   Reserved  1  0  1  0  0  0  1  0  0  0   0   0   0   0   1

    # TODO copy rows to candidates in multiple locations if they are in the data somewhere but missing in another constituency

Check for missing members in the results data.

    # Confirm all remaining dupes are same person within constituency
    # and entry time, for now just pick the first one by `key`
    # TODO: resolve duplicate data quality better
    remaining_dups <- cleaned_assets %>%
      group_by(uid) %>%
      mutate(n = n()) %>%
      filter(!is.na(uid) & uid != "-9999") %>%
      filter(n > 1) %>%
      select(uid, n, constituency_code, entry)

    # Check whether there are any easily solvable duplicates that were
    # duplicated across entry waves ("new" and "old")
    remaining_dups %>%
      mutate(duplicated = length(unique(paste0(constituency_code, entry))) == 1) %>%
      filter(!duplicated)

    #> # A tibble: 0 x 5
    #> # Groups:   uid [0]
    #> # ... with 5 variables: uid <chr>, n <int>, constituency_code <chr>,
    #> #   entry <chr>, duplicated <lgl>

    # None as of 2011-11-26

    remaining_dups %>%
      summarize() %>%
      nrow

    #> [1] 34

    nrow(remaining_dups)

    #> [1] 69

    n_to_delete <- nrow(remaining_dups) - nrow(summarize(remaining_dups))
    n_to_delete

    #> [1] 35

    orig_asset_n <- nrow(cleaned_assets)
    cleaned_assets <- cleaned_assets %>%
      arrange(uid, key) %>%
      group_by(uid) %>%
      mutate(keep_uid_dup = case_when(
        n() == 1 | is.na(uid) | uid == "-9999" ~ TRUE,
        TRUE ~ !duplicated(paste0(constituency_code, entry)) # keeps only first row
      )) %>%
      filter(keep_uid_dup) %>%
      select(-keep_uid_dup) %>%
      mutate(asset_data = 1)

    if(nrow(cleaned_assets) + n_to_delete != orig_asset_n) {
      stop("not right N deleted")
    }

    # any dupes left?
    cleaned_assets %>%
      group_by(uid) %>%
      mutate(n = n()) %>%
      filter(!is.na(uid) & uid != "-9999") %>%
      filter(n > 1) %>%
      select(uid, n, constituency_code, entry)

    #> # A tibble: 0 x 4
    #> # Groups:   uid [0]
    #> # ... with 4 variables: uid <chr>, n <int>, constituency_code <chr>,
    #> #   entry <chr>

    # should be no

    filter(cleaned_assets, is.na(uid)) %>%
      select(key, uid, uid_initial, starts_with("uid_dup"))

    #> # A tibble: 4 x 7
    #> # Groups:   uid [1]
    #>   key        uid   uid_initial uid_dup_resolved uid_dup_notes uid_dup_drop
    #>   <chr>      <chr> <chr>       <lgl>            <chr>         <lgl>       
    #> 1 uuid:4dbb… <NA>  <NA>        NA               <NA>          NA          
    #> 2 uuid:9b40… <NA>  <NA>        NA               <NA>          NA          
    #> 3 uuid:bd4d… <NA>  09406       TRUE             uid_NA_in_res FALSE       
    #> 4 uuid:f8bc… <NA>  10298       TRUE             uid_NA_in_res FALSE       
    #> # ... with 1 more variable: uid_dup_new <chr>

Additional matching!

    constituency_codes <- paste0(cleaned_assets$type_seat,
                                 "-",
                                 cleaned_assets$const_number)
    cons_ids <- c(
      paste0("NA-", 1:272),
      paste0("PP-", 1:297),
      paste0("PB-", 1:51),
      paste0("PK-", 1:99),
      paste0("PS-", 1:130)
    )
    missing_constituencies <- setdiff(cons_ids, constituency_codes)

    # Get those unmatched in results by UID
    res_unmatched <- res_u %>%
      select(id, constituency_code, candidate_name, candidate_party, candidate_rank, uid, found_initially) %>%
      left_join(
        cleaned_assets %>%
          filter(uid != "-9999" & uid != "-999", !is.na(uid)) %>%
          mutate(asset_data = 1) %>%
          select(uid, asset_data)
      ) %>%
      mutate(missing_constituencies = constituency_code %in% missing_constituencies) %>%
      filter(is.na(asset_data)) %>%
      select(-asset_data)

    #> Joining, by = "uid"

    # Attempt match by constituency-candidate
    res_names <- res_unmatched %>%
      mutate(candidate_name_lower = gsub(" ", "", tolower(candidate_name))) %>%
      left_join(cleaned_assets %>%
                  ungroup() %>%
                  mutate(candidate_name_lower = gsub(" ", "", tolower(candidate_name)),
                         uid_asset = uid) %>%
                  select(key, constituency_code, candidate_name_lower, asset_data, uid_asset, entry),
                by = c("constituency_code", "candidate_name_lower"))

    # People duplicated in this merge
    if (nrow(res_names) != nrow(res_unmatched)) {
      stop("Some names duplicated in merge")
    }

    table(res_names$uid_asset>0) # 332 had missing uids in the asset data, now matched on constituency-name

    #> 
    #> FALSE  TRUE 
    #>   336    31

    # Those who we should have matched before (i.e. they have UIDs in the asset data but were
    # only matched by name and constituency now)
    res_name_errs <- res_names %>%
      filter(uid_asset > 0) %>%
      as.data.frame
    # 3 are NA in the result UID (new results after UIDs were generated)
    res_name_errs %>%
      select(candidate_name, uid, uid_asset) %>%
      as.data.frame

    #>                 candidate_name   uid uid_asset
    #> 1                Jamil Hussain 02678     02679
    #> 2                  Waqas Ahmed 00344     00334
    #> 3              Ali Javed Dogar 00369     01370
    #> 4                 SAIMA DAWOOD 00510     06945
    #> 5         Aleem Ullah Warraich 00915     00922
    #> 6           Abid Hussain Jatoi 01223     01222
    #> 7               Muhammad Aslam 01488     01487
    #> 8                 ASHFAQ AHMED 01695     01696
    #> 9                  Abdul Ghani 01802     01803
    #> 10                Abdul Wadood 02323     02322
    #> 11                Qurat ul Ain 10168     10160
    #> 12          Afsar ul Mulk Khan 04510     04150
    #> 13               Dost Muhammad 10899     08367
    #> 14                Sikandar Ali 10920     03473
    #> 15              Akhtar Hussain 10966     07533
    #> 16            Muhammad Sagheer 11123     09266
    #> 17                 Abdul Jalil 11502     05674
    #> 18              Muhammad Noman 11527     05699
    #> 19   Syed Muhammad Naseem Shah 11537     05709
    #> 20           Naseem Ahmed Khan 11555     05727
    #> 21 Muhammad Mehboobo ur Rehman 11566     05738
    #> 22            Syed Mehmood Ali 11589     05761
    #> 23            Abdul Azeem Khan 11592     05765
    #> 24          Haleem Adil Sheikh 11635     05807
    #> 25                 Shahid Rana 09678     09679
    #> 26                   IRFAN ALI 06195     06197
    #> 27              GHULAM MURTAZA 06621     06620
    #> 28                 Zahid Iqbal  <NA>     07631
    #> 29                Nazeer Ahmad  <NA>     07965
    #> 30          Syed Usman Mehmood 07985     07991
    #> 31                Sajjad Ahmad  <NA>     08281

    # Many seem like simple transposition errors. Let's take them case by case!
    # name_match_asset_uid <- map_dfr(seq_len(nrow(res_name_errs)), ~ {
    # 
    #   uid_err <- res_name_errs$uid[.x]
    #   uid_asset_err <- res_name_errs$uid_asset[.x]
    #   print("uid_err")
    #   print(uid_err)
    #   print("uid_asset_err")
    #   print(uid_asset_err)
    #   res_u %>%
    #     filter(uid %in% c(uid_err, uid_asset_err)) %>%
    #     select(constituency_code, candidate_name, candidate_rank, uid, found_initially) %>%
    #     as.data.frame %>%
    #     print
    # 
    #   filter(cleaned_assets, uid %in% c(uid_err, uid_asset_err)) %>%
    #     select(constituency_code, candidate_name, uid) %>%
    #     as.data.frame %>%
    #     print
    # 
    #   dat <- data.frame(
    #     key = res_name_errs$key[.x],
    #     stringsAsFactors = FALSE
    #   )
    #   m <- readline("correct uid: ")
    #   if (m == "q") {
    #     stop()
    #   } else if (m == "m") {
    #     dat$uid_asset_correct_name_match <- "uid_two_constituency_candidate"
    #   } else if (m == "n"){
    #     dat$uid_asset_correct_name_match <- "do_not_match"
    #   } else if (m == "md") {
    #     dat$uid_asset_correct_name_match <- "replace_error"
    #   } else {
    #     dat$uid_asset_correct_name_match <- m
    #   }
    #   dat
    # })
    name_match_asset_uid <- structure(list(key = c("uuid:b98d1a6d-f55f-4571-a160-8d552804819c", 
    "uuid:156d859f-621e-4975-8869-537f7670b72c", "uuid:8422567d-2464-43d5-8d96-291bd59ac23b", 
    "uuid:9daba778-d30b-4945-a0cf-b32777a29f5d", "uuid:c60bd7ed-b9d2-4d74-83bd-16d33d7f12a5", 
    "uuid:b78ba864-d55b-459c-921d-17ea56a46e86", "uuid:8391ddc1-cf99-4fc7-906a-9d2b0017df74", 
    "uuid:0d42b7c4-7007-445f-91ae-021efdbcf71d", "uuid:5f3039fe-df2e-4d7a-84ca-b7013c7dde0b", 
    "uuid:e53c1d32-bfcd-4711-afec-867359ac0af9", "uuid:d9f13a7e-4dfc-4b66-b020-d67e5f22b8b1", 
    "uuid:5a1d8f9c-fa57-4c34-aaf9-cdc5294db852", "uuid:74dd66da-f5a9-447e-a91a-a97ff89a977a", 
    "uuid:416eeaaa-bcf1-4ff7-87d0-2ff66068fa40", "uuid:91abdba6-2b65-46aa-939b-feaf1551e5e5", 
    "uuid:33fd0756-00d2-4378-93b0-fbeccdf380a0", "uuid:a4103966-b7a0-4dcc-9bfe-b04f090deb6b", 
    "uuid:bd59db1c-76b5-478f-b505-ce2d42387872", "uuid:be19fb08-dd64-43cd-8650-233c1c648447", 
    "uuid:269b4d89-2fd7-44d9-b7e8-05807a5e1e1a", "uuid:2935fc09-7b28-445d-9d05-39f58d3107cf", 
    "uuid:22c5aaf5-d96c-4dda-b82f-bafc63f4c9fc", "uuid:e5480b19-8bfa-4f02-9056-d3cdc5669b64", 
    "uuid:7ce6dc32-3978-4808-9f25-561cdbeac310", "uuid:d17bfb41-db7b-4fe8-88af-ee16a78d5487", 
    "uuid:5876aad5-b711-48df-84d1-423d736ebdf8", "uuid:d5a66bd7-1ed3-411f-8ea4-25526b3f345a", 
    "uuid:4bd033e6-570e-43b9-bb8a-48ccf997bb71", "uuid:6bdaa21b-c75f-4eff-be11-4bca9021c1e0", 
    "uuid:0f44c6a4-da23-48a6-918d-cd9676d66985", "uuid:7da45821-265a-4fae-adbf-368af705504e", 
    "uuid:e64541f5-3c7a-4cec-821c-3c80325c6ae6", "uuid:77118f6a-1d17-45f2-9697-a51aa9dfe1a2"
    ), uid_asset_correct_name_match = c("do_not_match", "replace_error", 
    "replace_error", "uid_two_constituency_candidate", "uid_two_constituency_candidate", 
    "replace_error", "do_not_match", "replace_error", "replace_error", 
    "replace_error", "replace_error", "replace_error", "replace_error", 
    "do_not_match", "replace_error", "replace_error", "replace_error", 
    "replace_error", "replace_error", "replace_error", "replace_error", 
    "replace_error", "replace_error", "replace_error", "replace_error", 
    "replace_error", "replace_error", "replace_error", "do_not_match", 
    "replace_error", "replace_error", "uid_two_constituency_candidate", 
    "replace_error")), row.names = c(NA, -33L), class = "data.frame")

    # one fewer resolved, so no worries
    res_name_errs <- res_name_errs %>%
      left_join(name_match_asset_uid)

    #> Joining, by = "key"

    # Fix UIDs after name merging
    cleaned_assets <- cleaned_assets %>%
      left_join(
        res_name_errs %>%
          select(key, uid, uid_asset_correct_name_match) %>%
          rename(uid_name_asset = uid)
      ) %>%
      ungroup() %>%
      mutate(
        uid = case_when(
          uid_asset_correct_name_match == "replace_error" & !is.na(uid_asset_correct_name_match) ~ uid_name_asset,
          key == "uuid:614e74fc-ce76-40b8-b943-0d570e161106" ~ "02322",
          key == "uuid:55503095-747f-43e4-9905-4b8f3a2a091d" ~ "01698",
          TRUE ~ uid
        )
      )

    #> Joining, by = "key"

    # Now some old matches that were wrong were dropped, so redo matching on uid
    res_uid_matched <- res_u %>%
      select(id, constituency_code, candidate_name, candidate_party, candidate_rank, uid, found_initially) %>%
      left_join(
        cleaned_assets %>%
          filter(uid != "-9999" & uid != "-999" & !is.na(uid)) %>%
          mutate(asset_data = 1) %>%
          select(uid, key, asset_data)
      ) %>%
      mutate(missing_constituencies = constituency_code %in% missing_constituencies)

    #> Joining, by = "uid"

    res_unmatched_2 <- res_uid_matched %>%
      filter(is.na(asset_data)) %>%
      select(-asset_data, -key)

    new_uids <- setdiff(res_unmatched_2$uid, res_unmatched$uid)

    length(new_uids)

    #> [1] 22

    res_names_2 <- res_unmatched_2 %>%
      mutate(candidate_name_lower = gsub(" ", "", tolower(candidate_name))) %>%
      left_join(cleaned_assets %>%
                  ungroup() %>%
                  mutate(candidate_name_lower = gsub(" ", "", tolower(candidate_name)),
                         uid_asset = uid) %>%
                  select(key, constituency_code, candidate_name_lower, asset_data, uid_asset, entry),
                by = c("constituency_code", "candidate_name_lower"))

    if (nrow(res_names_2) != nrow(res_unmatched_2)) {
      stop("Some names duplicated in merge")
    }
    res_name_errs_2 <- res_names_2 %>%
      filter(uid_asset > 0) %>%
      select(key, constituency_code, candidate_name, starts_with("uid")) 

    res_name_errs_2 %>%
      as.data.frame

    #>                                         key constituency_code
    #> 1 uuid:b98d1a6d-f55f-4571-a160-8d552804819c             NA-46
    #> 2 uuid:9daba778-d30b-4945-a0cf-b32777a29f5d            NA-138
    #> 3 uuid:c60bd7ed-b9d2-4d74-83bd-16d33d7f12a5            NA-171
    #> 4 uuid:8391ddc1-cf99-4fc7-906a-9d2b0017df74            NA-216
    #> 5 uuid:6bdaa21b-c75f-4eff-be11-4bca9021c1e0            PP-166
    #> 6 uuid:e64541f5-3c7a-4cec-821c-3c80325c6ae6            PP-263
    #>         candidate_name   uid uid_asset
    #> 1        Jamil Hussain 02678     02679
    #> 2         SAIMA DAWOOD 00510     06945
    #> 3 Aleem Ullah Warraich 00915     00922
    #> 4       Muhammad Aslam 01488     01487
    #> 5       GHULAM MURTAZA 06621     06620
    #> 6   Syed Usman Mehmood 07985     07991

    # all of these we have already resolved
    res_name_errs %>%
      filter(uid_asset_correct_name_match != "replace_error")

    #>      id constituency_code       candidate_name
    #> 1   570             NA-46        Jamil Hussain
    #> 2  1726            NA-138         SAIMA DAWOOD
    #> 3  2081            NA-171 Aleem Ullah Warraich
    #> 4  2608            NA-216       Muhammad Aslam
    #> 5 10004            PP-166       GHULAM MURTAZA
    #> 6 11249            PP-263   Syed Usman Mehmood
    #>                           candidate_party candidate_rank   uid
    #> 1                             Independent             12 02678
    #> 2                             Independent             14 00510
    #> 3                             Independent              7 00915
    #> 4                             Independent             11 01488
    #> 5                             Independent             15 06621
    #> 6 Pakistan Peoples Party Parliamentarians              3 07985
    #>   found_initially missing_constituencies candidate_name_lower
    #> 1           FALSE                  FALSE         jamilhussain
    #> 2           FALSE                  FALSE          saimadawood
    #> 3           FALSE                  FALSE   aleemullahwarraich
    #> 4           FALSE                  FALSE        muhammadaslam
    #> 5           FALSE                  FALSE        ghulammurtaza
    #> 6           FALSE                  FALSE     syedusmanmehmood
    #>                                         key asset_data uid_asset entry
    #> 1 uuid:b98d1a6d-f55f-4571-a160-8d552804819c          1     02679   old
    #> 2 uuid:9daba778-d30b-4945-a0cf-b32777a29f5d          1     06945   old
    #> 3 uuid:c60bd7ed-b9d2-4d74-83bd-16d33d7f12a5          1     00922   old
    #> 4 uuid:8391ddc1-cf99-4fc7-906a-9d2b0017df74          1     01487   old
    #> 5 uuid:6bdaa21b-c75f-4eff-be11-4bca9021c1e0          1     06620   old
    #> 6 uuid:e64541f5-3c7a-4cec-821c-3c80325c6ae6          1     07991   old
    #>     uid_asset_correct_name_match
    #> 1                   do_not_match
    #> 2 uid_two_constituency_candidate
    #> 3 uid_two_constituency_candidate
    #> 4                   do_not_match
    #> 5                   do_not_match
    #> 6 uid_two_constituency_candidate

    # TODO drop those that are do_not_match from name merge

    res_unmatched_still <- res_names_2 %>%
      filter(is.na(asset_data) | id %in% res_name_errs$id[res_name_errs$uid_asset_correct_name_match == "do_not_match"]) %>%
      filter(!missing_constituencies)

    # Fuzzy name match
    matches <- map(setdiff(res_unmatched_still$constituency_code,
                           gsub("\\.csv", "", list.files("data/manual_fuzzy_match"))),
        ~ {
          print(sprintf("CONSTITUENCY: %s", .x))
          res_cons <- res_unmatched_still %>%
            filter(constituency_code == .x)
          ass_cons <- cleaned_assets %>%
            filter(constituency_code == .x) %>%
            select(father_name,constituency_code, key, candidate_name,  uid) %>%
            mutate(uid_fuzzy_match = NA)
          
          
          distmat <- stringdist::stringdistmatrix(
            tolower(gsub(" ", "", ass_cons$candidate_name)),
            res_cons$candidate_name_lower
          )
          
          
          
          out_dat <- map_dfr(seq_len(ncol(distmat)), ~ {
            ass_cons_i <- ass_cons %>%
              mutate(strdist = distmat[, .x]) %>%
              arrange(strdist) 
            
            print(res_cons[.x, c("candidate_name", "uid", "candidate_rank")])
            
            ass_cons_i %>%
              select(-key, constituency_code) %>%
              as.data.frame %>%
              print
            
            m <- readline("which_row? ")
            rowd <- data.frame(
                id = res_cons$id[.x],
                uid = res_cons$uid[.x],
                candidate_name = res_cons$candidate_name[.x],
                key = NA,
                matched_name = NA,
                stringsAsFactors = FALSE
              )
            if (m == "q") {
              stop("")
            } else if (m == "") {
              rowd$key <- NA
              rowd$matched_name <- NA
            } else {
              rowd$key <- ass_cons_i$key[as.integer(m)]
              rowd$matched_name <- ass_cons_i$candidate_name[as.integer(m)]
            }
            rowd
          })

          write_csv(
            out_dat,
            path = sprintf("data/manual_fuzzy_match/%s.csv", .x)
          )
        }
    )

    # GO THROUGH TXT provinces
    cccheck <- "PS-114"
    filter(res_u, constituency_code == cccheck) %>%
      select(id, constituency_code, uid, candidate_rank, candidate_name, found_initially) %>%
      arrange(uid) %>%
      as.data.frame

    #>      id constituency_code   uid candidate_rank          candidate_name
    #> 1  8874            PS-114 09822             10              Asif Rehan
    #> 2  8870            PS-114 09823              8          Ghulam Mustafa
    #> 3  8860            PS-114 09824              3 Mir Talib Hussain Brohi
    #> 4  8875            PS-114 09825             11      Muhammad Asad Khan
    #> 5  8878            PS-114 09826             14      Muhammad Asif Khan
    #> 6  8876            PS-114 09827             12          Muhammad Imran
    #> 7  8856            PS-114 09828              1        Muhammad Shabbir
    #> 8  8864            PS-114 09829              5          Muhammad Usman
    #> 9  8877            PS-114 09830             13          Muhmmad Rustam
    #> 10 8866            PS-114 09831              6         Qaiser Ali Butt
    #> 11 8862            PS-114 09832              4         Saeedullah Khan
    #> 12 8868            PS-114 09833              7        Syed Hafeezuddin
    #> 13 8858            PS-114 09834              2        Syed Shahid Mian
    #> 14 8872            PS-114 09835              9              Zafar Khan
    #>    found_initially
    #> 1             TRUE
    #> 2             TRUE
    #> 3            FALSE
    #> 4            FALSE
    #> 5             TRUE
    #> 6            FALSE
    #> 7            FALSE
    #> 8            FALSE
    #> 9            FALSE
    #> 10           FALSE
    #> 11           FALSE
    #> 12            TRUE
    #> 13           FALSE
    #> 14           FALSE

    filter(cleaned_assets, constituency_code == cccheck) %>%
      select(enum_name, constituency_code, uid, candidate_name, father_name, key) %>%
      arrange(uid) %>%
      as.data.frame

    #>    enum_name constituency_code   uid          candidate_name
    #> 1        152            PS-114 -9999       Sayed Shahid Mian
    #> 2        152            PS-114 -9999            Idrees Ahmed
    #> 3        152            PS-114 -9999               Ayaz Khan
    #> 4        152            PS-114 -9999          Qaisar Ali But
    #> 5        152            PS-114 -9999                   Walid
    #> 6        152            PS-114 -9999       Amdad Islam Amjad
    #> 7        152            PS-114 -9999  Irfan Ullah Khan Niazi
    #> 8        152            PS-114 -9999 Chudhari Muhammad Aslam
    #> 9        152            PS-114 -9999            Abdul Qayyum
    #> 10       152            PS-114 -9999         Muhammad Shabir
    #> 11       152            PS-114 -9999       Sher Rousham Khan
    #> 12       152            PS-114 -9999              Shahid Ali
    #> 13       152            PS-114 -9999          Muhammad Iqbal
    #> 14       152            PS-114 -9999                   Adnan
    #> 15       152            PS-114 -9999         Mehnaz Yar Khan
    #> 16       152            PS-114 -9999       Abdul Malang Khan
    #> 17       152            PS-114 -9999        Habeeb Ur Rehman
    #> 18       152            PS-114 -9999           Niaz Muhammad
    #> 19       152            PS-114 -9999              Riaz Ahmed
    #> 20       152            PS-114 -9999       Tariq Javaid Khan
    #> 21       152            PS-114 -9999         Muhammad Rustam
    #> 22       152            PS-114 -9999 Mer Talib Hussain Brohi
    #> 23       152            PS-114 -9999              Babar Khan
    #> 24       152            PS-114 -9999      Muhammad Asad Khan
    #> 25       152            PS-114 -9999                   Zafar
    #> 26       152            PS-114 -9999       Fayaz Qasim Khani
    #> 27       152            PS-114 -9999          Muhammad Imran
    #> 28       152            PS-114 -9999        Saeed Ullah Khan
    #> 29       152            PS-114 -9999    Saud Ur Rehman Abasi
    #> 30       152            PS-114 -9999          Muhammad Usman
    #> 31       152            PS-114 -9999         Muhammad Ashraf
    #> 32       152            PS-114 -9999        Azizan Ur Rehman
    #> 33       152            PS-114 09822              Asif Rehan
    #> 34       152            PS-114 09823           Ghulam Mustfa
    #> 35       152            PS-114 09826      Muhammad Asif Khan
    #> 36       152            PS-114 09833        Syed Hafeezuddin
    #>                    father_name                                       key
    #> 1             Sayed Achey Mian uuid:03bc9a07-8d51-4a6a-bc1e-efd95856eb1b
    #> 2                  Nisar Ahmed uuid:0bc8b44f-ce48-4b66-a66a-0f153192005e
    #> 3            Mean Hussain Shah uuid:100fdfe5-8bd4-48f5-a564-7e49d4182056
    #> 4          Muhammad Ishaq Butt uuid:1517a954-1399-4ca6-bdd2-f3691d1d9514
    #> 5             Mir Mula Bakhash uuid:26e2f70f-4144-417a-833c-989e96462a0d
    #> 6                   Abdulqudus uuid:3a3503c0-2da1-448d-8a5b-fd9dd60a3a74
    #> 7                   Wazir Khan uuid:4823daf0-651f-491e-8064-e64940697be8
    #> 8               Fateh Muhammad uuid:4a0a4775-3bc1-4aaa-88bf-87a91c15148a
    #> 9                Abdul Rasheed uuid:52df7235-178a-486a-83e0-baaa8faa8fb0
    #> 10            Muhammad Ibrahim uuid:62cc9a7d-5dfb-4540-8f8a-eda8c45ecab3
    #> 11         Muhammad Rafiq Khan uuid:66a23a8b-3bcc-4c0a-87ae-484e00cdf429
    #> 12              Shareef Hassan uuid:66a9714e-4d94-430b-a77a-1d9b66ffa787
    #> 13          Maqsood Ahmed Khan uuid:803316dc-6cfc-433c-9446-68d18355cf26
    #> 14               Jahanger Khan uuid:82602bd9-e79a-41c0-85c9-0fa5c1d02814
    #> 15           Mehmboob Yar Khan uuid:868debdf-a034-4668-9049-949b5479c684
    #> 16               Abdullah Khan uuid:8740de9b-37f6-4754-a891-b25534200253
    #> 17                Abdul Rehman uuid:8bd5d01b-cd3d-4a1d-9b8c-06faf3e9dae8
    #> 18               Wali Muhammad uuid:911da105-bdf5-44b4-9e22-d7fb4eeef325
    #> 19                Rajab Khusro uuid:95460fa6-173f-48c7-ad09-311b5d326db7
    #> 20          Abdul Ghafoor Khan uuid:b1580a76-46be-4bc4-a497-23a90cc9dcf3
    #> 21              Muhammad Hanif uuid:b38b276e-bc56-4b53-9366-c03a8af1ec4b
    #> 22 Sardar Mumtaz Hussain Brohi uuid:b54d3fa8-840d-493f-91a0-10ee9d9d59a4
    #> 23              Muhammad Sadiq uuid:bc88fd74-b14b-492f-9e40-5ccdbbdc7674
    #> 24          Muhammad Ayub Khan uuid:c9398727-cec2-48f4-8f36-9d4aaba39a8f
    #> 25             Muhammad Khalil uuid:cc8dd6cb-e93a-4edc-bce2-605b15217a6a
    #> 26              Niyaz Muhammad uuid:cec47705-af17-4073-8a43-54cc5bba5e1e
    #> 27              Anwar Ali Shah uuid:d4fb8b28-a40b-48b2-b6d2-d8cf788e7629
    #> 28            Aqal Khan Afridi uuid:d5273e5f-0b69-4736-9bd8-3e21ca531661
    #> 29                Abdul Majeed uuid:dc41f181-cfd2-49f0-a6b5-b5cea81bd9ce
    #> 30                Ghulam Yahya uuid:e63cf01c-24ec-4e0c-a9f6-fa6ec82a8ea5
    #> 31                Khaista Khan uuid:eb89cf40-cb3e-4146-9c04-ffe9358b277b
    #> 32                Zorawar Khan uuid:ef83a0d1-d224-4dbe-bffb-920cebcaa69e
    #> 33                  Gull Rehan uuid:d63a7856-4a6b-4760-bd79-193c6b4ef990
    #> 34               Khawar Bakhsh uuid:47fe5bff-189e-4680-a0f9-4d509ea00eed
    #> 35                Ghuncha Wari uuid:aa05cc45-5b7c-40b0-b7a3-ec46ee2b4fcb
    #> 36           Syed Qutub Ud Din uuid:02a204e5-5eb0-438e-823f-d2a22f6215d7

    read_csv(sprintf("data/manual_fuzzy_match/%s.csv", cccheck))

    #> Parsed with column specification:
    #> cols(
    #>   id = col_integer(),
    #>   uid = col_character(),
    #>   candidate_name = col_character(),
    #>   key = col_character(),
    #>   matched_name = col_character()
    #> )

    #> # A tibble: 6 x 5
    #>      id uid   candidate_name          key                 matched_name    
    #>   <int> <chr> <chr>                   <chr>               <chr>           
    #> 1  8856 09828 Muhammad Shabbir        uuid:62cc9a7d-5dfb… Muhammad Shabir 
    #> 2  8858 09834 Syed Shahid Mian        uuid:03bc9a07-8d51… Sayed Shahid Mi…
    #> 3  8860 09824 Mir Talib Hussain Brohi uuid:b54d3fa8-840d… Mer Talib Hussa…
    #> 4  8866 09831 Qaiser Ali Butt         uuid:1517a954-1399… Qaisar Ali But  
    #> 5  8872 09835 Zafar Khan              uuid:cc8dd6cb-e93a… Zafar           
    #> 6  8877 09830 Muhmmad Rustam          uuid:b38b276e-bc56… Muhammad Rustam

    # filter(res_u, constituency_code %in% c("PK-64", "PK-63")) %>%
    #   select(id, constituency_code, uid, candidate_rank, candidate_name, candidate_party, found_initially) %>%
    #   arrange(uid) %>%
    #   as.data.frame
    # filter(cleaned_assets, constituency_code %in% c("PK-64", "PK-63")) %>%
    #   select(enum_name, constituency_code, uid, candidate_name, father_name, key) %>%
    #   arrange(uid) %>%
    #   as.data.frame
    # 
    # filter(res_u, uid %in% c("28390", "08891", "08392", "08391") ) %>%
    #   select(id, constituency_code, uid, candidate_rank, candidate_name, candidate_party, found_initially) %>%
    #   arrange(uid) %>%
    #   as.data.frame
    # 
    # filter(cleaned_assets,uid %in% c("28390", "08891", "08392", "08391")) %>%
    #   select(enum_name, constituency_code, uid, candidate_name, father_name, key) %>%
    #   arrange(uid) %>%
    #   as.data.frame
    # Kiran Aleem Khan in NA-129 is son of Abdul Aleem Khan, who got second in NA-129
    # Balash Ahmad Junejo in NA-201 is son of Khurshee Ahmad Junejo, who got first
    # id 2598 had name change, should have uid 01491, done in file
    # id 2633 had name change, should have uid 01550, done in file
    # several in NA223, 231
    # leave mhd arshad 246 as is

    # uid 00984 should have NA-179 in assets instead of NA-180
    # uid 02112 should have NA-259 in assets instead of NA-246
    # TODO check numbers make sense within constituency
    # TODO look for name matches across consitutency (imran khan)

    uid_dupes <- cleaned_assets %>%
      filter(!is.na(uid), uid > 0) %>%
      group_by(uid) %>%
      summarize(n = n())
    table(uid_dupes$n)

    #> 
    #>    1 
    #> 9075

    table(duplicated(res_u$id))

    #> 
    #> FALSE 
    #> 11697

    found_uid_data <- res_uid_matched %>%
      filter(asset_data == 1) %>%
      select(id, key) %>%
      mutate(match = "uid")
      
    found_cons_names_data <- res_unmatched_2 %>%
      mutate(candidate_name_lower = gsub(" ", "", tolower(candidate_name))) %>%
      left_join(cleaned_assets %>%
                  ungroup() %>%
                  filter(uid < 0 | is.na(uid)) %>%
                  mutate(candidate_name_lower = gsub(" ", "", tolower(candidate_name)),
                         uid_asset = uid) %>%
                  select(key, constituency_code, candidate_name_lower, asset_data, uid_asset, entry),
                by = c("constituency_code", "candidate_name_lower")) %>%
      filter(asset_data == 1) %>%
      select(id, key) %>%
      mutate(match = "cons_name")

    found_fuzzy_data <- map_dfr(
      list.files("data/manual_fuzzy_match", full.names = TRUE),
      ~ read_csv(file = .x, col_types = cols(uid = col_character()))
    ) %>%
      filter(!is.na(key)) %>%
      select(id, key) %>%
      mutate(match = "fuzzy")

    found_key <- bind_rows(
      found_uid_data,
      found_cons_names_data,
      found_fuzzy_data
    )

    dup_keys <- found_key %>%
      group_by(key) %>%
      mutate(n_key = n()) %>%
      ungroup() %>%
      group_by(id) %>%
      mutate(n_id = n()) %>%
      filter(n_id > 1 | n_key > 1) %>%
      arrange(key)
    dup_keys

    #> # A tibble: 0 x 5
    #> # Groups:   id [0]
    #> # ... with 5 variables: id <int>, key <chr>, match <chr>, n_key <int>,
    #> #   n_id <int>

    table(duplicated(found_key$id))

    #> 
    #> FALSE 
    #> 10207

    table(duplicated(found_key$key))

    #> 
    #> FALSE 
    #> 10207

    table(duplicated(found_uid_data$id))

    #> 
    #> FALSE 
    #>  9040

    table(duplicated(found_uid_data$key))

    #> 
    #> FALSE 
    #>  9040

    table(duplicated(found_cons_names_data$id))

    #> 
    #> FALSE 
    #>   347

    table(duplicated(found_cons_names_data$key))

    #> 
    #> FALSE 
    #>   347

    table(duplicated(found_fuzzy_data$id))

    #> 
    #> FALSE 
    #>   820

    table(duplicated(found_fuzzy_data$key))

    #> 
    #> FALSE 
    #>   820

    res_missing_names <- res_u %>%
      filter(!(id %in% found_key$id)) %>%
      group_by(candidate_name) %>%
      summarize(n = n(), 
                avg_rank = mean(candidate_rank),
                best_rank = min(candidate_rank)) %>%
      filter(n > 1) %>%
      arrange(best_rank, n)
    res_missing_names %>% as.data.frame

    #>                    candidate_name n  avg_rank best_rank
    #> 1               Abdul Majeed Khan 2  2.000000         1
    #> 2                  Jam Kamal Khan 2  1.500000         1
    #> 3    Mian Muhammad Shahbaz Sharif 2  1.500000         1
    #> 4                    Naseer Ahmed 2 17.500000         1
    #> 5          Sardar Sanaullah Zehri 2  3.000000         1
    #> 6          Imran Ahmed Khan Niazi 3  1.000000         1
    #> 7                   Muhammad Arif 3  7.333333         1
    #> 8                 Muhammad Hashim 3  7.333333         1
    #> 9        Sardar Yar Muhammad Rind 3  4.333333         1
    #> 10                      Asadullah 4  6.000000         1
    #> 11                 Muhammad Qasim 4  7.250000         1
    #> 12               Abdul Aleem Khan 2  2.000000         2
    #> 13                   Ahmed Bakhsh 2  2.000000         2
    #> 14                     Aman Ullah 2  3.500000         2
    #> 15         Mir Muhammad Asim Kurd 2  8.000000         2
    #> 16                Muhammad Shahid 2  5.000000         2
    #> 17                      Nasrullah 2 13.000000         2
    #> 18                     Abdul Bari 3  3.333333         2
    #> 19                   Abdul Rehman 4 16.750000         2
    #> 20                  Ghulam Rasool 5  9.400000         2
    #> 21                   Abdul Hafeez 2  9.500000         3
    #> 22                   ASHFAQ AHMAD 2  3.500000         3
    #> 23               Attaullah Baloch 2  5.000000         3
    #> 24               Waqar Ahmad Khan 2  5.500000         3
    #> 25                   Abdul Majeed 3  3.666667         3
    #> 26                   Zulfiqar Ali 3  6.666667         3
    #> 27                      Attaullah 2  5.000000         4
    #> 28                Fazal ur Rehman 2 10.000000         4
    #> 29                   Ghulam Abbas 2  6.500000         4
    #> 30                     Gul Hassan 2  5.000000         4
    #> 31                     Liaqat Ali 2 11.000000         4
    #> 32            Mian Muhammad Aslam 2  4.000000         4
    #> 33                Muhammad Rashid 2 10.000000         4
    #> 34                   Abdul Jabbar 3  9.000000         4
    #> 35                    Abdul Majid 3  9.666667         4
    #> 36                 Ghulam Murtaza 3 15.000000         4
    #> 37                Muhammad Ashraf 3  8.000000         4
    #> 38                 Muhammad Aslam 3  7.666667         4
    #> 39                 Muhammad Ishaq 3  7.333333         4
    #> 40                  Ghulam Farooq 4  9.250000         4
    #> 41                  Manzoor Ahmed 5 12.800000         4
    #> 42                Akhtar Muhammad 2 14.500000         5
    #> 43                   Allah Bakhsh 2  7.000000         5
    #> 44                       Mir Khan 2 13.000000         5
    #> 45                     Murad Khan 2  7.500000         5
    #> 46                    Najeebullah 2 17.000000         5
    #> 47                    Nizam Uddin 2  9.000000         5
    #> 48     Sardar Muhammad Nawaz Khan 2  8.500000         5
    #> 49                    Abdul Qadir 4  8.500000         5
    #> 50                    Abdul Wahid 2 17.000000         6
    #> 51                 Atta ur Rehman 2 11.500000         6
    #> 52                  Khan Muhammad 2  6.500000         6
    #> 53                 Mohammad Akram 2  7.500000         6
    #> 54                   Rehmat Ullah 2 13.500000         6
    #> 55                Sheikh Abdullah 2 11.000000         6
    #> 56                Muhammad Farooq 3 13.333333         6
    #> 57                  Muhammad Asif 4 13.250000         6
    #> 58                Muhammad Saleem 5 14.400000         6
    #> 59                     Ahmed Raza 2 16.500000         7
    #> 60                      Ali Ahmed 2 10.500000         7
    #> 61                  Altaf Hussain 2 17.000000         7
    #> 62                  Jamil Hussain 2  9.500000         7
    #> 63                  Sher Muhammad 2 13.000000         7
    #> 64        Syed Imdad Hussain Shah 2  9.000000         7
    #> 65                    Allah Ditta 2  9.000000         8
    #> 66                      Mir Ahmed 2 11.000000         8
    #> 67                 Muhammad Anwar 2  9.000000         8
    #> 68                Muhammad Sarwar 2  9.000000         8
    #> 69                 Saddam Hussain 2  8.000000         8
    #> 70                     Ali Hassan 3 11.333333         8
    #> 71        Sardar Muhammad Hussain 3 10.666667         8
    #> 72                   Abdul Waheed 4 11.000000         8
    #> 73               Abdul Qadir Luni 2 10.500000         9
    #> 74                     Habibullah 2 19.500000         9
    #> 75                     Nizamuddin 2 11.500000         9
    #> 76                   Riaz Hussain 2  9.500000         9
    #> 77                   Shams Ul Haq 2  9.500000         9
    #> 78                     Khuda Rahm 2 11.000000        10
    #> 79                    Liaquat Ali 2 10.500000        10
    #> 80       Mir Muhammad Hashim Khan 2 17.500000        10
    #> 81                Muhammad Malook 2 12.500000        10
    #> 82                  Shakeel Ahmed 2 22.500000        10
    #> 83                   Abdul Sattar 3 13.666667        10
    #> 84                   Farooq Ahmed 2 14.000000        11
    #> 85         Muhammad Ali Raza Khar 2 12.500000        11
    #> 86                Muhammad Luqman 2 21.000000        11
    #> 87                   Yar Muhammad 2 13.500000        11
    #> 88                      Abdul Haq 2 13.000000        12
    #> 89                  Abdullah Khan 2 14.500000        12
    #> 90                     Ahmed Khan 2 19.000000        12
    #> 91                 Muhammad Akram 2 13.000000        12
    #> 92       Sardar Abdul Sattar Khan 2 13.000000        12
    #> 93           Mir Sardar Khan Rind 3 18.333333        12
    #> 94                   Taj Muhammad 3 19.000000        12
    #> 95                  Abdul Rasheed 2 13.000000        13
    #> 96  Mir Rehmat Ullah Khan Notezai 2 16.500000        13
    #> 97           Muhammad Kamran Khan 2 16.500000        13
    #> 98                    Sain Bakhsh 2 21.000000        13
    #> 99            Ghulam Mujtaba Abro 3 15.333333        13
    #> 100                  Moula Bakhsh 2 16.000000        14
    #> 101                 Ghulam Haider 2 17.500000        16
    #> 102                Syed Sanaullah 2 20.500000        16
    #> 103                   Nisar Ahmed 3 17.333333        16
    #> 104                 Abdul Ghaffar 2 17.000000        17
    #> 105                     Amanullah 2 21.000000        18
    #> 106               Muhammad Raheem 2 22.500000        18
    #> 107                    Sheraz Ali 2 20.000000        18
    #> 108                   Javed Iqbal 2 23.000000        19
    #> 109             Wazir Khan Mengal 2 21.000000        19
    #> 110                          <NA> 7        NA        NA

    filter(res_u, candidate_name == "Sardar Sanaullah Zehri")

    #> # A tibble: 2 x 24
    #>      id constituency_co… candidate_name uid   candidate_rank election_date
    #>   <int> <chr>            <chr>          <chr>          <int> <date>       
    #> 1  3338 NA-267           Sardar Sanaul… 02273              5 2018-07-25   
    #> 2  5556 PB-38            Sardar Sanaul… 04043              1 2018-07-25   
    #> # ... with 18 more variables: election_type <chr>, contest_status <chr>,
    #> #   assembly <chr>, province <chr>, constituency_number <int>,
    #> #   constituency_name <chr>, voter_reg <int>, votes_polled <int>,
    #> #   turnout <dbl>, turnout_calc <dbl>, valid_votes <int>,
    #> #   valid_calc <int>, votes_disq <int>, candidate_party <chr>,
    #> #   candidate_votes <int>, candidate_share <dbl>, outcome <chr>,
    #> #   found_initially <lgl>

    found_key %>%
      filter(id %in% res_u$id[res_u$candidate_name == "Sardar Sanaullah Zehri"])

    #> # A tibble: 0 x 3
    #> # ... with 3 variables: id <int>, key <chr>, match <chr>

    filter(res_u, candidate_name == "")

    #> # A tibble: 0 x 24
    #> # ... with 24 variables: id <int>, constituency_code <chr>,
    #> #   candidate_name <chr>, uid <chr>, candidate_rank <int>,
    #> #   election_date <date>, election_type <chr>, contest_status <chr>,
    #> #   assembly <chr>, province <chr>, constituency_number <int>,
    #> #   constituency_name <chr>, voter_reg <int>, votes_polled <int>,
    #> #   turnout <dbl>, turnout_calc <dbl>, valid_votes <int>,
    #> #   valid_calc <int>, votes_disq <int>, candidate_party <chr>,
    #> #   candidate_votes <int>, candidate_share <dbl>, outcome <chr>,
    #> #   found_initially <lgl>

    multiple_constituencies <- bind_rows(
      data_frame( # Mian Muhammad Shahbaz Sharif
        id = c(23, 9973, 1632),
        key = "uuid:317d65de-b7d7-423a-856e-93e051b09f2a"
      ),
      data_frame( # Imran Ahmed Khan Niazi, select first row with more complete data
        id = c(731, 1225, 1617),
        key = "uuid:b9e1b37a-a0fb-4251-8685-3877b009b07d"
      )
    )


    all_keys <- bind_rows(
      found_key,
      multiple_constituencies
    )

    table(duplicated(all_keys$id))

    #> 
    #> FALSE 
    #> 10213

    res_total <- left_join(res_u, all_keys) %>%
      mutate(asset_found = !is.na(key))

    #> Joining, by = "id"

    with(res_total, table(asset_found, candidate_rank))

    #>            candidate_rank
    #> asset_found   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
    #>       FALSE  79  91  88  86  87  81  79  80  80  77  71  77  68  58  49
    #>       TRUE  761 749 752 754 749 744 722 681 637 586 528 452 397 336 285
    #>            candidate_rank
    #> asset_found  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30
    #>       FALSE  46  40  41  32  30  23  21  17  13  11  11   9   6   7   3
    #>       TRUE  223 184 143 114  83  83  56  43  35  24  19  13  11   8  10
    #>            candidate_rank
    #> asset_found  31  32  33  34  35  36  37  38
    #>       FALSE   3   3   4   1   4   0   0   0
    #>       TRUE    6   4   6   6   2   3   2   1

    # unmatched keys distmatrix winners
    unmatched_winners <- res_total %>%
      filter(!asset_found & candidate_rank == 2)

    cleaned_assets$matched <- cleaned_assets$key %in% all_keys$key

    str_dist <- stringdist::stringdistmatrix(
      gsub(" ", "", tolower(unmatched_winners$candidate_name)),
      gsub(" ", "", tolower(cleaned_assets$candidate_name))
    )
    dim(str_dist)

    #> [1]    91 17927

    # 
    # out_dat <- map_dfr(seq_len(nrow(str_dist)), ~ {
    #   if (unmatched_winners$id[.x] %in% 
    #         gsub("\\.csv", "", list.files("data/manual_rank_match")) |
    #       unmatched_winners$id[.x] %in%
    #       gsub("\\.csv", "", list.files("data/manual_rank_unmatch"))
    #       ) {
    #     return(NULL)
    #   }
    #   cleaned_assets_i <- cleaned_assets %>%
    #     mutate(strdist = str_dist[.x, ]) %>%
    #     arrange(strdist) %>%
    #     filter(strdist < 4)
    #   
    #   
    #   print(unmatched_winners[.x, c("id", "constituency_code", "candidate_name", "uid", "candidate_rank")])
    #   
    #   cleaned_assets_i %>%
    #     select(matched, candidate_name, strdist, constituency_code, father_name, uid) %>%
    #     as.data.frame %>%
    #     print
    #   
    #   m <- readline("which_row? ")
    #   rowd <- data.frame(
    #       id = unmatched_winners$id[.x],
    #       uid = unmatched_winners$uid[.x],
    #       candidate_name = unmatched_winners$candidate_name[.x],
    #       key = NA,
    #       matched_name = NA,
    #       stringsAsFactors = FALSE
    #     )
    #   if (m == "q") {
    #     stop("")
    #   } else if (m == "") {
    #     rowd$key <- NA
    #     rowd$matched_name <- NA
    #     write_csv(
    #       rowd,
    #       path = sprintf("data/manual_rank_unmatch/%s.csv", unmatched_winners$id[.x])
    #     )
    #   } else {
    #     rowd$key <- cleaned_assets_i$key[as.integer(m)]
    #     rowd$matched_name <- cleaned_assets_i$candidate_name[as.integer(m)]
    #     write_csv(
    #       rowd,
    #       path = sprintf("data/manual_rank_match/%s.csv", unmatched_winners$id[.x])
    #     )
    #   }
    # 
    # })

    other_string_matches <- map_dfr(
      list.files("data/manual_rank_match", full.names = TRUE),
      ~ read_csv(file = .x, col_types = cols(uid = col_character()))
    ) %>%
      filter(!is.na(key)) %>%
      select(id, key) %>%
      mutate(match = "fuzzy_rank")

    matched_keys <- bind_rows(
      all_keys,
      other_string_matches
    )

    write_csv(left_join(res_u, matched_keys), "data/pk18_results_with_matched_keys.csv")

    #> Joining, by = "id"

    write_csv(cleaned_assets, "analysis/data_cleaning/01_assets.csv")
