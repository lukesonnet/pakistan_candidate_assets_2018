TODO CNIC and UID backchecks we discuseed with AT - back check on ~50
CNICs that don't match - back check on ~20 missing CNICs

TODO for UIDs, start with final cand uid list and see how many are
missing the asset data

First things first, relative to the previous data which had 17398, we
now have a total of 17265 observations.

#### 1. CNICs

The latest dataset gives us 780 cnic flags:

-   Length error: The length of the entered cnic != 13 (and the cnic!= 0
    or 888888888888 (length 12) )
-   Missing value - 0: Missing value entered as 0
-   Missing value - 9: Missing value correctly entered as 9999999999999
-   Missing value - 8: Missing value correctly entered as 888888888888
    (length 12)
-   value Available: Value is not missing and does not have a length
    error - correctly entered

As shown in the table below, we have 16485 values that appear to be
correctly entered

    #> 
    #> FALSE  TRUE 
    #> 16485   780

    #> 
    #> Length error - 10 or 12       Missing value - 0       Missing value - 9 
    #>                      12                      12                     755 
    #>        Missing value -8         Value available 
    #>                       1                   16485

    #> 
    #> FALSE  TRUE 
    #>  4067 13198

One additional check we can do is to match the list of cnics we have
with the original scrutiny list:

    table(cnic_flags$cnic_empty_category, cnic_flags$cnic_in_scrutiny)

    #>                          
    #>                           FALSE  TRUE
    #>   Length error - 10 or 12    12     0
    #>   Missing value - 0          12     0
    #>   Missing value - 9         755     0
    #>   Missing value -8            1     0
    #>   Value available          3287 13198

We see that of the 16485 correctly entered CNICs, 3287 are not in the
scrutiny list. This could be due to 2 reasons:

1.  The CNIC was incorrectly read and entered from the form
2.  The candidate details are not in the scrutiny list

What might be helpful would be to have AT backcheck a percentage of
these to ascertain which case it may be, as 1 is more concerning than 2.

With the current data, we have 13198 confirmed correctly entered CNICs
~76 percent of our total observations.

#### 2. Missing UIDs

Of the total 17265 observations, we now have uids for 8745 candidates.
The UID is missing (-9999) for 8517 candidates.And 3 are not matched
(likely due to errors which we'll check).

    #> 
    #>     Matched     Missing Not Matched 
    #>        8745        8517           3

    #> # A tibble: 3 x 7
    #>   type_seat const_number candidate_name  enum_name cnic  uid   uid_missing
    #>   <chr>     <dbl+lbl>    <chr>           <dbl+lbl> <chr> <chr> <chr>      
    #> 1 PP        51           Malik Zabeeh U… 123       3410… 98773 Not Matched
    #> 2 PP        290          Atifali Daresh… 105       3210… 28390 Not Matched
    #> 3 PS        24           Muneeb Ahmad    146       4550… 21035 Not Matched

The unmatched uids are probably due to some error since they're outside
the range of the uids we have. I am going to try and see if there was in
error in typing the **first** number to catch the correct version in
candidate list possibly.

    final_cand_uids%>%
      filter(as.numeric(uid) %in% c(18773, 8773, 18390, 8390, 11035, 1035))

    #> # A tibble: 4 x 3
    #>   pa_id  candidate_name   uid  
    #>   <chr>  <chr>            <chr>
    #> 1 NA-182 Muhammad Hussain 01035
    #> 2 PP-29  Tanveer Ahmad    08390
    #> 3 PP-51  Malik Zabi Ullah 08773
    #> 4 PS-63  Javed Rajput     11035

Right then, found an error, Malik Zabeeh Ullah correct uid: 08773.
Couldn't catch the other two errors hmm.

**NEW:** Another thing we can do is go the reverse way; see how many of
candidates from the candidate uid list are in our asset data. So far one
plausible explanation is that candidates who submitted asset forms (such
as Perveez Musaraf) but did not end up running might be the source of
the uids. However we still don't expect ~8000 missing uids, there can't
be that many. We expect no more than 5605 "-9999" UIDs.

    select(final_assets, 1:4, candidate_name, uid) %>% head(20)

    #> # A tibble: 20 x 6
    #>    enum_name enum_other type_seat const_number candidate_name        uid  
    #>    <dbl+lbl> <chr>      <chr>     <dbl+lbl>    <chr>                 <chr>
    #>  1 118       ""         NA        1            Abdul Qayum           -9999
    #>  2 118       ""         NA        1            Hidyat Ul Rehman      -9999
    #>  3 118       ""         NA        1            Hizbullah             -9999
    #>  4 118       ""         NA        1            Perveez Musaraf       -9999
    #>  5 118       ""         NA        1            Shah Abdul Mansoor    -9999
    #>  6 118       ""         NA        1            Sultan Wazeer         -9999
    #>  7 118       ""         NA        1            Wajih U Deen          -9999
    #>  8 118       ""         NA        1            Abdul Latif           00001
    #>  9 118       ""         NA        1            Eid Ul Hussain        00002
    #> 10 118       ""         NA        1            Ifthkhar U Deen       00003
    #> 11 118       ""         NA        1            Molana Abdul Chitrali 00004
    #> 12 118       ""         NA        1            Muhammad Amjad        00005
    #> 13 118       ""         NA        1            Muhammad Yahya Khan   00006
    #> 14 118       ""         NA        1            Nisar Dastgeer        00007
    #> 15 118       ""         NA        1            Shyed Ul Rehman       00008
    #> 16 118       ""         NA        1            Saleem Khan           00009
    #> 17 118       ""         NA        1            Shezada Muhamad Taim… 00010
    #> 18 118       ""         NA        1            Taqdira Ajmal         00011
    #> 19 118       ""         NA        2            Altaf Ullah           -9999
    #> 20 118       ""         NA        2            Bakht Zada            -9999

    final_cand_uids %>% head(20)

    #> # A tibble: 20 x 3
    #>    pa_id candidate_name                   uid  
    #>    <chr> <chr>                            <chr>
    #>  1 NA-1  Abdul Latif                      00001
    #>  2 NA-1  Eid Ul Hussain                   00002
    #>  3 NA-1  Iftikhar Ud Din                  00003
    #>  4 NA-1  Moulana Abdul Akbar Chitrali     00004
    #>  5 NA-1  Muhammad Amjad                   00005
    #>  6 NA-1  Muhammad Yahya                   00006
    #>  7 NA-1  Nisar Dastageer                  00007
    #>  8 NA-1  Saeed ur Rehman                  00008
    #>  9 NA-1  Saleem Khan                      00009
    #> 10 NA-1  Shahzada Muhammad Taimur Khisrao 00010
    #> 11 NA-1  Taqdira Ajmal                    00011
    #> 12 NA-10 Amir Sultan                      00012
    #> 13 NA-10 Bakht Nasib Khan                 00013
    #> 14 NA-10 Hamid Iqbal Khan                 00014
    #> 15 NA-10 Ibadullah Khan                   00015
    #> 16 NA-10 Praviz Ahmad                     00016
    #> 17 NA-10 Sadeed ur Rahman                 00017
    #> 18 NA-10 Safeer Khan                      00018
    #> 19 NA-10 Said Fareen                      00019
    #> 20 NA-10 Waqar Ahmad Khan                 00020

    nrow(final_assets)

    #> [1] 17265

    nrow(final_cand_uids)

    #> [1] 11660

    final_cand_uids%>%
      filter(as.numeric(uid)==-9999)

    #> # A tibble: 0 x 3
    #> # ... with 3 variables: pa_id <chr>, candidate_name <chr>, uid <chr>

    final_cand_uids%>%
      mutate(uid_match=case_when(
        !(as.numeric(uid) %in% as.numeric(final_assets$uid)) ~ "Not in Final Assets",
        (as.numeric(uid) %in% as.numeric(final_assets$uid)) ~ "In Final Assets"
        )
      )%>%
      {with(.,table(uid_match))}

    #> uid_match
    #>     In Final Assets Not in Final Assets 
    #>                8286                3374

It appears that from the final candidate uid list, we only found matches
for 8286 uids and did not find matches for 3374 uids. This means that
3374 candidate uids are either missing from the asset data OR that the
candidates are present but with a -9999.

Additionally, if we add the gap between nrows(final\_assets) and
nrows(final\_cand\_uids) with the uids not in final assets, we get 8979
candidates (our final asset data has missing 8517 candidate uids). We
can further investigate those not in final assets:

    final_cand_uids%>%
      mutate(uid_match=case_when(
        !(as.numeric(uid) %in% as.numeric(final_assets$uid)) ~ "Not in Final Assets",
        (as.numeric(uid) %in% as.numeric(final_assets$uid)) ~ "In Final Assets"
        )
      )%>%
      filter(uid_match=="Not in Final Assets")%>%
      #{with(.,table(pa_id))}
      filter(pa_id=="NA-129")

    #> # A tibble: 11 x 4
    #>    pa_id  candidate_name            uid   uid_match          
    #>    <chr>  <chr>                     <chr> <chr>              
    #>  1 NA-129 Abdul Aleem Khan          00368 Not in Final Assets
    #>  2 NA-129 Ali Javed Dogar           00369 Not in Final Assets
    #>  3 NA-129 Farooq Tahir Chishti      00370 Not in Final Assets
    #>  4 NA-129 Farzana Butt              00371 Not in Final Assets
    #>  5 NA-129 Humayun Akhtar Khan       00372 Not in Final Assets
    #>  6 NA-129 Iftikhar Shahid           00373 Not in Final Assets
    #>  7 NA-129 Kishwar Bano              00374 Not in Final Assets
    #>  8 NA-129 Muhammad tajammal hussain 00375 Not in Final Assets
    #>  9 NA-129 Muhammad Yousuf           00376 Not in Final Assets
    #> 10 NA-129 Sardar Ayaz Sadiq         00377 Not in Final Assets
    #> 11 NA-129 Sohail Shaukat Butt       00378 Not in Final Assets

    #lets check out NA-129


      final_assets%>%
        filter(type_seat=="NA", const_number==129)%>%
        select(type_seat, const_number, candidate_name, uid)

    #> # A tibble: 18 x 4
    #>    type_seat const_number candidate_name       uid  
    #>    <chr>     <dbl+lbl>    <chr>                <chr>
    #>  1 NA        129          Khwaja Saad Rafique  -9999
    #>  2 NA        129          Kiran Aleem Khan     -9999
    #>  3 NA        129          M Shoaib Siddiqui    -9999
    #>  4 NA        129          Nadeem Ahmad         -9999
    #>  5 NA        129          Narjis Fatima        -9999
    #>  6 NA        129          Syeda Irm Noreen     -9999
    #>  7 NA        129          Walid Iqbal          -9999
    #>  8 NA        129          Ali Javed Dogar      01370
    #>  9 NA        129          Farooq Tahir Chishti 01371
    #> 10 NA        129          Frzana Butt          01372
    #> 11 NA        129          Humayun Akhtar Khan  01373
    #> 12 NA        129          Iftikhar Shahid Adv  01374
    #> 13 NA        129          Kishwar Bano         01375
    #> 14 NA        129          M Tajjamul Hussain   01376
    #> 15 NA        129          M Yousaf             01377
    #> 16 NA        129          Sardar Ayaz Siddiqui 01378
    #> 17 NA        129          Sohail Shoukat Butt  01379
    #> 18 NA        129          M Jahngeer Dogar     01411

Okay this is pointing to a bigger problem. 10/ 11 candidates in the
final\_cand\_uid list of NA-129 that were flagged as having their uids
not in the final\_assets data, are present in the final\_assets data
**but** instead of being given (-9999), they've been given **different
uids** that are likely to be typos in data entry. Let me try and flesh
this out a bit further:

    #PART 1: first checking the uids given to candidates of NA-129 in final_assets
    final_cand_uids%>%
      filter(as.numeric(uid) %in% c(01370:01379))

    #> # A tibble: 10 x 3
    #>    pa_id  candidate_name                   uid  
    #>    <chr>  <chr>                            <chr>
    #>  1 NA-208 Syed Ghous Ali Shah              01370
    #>  2 NA-208 Syed Nawaz Ali Shah Alies Farukh 01371
    #>  3 NA-208 Syed Pervez Ali Shah Jeelani     01372
    #>  4 NA-208 Syed Qaim Ali Shah               01373
    #>  5 NA-208 Veero Mal                        01374
    #>  6 NA-209 Abdul Haq                        01375
    #>  7 NA-209 Ghulam Ali                       01376
    #>  8 NA-209 Ghulam Mustafah                  01377
    #>  9 NA-209 Ismail Shah                      01378
    #> 10 NA-209 Munwar Ali Wassan                01379

    final_assets%>%
      filter(as.numeric(uid) %in% c(01370:01379))%>%
      select(type_seat, const_number, candidate_name, uid)

    #> # A tibble: 19 x 4
    #>    type_seat const_number candidate_name       uid  
    #>    <chr>     <dbl+lbl>    <chr>                <chr>
    #>  1 NA        129          Ali Javed Dogar      01370
    #>  2 NA        129          Farooq Tahir Chishti 01371
    #>  3 NA        129          Frzana Butt          01372
    #>  4 NA        129          Humayun Akhtar Khan  01373
    #>  5 NA        129          Iftikhar Shahid Adv  01374
    #>  6 NA        129          Kishwar Bano         01375
    #>  7 NA        129          M Tajjamul Hussain   01376
    #>  8 NA        129          M Yousaf             01377
    #>  9 NA        129          Sardar Ayaz Siddiqui 01378
    #> 10 NA        129          Sohail Shoukat Butt  01379
    #> 11 NA        208          Syed Nawaz Ali       01371
    #> 12 NA        208          Syed Parvaiz         01372
    #> 13 NA        208          Syed Qaim Ali Shah   01373
    #> 14 NA        208          Veeru Maal           01374
    #> 15 PS        209          Abdul Haq            01375
    #> 16 PS        209          Ghulam Ali           01376
    #> 17 PS        209          Ghulam Mustafa       01377
    #> 18 PS        209          Ismail Shah          01378
    #> 19 PS        209          Munwar Ali           01379

     #PART 2: first checking the uids given to candidates of NA-129 in final_cand_uids
    final_cand_uids%>%
      filter(as.numeric(uid) %in% c(00368:00378))

    #> # A tibble: 11 x 3
    #>    pa_id  candidate_name            uid  
    #>    <chr>  <chr>                     <chr>
    #>  1 NA-129 Abdul Aleem Khan          00368
    #>  2 NA-129 Ali Javed Dogar           00369
    #>  3 NA-129 Farooq Tahir Chishti      00370
    #>  4 NA-129 Farzana Butt              00371
    #>  5 NA-129 Humayun Akhtar Khan       00372
    #>  6 NA-129 Iftikhar Shahid           00373
    #>  7 NA-129 Kishwar Bano              00374
    #>  8 NA-129 Muhammad tajammal hussain 00375
    #>  9 NA-129 Muhammad Yousuf           00376
    #> 10 NA-129 Sardar Ayaz Sadiq         00377
    #> 11 NA-129 Sohail Shaukat Butt       00378

    final_assets%>%
      filter(as.numeric(uid) %in% c(00368:00378))%>%
      select(type_seat, const_number, candidate_name, uid)

    #> # A tibble: 0 x 4
    #> # ... with 4 variables: type_seat <chr>, const_number <dbl+lbl>,
    #> #   candidate_name <chr>, uid <chr>

Ho dear. Basically, the same uid has been assigned to two different
people in two different constituencies. And the uids the NA-129
candidates **should have** been given in the final\_assets data, they
weren't assigned to anyone.

Basically this tells me the uids are no longer trustworthy.

Let me generate a csv with the duplicate

    duplicated_by_uid<- final_assets%>%
      mutate(dup_by_uid=duplicated(final_assets["uid"], fromLast = FALSE))%>%
      filter(dup_by_uid & uid!=-9999)%>%
      arrange(as.numeric(uid))%>%
      select(type_seat, const_number, candidate_name, uid, cnic)

    duplicated_by_uid%>%
      as.data.frame()

    #>     type_seat const_number                      candidate_name   uid
    #> 1          PP          246                     Amad Ullah Awan 00014
    #> 2          NA          101                     Farman Ali Khan 00034
    #> 3          NA          101                Muhammad Azeem Aslam 00040
    #> 4          PP          246                     Amad Ullah Awan 00041
    #> 5          PP          246                     Amad Ullha Awan 00041
    #> 6          PP          246                  Amadeus Ullah Awam 00041
    #> 7          PP          246                      Amadullah Awan 00041
    #> 8          PP          246                       Amadullahawan 00041
    #> 9          PS           67                       Muhammad Asif 00101
    #> 10         PS           67                     Muhammad Furqan 00165
    #> 11         NA          169                          Abrar Azam 00169
    #> 12         PS           67                   Abdul Sattar Khan 00174
    #> 13         NA          120                 Rana Afzaal Hussain 00250
    #> 14         PS           49                     Muhammad Yousaf 00256
    #> 15         NA          121                          Liaqat Ali 00275
    #> 16         PS           91                       Muhammad Syed 00279
    #> 17         NA          128                      Raheel Hussain 00326
    #> 18         NA          138                              Salman 00494
    #> 19         NA          138                                Asif 00495
    #> 20         NA          138                 Dawood Anis Qureshi 00499
    #> 21         NA          167           Mian Mumtaz Ahmad Matyana 00569
    #> 22         PP          144                       Noor Ul Ameen 00593
    #> 23         PS           91                          Sarder Ali 00595
    #> 24         NA          146                        Waseem Zafar 00620
    #> 25         NA          157              Sayed Ali Moosa Gilani 00740
    #> 26         PS           91                         Ameerahmend 00750
    #> 27         NA          161               Nawab Amanullaha Khan 00791
    #> 28         PS          120                         Abdul Hamid 00954
    #> 29         PS           55                        Zulfiqar Ali 00972
    #> 30         NA          180                     Muhammad Arshad 01009
    #> 31         NA          182                      Muhammad Naeem 01036
    #> 32         NA          182                         Rana Mustiq 01042
    #> 33         PS           91                             M Bilal 01104
    #> 34         NA          189        Sardar Ameer Badshah Qasrani 01123
    #> 35         NA          191                        Abdul Rehman 01141
    #> 36         NA          191                Dr Mian Abdul Rehman 01141
    #> 37         NA          191                      Parveen Akthar 01149
    #> 38         PP           61                           Ali Ahmad 01161
    #> 39         NA          195                 Shamsher Ali Mazari 01189
    #> 40         PS           67                         Shoaib Khan 01217
    #> 41         PP            2                       Naveed Zamurd 01247
    #> 42         NA          203                       Faiz Muhammad 01294
    #> 43         PP           61                       Faiz Muhammad 01294
    #> 44         PS           53                         Abdul Haque 01304
    #> 45         PS           59                      Abdul Razzaque 01315
    #> 46         PS          207                        Hazoor Buksh 01347
    #> 47         NA          208                     Syed Asad Abbas 01369
    #> 48         NA          208                      Syed Nawaz Ali 01371
    #> 49         NA          208                        Syed Parvaiz 01372
    #> 50         NA          208                  Syed Qaim Ali Shah 01373
    #> 51         NA          208                          Veeru Maal 01374
    #> 52         PS          209                           Abdul Haq 01375
    #> 53         PS          209                          Ghulam Ali 01376
    #> 54         PS          209                      Ghulam Mustafa 01377
    #> 55         PS          209                         Ismail Shah 01378
    #> 56         PS          209                          Munwar Ali 01379
    #> 57         PS          210                        Inayat Ullah 01390
    #> 58         PS          210                            M.Ramzan 01394
    #> 59         PS          210                    Mukhtiar Hussain 01395
    #> 60         PS          210                        Naimat Ullah 01396
    #> 61         PS          210                      Syed Fahad Ali 01397
    #> 62         PS          210                Syed Javeed Ali Shah 01398
    #> 63         PS          210                   Syed Muharram Ali 01400
    #> 64         PS          211                 Abdul Ghaffar Alias 01401
    #> 65         PS          211                        Abdul Sattar 01402
    #> 66         PS          211                    Allalahando Shah 01403
    #> 67         PS          211                           Ameer Bux 01404
    #> 68         PS          211                          Asghar Ali 01405
    #> 69         PS          211                     Ghazala Hussain 01406
    #> 70         PS          211                       Khalid Masood 01408
    #> 71         PS          211                         Maqsood Ali 01409
    #> 72         PS          211                      Mehmooda Begum 01410
    #> 73         PS          213                   M.Ikhtiar Hussain 01441
    #> 74         PS          213                         M.Qadir Bux 01446
    #> 75         PS          215                      Mumtaz Hussain 01475
    #> 76         PS           54                          Abdul Aziz 01493
    #> 77         PS           48                Ghulam Jaffar Junejo 01510
    #> 78         PS           48       Pir Aftab Hussain Shah Jelani 01516
    #> 79         PS           48                          Roop Chand 01519
    #> 80         PS           48                      Ali Nawaz Shah 01522
    #> 81         NA           22                         Shoaib Khan 01537
    #> 82         NA          220               Nawab M Yousaf Talpur 01543
    #> 83         PS           54                        Dilawar Khan 01546
    #> 84         NA          222                            M Ramzan 01553
    #> 85         PS           55                         Anwer Jabar 01553
    #> 86         PS           57                         Arbab Anwer 01553
    #> 87         PS           56                 Arbab Togachi Fawad 01556
    #> 88         PS           56                     Arbab Zakaullah 01557
    #> 89         PS           57                      Kangaroo Kumar 01560
    #> 90         PS           58                      Gul Sher Sario 01567
    #> 91         PS           58              Makhdoom Fazal Hussain 01568
    #> 92         PS           58             Makhdoom Jamil Uz Zaman 01569
    #> 93         PS           58                Makhdoom Shahzad Ali 01570
    #> 94         PS           63                       Altaf Hussain 01578
    #> 95         PP           61             Pir Ghulam Namibia Shah 01586
    #> 96         PS           49                      Aki Nawaz Shah 01588
    #> 97         PS           63                  Khawand Bux Ghulam 01595
    #> 98         NA          237              Syed Zahid Abbas Naqvi 01722
    #> 99         NA          240                   Syed Asif Hasnain 01786
    #> 100        NA          243           Muhammad Muzammil Qureshi 01828
    #> 101        NA          244         Mian Vaqar Ahmad Pagaanwala 01830
    #> 102        NA          245                      M Shafiq Paper 01866
    #> 103        NA          246                 Abdul Shakoor Shaad 01872
    #> 104        NA          248                           M. Salman 01919
    #> 105        PP          259                        Abdul Gaffar 02086
    #> 106        PS           52                            Allahdad 02089
    #> 107        NA          261                    Muhammad Hussain 02155
    #> 108        NA          263                     Busmillah Kakar 02184
    #> 109        PS           57                          Arbab Amir 02365
    #> 110        NA           41                        Salah Ud Din 02550
    #> 111        NA          226                         Ushair Khan 02611
    #> 112        NA          264                     Ghulam Muhammad 02703
    #> 113        NA           55                     Malik Mir Afzal 02913
    #> 114        NA           71                   Muhammad Ilyas Ch 03088
    #> 115        NA           71                 Chaudhary Abid Raza 03089
    #> 116        NA           71          Syed Mubashar Hussain Shah 03089
    #> 117        NA           79             Muhammad Fayyaz Chattha 03184
    #> 118        NA           90                           Jamal Din 03330
    #> 119        NA           91                   Hafiz Talha Saeed 03345
    #> 120        NA           95                         Ameer Ullah 03382
    #> 121        NA           97                 Farrukh Hassan Khan 03402
    #> 122        NA           98                       Ejaz Ali Khan 03412
    #> 123        NA           98                      Muhammad Amjad 03415
    #> 124        PB            1                       Hameed Ahamad 03433
    #> 125        PB            1                          Jalil Khan 03435
    #> 126        PS           54                          Habibullah 03514
    #> 127        PS           63                          Habibullah 03514
    #> 128        PS           59                            Ali Sher 03524
    #> 129        PB           24                   Abdul Naeem Nasir 03715
    #> 130        PB           26                         Barkat Khan 03760
    #> 131        PB           26                        Wilayat Khan 03777
    #> 132        PS           58                       Wali Muhammad 03778
    #> 133        PS           54                             Ibrahim 03804
    #> 134        PB           30                                Essa 03909
    #> 135        PS           50                 Mir Zafarullah Khan 04002
    #> 136        PB           47                              Bushra 04191
    #> 137        PS           59               Syed Taimoor Ali Shah 04200
    #> 138        PS           57                           Nawab Ali 04222
    #> 139        PB            5                            M Rasool 04244
    #> 140        PB            5                             M Zahid 04245
    #> 141        PK           10                        Abdul Gaffar 04351
    #> 142        PK            1                Abdul Wali Khan Abid 04375
    #> 143        PK           10                        Muhammad Ali 04393
    #> 144        PS           50                        Sajjad Ahmad 04440
    #> 145        PS           53                           Gul Zaman 04475
    #> 146        PK           20                     Bakhtjehan Khan 04491
    #> 147        PS           67                        Rashid Ahmad 04518
    #> 148        PK           27                       Naseer Ud Din 04558
    #> 149        PS           48                          Javaid Ali 04589
    #> 150        PK           38                 Malik Naveed Sarwar 04604
    #> 151        PK           41                      Farzana Ramzan 04661
    #> 152        PK           42                   Malik Adeel Iqbal 04772
    #> 153        PK           47                        Shehram Khan 04829
    #> 154        PK           55                         Nadeem Khan 04912
    #> 155        PK           56                          Maza Ullah 04921
    #> 156        PP          105                      Muhammad Akram 05010
    #> 157        PK           66                     Saif Ullah Khan 05039
    #> 158        PS           51                        Abdul Raheem 05134
    #> 159        PK           74                      Atif Ur Rehman 05136
    #> 160        PK           76                         Arshad Khan 05160
    #> 161        PK           89                         Shah M.Khan 05324
    #> 162        PK           93                            Ali Khan 05404
    #> 163        PS           67                    Muhammad Dilawar 05436
    #> 164        PP          100                Falik Sheer Sikandar 05532
    #> 165        PP          101                    Rai Ijaz Hussain 05555
    #> 166        PP          101                 Rai Haider Ali Khan 05556
    #> 167        PP          102                    Mujeeb Ul Rehman 05583
    #> 168        PP          105                         Zafar Iqbal 05612
    #> 169        PS           91                      Zunaira Rehman 05652
    #> 170        PS           91                          Abdulsitar 05653
    #> 171        PS           91                            Ahsan Ul 05654
    #> 172        PS           91                           Asif Khan 05655
    #> 173        PS           91             Haji Muzafar Ali Shujra 05656
    #> 174        PS           91                        Haroon Rasid 05657
    #> 175        PS           91                               Kamal 05658
    #> 176        PS           91                       Khursheed Ali 05660
    #> 177        PP          107                              Shukat 05661
    #> 178        PS           91                Muhammad Alam Jamood 05661
    #> 179        PS           91                  Malik Muhammad Taj 05662
    #> 180        PS           91                Muhhmad Aslam Bhutto 05663
    #> 181        PS           91                   Nazir Ahmed Bhuto 05663
    #> 182        PS           91                     Muhammad Haneef 05664
    #> 183        PS           91                          Mumtaz Ali 05666
    #> 184        PS           91                     Raheem Dad Khan 05668
    #> 185        PP          108                     Muhammad Masood 05669
    #> 186        PS           91                         Rasual Khan 05669
    #> 187        PS           91                           Samiuaahl 05670
    #> 188        PS           91                       Sher Ali Khan 05671
    #> 189        PS           91                         Siraj Ahmed 05672
    #> 190        PS           91             Syed Imdad Hussain Shah 05673
    #> 191        PS           92                         Abduljilani 05675
    #> 192        PS           92                            Aqibkhan 05676
    #> 193        PS           92                        Atif Mansoor 05678
    #> 194        PS           92                    Barket Ul Shtiqi 05679
    #> 195        PS           92                         Moiz Shazad 05680
    #> 196        PS           92                Muhammad Asreef Khan 05682
    #> 197        PS           92                     Muhammad Farooq 05683
    #> 198        PS           92               Muhammad Hussain Khan 05684
    #> 199        PS           92                     Muhammad Kashif 05685
    #> 200        PP          110                        Khayal Ahmad 05697
    #> 201        PS           93                         Karim Buksh 05697
    #> 202        PS           93                  Rana Sajid Mehmood 05701
    #> 203        PS           93                   Sanaullah Qureshi 05703
    #> 204        PP          110                     Muhammad Shakil 05704
    #> 205        PS           91                      Muhmmad Saleem 05704
    #> 206        PS           93                            Shanawaz 05704
    #> 207        PS           93              Shakeel Uddin Siddiqui 05706
    #> 208        PS           93              Syed Zahid Zafar Ahmed 05707
    #> 209        PS           93                      Syed Adnan Ali 05708
    #> 210        PS           94                     Aurghenzab Khan 05713
    #> 211        PS           92                         Fareed Ullh 05714
    #> 212        PS           94                   Gul E Rana Azahar 05715
    #> 213        PS           94                   Javeed Ali Sheikh 05717
    #> 214        PS           94                         Javid Iqbal 05718
    #> 215        PS           99                      Mohammad Hanif 05718
    #> 216        PS           94                 Marizq Farheen Bheg 05719
    #> 217        PS           94                      Muhammad Ahmad 05720
    #> 218        PS           94       Muhammad Aslam Pervaiz Abbasi 05721
    #> 219        PS           94                Muhhmad Irfan Waheed 05722
    #> 220        PS           94                    Muhammad Rafuiqe 05723
    #> 221        PS           94               Muhammad Saleeem Khan 05724
    #> 222        PS           94           Muhammad Shoiab Ur Rehman 05725
    #> 223        PS           94                     Raees Ur Rehman 05728
    #> 224        PS           94                        Shamim Ahmed 05729
    #> 225        PS           94                         Uzma Farooq 05730
    #> 226        PS           91                               Bachu 05734
    #> 227        PS           95                               Bachu 05734
    #> 228        PS           95           Muhammad Javed Hanif Khan 05737
    #> 229        PS           95            Muhammad Tanveer Qurashi 05740
    #> 230        PS           95                      Muhammad Tariq 05741
    #> 231        PS           95                          Raiz Ahmad 05743
    #> 232        PS           95                        Sajid Hasnan 05744
    #> 233        PS           95                        Sharaz Wahid 05745
    #> 234        PS           95                   Saeed Adnan Hasan 05746
    #> 235        PS           96                        Asad Mujahid 05749
    #> 236        PS           96                  Mohammad Abu Bakar 05751
    #> 237        PS           96                        Mohammad Ali 05752
    #> 238        PS           96                     Mohammad Kamran 05754
    #> 239        PS           96                    Mohammad Wajahad 05756
    #> 240        PS           96                       Sajid Husnain 05758
    #> 241        PP          114                Sajjad Haider Cheema 05759
    #> 242        PS           96              Sheikh Mohammad Saleem 05759
    #> 243        PS           96                    Syed Mehmood Ali 05761
    #> 244        PS           95                               -9999 05762
    #> 245        PS           97                    Abdul Azeem Khan 05765
    #> 246        PS           97                        Abdul Hafeez 05765
    #> 247        PS           97                     Hazar Khan Abro 05766
    #> 248        PS           97                         Imran Baghi 05767
    #> 249        PS           97                       Maqbool Ahmed 05769
    #> 250        PS           97                Mian Mohammad Illyas 05770
    #> 251        PS           97                       Mohamad Ahmad 05771
    #> 252        PS           97                      Mohammad Naeem 05773
    #> 253        PS           97                     Mohammad Rizwan 05774
    #> 254        PS           97                       Saad Saddiqui 05775
    #> 255        PS           97                      Shakeel Ahmend 05779
    #> 256        PS           97                 Shyeed Gayas Ahmend 05780
    #> 257        PS           97                   Waqar Hussan Shah 05781
    #> 258        PS           98                           Abdul Haq 05782
    #> 259        PS           98                           Andul Haq 05782
    #> 260        PS           98                    Abdul Haq Usmani 05783
    #> 261        PS           98                            Asif Ali 05785
    #> 262        PS           98                         Bhari Kamal 05786
    #> 263        PS           98                       Fahmida Zahir 05787
    #> 264        PS           98                           Fazal Ali 05788
    #> 265        PS           98                        Kaleemuallah 05789
    #> 266        PS           98                          Kareem Bux 05790
    #> 267        PS           98                      Masood Mahmood 05791
    #> 268        PS           98                        Mehtab Ahmed 05792
    #> 269        PS           98                       M Haroon Khan 05793
    #> 270        PS           98                      Mohammad Iqbal 05794
    #> 271        PP          117                       M Zafer Iqbal 05796
    #> 272        PS           98                Mohammad Waqar Azeem 05796
    #> 273        PS           98                       Rehan Mansoor 05797
    #> 274        PS           98                          Roshan Ali 05798
    #> 275        PS           98                Syed Mohammad Zahoor 05799
    #> 276        PS           98                  Umar Ahmed Sadique 05800
    #> 277        PS           98                         Yasir Uddin 05801
    #> 278        PP          117                        Sajaid Numan 05802
    #> 279        PS           98                         Zafar Iqbal 05802
    #> 280        PS           99                  Ali Mohammad Gabol 05803
    #> 281        PS           98                          Deedar Ali 05806
    #> 282        PS           99                              Diibar 05806
    #> 283        PS           99                 Jalil Ahmad Mugheri 05808
    #> 284        PS           99                        Jan Muhammad 05809
    #> 285        PS           99                      Manzoor Burfat 05810
    #> 286        PS           99                    Mohi Uddin Ahmed 05811
    #> 287        PS           99                      Mohammad Akram 05812
    #> 288        PS           99                Mohammad Aslam Bhuto 05814
    #> 289        PS           99                Mohammad Hammad Khan 05816
    #> 290        PS           99                Mohammad Hammad Khan 05816
    #> 291        PS           99                    Mohammad Hussain 05818
    #> 292        PS           99                     Mohammad Ismail 05819
    #> 293        PS           99                 Mohammad Jamil Gill 05820
    #> 294        PS           99                     Mohammad Marjan 05821
    #> 295        PS           99                     Mohammad Ramzan 05822
    #> 296        PS           99                 Mohammad Zahid Awan 05823
    #> 297        PS           99                    Mohammad Farrukh 05824
    #> 298        PS           99                    Umar Uddin Zafat 05825
    #> 299        PS           99              Raees Anwar Ullah Khan 05826
    #> 300        PS           99                           Sati Khan 05828
    #> 301        PS           59                         Saeed Ahmad 05859
    #> 302        PS           67                            Ali Raza 05865
    #> 303        PS           63                        Abdul Jabbar 05928
    #> 304        PP          127                   Khuram Abbas Sial 05949
    #> 305        PP          131                       Shazia Cheema 06012
    #> 306        PP          133                Atta Ul Rehman Azher 06036
    #> 307        PP          134                Rana Shaheen Ikhlaak 06060
    #> 308        PP          135                 Muhammad Umar Afzal 06078
    #> 309        PS           67                        Waseem Ahmad 06160
    #> 310        PS           50                       Mukhtar Ahmad 06178
    #> 311        PP          142               Muhammad Kashif Miraj 06219
    #> 312  Reserved           NA                          Asad Ullha 06229
    #> 313        PP          143                     Muhammad Saleem 06247
    #> 314        PP          143                  Sarfraz Ahmad Khan 06256
    #> 315        PP          146                       Hamza Shahbaz 06294
    #> 316        PP          153                 Robina Solehri Noor 06304
    #> 317        PP          148                             Khurram 06311
    #> 318        PP           15              Malik Abdul Karim Khan 06338
    #> 319        PP           15                M. Javed Iqbal Malik 06339
    #> 320        PP           15                         Tahir Iqbal 06342
    #> 321        PP           15                       Tahir Mehmood 06343
    #> 322        PP           15                         Umer Tanvir 06344
    #> 323        PP          150                  Ch Muhammad Asghar 06347
    #> 324        PP          150                 Waqas Mansha Prince 06349
    #> 325        PP          151                             M.Fasal 06367
    #> 326        PP          152                        Hasan Irshad 06374
    #> 327        PP          153                      Shahzad Arshad 06404
    #> 328        PP          157               Kjawaja Salma Rafique 06470
    #> 329        PP          158                    Abdul Aleem Khan 06480
    #> 330        PP          158                    Kiran Aleem Khan 06480
    #> 331        PS           64                    Abdul Aleem Khan 06480
    #> 332        PP          159              Muhammad Mumtaz Khalid 06504
    #> 333        PP          164                     Ghulam Muhammad 06584
    #> 334        PS           56                          Abdul Krim 06616
    #> 335        PP          174                        Abdul Hannan 06777
    #> 336        PP          175                    Malik Ahmad Syed 06806
    #> 337        PP          182                               Amjad 06929
    #> 338        PP          182                            M.Illyas 06937
    #> 339        PP          199                    Syed Nazir Ahmad 07185
    #> 340        PP          200                       Waheed Asghar 07212
    #> 341        PP          200                       Waheed Asghar 07212
    #> 342        PP          202          Muhammad Daood Urf Alqasim 07222
    #> 343        PP          212               Malik Sikandar Hayyat 07340
    #> 344        PS           55                        Lal Muhammad 07465
    #> 345        PS           63                           Asif Raza 07468
    #> 346        PP          224                 Muhammad Iqbal Shah 07489
    #> 347        PP          228                 Syed M. Rafi Ud Din 07529
    #> 348        PS           55                          Imran Khan 07578
    #> 349        PP          255 Makhdoom Syed Muhammad Masood Aslam 07865
    #> 350        PS           55                         Sardar Khan 07891
    #> 351  Reserved           NA                         Sofia Sadiq 07925
    #> 352        PP          260                         Junaid Riaz 07943
    #> 353        PS           50                         Nzeer Ahmad 07965
    #> 354        PP          266                    Khalil Ur Rehman 08013
    #> 355        PP          270              Muhammad Javed Hussain 08092
    #> 356        PP          270                      Mumtaz Hussain 08099
    #> 357        PP          270                        Mumtaz Saeed 08099
    #> 358        PS           63                            Atif Ali 08130
    #> 359        PP          273                  Muhammad Jahangeer 08135
    #> 360        PP          276            Muhammad Aon Hamid Dogar 08172
    #> 361        PP          285                        Shazad Hanif 08322
    #> 362        PP          286                        Abdul Hafeez 08325
    #> 363        PP          288                   Mohsan Atta Khosa 08352
    #> 364        PP          289                      Shaheena Karim 08386
    #> 365        PP          291                        Gulam Sarwer 08402
    #> 366        PP          294   Sardar Pervez Iqbal Khan Gorchani 08433
    #> 367        PP          294                 Sher Zaman Gorchani 08436
    #> 368        PP           34                          Naeem Raza 08551
    #> 369        PP          105                    Muhammad Hussain 08613
    #> 370        PP          105                         Munir Ahmed 08616
    #> 371        PS           63                      Ghulam Mustafa 08631
    #> 372        PS           53                              Bushra 08745
    #> 373        PP           53                       Rizwan Yousaf 08803
    #> 374        PP           55                           Asim Khan 08838
    #> 375        PP           56                    Muhammas Sadique 08859
    #> 376        PS           53                               Salma 08922
    #> 377        PP           59                 Sohail Zafar Cheema 08924
    #> 378        PP           65                         Mirza Ikram 09020
    #> 379        PP           67                         Sajjid Khan 09050
    #> 380        PP           70                       Fakkhar Abbas 09098
    #> 381        PP           76                 Sardar Kamil Shamim 09167
    #> 382        PP           77                        M Imran Baig 09184
    #> 383        PP           77                        Zulfiqar Ali 09194
    #> 384        PP           86                     Muhammad Khalid 09304
    #> 385        PS           50                            Rukhsana 09317
    #> 386        PP           89                     Muhammad Arshad 09327
    #> 387        PP           90                       Ejaz Ali Khan 09348
    #> 388        PP           91                     Muhammad Ramzan 09359
    #> 389        PS           67                     Muhammad Haroon 09406
    #> 390        PS            1                               Abbas 09430
    #> 391        PS            1                 Muhammad Akram Abro 09487
    #> 392        PS          108                  Syed Abdul Rasheed 09696
    #> 393        PS           11                         Moazzam Ali 09727
    #> 394        PS          110                  Syed Farhan Askari 09752
    #> 395        PS          111                       Muhammad Asif 09763
    #> 396        PS          112                       Jamshaid Alam 09788
    #> 397        PS          112                      Mubarak Baloch 09791
    #> 398        PS          113                        Sultan Ahmad 09820
    #> 399        PS          117        Syed Muhammad Saleem Ashrafi 09887
    #> 400        PS          118                      Amanullah Khan 09902
    #> 401        PS          120                     Muhammad Ismail 09964
    #> 402        PS          121                           Saqib Ali 09991
    #> 403        NA          187                  Malik Allah Bakhsh 09999
    #> 404        NA          188                   Ali Hassan Jhakar 09999
    #> 405        NA          188                        Naveed Iqbal 09999
    #> 406        PP           70                         Asad Ullaha 09999
    #> 407        PP           70                       Dibag Jaffari 09999
    #> 408        PP           70                     Fiaz Ahmad Awan 09999
    #> 409        PP           70                          Gull Nawaz 09999
    #> 410        PP          280                    M. Zia Ul Hassan 09999
    #> 411        PP          280             Muhammad Duryab Mahwish 09999
    #> 412        PP          280                          Sobia Inam 09999
    #> 413        PS           51                  Haji Muhamad Ilyas 09999
    #> 414        PS           67                       Babar Chandio 09999
    #> 415        PS           67               Muhammad Hassan Elahi 09999
    #> 416        PS           67               Muhammad Umar Qureshi 09999
    #> 417        PS           67                   Qadeer Ahmed Khan 09999
    #> 418        PS           67                 Qazi Muhammad Ilyas 09999
    #> 419        PS          122                      Ghani Ul Fahad 09999
    #> 420        PS          124             Khawaja Izhar Ul Hassan 10042
    #> 421        PS          125           Syed Mohammad Abbas Jafri 10074
    #> 422        PS          128                       M Arif Sheikh 10124
    #> 423        PS           13                     Muhammad Mohsin 10165
    #> 424        PS          130                       Rehan Mansoor 10180
    #> 425        PS           63                           Abdul Rub 10269
    #> 426        PS           59                         Sarang Khan 10278
    #> 427        PS            2                     Zohaib Zulfiqar 10294
    #> 428        PS           51                           Ali Murad 10298
    #> 429        PS           23                    Syed Awais Qadir 10342
    #> 430        PS           28                      Ghulam Mustafa 10375
    #> 431        PS           25             Syed Nasir Hussain Shah 10391
    #> 432        PS           26                        Bishar Ahmad 10401
    #> 433        PS           26                             Shahnan 10412
    #> 434        PS           26                            Shahnwaz 10415
    #> 435        PS           28                       Asad Ali Shar 10431
    #> 436        PS           29                   Sajid Ali Banbhan 10443
    #> 437        PS           30                           Shahnawaz 10480
    #> 438        PS           51                      Ghulam Murtaza 10511
    #> 439        PS           35                        Zulfiqar Ali 10574
    #> 440        PS           40                           Nasrullah 10668
    #> 441        PS           53                             Ali Bux 10671
    #> 442        PS           67                       Raza Muhammad 10715
    #> 443        PS           48                     Syed Imtiaz Ali 10793
    #> 444        PS           49                Syed Imtiaz Ali Shah 10793
    #> 445        PS            5                    Ahsan Ali Mirani 10824
    #> 446        PS           57                        Arbab Ghulam 10834
    #> 447        PS           53                        Muhammad Jam 10890
    #> 448        PS           55                           Ali Akber 10908
    #> 449        PS           59              Syed Jalal Shah Jomate 10978
    #> 450        PS           59                   Syed Mir Ali Shah 10981
    #> 451        PS           60                    Habib Ullah Palh 10988
    #> 452        PS           62                     Habibur Rehaman 11020
    #> 453        PS           62                            Jam Khan 11020
    #> 454        PS           65                     Muhammad Farhan 11077
    #> 455        PS           68             Muhammad Altaf Nizamani 11138
    #> 456        PS           69              Syed Zulfiqar Ali Shah 11155
    #> 457        PS            7                       Tanveer Ahmed 11167
    #> 458        PS           70               Ghulam Sarwar Leghari 11170
    #> 459        PS           71                      Ghulam Mustafa 11184
    #>              cnic
    #> 1   3120284749857
    #> 2   3310092607361
    #> 3   3310443610075
    #> 4   3120284749857
    #> 5   3120284749857
    #> 6   3120284749857
    #> 7   3120284545555
    #> 8   3120284749857
    #> 9   4130310880773
    #> 10  4130225685079
    #> 11  3110385472859
    #> 12  4420298881481
    #> 13  3540168744679
    #> 14  4130664531267
    #> 15  3540403938467
    #> 16  4250178317185
    #> 17  3520180925675
    #> 18  3510258703477
    #> 19  3520218372130
    #> 20  3520221522343
    #> 21  3110283813905
    #> 22  3530181678807
    #> 23  1730184756427
    #> 24  3520272169519
    #> 25  3520126954701
    #> 26  4250152730333
    #> 27  3620242048821
    #> 28  4240127097801
    #> 29  4410703163689
    #> 30  3130747360417
    #> 31  3230415516999
    #> 32  3230450595887
    #> 33  4250141315313
    #> 34  3210323975735
    #> 35  3210286640313
    #> 36  3210286640313
    #> 37  3210243408290
    #> 38  3520267882385
    #> 39  3520184166637
    #> 40  8888888888888
    #> 41  3740268066422
    #> 42  4230198503801
    #> 43  4130710713457
    #> 44  4410498381949
    #> 45  4130557129437
    #> 46  4550411092212
    #> 47  4520378562105
    #> 48  4520308205261
    #> 49  4230192688975
    #> 50  4230384633667
    #> 51  4520318481615
    #> 52  4520643564923
    #> 53  4520697733639
    #> 54  4520674472307
    #> 55  4230193361273
    #> 56  4520396405839
    #> 57  4520415440319
    #> 58  4520203166847
    #> 59  4520266097671
    #> 60  4520415439963
    #> 61  4520351240345
    #> 62  6110152471235
    #> 63  4230112546117
    #> 64  4530278910227
    #> 65  4230106601065
    #> 66  4230107906385
    #> 67  8888888888888
    #> 68  4530221811559
    #> 69  4530262946774
    #> 70  4540269427893
    #> 71  4220129168135
    #> 72  4550466404330
    #> 73  4530207757901
    #> 74  4130614392399
    #> 75  4420383219817
    #> 76  4430176536891
    #> 77  4420218088419
    #> 78  8130238079213
    #> 79  4410319997809
    #> 80  4200082408213
    #> 81  1610234576395
    #> 82  8888888888888
    #> 83  4430126133313
    #> 84  4430123630887
    #> 85  4430283137293
    #> 86  4430283137293
    #> 87  4430218770411
    #> 88  4430277907301
    #> 89  4430391449923
    #> 90  4130587141963
    #> 91  4130168696387
    #> 92  4230107181431
    #> 93  4130335132769
    #> 94  4130320150969
    #> 95  4130398021727
    #> 96  4200082488213
    #> 97  4130315142423
    #> 98  4220189135463
    #> 99  4210141604663
    #> 100 4220104854095
    #> 101 6110169809055
    #> 102 4220126630445
    #> 103 4230124743733
    #> 104 4230148403587
    #> 105 4220126221911
    #> 106 4410746920319
    #> 107 5540314106631
    #> 108 9999999999999
    #> 109 4430218659051
    #> 110 2110499532243
    #> 111 4130410753195
    #> 112 5440090351209
    #> 113 3710116857051
    #> 114 3520258859543
    #> 115 3420241959731
    #> 116 3420312887539
    #> 117 3520014017353
    #> 118 3840321181271
    #> 119 3520228341335
    #> 120 3830118713275
    #> 121 3818313025101
    #> 122 3810131382251
    #> 123 9999999999999
    #> 124 5640183364547
    #> 125 5630414313527
    #> 126 4430134566283
    #> 127 4130353395825
    #> 128 4130538491067
    #> 129 5430320440065
    #> 130 5440027685693
    #> 131 5440005597055
    #> 132 6110157436703
    #> 133 4430135916125
    #> 134 5440005378917
    #> 135 4410101580343
    #> 136 5220338016944
    #> 137 4220125040031
    #> 138 4410156082903
    #> 139 5630140692387
    #> 140 8888888888888
    #> 141 1570116989403
    #> 142 1520106965539
    #> 143 1570368084493
    #> 144 4410118719301
    #> 145 4410434407717
    #> 146 1510132690077
    #> 147 4130480810277
    #> 148 1340259052413
    #> 149 4410303009359
    #> 150 1310101815133
    #> 151 9999999999999
    #> 152 1330108517617
    #> 153 1620205400595
    #> 154 1610230934357
    #> 155 1710216476241
    #> 156 1730134300809
    #> 157 1730101673683
    #> 158 4410738847253
    #> 159 1730191716499
    #> 160 1730135078501
    #> 161 1110195963127
    #> 162 1120103554353
    #> 163 4130315233951
    #> 164 9999999999999
    #> 165 3310404721113
    #> 166 3310446365177
    #> 167 3310403885785
    #> 168 3310365398843
    #> 169 4220193858536
    #> 170 4250113612475
    #> 171 4250116123203
    #> 172 8888888888888
    #> 173 4250115266845
    #> 174 4220175502589
    #> 175 4220144483309
    #> 176 4250114626353
    #> 177 3310006034667
    #> 178 4250144847877
    #> 179 4250198589319
    #> 180 4220107800325
    #> 181 4310405671445
    #> 182 4250115155661
    #> 183 8888888888888
    #> 184 4250122196989
    #> 185 3310038191959
    #> 186 4250177146255
    #> 187 4250165147085
    #> 188 1320283941593
    #> 189 4250121953849
    #> 190 4250118899983
    #> 191 4200006867573
    #> 192 4220145287663
    #> 193 4220102244719
    #> 194 4220198753839
    #> 195 4220138240865
    #> 196 4220155562915
    #> 197 4220174500529
    #> 198 4220186405719
    #> 199 4220171315059
    #> 200 3310218168771
    #> 201 4220140039627
    #> 202 4220196115921
    #> 203 3410118480487
    #> 204 3310010250605
    #> 205 4250115645979
    #> 206 4250115900749
    #> 207 4250187827261
    #> 208 4220196861423
    #> 209 9999999999999
    #> 210 4220196583775
    #> 211 1540106957665
    #> 212 4200003602584
    #> 213 4130214208375
    #> 214 4250191261827
    #> 215 4250128062177
    #> 216 4220166926833
    #> 217 4220113841557
    #> 218 4220123299707
    #> 219 4220103994371
    #> 220 4220170345899
    #> 221 4220121526967
    #> 222 4220103977277
    #> 223 4210116435789
    #> 224 4220102662535
    #> 225 4210147050084
    #> 226 9999999999999
    #> 227 4200005028765
    #> 228 4200005237621
    #> 229 4200003876453
    #> 230 4220102954185
    #> 231 3660175334101
    #> 232 4220107403124
    #> 233 4220106027263
    #> 234 8220330059585
    #> 235 4220145246761
    #> 236 4220146621651
    #> 237 4250112765451
    #> 238 4200085928665
    #> 239 4220174585247
    #> 240 8220330059585
    #> 241 3310045162829
    #> 242 4220199780129
    #> 243 4220149767251
    #> 244 4220152733011
    #> 245 4220178385057
    #> 246 4220151048551
    #> 247 4220121213249
    #> 248 4250106935043
    #> 249 4220196970107
    #> 250 4220124862873
    #> 251 4220107805500
    #> 252 4220105980185
    #> 253 4220105204723
    #> 254 4220123828203
    #> 255 4220175308309
    #> 256 4220106620595
    #> 257 4220105647779
    #> 258 4200005401593
    #> 259 4200005401593
    #> 260 4220103668571
    #> 261 4220137711957
    #> 262 1550122366981
    #> 263 8888888888888
    #> 264 4220141983635
    #> 265 4220127786779
    #> 266 3630109816161
    #> 267 4230184044417
    #> 268 4220192299533
    #> 269 4220169337823
    #> 270 4220102849933
    #> 271 3310028373371
    #> 272 4220176788453
    #> 273 4203169897845
    #> 274 4220105729179
    #> 275 4220150980307
    #> 276 8888888888888
    #> 277 4230129334791
    #> 278 3310204301713
    #> 279 4220108008935
    #> 280 4240189554209
    #> 281 4320392436081
    #> 282 4250119192961
    #> 283 8888888888888
    #> 284 4250105640947
    #> 285 8888888888888
    #> 286 4210176185711
    #> 287 4250196145023
    #> 288 4220107800328
    #> 289 4250191207691
    #> 290 4200004706031
    #> 291 4240112917113
    #> 292 4240119822893
    #> 293 4220147394999
    #> 294 4200004553235
    #> 295 4250150141993
    #> 296 8888888888888
    #> 297 4250142339339
    #> 298 4230196914103
    #> 299 8888888888888
    #> 300 4250147082875
    #> 301 4130555730001
    #> 302 4130380672229
    #> 303 4130387542231
    #> 304 3320293192331
    #> 305 3540310121000
    #> 306 3540225026989
    #> 307 3540296695941
    #> 308 3540141229069
    #> 309 9999999999999
    #> 310 4410198768023
    #> 311 3540435177161
    #> 312 3510228211749
    #> 313 3540493263313
    #> 314 3520113130517
    #> 315 3520107131839
    #> 316 3520203799830
    #> 317 3520224559727
    #> 318 3740504084429
    #> 319 3740505064685
    #> 320 3740516755477
    #> 321 6110168273957
    #> 322 6110193237321
    #> 323 3520222691965
    #> 324 3520285333831
    #> 325 3520285824805
    #> 326 3520265590921
    #> 327 3520221561831
    #> 328 3520277965077
    #> 329 3520116596051
    #> 330 3520115309082
    #> 331 4130463875034
    #> 332 3520248685935
    #> 333 3520186741513
    #> 334 4410426618167
    #> 335 3510268607685
    #> 336 3510243065419
    #> 337 9999999999999
    #> 338 3520297121927
    #> 339 3650277208675
    #> 340 3650118353401
    #> 341 3650118353405
    #> 342 3650189910639
    #> 343 3630309398157
    #> 344 4410792675451
    #> 345 4130352906503
    #> 346 3620186061797
    #> 347 3620344054289
    #> 348 4430269329533
    #> 349 3130262548965
    #> 350 4430424199113
    #> 351 9999999999999
    #> 352 3130150583723
    #> 353 4410159397489
    #> 354 3130410838813
    #> 355 3230472587391
    #> 356 3230484085341
    #> 357 3740557620678
    #> 358 4130373572047
    #> 359 3630213827259
    #> 360 3230496332929
    #> 361 3210305110169
    #> 362 3210118573899
    #> 363 3210274827175
    #> 364 3210236789214
    #> 365 3210267980130
    #> 366 9999999999999
    #> 367 9999999999999
    #> 368 3420330314649
    #> 369 8888888888888
    #> 370 8888888888888
    #> 371 4130640424779
    #> 372 4410652059916
    #> 373 3410189301675
    #> 374 3410170903187
    #> 375 9999999999999
    #> 376 4410466088138
    #> 377 3410170911269
    #> 378 3440216028479
    #> 379 3520205453507
    #> 380 9999999999999
    #> 381 3840309687741
    #> 382 3840322192551
    #> 383 3840322222975
    #> 384 9999999999999
    #> 385 4410117939738
    #> 386 3810389866883
    #> 387 3810131382251
    #> 388 3810136748775
    #> 389 8888888888888
    #> 390 4310299258233
    #> 391 4310224812922
    #> 392 4230108147547
    #> 393 4320324294293
    #> 394 4220136764525
    #> 395 4230141561207
    #> 396 4240117034927
    #> 397 4240189700989
    #> 398 3660209789081
    #> 399 4240118594281
    #> 400 2170584413689
    #> 401 4240120757351
    #> 402 4240195177631
    #> 403 3220315576555
    #> 404 3220264205541
    #> 405 3220332417563
    #> 406 3430218038589
    #> 407 9999999999999
    #> 408 3430117817363
    #> 409 9999999999999
    #> 410 3220225160495
    #> 411 3220218185481
    #> 412 3540117676792
    #> 413 4410720496465
    #> 414 4130435889605
    #> 415 4200020933933
    #> 416 4130373396357
    #> 417 4130355816413
    #> 418 4130322492197
    #> 419 4210171011973
    #> 420 4210145940583
    #> 421 4210184498711
    #> 422 4210118937893
    #> 423 4200055621775
    #> 424 4230169897845
    #> 425 4130315476639
    #> 426 4130567605961
    #> 427 4230186870381
    #> 428 4410784281675
    #> 429 4550411270597
    #> 430 4520674472707
    #> 431 4550268335697
    #> 432 9999999999999
    #> 433 4520371723946
    #> 434 9999999999999
    #> 435 4520633710713
    #> 436 4520152055771
    #> 437 4520823788089
    #> 438 4230189079923
    #> 439 9999999999999
    #> 440 4540286305223
    #> 441 4230121415923
    #> 442 4130325247687
    #> 443 4410329751797
    #> 444 4410329751797
    #> 445 4220182739671
    #> 446 4220148315141
    #> 447 4410425813177
    #> 448 4410714558247
    #> 449 4130579538741
    #> 450 4130590866631
    #> 451 4130705859789
    #> 452 4130657945501
    #> 453 4130623122747
    #> 454 4130402903035
    #> 455 4130816066135
    #> 456 4130831152847
    #> 457 4330421750135
    #> 458 4110381989467
    #> 459 4110444454013

    dup_by_uid <-final_assets%>%
      group_by(uid)%>%
      mutate(n_names=n_distinct(candidate_name))%>%
      filter(as.numeric(uid)!=-9999)%>%
      filter(n()>1)%>%
      filter(n_names!=1)%>%
      select(key, type_seat, const_number, candidate_name, uid, cnic)%>%
      arrange(uid)

    dup_by_uid%>%
      as.data.frame()

    #>                                           key type_seat const_number
    #> 1   uuid:e7ff4d5a-c2b6-4dcd-98fd-c9230a007d86        NA           10
    #> 2   uuid:b6e854e2-c0cb-499b-aa03-3cc03ddf2623        PP          246
    #> 3   uuid:939c964b-b310-45f8-9575-2d3af2f43869        NA          101
    #> 4   uuid:a2da3630-30f8-4e9b-9b7c-8e361ac82d68        NA          101
    #> 5   uuid:99510e1a-955b-42b7-8063-58bee890ac88        NA          101
    #> 6   uuid:ca8ddd05-696c-423a-a633-f657ab99efae        NA          101
    #> 7   uuid:e79929a9-6144-4f86-b240-3e27a4db3783        PP          246
    #> 8   uuid:24e35aec-90a5-4df2-930c-c0f0fab6a528        PP          246
    #> 9   uuid:746b8006-119f-4860-a2c2-3ad9afa8a6d7        PP          246
    #> 10  uuid:4988f9d9-04e7-41ea-92d3-f501fb12ad75        PP          246
    #> 11  uuid:5aed9e42-bf88-4a24-8461-f8caf820e4fb        PP          246
    #> 12  uuid:cda1234c-8605-465a-912a-737a83cb3f1e        PP          246
    #> 13  uuid:f3a6f064-6b56-42d2-873b-87ed892a6de4        NA          112
    #> 14  uuid:195f309f-2ec7-4465-995b-2177ab1e81f4        PS           67
    #> 15  uuid:c876a191-f48e-49e3-9d47-9b856544b8a0        NA          112
    #> 16  uuid:21a754ad-301d-4fdf-8342-f65e5b3e3911        NA          169
    #> 17  uuid:68d61061-073e-4c12-b8d8-cc641b5e52ba        NA          112
    #> 18  uuid:4cc911a0-1036-4d37-847e-e072935b785c        PS           67
    #> 19  uuid:df297e09-714a-4746-8734-370844479aaf        NA           12
    #> 20  uuid:25e8f91a-7ad8-408c-afe0-29413ce3355c        NA          121
    #> 21  uuid:f0b48232-e73b-48f2-99ff-5ba42baadbeb        NA          121
    #> 22  uuid:127fc669-c22e-4124-95c8-89b2c54c3f4a        PS           91
    #> 23  uuid:1c28dbe9-cfc5-4f17-8bfe-b73c2196e9ff        NA          124
    #> 24  uuid:a1a90052-16c4-47ed-aec9-5587bd18ffb6        NA          128
    #> 25  uuid:beeaab01-ac84-4b66-9681-dab97cf3fb6b        NA          137
    #> 26  uuid:7063096d-0eb5-4fd7-977e-d0a8f3dcb62f        NA          138
    #> 27  uuid:b4cacdd0-19df-48f1-ac87-c3e338bce9ff        NA          138
    #> 28  uuid:4cd895a5-eff3-487f-ab26-73127e4ef681        NA          138
    #> 29  uuid:062547f1-5bb9-45b4-942f-6b426df40f7a        NA          142
    #> 30  uuid:23355984-0dff-4b0d-954c-f4507e7d8f87        NA          167
    #> 31  uuid:fa7f52a1-756b-434d-8fb5-a6d7b647a037        NA          137
    #> 32  uuid:ccaa1d89-ba41-409a-bdc8-7c400cbdb254        PP          144
    #> 33  uuid:3b3ba92c-87a3-46ce-9db1-c349a0de576b        NA          144
    #> 34  uuid:4044e270-1961-4acd-a56e-a39ac1c5a5f3        PS           91
    #> 35  uuid:2866613e-189b-4537-9e80-68f909a6c950        NA          146
    #> 36  uuid:20d90571-fd3c-4399-ac4c-71e3ebf736de        NA          146
    #> 37  uuid:fdd3fd83-c730-4964-a245-c1b66e505fd2        NA          156
    #> 38  uuid:7b4da6f1-e9c1-48ea-bc6e-45e20c1c4aaa        NA          157
    #> 39  uuid:2e1fc054-9080-41d6-863e-0558c1c4f452        NA          158
    #> 40  uuid:75a5af89-9ec5-4810-82c5-00bec387ab40        PS           91
    #> 41  uuid:a673b3e6-d120-4fa8-8be1-859de6df4bab        NA          160
    #> 42  uuid:85742a64-3af7-46cc-aeac-000969c6328f        NA          161
    #> 43  uuid:286924c0-8358-4450-b740-e0a096a6fd38        NA          176
    #> 44  uuid:d247f644-c6b9-4c49-9330-acc83129097b        PS          120
    #> 45  uuid:8c38e261-f48b-4830-920d-bee28a51c8c8        NA          177
    #> 46  uuid:489c75d7-e112-4887-80e2-c1fd233a4dc4        PS           55
    #> 47  uuid:45f1a9a3-bd6a-437f-9648-ef91271da7d9        NA          182
    #> 48  uuid:38a6eb92-81cb-4475-b0cb-ae1b3ce9ac41        NA          182
    #> 49  uuid:5050d122-d7e5-4ad0-aa93-a224dc32b537        NA          182
    #> 50  uuid:0eb63c47-4524-464e-a563-688ef3c80fef        NA          182
    #> 51  uuid:5e539496-b4b7-4c13-9065-b2d43fe0eb06        NA          188
    #> 52  uuid:59d761db-ea7b-4c85-bc92-5ca23d14f0af        PS           91
    #> 53  uuid:318d6b8c-e33d-4fe0-98b8-ec7d6d0cd71a        NA           19
    #> 54  uuid:8e0f742b-2fcd-4e00-89a9-c57ee128e7ff        NA          189
    #> 55  uuid:764b53e1-4604-426f-b0b5-c0aa889f94e6        NA          191
    #> 56  uuid:ca7e9822-d75c-4b2e-8534-05168e329781        NA          191
    #> 57  uuid:0ec8e338-1e55-4456-ad6d-18a8df23ef8e        NA          191
    #> 58  uuid:399065a6-69b4-4551-ae09-b7a0b6962a61        NA          191
    #> 59  uuid:144bd870-6bfe-4a6a-8d3f-fab9dc30fed9        NA          191
    #> 60  uuid:b8a6d612-f644-4dd5-960c-8d1b45bc027e        NA          193
    #> 61  uuid:eaf25380-7393-4166-a397-a3b56cfbcc13        PP           61
    #> 62  uuid:b930c36e-2687-478c-a266-264366c86d6d        NA          186
    #> 63  uuid:abd9efe7-7a4b-4ec4-8813-ecd6b65843a4        NA          195
    #> 64  uuid:88f3c649-2dbf-4994-a3cd-08feddbc7c79        NA          197
    #> 65  uuid:011b3c0a-a14a-4a41-a589-7c3313a7e425        PS           67
    #> 66  uuid:fc571823-eab0-4ffd-ae78-d42576bb196c        NA            2
    #> 67  uuid:29884a7c-4387-446b-b230-a4a0de552b52        PP            2
    #> 68  uuid:ec7bae84-e3ca-4fec-9ba5-bee1752b5357        NA          137
    #> 69  uuid:b4a376a9-d7fe-4ee5-b2cd-9337fb1f1880        NA          203
    #> 70  uuid:6c2a284d-f685-4816-bc68-f4eab07cdf48        PP           61
    #> 71  uuid:67ec84f9-9233-4d49-b521-418844b21fbd        NA          204
    #> 72  uuid:e7c85ed8-eded-4b3f-bffe-4810eab45150        PS           53
    #> 73  uuid:c283f798-a23e-4f30-8b3d-d0f63b9d68eb        NA          205
    #> 74  uuid:19251a65-acd3-4a09-b1be-ee8fc3774be4        PS           59
    #> 75  uuid:ea20aa84-dc02-4558-9f27-6e22002b3831        PS           58
    #> 76  uuid:b68b9d8c-c5a5-4a5f-9149-f24820c922bd        PS          207
    #> 77  uuid:28c3a024-39f9-4f6b-9497-2bd40c29d644        NA          131
    #> 78  uuid:b8f69fd7-f2d2-4f3b-b4cc-4d725d0a6cc3        NA          208
    #> 79  uuid:adf8f662-0ff6-4562-b428-59692d6a75ab        NA          129
    #> 80  uuid:6a050621-184d-49cd-a167-7c607809c2bc        NA          208
    #> 81  uuid:26713378-9b89-4cdc-b54e-74ebdde98b7f        NA          129
    #> 82  uuid:d988ffbe-3fc4-4824-8e3a-8f1804a6d25d        NA          208
    #> 83  uuid:c8eb139c-449e-47bf-b4fb-0f30d53ddf12        NA          129
    #> 84  uuid:5425ffb2-756f-4445-93d0-1c0ad90b26a8        NA          208
    #> 85  uuid:c11c7e63-3e16-4580-ab1a-7b3fe85bb079        NA          129
    #> 86  uuid:861cab13-9351-4c82-aeae-4860021f815e        NA          208
    #> 87  uuid:bd20c885-31e3-4f22-bed2-5b958c2e57c8        NA          129
    #> 88  uuid:c91474e1-68f1-4a19-b2ad-ddb18ee57b94        PS          209
    #> 89  uuid:469bf4b9-e0d8-49f1-8f73-dbd8a00e01b9        NA          129
    #> 90  uuid:1f6fe5eb-7ba2-4f45-bc9d-519f18b7e421        PS          209
    #> 91  uuid:19711673-1b7f-4a4c-aef4-b40a58402a10        NA          129
    #> 92  uuid:e8bb7842-0db8-4558-b436-788f76b85ce2        PS          209
    #> 93  uuid:d6b56c5b-5095-4929-acc7-f22502db3d72        NA          129
    #> 94  uuid:ca791835-cdb5-48c7-9898-226be58c5a05        PS          209
    #> 95  uuid:94d5abb3-7ea5-44d6-9713-cdc6f23f83fe        NA          129
    #> 96  uuid:fd0eaa5a-c971-4b20-9a22-d0ee9b30f784        PS          209
    #> 97  uuid:95e1b78d-8add-4b0a-aad2-9502ac82937f        PS           50
    #> 98  uuid:ea4518af-ecbe-4fe1-875c-94731640ae7c        PS          210
    #> 99  uuid:b71aed2b-306f-43f6-9519-0f91b8c2ad29        NA          130
    #> 100 uuid:02bacc6a-b1f5-4698-9f64-2f3e5c804e15        PS          210
    #> 101 uuid:a3e74f1f-b03a-40a0-ab72-52a87deee9aa        NA          130
    #> 102 uuid:c45627c1-ac40-49bb-9730-e8dc2b45aecc        PS          210
    #> 103 uuid:e48f6168-ac7d-4e1d-a394-5a1c12505454        NA          130
    #> 104 uuid:e4481526-d045-4f71-a1af-d13278c5ecd9        PS          210
    #> 105 uuid:75e9c354-b68e-4c78-ae8c-25449803ef7d        NA          130
    #> 106 uuid:c632504d-3012-438d-b1aa-dcc121da744c        PS          210
    #> 107 uuid:b196a4f9-b27d-43b1-88aa-8938c400dc0f        NA          130
    #> 108 uuid:8ab63f5f-4fc7-48d9-8ef1-f501aac5db0b        PS          210
    #> 109 uuid:b135b408-cdcb-4569-a5a9-5663fc8c7746        NA          130
    #> 110 uuid:9207e41b-2e16-4bb1-9ae1-dbc9d247f30d        PS          210
    #> 111 uuid:3d660e23-253b-482a-90be-3ff9c0f0b657        NA          130
    #> 112 uuid:4768c287-f640-4d34-a618-5b8e1640e94f        PS          211
    #> 113 uuid:efba84e9-90d7-491e-8e9c-6708a0a78b26        NA          130
    #> 114 uuid:ff28a114-b1a9-4261-9bc2-2549c6411bf2        PS          211
    #> 115 uuid:c2f72f1d-51df-417d-bc84-2f1d93612085        NA          130
    #> 116 uuid:f407c4d6-4d3a-4d3e-830d-66fb97add30b        PS          211
    #> 117 uuid:38ae4a96-ddb6-4cb4-88c2-5a6a2e2ca46d        NA          130
    #> 118 uuid:0cf4ef45-bd55-4c51-98f9-6553a3508b42        PS          211
    #> 119 uuid:7338f127-96a2-44a0-80a8-83dc4b4d773d        NA          130
    #> 120 uuid:e69412e4-9f00-49e7-baa9-023d6b13bc5b        PS          211
    #> 121 uuid:00e9c168-1378-4fd3-82e1-b1f008b37121        NA          130
    #> 122 uuid:8c048251-3187-4df1-b014-34fdd175d421        PS          211
    #> 123 uuid:2332360e-be13-46d4-af3d-13250b80d2a1        NA          131
    #> 124 uuid:c8c1e0fc-5970-44d4-b395-d3826ec800d2        PS          211
    #> 125 uuid:c7f38f71-b4e3-48f3-8bf7-7794af5acad6        NA          131
    #> 126 uuid:79ffb1de-397f-4c27-a2f8-53e216c7b1f0        PS          211
    #> 127 uuid:1d08b711-9d1e-4ee7-8c5c-eac57d94e4e8        NA          131
    #> 128 uuid:dc7116ed-7796-4415-88da-efd8f3866873        PS          211
    #> 129 uuid:19c76cb8-1956-492f-8155-68da0946dbad        PS           91
    #> 130 uuid:922ae869-e7bd-415b-85e0-3b6d94f9f1f1        PS          213
    #> 131 uuid:409fdd29-d1f0-46e0-a984-5602d9d81829        NA          191
    #> 132 uuid:915562e6-6610-4378-8ecb-65435330adc4        PS          213
    #> 133 uuid:971f61e7-2025-4284-947b-2d23b80d2aa1        PS          215
    #> 134 uuid:ecd10953-b3fb-414f-963e-93343234a0eb        PS          215
    #> 135 uuid:c9dba076-06d1-4a88-a2f1-4ad33a7c110f        NA          218
    #> 136 uuid:003c60fd-c5f1-458b-91eb-2d13b4294bd1        PS           48
    #> 137 uuid:6f4578db-0d64-4066-a3f9-fa458c8ff532        NA          218
    #> 138 uuid:427a0cb7-88f3-4875-b5e9-42d569b8b92c        PS           48
    #> 139 uuid:99b3276f-495a-4fbc-9978-9c516af6fba7        NA           22
    #> 140 uuid:19bcfda9-4af2-490a-9656-7d143c5315cf        NA           22
    #> 141 uuid:9e7d4937-f2d0-4db3-bf0d-941f4e09f2fd        NA          220
    #> 142 uuid:69a992aa-1ce6-41d0-ad7a-6ac78a09dedf        NA          220
    #> 143 uuid:ea98b4c9-250a-4cec-b6e1-60640d26e655        NA          222
    #> 144 uuid:0b10c041-14b4-4b06-a3eb-611bae863a71        NA          222
    #> 145 uuid:488552a5-2953-46f7-a789-bc0c025ff6be        PS           55
    #> 146 uuid:099d2355-348b-4032-9c59-003d43073185        PS           57
    #> 147 uuid:0934e7b6-5b26-42c6-b7e8-8f3eafa6873c        NA          222
    #> 148 uuid:c7352dd5-918e-4f8e-a0ca-d21ad9da7ccf        PS           56
    #> 149 uuid:0c1cebb9-c313-425f-a531-535aea586b3a        NA          222
    #> 150 uuid:3f1f8a83-d726-43e4-9995-523cb6caa98e        PS           57
    #> 151 uuid:a2bd22ed-8c00-46a7-81bb-6ba6ead26a1c        NA          223
    #> 152 uuid:19f0e9cc-fdf8-4303-b5e9-ebcf95387f58        PS           58
    #> 153 uuid:eb372a9c-958b-43f5-bdd9-81e7e0787823        NA          223
    #> 154 uuid:b795e735-db8b-4551-a9ec-54e7216c679e        PS           58
    #> 155 uuid:4cc8e4bc-37cc-49f5-a068-860d0a098e51        PP           61
    #> 156 uuid:627476e3-2d93-4560-8ce1-c91bbe0214b3        PS           63
    #> 157 uuid:25c5f102-a47a-4657-9942-62d965cd71eb        NA          224
    #> 158 uuid:5e2062ca-5501-415f-b00a-1663f26af8c5        PP           61
    #> 159 uuid:155c3f2a-605b-458c-b9b4-23d8d91c6b12        NA          224
    #> 160 uuid:d396c678-2bc5-451a-a12b-d7d5f7df900b        PS           49
    #> 161 uuid:345eef3e-2406-4361-bea1-899ee3431cf5        NA          225
    #> 162 uuid:e63ff73a-e4f7-4685-ad49-4e1eec101fc6        PS           63
    #> 163 uuid:52d8731c-05c8-4981-ad73-ababcd86c883        NA          237
    #> 164 uuid:82c07262-e58f-4721-a515-57e83c3935a9        NA          237
    #> 165 uuid:c12193bd-f04f-42cb-bc49-66cf2019157f        NA          240
    #> 166 uuid:36b663ff-e780-4c24-82dd-388c65d96758        NA          240
    #> 167 uuid:f4c076be-4ed9-44e3-a3e2-afdabebb3439        NA          243
    #> 168 uuid:3bc1cee0-377d-4d9f-8a18-e1566a913e7f        NA          243
    #> 169 uuid:732dcca3-eb72-4b70-b6d3-a1a3de829e5a        NA          243
    #> 170 uuid:6af4159d-eacc-49e8-a3e7-df8fb80882d8        NA          244
    #> 171 uuid:b0258a8c-878a-4b1b-ad84-cc16e220ca31        NA          245
    #> 172 uuid:fb0e48ca-b16d-488a-aeeb-a9a76ea168c3        NA          245
    #> 173 uuid:78379f86-d5c7-4161-9422-15142b6668a4        NA          245
    #> 174 uuid:f2b45328-0eac-406a-9108-991abd8c1a17        NA          246
    #> 175 uuid:ac7e4bd5-f8b8-43a9-ad50-1f27a5637d27        NA          247
    #> 176 uuid:4d5a9294-4674-4b97-92f8-559696dcba8a        NA          248
    #> 177 uuid:0be6149b-d91f-4157-94d9-289a430b6077        NA          259
    #> 178 uuid:faf55413-ee89-4529-b620-7ea9ec41aa65        PP          259
    #> 179 uuid:1b9aa8a5-641f-457d-ae5f-c6dcaee8a05a        NA          259
    #> 180 uuid:3a8393fe-d8a7-458f-995d-05ff72a62fa9        PS           52
    #> 181 uuid:8ea34267-2bf4-4968-b733-0a27136cc06e        NA          261
    #> 182 uuid:cd5644f0-560b-447e-8f20-2d403030f623        NA          261
    #> 183 uuid:2e1b66a3-fe1e-4ccf-a46e-70ee464afde7        NA          263
    #> 184 uuid:36c73832-c0e1-4365-b205-506a30e6995f        NA          263
    #> 185 uuid:fcf37321-28bf-4705-ab74-0c75cf7afd25        NA           28
    #> 186 uuid:fb0e3b72-9e77-4974-b8ea-1f6fa34f3790        PS           57
    #> 187 uuid:b1211635-64bc-4fa6-af72-565a850d6c0f        NA            4
    #> 188 uuid:07e23310-c124-4188-9aa5-f30976097fad        NA           41
    #> 189 uuid:2b51fdf8-1846-4bbf-a59b-a131c8bbc98d        NA           44
    #> 190 uuid:e42c8e60-e58a-4956-8468-7a55b0f82606        NA          226
    #> 191 uuid:6c979c0b-924e-48e9-a64b-1c90002fc51d        NA           47
    #> 192 uuid:818da9a0-97c6-40eb-b39a-8924152e35f2        NA          264
    #> 193 uuid:0b8ba062-34a0-4c1a-8dc2-665d5ef290db        NA           55
    #> 194 uuid:99dff510-8ff7-4baa-9ec8-8f198f1d115e        NA           55
    #> 195 uuid:d47d6324-dc98-450c-be42-ddc6f3fd405d        NA           71
    #> 196 uuid:f33d7200-5491-4970-9395-b8b11fe2a648        NA           71
    #> 197 uuid:82f70958-2c0f-472d-b47f-9cfe4deaac66        NA           71
    #> 198 uuid:0ebd7956-48e6-407d-8da6-bcdec782802a        NA           71
    #> 199 uuid:99ffe3cd-890e-42b4-a4df-09bd4efea4d6        NA           71
    #> 200 uuid:0fe4f6dd-5053-4078-9b05-8ddde70d664f        NA           79
    #> 201 uuid:34b3d85d-ddf2-4cbe-bca8-56bacc1352ec        NA           79
    #> 202 uuid:e1816886-e4be-4032-b7ec-867f89ad3d57        NA           90
    #> 203 uuid:6886557e-6224-4c30-8039-c937688cba41        NA           90
    #> 204 uuid:73f57dfa-7dbb-4b7a-a042-04329b4da1f9        NA           91
    #> 205 uuid:e0e75786-7c40-4c4e-b167-d75ab184de2e        NA           91
    #> 206 uuid:f14b7493-3835-44f8-b00c-77ad85e68205        NA           85
    #> 207 uuid:0485e976-16c1-40e6-bcd0-b73e522da3a2        NA           95
    #> 208 uuid:fd587e0e-6fa6-43b0-b0e6-35a9d17fd311        NA           97
    #> 209 uuid:6041dd0a-7bf7-4ee6-8faa-fdd672e30362        NA           97
    #> 210 uuid:5f0738bc-29e7-4a18-9f7f-db1c2d7c3670        NA           92
    #> 211 uuid:a397c7bb-399c-4d6c-8991-81fe66a2b2a7        NA           98
    #> 212 uuid:f06b8b0b-434b-4adb-89dd-775ee7481d86        NA           98
    #> 213 uuid:5fb73796-6709-42da-8119-3f4cd40ebd6b        NA           98
    #> 214 uuid:adc33ca3-3bf3-4fe0-a276-ee8335a77e71        PB            1
    #> 215 uuid:b484a136-d61b-4565-ab96-d8b9048565f9        PB            1
    #> 216 uuid:c80ff99d-fde6-43d9-b592-9f0f9db36472        PB            1
    #> 217 uuid:f67de0fa-5cdd-474b-8f47-f946ccfc22d7        PB            1
    #> 218 uuid:7d904e42-5f34-4564-bb87-be047b588614        PB           24
    #> 219 uuid:1d4b1bc4-48c3-4cec-a41b-673016e74b0a        PB           24
    #> 220 uuid:9acf004d-2968-477f-8607-b97c4c7c49f8        PB           26
    #> 221 uuid:71244aff-e966-49b1-8b49-a57632df301a        PB           26
    #> 222 uuid:7c06dc11-28d4-4f70-b0a9-b27cbd592d8e        PB           26
    #> 223 uuid:9c2e163c-6bd0-49e3-bd86-4625a4b1c0cd        PB           26
    #> 224 uuid:dc8b8643-572d-49df-aa89-0b53d66f7f0f        PB           26
    #> 225 uuid:0dacdbea-61b0-406b-affc-53760f2770bc        PS           58
    #> 226 uuid:1b017210-f1c7-40b3-b5a2-63e3c267c0d2        PB           13
    #> 227 uuid:6771337d-cfc6-4ee5-81b7-3ba44dca64f0        PB           30
    #> 228 uuid:819bfb83-9803-4491-9768-7df1a8e1e1bd        PB           36
    #> 229 uuid:83267540-2637-4fb5-b7ab-967022f91ee5        PS           50
    #> 230 uuid:72774252-aea1-497c-89cc-8e127b96b16c        PB           47
    #> 231 uuid:5dd7e2f6-a4d1-43d3-ba2e-1a4f721d521c        PB           47
    #> 232 uuid:43fad123-2da0-427c-9ff5-8c7a399e4d15        PB           47
    #> 233 uuid:0a521b63-8173-4a32-ad36-32fb7aa27deb        PS           59
    #> 234 uuid:9c494a6f-b2ba-43d6-93b3-975cb9d0d287        PB           49
    #> 235 uuid:f7748f4d-6362-41a4-a02d-4656f9020797        PS           57
    #> 236 uuid:c959add8-26d8-47e5-aed2-26d9122dce48        PB            5
    #> 237 uuid:cf30743c-279c-4aed-aa74-860485145878        PB            5
    #> 238 uuid:11ca03d0-45d5-4220-b3f4-7ccbaf546a55        PB            9
    #> 239 uuid:1fd63ccb-6804-4849-867c-0af59688829f        PK           10
    #> 240 uuid:b8830776-5fe5-4a43-995d-25e61580b97d        PB           51
    #> 241 uuid:b3af54cc-52dc-4564-86c2-53b5efd5e6c2        PK            1
    #> 242 uuid:f84289e0-37f4-4142-b428-9c0aa8d7f4f5        PK           10
    #> 243 uuid:ab332cc8-5875-4f6a-b603-3185ca69520d        PK           10
    #> 244 uuid:e5796b2c-b044-4533-aa33-d1a5c76ba09b        PK           20
    #> 245 uuid:25759dea-e30e-4534-b5ef-04c0d3781b86        PK           20
    #> 246 uuid:68cb4f23-6d80-4a0b-910e-4ed82d4529e1        PK           23
    #> 247 uuid:631fb218-c618-406d-aa8b-bae4087de033        PS           67
    #> 248 uuid:55b3e243-cfdc-487d-aa33-34ec96779227        PK           27
    #> 249 uuid:09547d1a-f37e-4eb7-8d86-84fa1d83a063        PK           27
    #> 250 uuid:c343dd46-d922-454c-90a1-f8bad6cfa8ea        PK           30
    #> 251 uuid:1c636531-0134-43c8-8ccb-4eec2ab6b310        PK           38
    #> 252 uuid:10a120d4-a41a-4281-a2b5-ceb941ef1d75        PK           35
    #> 253 uuid:526e09c1-780c-4579-9dca-7caf14711d12        PK           41
    #> 254 uuid:96d3bbbb-a9cd-4a32-ad19-75ba61a806e4        PK           39
    #> 255 uuid:bb8b4ef4-36e0-4a77-b746-d852128dd325        PK           42
    #> 256 uuid:eb0f091c-17fd-4563-9b53-e69d26d50e98        PK           46
    #> 257 uuid:762c3a8b-e3df-409e-a2b0-3903e1483baa        PK           47
    #> 258 uuid:929e1d7f-5f41-45fe-a457-99dda58d0ae7        PK           56
    #> 259 uuid:afeb5e0f-ba25-44ac-9fa5-7f7d0aea20b3        PK           56
    #> 260 uuid:39e82b72-b427-46a9-917b-624c6b92e364        PK           63
    #> 261 uuid:f5b4abeb-9eba-4690-91f9-b1c05853adbf        PP          105
    #> 262 uuid:b96816dc-2106-49ef-9553-ad74b83ea72a        PK           74
    #> 263 uuid:ce179264-a931-45a0-8415-844a417975e2        PK           74
    #> 264 uuid:4f17b3b9-037e-48fc-802f-d4a2ef3d9097        PK           76
    #> 265 uuid:b909b1cf-ee95-4884-bff5-0aa6c1b89d8a        PK           76
    #> 266 uuid:a72fa278-0a9e-41f2-ad08-f43437976dda        PK           88
    #> 267 uuid:a32ecbc1-866c-4e10-a1fb-2fdfde782ce3        PK           89
    #> 268 uuid:f933be13-e0ec-49f2-b2f9-016dd1de47f5        PK           92
    #> 269 uuid:5f93d7fc-4269-4d88-b48b-2862cbe2726e        PK           93
    #> 270 uuid:7da336fb-efa4-4e86-b79b-334f70e00d2e        PK           94
    #> 271 uuid:064d3aae-0a37-4005-8cba-d4401cc97103        PS           67
    #> 272 uuid:7f3de0be-44c0-4316-a13c-26835672aeb6        PP          100
    #> 273 uuid:a3b808fb-3f16-4394-8f6f-7558c3691d0a        PP          100
    #> 274 uuid:39ffce9d-b389-4985-b91c-5986a453e55e        NA          141
    #> 275 uuid:aa8e6d19-9186-45eb-ad6f-624cc4741a16        PP          101
    #> 276 uuid:2873bd40-623b-498d-820d-2915e11e4172        PP          101
    #> 277 uuid:286beb3f-e255-4e7e-848a-838499f75494        PP          101
    #> 278 uuid:053c2a37-2ccc-43c7-b9f9-cf165ae2d0cd        NA          143
    #> 279 uuid:fb859a89-4dc5-40ad-8a85-21be27112dbf        PP          102
    #> 280 uuid:e8a7092e-73ea-44f6-a649-45b5d0bc4db4        PP          105
    #> 281 uuid:4afdf836-3588-4695-bfe3-056ed2e85b62        PP          105
    #> 282 uuid:c2f511cd-c78c-4af5-a92e-f64464ca7ca6        PP          107
    #> 283 uuid:ef70e365-5a6b-46c7-a1dd-bcff758f5fca        PS           91
    #> 284 uuid:1e0fe50d-ecd6-4f2a-a554-75280b642bd0        PS           91
    #> 285 uuid:4b1cf0ea-73bc-4ab2-ac60-b65da1cf2822        PS           91
    #> 286 uuid:bab2290b-7716-4147-9ec2-69459bff8a95        PP          107
    #> 287 uuid:ed419fda-6e2c-4458-b647-37785418708d        PS           91
    #> 288 uuid:9bacbcd2-a718-4041-a578-d2488eb2628a        PP          107
    #> 289 uuid:b5d1fc6b-f139-41d7-9078-3f2c97d49d9a        PS           91
    #> 290 uuid:3f2eebf6-b8fe-4e0a-80fb-aff7cb3e72cc        PP          107
    #> 291 uuid:a35a3a3e-3b43-4b82-8426-a8d549560d95        PS           91
    #> 292 uuid:f7ad97f7-14c8-4541-a5f9-fb70022d19a9        PP          107
    #> 293 uuid:23271b74-50f8-48f4-9a4c-6273ee7f69ee        PS           91
    #> 294 uuid:8773bf7b-84f7-4a12-bc67-e9f44e204ee1        PP          107
    #> 295 uuid:ef8012d1-1129-48b4-8d78-7da57230b800        PS           91
    #> 296 uuid:5419738a-3138-4539-aaf5-47fef3c16fbd        PP          107
    #> 297 uuid:c42e075b-bb85-4b0c-a5a5-30b4b2a52c5c        PS           91
    #> 298 uuid:4bbf4f1f-90e9-4fef-ae64-095d185f2e67        PP          107
    #> 299 uuid:c8f954a2-cab2-4b83-ab21-bff7dd63b9dd        PP          107
    #> 300 uuid:16583e58-abbe-4ce6-9038-e24f7cd386d4        PS           91
    #> 301 uuid:333d7354-c398-4d59-ae9a-680066f59eca        PP          107
    #> 302 uuid:6f8d06b9-f38a-4eb5-a54e-da02bdace36a        PS           91
    #> 303 uuid:90ede7ba-a9dd-4966-9d4c-843260f9fc8c        PP          107
    #> 304 uuid:0868e67b-d1a3-411f-ad46-8e1fc0ed3c68        PS           91
    #> 305 uuid:eee4dea7-3127-4ea2-adb5-529a9ba1b05d        PS           91
    #> 306 uuid:aa7025df-1c82-4229-b1f4-41417f0f615f        PP          108
    #> 307 uuid:6e640753-80f3-4fed-b1d6-45b27f961a6c        PS           91
    #> 308 uuid:cd1efbcf-8377-49a8-bd8f-6b265f08cb2e        PP          108
    #> 309 uuid:016eae95-5255-4aef-ad97-445fcbf67318        PS           91
    #> 310 uuid:61ee4e5d-564e-4322-9836-d3ca8740e23c        PP          108
    #> 311 uuid:e1934843-07ed-45ad-a195-d96cb2dd3dae        PS           91
    #> 312 uuid:226bdbc1-768e-4648-838a-aa8d77644f28        PP          108
    #> 313 uuid:e151cd23-e25d-424a-ae73-05893d444dd8        PP          108
    #> 314 uuid:1bb2cbc9-4e3b-4f80-ba9a-f68e5314ebc0        PS           91
    #> 315 uuid:84b61750-921f-4d7b-a53e-36f7fb64cc1d        PP          108
    #> 316 uuid:ff4168a9-e654-44e7-a982-695c251ac281        PS           91
    #> 317 uuid:f73ff0a6-474d-44a8-b05b-445031bbf515        PP          108
    #> 318 uuid:8cfbd6b2-589d-469d-9551-4814ed004793        PS           91
    #> 319 uuid:05af5ae1-1155-4d79-9157-5cfeebde79f6        PP          107
    #> 320 uuid:6ddcd85c-4fc9-4bc4-99c3-cef1e4a91412        PS           91
    #> 321 uuid:a3ddf2d8-ae1c-447d-9536-603ce4312288        PP          108
    #> 322 uuid:12fc9545-622a-4a5e-8479-f4581a473039        PS           91
    #> 323 uuid:1c62a8d0-e907-41bf-8390-129acda1934a        PP          109
    #> 324 uuid:8eb1f76c-e031-4456-a842-f1abc2a53863        PS           92
    #> 325 uuid:d436e8ae-5ec4-4567-ab94-02f311366cda        PP          109
    #> 326 uuid:66d8b202-82ca-4787-90b0-b5a9cf7ddeae        PS           92
    #> 327 uuid:f159a227-7ef5-4ab1-b89e-9f32f73fd7d0        PP          109
    #> 328 uuid:f6bf95b3-50be-4ba6-b1c0-c02e5339385b        PS           92
    #> 329 uuid:e151322d-9b8a-4c59-826f-f657754d6598        PP          109
    #> 330 uuid:cacc668b-969d-4bb3-ade3-7c60066fdf0d        PS           92
    #> 331 uuid:e1314087-9334-441d-b748-0835673c1476        PP          109
    #> 332 uuid:6eb1af2c-c4c0-473a-8026-10a070588936        PS           92
    #> 333 uuid:87418840-818e-47f8-8115-9be2d4f9d692        PP          109
    #> 334 uuid:26c4552d-6616-4142-b463-cb61663298c9        PS           92
    #> 335 uuid:888f7c11-729a-405d-a043-744579bc3e5f        PP          109
    #> 336 uuid:c2298bf8-fb4c-4abf-b989-795f4df50290        PS           92
    #> 337 uuid:ea353bdc-11ec-4d9d-8488-9fb284db417a        PP          109
    #> 338 uuid:942bbf2d-e1b3-4799-acf8-cd6942d7387b        PS           92
    #> 339 uuid:39448fab-6b2e-4a23-b278-880308a92ede        PP          109
    #> 340 uuid:607ac114-138e-4bc1-90eb-a55134f45289        PS           92
    #> 341 uuid:b7f4e806-9d47-45d1-8fe6-3acb3dbd9f00        PP          110
    #> 342 uuid:28b33622-fa9f-404c-bafa-a6836859f34b        PP          110
    #> 343 uuid:6385b95c-2f8a-43de-8e69-aa223825a0f6        PS           93
    #> 344 uuid:3a29fec1-8132-4bda-8e4c-6ba7beceb3a9        PP          110
    #> 345 uuid:cf76b4e6-6fc7-42d7-95dd-67c934c28fe7        PS           93
    #> 346 uuid:fa8497bf-c37f-4578-b756-b2f955906eca        PP          110
    #> 347 uuid:260d4cd3-9ec2-4e65-9309-249be77de54b        PS           93
    #> 348 uuid:ed4cfd63-0963-4400-8112-62281d721844        PP          110
    #> 349 uuid:1e7e3612-d62c-4f65-bcb0-68c3136db193        PP          110
    #> 350 uuid:afa079eb-9e8a-4a96-a1d2-90fc187109a4        PS           91
    #> 351 uuid:e5d01d84-b7f8-4c98-b321-e5194824b747        PS           93
    #> 352 uuid:b635a922-762c-4c0f-90cd-71c3f17ab6ed        PP          110
    #> 353 uuid:c3c26107-4c22-42fa-9593-a1444fd0c918        PS           93
    #> 354 uuid:6f51f2fc-2068-4275-8ced-ece5205f44d9        PP          110
    #> 355 uuid:be9249c3-fe75-4f48-998c-68f744a70523        PS           93
    #> 356 uuid:1bc90d65-a3e5-4195-bd47-b6807da7fa98        PP          110
    #> 357 uuid:f9e94c0b-2604-44bc-b077-bbb351f929b2        PS           93
    #> 358 uuid:0ffb5b62-cc20-461d-a617-674635625e86        PP          111
    #> 359 uuid:aa75905a-3a98-4d28-b49d-caeb6d2cd8e2        PS           94
    #> 360 uuid:5475fdc4-d6ca-42e0-ba48-ff4e3c794e12        PP          111
    #> 361 uuid:51f37268-d3e4-4eff-a611-25776410dbd2        PS           92
    #> 362 uuid:528bb758-0657-4745-9a8a-e2718e95b4f6        PP          111
    #> 363 uuid:39790cde-3491-418a-8e52-464c20e221bf        PS           94
    #> 364 uuid:d34402e6-84c1-4685-812c-e26f6f92301e        PP          111
    #> 365 uuid:37bbfbad-3c38-4384-b4b6-f80a4cb4e6b9        PS           94
    #> 366 uuid:126cd604-f3c6-43f3-9960-d25713755510        PP          111
    #> 367 uuid:45eba184-2d43-4327-9249-9eb08908828a        PS           94
    #> 368 uuid:56b00777-6b28-4a71-8490-b7dd65194298        PS           99
    #> 369 uuid:d8d1fde9-5d75-46be-9096-b4db7d976ff1        PP          111
    #> 370 uuid:5abb721c-fecd-4d62-a752-00f0dfd3cef3        PS           94
    #> 371 uuid:4579e4d3-9781-46e1-b452-64f041d9ec84        PP          111
    #> 372 uuid:8c6b4952-35d1-4447-8446-1c939387bdb4        PS           94
    #> 373 uuid:bfa4161c-86e8-4e7e-8b31-dfca47fd47bc        PP          111
    #> 374 uuid:5b8b6bf0-7e98-40a5-8107-952939785325        PS           94
    #> 375 uuid:b0f7dd38-ff22-4700-bb6c-69d707f1885d        PS           94
    #> 376 uuid:db09ff8d-fe4e-44ce-88ca-f1b8ae008690        PS           94
    #> 377 uuid:20ed579f-03f9-433e-be26-a3c7cbd420f3        PP          111
    #> 378 uuid:7d9b33b2-b350-41ea-9d02-27bc488376bf        PS           94
    #> 379 uuid:d07767f9-d8de-4bd3-bded-c2ca2fb6727a        PP          111
    #> 380 uuid:ac1f1a30-9d5f-497b-a0ff-886f75cbcfd4        PS           94
    #> 381 uuid:eaa29fc9-ce45-4986-ac05-23ec1733dce0        PP          111
    #> 382 uuid:662782ed-84b4-464d-85a4-438db750907f        PS           94
    #> 383 uuid:558c3b4b-a14a-4e30-8f01-30c20f814b34        PP          112
    #> 384 uuid:c5d06f22-dd76-47c3-9a34-2d9418087b0a        PS           94
    #> 385 uuid:bd40bd48-7fb1-48cf-8443-9b53dac186a8        PP          112
    #> 386 uuid:067cc163-d7ab-4385-99e9-c8262fc2d83a        PS           94
    #> 387 uuid:a46f1143-13d3-4f8e-bae2-8493b7e92144        PP          112
    #> 388 uuid:95454fdd-ccf1-49bb-9d14-cfddf0ef674d        PS           94
    #> 389 uuid:5db33bb9-21bd-4934-8e33-a408ccb8c8c1        PP          112
    #> 390 uuid:8d7ddab3-88a2-440f-8abd-542b6f65bbb7        PS           91
    #> 391 uuid:8169c5df-3b86-4d3a-817a-1db7bec660c8        PS           95
    #> 392 uuid:48a5083f-cf0e-49aa-8db6-ecc4f59f94c4        PP          112
    #> 393 uuid:fd61ef3e-f63b-4803-8676-654582618239        PS           95
    #> 394 uuid:88da3e45-312d-49cf-92f6-febc38495a75        PP          112
    #> 395 uuid:c5696031-12a0-46d5-8606-28fc5ce3674f        PS           95
    #> 396 uuid:c3761baa-f50f-487f-9bdf-61effc08fd15        PP          112
    #> 397 uuid:f9c6b0d4-31e8-4079-8d5c-1cf086589e06        PS           95
    #> 398 uuid:e53aed0b-3a54-44ac-8cff-48a44b7c369d        PP          112
    #> 399 uuid:ef268023-d480-43a7-b818-7f033a5301ed        PS           95
    #> 400 uuid:aa8ca1ee-03b8-4755-9b5f-97e2d50cd240        PP          113
    #> 401 uuid:5fe5499f-ef3d-43c2-b7b0-5d4aeef5d916        PS           95
    #> 402 uuid:56a73e66-a3e4-4be6-ba88-7e22125bda42        PP          113
    #> 403 uuid:cd7bcf5a-a3fd-42ef-8080-387920c04431        PS           95
    #> 404 uuid:506b266a-ea28-4427-b95e-2dd7231dd4e0        PP          113
    #> 405 uuid:6454c86d-9c47-40f0-9c69-38cf50e93303        PS           95
    #> 406 uuid:65c35247-eafa-4109-8b50-522c4464ab78        PP          113
    #> 407 uuid:bfa59faf-ce3b-4a57-a12c-e61857e8a748        PS           96
    #> 408 uuid:2d2dc41e-15bc-4963-9932-a4d99ef3c643        PP          113
    #> 409 uuid:d7fae0bc-0296-4a0d-a6c7-44a4f362c9f0        PS           96
    #> 410 uuid:8524a4eb-de50-4fc8-b3dc-03765a933385        PP          113
    #> 411 uuid:3228319a-260e-4229-91d8-03e0fa587c6d        PS           96
    #> 412 uuid:5bdc5b7d-7100-45f9-a85d-0c3a3d3cd749        PP          113
    #> 413 uuid:5720e3c5-78f9-4ab9-932c-d7acf2162aa6        PS           96
    #> 414 uuid:e3c0eb22-90fd-4695-ae83-275526ee6510        PP          113
    #> 415 uuid:b6ca8a08-8f25-4300-9270-d53c56421246        PS           96
    #> 416 uuid:6e19d6c6-79ae-473c-ac0d-d4b3a4de2ec6        PP          113
    #> 417 uuid:6ac87637-e6f4-42fb-8ea9-7eb3c7ad1ed4        PS           96
    #> 418 uuid:f106db0e-8f89-4371-81da-9f39bd5e0d37        PP          113
    #> 419 uuid:8528d1bb-f761-4889-b5ef-821d728c2d14        PP          114
    #> 420 uuid:887e479b-a549-43b2-a7ed-4d2eb8abacda        PS           96
    #> 421 uuid:dbf7d452-c861-43f5-a70d-cdf79f3fbfe4        PP          114
    #> 422 uuid:7ce6dc32-3978-4808-9f25-561cdbeac310        PS           96
    #> 423 uuid:451f38dc-ea78-4865-94e2-5d4d34b07959        PP          114
    #> 424 uuid:b0e1294f-34da-4cd8-b020-5f48f1f4c0f5        PS           95
    #> 425 uuid:09fea466-eb57-4309-a2ae-89eace7c854d        PP          114
    #> 426 uuid:d17bfb41-db7b-4fe8-88af-ee16a78d5487        PS           97
    #> 427 uuid:8d862b39-2af6-4f80-ae23-003f92cb00a8        PS           97
    #> 428 uuid:1993f192-f0f4-448e-b671-63c96ea9bf56        PP          114
    #> 429 uuid:03fc239f-b57e-4d25-87f0-d8a67250810a        PS           97
    #> 430 uuid:c725c430-e7cf-4416-b86c-13bd6275391d        PP          114
    #> 431 uuid:e437528f-ee4b-4a40-9a55-a164e7524cca        PS           97
    #> 432 uuid:45503d05-6c08-4529-8009-f1466cd53312        PP          114
    #> 433 uuid:cb427491-5bd2-415f-8e86-7650ec2c1115        PS           97
    #> 434 uuid:492aa632-5c38-40ce-985f-61dda528fb83        PP          115
    #> 435 uuid:c96642f3-3975-4322-b692-6ae5663cdafb        PS           97
    #> 436 uuid:f4718a7b-011c-4af0-b6dd-e2c3637f9dfe        PP          115
    #> 437 uuid:47d73583-d44d-497b-8ca8-eaeca1992b50        PS           97
    #> 438 uuid:f8658033-9550-4a96-ab79-563db61de17c        PP          115
    #> 439 uuid:0ec0f4c4-ef0a-4fb9-8e26-1b750a95d44d        PS           97
    #> 440 uuid:47d9374a-a60c-41e3-a240-6be82e31b1c8        PP          115
    #> 441 uuid:8028450c-785e-4d61-a365-c53064118331        PS           97
    #> 442 uuid:7745dc05-4864-446e-ba20-8b8e2bea5511        PP          115
    #> 443 uuid:dddce80d-7dc2-4991-b0f2-38988f662fe0        PS           97
    #> 444 uuid:5a724b8c-1f49-49af-aa09-32093d8ad5e8        PP          116
    #> 445 uuid:274f6588-4880-4d4a-ba56-c556457b22b6        PS           97
    #> 446 uuid:7efd2b47-5110-4743-a2e0-2d4b66d4d947        PP          116
    #> 447 uuid:8b2f13fd-f2bb-4120-a907-59c87f097676        PS           97
    #> 448 uuid:ff7f342b-1c97-4df3-8e8b-ab5f85c95165        PP          116
    #> 449 uuid:d7c9acdb-5975-4cdd-8e76-710e1f06a58d        PS           97
    #> 450 uuid:002b1196-66f6-42e7-bcbc-6120947ddaef        PP          116
    #> 451 uuid:fc78fc0c-e412-4471-a3f9-9a50f495c27a        PS           98
    #> 452 uuid:6421b0e1-243b-4ff8-b17f-4956e05ee8dd        PS           98
    #> 453 uuid:77807a2b-5521-4834-839a-d902364bbb87        PP          116
    #> 454 uuid:c727bf32-0a93-4218-87c0-dc9c98ff4fdd        PS           98
    #> 455 uuid:9fae1354-1a4c-4c6a-9b83-d81945a35e8b        PP          117
    #> 456 uuid:eca5793a-c82e-4475-86ff-55f554592101        PS           98
    #> 457 uuid:408c59ee-8dc4-4fce-986a-4db906f14d6f        PP          117
    #> 458 uuid:ab29905e-448a-4b94-a503-7c52f8965b89        PS           98
    #> 459 uuid:0f89e9f0-8a67-45a6-99e2-36d9e1ac63b6        PP          114
    #> 460 uuid:ce02d400-17d2-48e5-94ad-21165b0354e9        PS           98
    #> 461 uuid:f3bed921-e286-4b57-aa7a-f9fbc828fb91        PP          117
    #> 462 uuid:8995fdcf-97b7-48df-aeb0-8eebca6fd043        PS           98
    #> 463 uuid:26443fe1-6bc5-4abe-8fd7-c62a4b62a467        PP          117
    #> 464 uuid:101e1ed4-1f7e-48d9-a044-78d3de4938fe        PS           98
    #> 465 uuid:2d311242-ae46-4535-a52b-24a06777bbf4        PP          117
    #> 466 uuid:2d66613a-1290-435f-9e2c-e2c879223ece        PS           98
    #> 467 uuid:d3b31f0f-43e4-4493-b6cd-ad3a9655f78f        PP          117
    #> 468 uuid:58fdd777-dc58-451f-96c6-8b5c78c79894        PS           98
    #> 469 uuid:411fd5e6-6f40-49ca-8384-109ae879186c        PP          117
    #> 470 uuid:f90448d0-e1c7-424b-9d58-77e0fa5ebeaf        PS           98
    #> 471 uuid:22de2851-c231-4cba-81c2-c2252673cd77        PP          117
    #> 472 uuid:1b574d30-21e1-490a-a83b-216c9769d30e        PS           98
    #> 473 uuid:4592d8dc-cba9-4f28-8b28-cb6e23e76e39        PP          117
    #> 474 uuid:af50bbac-5784-4da0-a55d-3ff1cb7b31bc        PS           98
    #> 475 uuid:f94889aa-050e-4279-805e-ec17409b43e9        PP          117
    #> 476 uuid:ec984568-d806-426f-a1aa-00c14bb234ce        PP          117
    #> 477 uuid:506e23d3-8598-46d8-b581-314a41ed148b        PS           98
    #> 478 uuid:0db49f34-7038-41d8-a46d-5e344e2353f6        PP          117
    #> 479 uuid:9ab579f3-fd2b-47b9-a9a3-1ba8c217b7d2        PS           98
    #> 480 uuid:994e87e1-073e-4e53-a777-95055b1d509d        PP          117
    #> 481 uuid:0e3f4718-a8cd-44bd-8bbe-42d758176034        PS           98
    #> 482 uuid:cdeaee97-5214-4d70-b271-faa1cf2b5cb4        PP          117
    #> 483 uuid:9b13b29f-fdfe-4b5d-aff6-5ce8e97af55c        PS           98
    #> 484 uuid:1cd8d126-ff49-4de6-8c8d-1b59b279c943        PP          117
    #> 485 uuid:22b1cc4f-847b-4e17-8575-6f4c74dad055        PS           98
    #> 486 uuid:a5cb798b-718c-4030-9597-5dab429be7bd        PP          117
    #> 487 uuid:7af2eb73-896d-4058-bd83-d4554fe4c2d9        PS           98
    #> 488 uuid:b47e8e3e-f87f-41b0-8adf-3acacdd32d5f        PP          104
    #> 489 uuid:07396706-897a-48c9-84fe-4575bb6e26bd        PP          117
    #> 490 uuid:d2b6c27f-97fc-4da6-a9a7-e114ca60cdd6        PS           98
    #> 491 uuid:87abd921-ce92-4310-8623-b42801c8d719        PP          118
    #> 492 uuid:e7d26c1f-7ddb-4ced-80e9-dc5666bd6d89        PS           99
    #> 493 uuid:dc6c9a33-16d7-4978-9829-6e5cd62fcf28        PP          118
    #> 494 uuid:f8b548c7-b073-4296-ad64-ec680285bdde        PS           98
    #> 495 uuid:8f0b94bc-9a31-4d11-a159-d75fa2374d18        PS           99
    #> 496 uuid:22a4a429-66f1-4e63-80bb-4e58a5c69576        PP          118
    #> 497 uuid:5e4eca3e-555b-4641-a830-e147db5b6986        PS           99
    #> 498 uuid:67a38b3e-69e7-4ecc-b33c-8ac5aed1ff45        PP          118
    #> 499 uuid:e566b3eb-128a-40f5-b659-69607bafce8e        PS           99
    #> 500 uuid:6d72b529-cac6-4e84-8a70-4fe9f78ef818        PP          119
    #> 501 uuid:2d22f220-487f-4b0a-b6f8-57807acf7772        PS           99
    #> 502 uuid:5551b0f7-4e6a-45c2-975a-4466d1551e3a        PP          119
    #> 503 uuid:c4ba02e3-7db7-4c6e-b1c8-bf9e6ab7564b        PS           99
    #> 504 uuid:55e38486-e3b4-4777-a956-cde31f6d548b        PP          119
    #> 505 uuid:7af9cd4d-8a31-42ba-9d4e-c3e2e52317c5        PS           99
    #> 506 uuid:699b4986-0cad-4a31-bc0a-c873117da5ba        PP          119
    #> 507 uuid:b8aee4c6-bd6b-4823-b5a9-f5d96657c606        PS           99
    #> 508 uuid:95d70fc5-1b78-423b-8085-f5930ba5ad56        PP          119
    #> 509 uuid:75bcc9a5-2c20-44b4-a439-a32e860c9113        PS           99
    #> 510 uuid:31b0f9db-5bda-4dd0-8dfd-9b5abf7c08bd        PS           99
    #> 511 uuid:0405252c-b0b9-4f5c-a115-27d13ace99a0        PP          119
    #> 512 uuid:224582fe-0f65-423e-9272-306259072b34        PS           99
    #> 513 uuid:5c145eb4-4e7d-4cb8-a156-a7ab704d8b9e        PP          119
    #> 514 uuid:92aa5c92-bfa0-4bd9-b8c4-c64da89fc52c        PS           99
    #> 515 uuid:ad20f772-d10b-4622-820b-2b6120fb1f0f        PP          119
    #> 516 uuid:75b46fc5-1a26-4605-ad72-e303165fab70        PS           99
    #> 517 uuid:47285dd3-fffb-40b1-ad26-fc734c04b473        PP           12
    #> 518 uuid:ecb0a759-c164-4627-9661-3e95f6ed32ec        PS           99
    #> 519 uuid:a76373bb-4748-42e1-96c2-cffedb56d856        PP           12
    #> 520 uuid:503f3f07-25a4-4a8a-a868-24e9b019096b        PS           99
    #> 521 uuid:6b7bc774-0318-48e2-b508-b04c30679afc        PP           12
    #> 522 uuid:628de99c-f193-486c-9cca-d2fe921ce08e        PS           99
    #> 523 uuid:e2863a17-fa2e-4683-9120-16d29c6b5afb        PP           12
    #> 524 uuid:b055237f-40c2-4d27-ad52-0dc0511bdc48        PS           99
    #> 525 uuid:8f154d3d-c2fd-4f77-a58e-f403ccef6a51        PP           13
    #> 526 uuid:5238c322-b0a8-44a7-8a96-8dd15fb3bf0c        PS           99
    #> 527 uuid:46dba098-f07b-4ee3-87ee-8ad45f9ec5a3        PP           12
    #> 528 uuid:259c0833-2bdc-45fd-bfb6-b097ef2ae197        PS           99
    #> 529 uuid:4126e907-109b-4eda-bf12-7c5557a76a02        PP           12
    #> 530 uuid:02de1da3-4837-4ef2-b193-e66473142c85        PS           99
    #> 531 uuid:bb204542-ef8e-4a1c-ab71-f19c08e92cb6        PP          122
    #> 532 uuid:8dff8111-44c8-4a71-a8dd-f2012253edbf        PS           67
    #> 533 uuid:fefd8845-bfba-4278-8845-def169945a5d        PP          126
    #> 534 uuid:b3fb4260-3bae-498a-83a1-d2b277840b74        PS           63
    #> 535 uuid:b535e156-9797-4d5f-a420-c27254992b9b        PP          126
    #> 536 uuid:d0f66bda-cef5-4f64-920d-46a6f960cbdc        PP          127
    #> 537 uuid:58843bc7-4654-49d9-b643-319dd6f805c2        PP          131
    #> 538 uuid:2e057ffb-6705-4211-b264-12931c8ea327        PP          131
    #> 539 uuid:3a03553e-446e-4b4f-98ba-8a9f3048e3a8        PP          133
    #> 540 uuid:9da83cd5-51ac-4099-99ad-83ca005b46d1        PP          133
    #> 541 uuid:c5b34c80-7b73-4c37-aa4b-960c7c37b918        PP          134
    #> 542 uuid:7ed84234-5a54-42fd-9d4a-8632cba85828        PP          134
    #> 543 uuid:08304eb1-2723-4bda-8235-296c4007acc3        PP           61
    #> 544 uuid:29c2c34a-aecd-4f51-b585-e9e5210ab381        PP          135
    #> 545 uuid:ca9d0981-0984-4c00-8273-c125a2d8f2c8        PP          142
    #> 546 uuid:7305a1fd-fd3a-4023-8218-ac24fa6035a1        PP          142
    #> 547 uuid:b59d7fbc-8e3f-4614-9927-ba251847fb05        PP          142
    #> 548 uuid:30cd28be-082e-474f-a108-7840d7c628b2  Reserved           NA
    #> 549 uuid:1d6ac813-db96-465c-b871-693591a2f713        PP          143
    #> 550 uuid:44aca6ae-dd2e-4e32-a74f-52af8994c668        PP          143
    #> 551 uuid:f930e2a7-0515-4190-ba29-6a9436c49c60        NA          146
    #> 552 uuid:1eff5c16-3ffa-469f-aed9-15e7d7ed967c        PP          146
    #> 553 uuid:ce22ea64-a30b-438a-93f7-efd953d5d7e5        PP          147
    #> 554 uuid:f23b75a3-6842-4d57-9f59-9be992b820c7        PP          153
    #> 555 uuid:9bd9f417-11f4-478e-8cde-0cdec0d8bfe8        PP          148
    #> 556 uuid:893ad2ab-bf54-4781-aac2-3715ba0d3dbe        PP          148
    #> 557 uuid:7ddbe0ce-cf09-4803-ab77-b75b419fa37e        PP           15
    #> 558 uuid:95e1e23e-d56b-4d72-b5c6-7bfd85f98323        PP           15
    #> 559 uuid:a6becc49-4561-4b71-9d64-8407331708d5        PP           15
    #> 560 uuid:6842380d-d7c2-43af-a0f7-9cac0fa0eba4        PP           15
    #> 561 uuid:da4815b2-2009-419f-bf9b-89a0d1f32ca3        PP           15
    #> 562 uuid:95ce78b0-c4bc-4fc0-8090-c01df62f3e15        PP           15
    #> 563 uuid:b97057e8-0264-45f7-80cd-66c682886ced        PP           15
    #> 564 uuid:0c305dc5-a6de-4fc9-9546-572dd66a2c9c        PP           15
    #> 565 uuid:8987b8de-dd7a-459a-9d7e-27bbd14c2905        PP          150
    #> 566 uuid:f0874a6f-c5ac-44c6-a1d6-396405bcaa5f        PP          150
    #> 567 uuid:81a16a19-53c8-4916-999a-a526aa1e51fb        PP          150
    #> 568 uuid:732798d2-6429-4e30-8557-2360f24a537f        PP          150
    #> 569 uuid:bbef4a5e-706e-4b98-99ce-5c621c23b641        PP          151
    #> 570 uuid:f06d09a5-a0e9-464b-a771-a15a6bb878d8        PP          151
    #> 571 uuid:75185a80-c2af-4592-9b3d-33c36ec4c92f        PP          152
    #> 572 uuid:7035dac2-746d-42d0-b12d-e80ffa70e286        PP          152
    #> 573 uuid:31d56194-92f0-4e45-9389-9aafcdfa76a3        PP          153
    #> 574 uuid:182f4e2f-82f4-410b-babb-866641c55f10        PP          153
    #> 575 uuid:f7c30b44-2253-4274-86fe-f525cf0cb89b        PP          157
    #> 576 uuid:79b797e4-94f6-444f-bb82-d56b164fb2f2        PP          157
    #> 577 uuid:b867c11b-4076-4101-bd34-21e7a5ee224e        PP          157
    #> 578 uuid:50bc41b1-a7ba-4385-bc48-6c7495a973a1        PP          158
    #> 579 uuid:8f51998c-80c2-42b3-8975-ec5e4c4cc10b        PP          158
    #> 580 uuid:2ffcf6d6-fad2-4f36-9a6c-d98e47c01bcb        PS           64
    #> 581 uuid:239c2f5d-0abd-4651-a943-d6080d85369a        PP          159
    #> 582 uuid:f94083d9-f933-49f2-bef8-db06eb3249ae        PP          159
    #> 583 uuid:b2932ff7-f574-4ac6-a744-707677db7eb6        PP          166
    #> 584 uuid:954ea55e-4bfd-4638-adb1-dea45a5f4908        PS           56
    #> 585 uuid:32dce849-c33d-4646-8643-6e52cca6ee30        NA          137
    #> 586 uuid:fe521350-f1db-426d-aaad-7f4d90988a20        PP          174
    #> 587 uuid:89c5049f-e36b-4dbf-9e15-2a8d2cfd916a        NA          137
    #> 588 uuid:57ddb410-e4a5-40dc-ae06-e45da02a89d1        PP          175
    #> 589 uuid:6b431919-df54-4adb-a632-7120f10ef2c4        NA          128
    #> 590 uuid:d9eabcd1-d1d4-4adc-977e-c6556ec9b76e        PP          182
    #> 591 uuid:0684d09b-6e47-4888-96c0-44f523a3d1a9        NA          137
    #> 592 uuid:1d3668f6-3868-4364-8f22-4a5546442843        PP          182
    #> 593 uuid:543fdd2a-059a-431a-8bc1-f423f4ff60c4        PP          199
    #> 594 uuid:fb7084a9-8601-4f3e-b2a4-e9bb03ac2e67        PP          199
    #> 595 uuid:aa71ef85-b6f2-4856-b619-a390a7511612        PP          200
    #> 596 uuid:0bde11f1-197b-416e-9310-8b7600e436c7        PP          200
    #> 597 uuid:4ebf0d5e-863c-4d63-b5e4-4d59d1996a3c        PP          200
    #> 598 uuid:bf836d1c-cb99-4c3d-ae3b-54be73e9f9b6        PP          201
    #> 599 uuid:4ffc0ddc-7699-4709-a516-339228fda686        PP          202
    #> 600 uuid:51e7a903-05a9-460b-a004-6157ab0ff4aa        PP          212
    #> 601 uuid:fe182d5c-079c-4761-bf23-feb61d5c309a        PP          212
    #> 602 uuid:2e3a92e6-8097-4d50-9b64-ae5bcfd17a4a        PP          222
    #> 603 uuid:ff40f5b4-357a-4a01-8f8e-c5377208ace0        PS           55
    #> 604 uuid:2485b0b1-27fb-4202-8bbb-8b23783c3377        PP          222
    #> 605 uuid:d71c708d-379a-4328-af06-8b45fa6193ab        PS           63
    #> 606 uuid:778a72ce-14e3-49b8-ae2c-ca2c13b77a65        PP          224
    #> 607 uuid:a4e765d1-5115-4de9-b9c5-116f6e5255c0        PP          224
    #> 608 uuid:d9d5072c-735a-49f2-9891-112511176c29        PP          228
    #> 609 uuid:c87c505d-a4e2-4181-9acb-a2fb6ee80f63        PP          228
    #> 610 uuid:9e6da3b7-d21e-4481-bf24-0a20bf5d63c9        PP          231
    #> 611 uuid:65493f92-87a5-446a-9e34-c6e44137107d        PS           55
    #> 612 uuid:a3c525d4-3332-4d8d-beb6-f28a846bbe3b        PP          255
    #> 613 uuid:080e7f32-f611-4798-8d1c-969714296996        PP          255
    #> 614 uuid:b1266344-9cac-48ed-9005-dee9c3a2bcc2        PP          259
    #> 615 uuid:8d50b139-eb65-4870-b7d4-ca68394d28ae  Reserved           NA
    #> 616 uuid:970b93a7-b663-4367-be70-71a9495c4027        PP          260
    #> 617 uuid:dd57b698-7e7f-474f-82db-909653b2e87a        PP          260
    #> 618 uuid:7da45821-265a-4fae-adbf-368af705504e        PP          261
    #> 619 uuid:7727efbf-489b-4164-89bb-fecc07de41cc        PS           50
    #> 620 uuid:a26b052b-aa4e-4767-b406-5b5e0f61da28        PP          266
    #> 621 uuid:a6c13b50-98d6-4757-92c9-6b50bfd7f7c0        PP          266
    #> 622 uuid:e1ffcc82-9655-4e5f-acf8-ed363691e611        PP          270
    #> 623 uuid:c88860d0-4ac7-44a3-bd7b-8e1f48dc6785        PP          270
    #> 624 uuid:7ab66a7c-d6b9-4000-8570-2452506af0ad        PP          270
    #> 625 uuid:3dae3808-496b-4655-a830-4a1f7552680f        PP          270
    #> 626 uuid:b38cda59-c371-4c32-b777-37a39c789f74        PP          270
    #> 627 uuid:260ad66d-3e34-41d5-a4c0-7882af9d85e6        PP          272
    #> 628 uuid:80aa67ba-2b20-42fb-84f0-ff8a77beec71        PS           63
    #> 629 uuid:547f2abc-89e6-4407-bc5a-b3f13b1f76a9        PP          273
    #> 630 uuid:6afeadcb-92de-4eef-8d6c-f1363aa27508        PP          273
    #> 631 uuid:6987a23d-c689-46da-b149-7620ea309c94        PP          272
    #> 632 uuid:019394ab-3ca8-489b-ade0-1766eeaf7bba        PP          276
    #> 633 uuid:a4ba2a6a-159a-40ae-b99f-7aa05d691077        PP          285
    #> 634 uuid:78eca7f0-75d9-41aa-8ff4-e736e9902be0        PP          285
    #> 635 uuid:aef4fac6-54be-4700-a40d-775c5ec80486        PP          288
    #> 636 uuid:6e6cdeb1-8663-40b9-ba1d-ab2e0b848e63        PP          288
    #> 637 uuid:bc2b7c94-bc81-46a9-a225-85fe0f3b1a8e        PP           29
    #> 638 uuid:98f9e7e6-8892-4b53-afe8-9e872cc6884c        PP          289
    #> 639 uuid:a8e4a8ed-b198-4141-932d-ae5bb67e8e25        PP          291
    #> 640 uuid:c922dd36-e198-49ae-99ee-ecfb701fb51e        PP          291
    #> 641 uuid:6662c8bb-dd67-40d4-8c65-a47b7a2c2b3e        PP          294
    #> 642 uuid:af959a18-9eeb-4d6f-a502-40a8cc87930e        PP          294
    #> 643 uuid:bbd683d0-b6f2-4d6b-a248-8488b26be203        PP          294
    #> 644 uuid:aa2be7ac-050a-4391-90e3-7da5434dc04d        PP          294
    #> 645 uuid:b288249b-c1f2-4f62-81f5-75fe075c4356        PP           24
    #> 646 uuid:6337782d-2405-43b9-aaf0-dfec5ebd5629        PP           34
    #> 647 uuid:86f987df-f5b8-4c3a-af44-fa6f627d25b5        PP           39
    #> 648 uuid:d443592e-2899-4e33-a324-983b19443df5        PP          105
    #> 649 uuid:368b3a53-77f4-4ed1-b943-24671fbe52d0        PP           39
    #> 650 uuid:e80b9646-b112-4506-b2e9-b00f109e621e        PP          105
    #> 651 uuid:6c798f5b-e5b8-4eb6-8b95-19e75e775cd1        PP           49
    #> 652 uuid:655aa14e-b914-497a-bcdd-c158dc3ce040        PS           53
    #> 653 uuid:7a27a16d-1fb4-44b4-9242-5d6b07714fdf        PP           53
    #> 654 uuid:34919c75-8ddd-46a4-b898-90f51f2c3e6a        PP           53
    #> 655 uuid:f9cb65d5-7284-48d5-aa4c-7bea959539c5        PP           55
    #> 656 uuid:d7789eb5-6de0-40a7-8a8f-54b932da2803        PP           55
    #> 657 uuid:99fed4cf-de28-4f3a-ad68-7bbe7a4e219b        PP           56
    #> 658 uuid:8d3384be-a2b8-4ba4-b7d2-e992557572bf        PP           56
    #> 659 uuid:9e6152f6-235d-4ff6-84fb-d22cc3ffa3fa        PP           59
    #> 660 uuid:8763f734-135f-47b8-b874-0c5c88a09b2b        PS           53
    #> 661 uuid:daffbd7d-ecdd-4c71-be15-2b9d4c27f452        PP           59
    #> 662 uuid:02808c34-bc9b-444f-a39c-6b2893ecfe38        PP           59
    #> 663 uuid:ef2b2319-b6e0-425f-ab3b-a98eb8fd7dd2        PP           65
    #> 664 uuid:854ad7dd-6ac2-4d19-b64d-f7658fe1005d        PP           65
    #> 665 uuid:4bf414b3-7503-447d-b995-cb06848412be        PP           67
    #> 666 uuid:367763e0-d43b-47cf-a9a9-1c107d902a36        PP           67
    #> 667 uuid:b74e8591-c467-4f24-81c8-36ec67b65277        PP           68
    #> 668 uuid:1dbca722-d4ab-49b5-a026-0e381a6abe87        PP           70
    #> 669 uuid:48eea478-32f2-49c5-8eef-e45a09c1becd        PP           76
    #> 670 uuid:34e115d1-e49f-4503-a51b-6571e68136a2        PP           76
    #> 671 uuid:7556862c-1f77-48a2-84ff-0d1ba8d139b6        PP           77
    #> 672 uuid:2ffc9ddd-6af8-446f-a02d-88f47ab52052        PP           77
    #> 673 uuid:11de2c70-af08-47ff-bb0a-67e52523c6e6        PP           77
    #> 674 uuid:0ff31a06-940a-41a1-bc69-778195bc852e        PP           77
    #> 675 uuid:b8df9c64-2421-4549-8fc8-909cb6923d47        NA           86
    #> 676 uuid:e0d7f71b-ca87-438d-bbeb-c73a6944f221        PP           86
    #> 677 uuid:3ab3d079-bf02-49b5-bbac-bbe367896969        PP           88
    #> 678 uuid:ad532969-dbbe-47c7-a266-ba7dcbd0500d        PS           50
    #> 679 uuid:da353f3d-7631-4974-8807-75cb9e505cb5        PP           61
    #> 680 uuid:698afb07-8626-461f-a79b-55eb66d06e71        PP           90
    #> 681 uuid:4fcbbec9-7ae7-4554-a05f-98cb166b32e6        PP           91
    #> 682 uuid:519c394f-6819-4e69-afea-8540d7e9ccd1        PP           91
    #> 683 uuid:5a612ff2-bb39-47e9-81d9-105d8b76b5d2        PP           97
    #> 684 uuid:44092c7c-c880-4795-b313-8654edd2f254        PS            1
    #> 685 uuid:cef0b6bc-fbd1-4ed8-99f1-19027539dd1b        PS            1
    #> 686 uuid:601a27b5-d10f-4ef5-aa04-6e717547985f        PS            1
    #> 687 uuid:2ef5987d-4f8c-456d-b9c6-6eacc223214d        PS           63
    #> 688 uuid:fc60bba0-05d3-4941-aebe-1b598eddff3b        PS          108
    #> 689 uuid:e553cd16-8e2b-4a33-a11d-6d29be08fd2a        PS           11
    #> 690 uuid:cad0b07d-030e-4e8b-a32a-27bbe560ff04        PS           11
    #> 691 uuid:0e49b3db-abe8-42d6-998b-62d459b07968        PS          110
    #> 692 uuid:6d2e6d34-89d7-4ce9-a131-f5aa03ebb213        PS          110
    #> 693 uuid:eb999aaf-dd5c-4736-a28b-55873a829623        PS           67
    #> 694 uuid:19e218a3-4412-4633-87da-7a2c812ded12        PS          112
    #> 695 uuid:d8dd076c-5503-465d-b293-a71e4f7ea157        PS           55
    #> 696 uuid:fc3053aa-69a1-4195-a479-de8d457ec388        PS          112
    #> 697 uuid:8bffdbdc-6acb-4731-aea8-66147000a3e3        PS          117
    #> 698 uuid:383be30d-2d98-4597-89ba-37f18454b900        PS          117
    #> 699 uuid:50664a8a-74bc-4c72-a619-d84287cf47e9        PS          117
    #> 700 uuid:e8b3acbe-77dd-4e6e-ad53-7cd81e203ac9        PS          118
    #> 701 uuid:e1a87d71-0c8e-439c-92bf-b5c7f36d6428        PS          120
    #> 702 uuid:01efffb3-56bc-455e-914e-517d8be5090e        PS          120
    #> 703 uuid:bee6ec58-9746-4cc4-aaa9-0521b44bff70        PS          121
    #> 704 uuid:8716e322-c3c3-40b5-bcf5-0bb1bff2df1c        PS          121
    #> 705 uuid:1407bdd3-9481-4aef-b2db-8550a739ee64        NA          187
    #> 706 uuid:dc208a8b-93d8-4476-b329-32e679eeb05e        NA          187
    #> 707 uuid:0937114e-734b-4913-9526-1532a028288c        NA          188
    #> 708 uuid:ad173511-a603-4c6d-b500-b7a592f9321c        NA          188
    #> 709 uuid:7b5f5120-61f7-44cd-84b2-7d2acc0d90d2        PP           70
    #> 710 uuid:9ab59114-d64e-4b99-92ad-d382f5058fbf        PP           70
    #> 711 uuid:60bc9ac4-ab36-4add-ac6b-3111ca69fafb        PP           70
    #> 712 uuid:60bce745-eeed-4589-a674-8edac76188ae        PP           70
    #> 713 uuid:58cfff0f-ad41-46ce-a2dc-ae30f53a345c        PP          280
    #> 714 uuid:3e109d60-1d7b-4356-bbc3-3cd9c35ac747        PP          280
    #> 715 uuid:1584d565-af50-4a09-9ff0-af63e2885fa9        PP          280
    #> 716 uuid:1e74523d-f35d-484f-9275-ccb25806dd4f        PS           51
    #> 717 uuid:69350b02-efea-4897-8691-7f8ad50a8d2d        PS           67
    #> 718 uuid:53ee6ee1-f380-48a3-869d-6a96b96801f9        PS           67
    #> 719 uuid:4dc72b7e-dfbd-4332-bdaa-10577e098a71        PS           67
    #> 720 uuid:8a3fa57a-604b-46ba-a5ff-2fd97c3790b6        PS           67
    #> 721 uuid:b82617e8-5539-41bf-8443-47012bd82c01        PS           67
    #> 722 uuid:2b08e393-5450-474a-b398-7ada5eb0b1e6        PS          122
    #> 723 uuid:a184de5d-5369-49ca-932c-28db7d351a20        PS          124
    #> 724 uuid:2fb4c02f-f994-42f4-8842-b1eeacb334bc        PS          124
    #> 725 uuid:ef3a4252-4048-446d-87dd-5dd8379aa2b9        PS          128
    #> 726 uuid:da2cdfa8-e9fb-4e4a-ac03-ae1b373e47db        PS          128
    #> 727 uuid:a6fa7728-a03d-4532-b2df-106bb771099f        PS          130
    #> 728 uuid:93a37ada-f3ca-41fc-ad7d-58ce9555d112        PS          130
    #> 729 uuid:baae9845-7771-408c-8742-3427ee98530f        PS           19
    #> 730 uuid:006f2bfb-6d8a-4213-a1ad-742cf7d7b04e        PS           63
    #> 731 uuid:b36e6e18-142f-4df7-900c-05943e78ad81        PS            2
    #> 732 uuid:9e670c8d-fab3-49a6-af58-351b9f23f35d        PS            2
    #> 733 uuid:5a8545fc-cf7d-4402-9d33-6c9cbbf7319c        PS           23
    #> 734 uuid:fee64b5f-b31c-40b5-9ca6-478a0d0dd9ee        PS           23
    #> 735 uuid:1df2eb25-d8db-4597-a114-48b2608a3215        PS           25
    #> 736 uuid:6f0f5532-16aa-41da-8227-68d6c43a2aba        PS           28
    #> 737 uuid:8be9935f-bf5e-4a7f-b490-f571dd1b9ace        PS           25
    #> 738 uuid:a1076f28-d689-46ba-8b83-b2ade19dc2cd        PS           25
    #> 739 uuid:dbc6d758-981d-4436-ba72-8614b6db17f5        PS           25
    #> 740 uuid:a1cca49f-2de9-422b-8f31-984c3ce575c8        PS           26
    #> 741 uuid:4129f608-ea01-4bab-8ebd-0a88cd80f978        PS           26
    #> 742 uuid:c9afb079-070b-4aa1-8390-c2e3457fdd6b        PS           26
    #> 743 uuid:eee9774c-de52-474c-bc04-0bc0b5f23703        PS           26
    #> 744 uuid:93d7fc00-ee32-4d6a-9cea-4ee87e915203        PS           26
    #> 745 uuid:a58f6aa5-30c2-4ad5-b648-0e9a9cc7fc02        PP           61
    #> 746 uuid:311c4cb4-0b85-405d-81e7-eeca871057f0        PS           28
    #> 747 uuid:961b0d24-f886-405b-a9ab-f4f892730489        PS           28
    #> 748 uuid:499871e9-6907-42f4-bd10-2b6d4ed0b671        PS           29
    #> 749 uuid:56680b5a-20b6-4abe-abad-c014b0120021        PS           33
    #> 750 uuid:42220147-e210-4fe9-9441-67e8c4aa2f65        PS           51
    #> 751 uuid:cb904c62-f9d4-466b-aff6-4fb39ffbdbc1        PS           40
    #> 752 uuid:5199406d-050d-4c98-aee0-ea2773a2c01b        PS           40
    #> 753 uuid:7340af0e-71b5-4a5f-a5ca-b17ae27431f4        PS           47
    #> 754 uuid:40bad924-2c29-45f3-9530-b55c7be0d13c        PS           48
    #> 755 uuid:b120a181-324b-4eca-8ddb-e9638ddb490e        PS           49
    #> 756 uuid:5f303810-76ca-4e7e-a3fb-1026bdd5ba6c        PS            5
    #> 757 uuid:3506a378-ddf2-47cc-b77e-1ebe859f0075        PS            5
    #> 758 uuid:8a0841a4-2b64-447d-a669-d59b05265993        PS           52
    #> 759 uuid:7226a4e2-a3f9-494c-b2bd-5bd864fd755c        PS           57
    #> 760 uuid:3e095823-ebc4-4df4-8086-9899946bc5f7        PS           48
    #> 761 uuid:6b5d1396-f508-40e0-bf6e-234e927f778f        PS           53
    #> 762 uuid:d9489429-3e0f-4ae3-b0aa-8a812e3faf60        PS           54
    #> 763 uuid:ff7ddf86-2593-44e3-8dc2-098675b94544        PS           55
    #> 764 uuid:c77988f1-068c-4d04-a038-6fd8ac8fd979        PS           59
    #> 765 uuid:031bcd94-97f0-4da9-8895-9286a3e52cf7        PS           59
    #> 766 uuid:98d95fd4-49a7-46bb-8765-1c1849dc0295        PS           55
    #> 767 uuid:95074fe8-2a1a-49f2-bec4-c91d611ef2b0        PS           59
    #> 768 uuid:ac3beaa7-9ea7-48f8-bb78-576a69f1a67f        PP           61
    #> 769 uuid:ac599ddb-d31d-4dd7-82a0-db47dbfae913        PS           60
    #> 770 uuid:ade54caf-35c1-4828-b479-845a543f1c90        PS           62
    #> 771 uuid:dcf2f2bc-097a-41d9-99cf-f2fd1d4d83f3        PS           62
    #> 772 uuid:2e89edd6-9d57-4d2e-813a-2cebd7c03483        PS           62
    #> 773 uuid:7f62ce10-3fad-47d9-b0ce-844a6c44b3cc        PS           65
    #> 774 uuid:7653cd60-0c64-4add-81c3-b157a0291961        PS           65
    #> 775 uuid:cf206f66-626e-41d3-9043-24bb7986739a        PS           68
    #> 776 uuid:ae48a9dc-00ad-404a-9a05-18ef2f0a3bc6        PS           68
    #> 777 uuid:dd562e62-c5cf-482f-a31d-8e7d51d47f39        PS           54
    #> 778 uuid:282436d8-1e7f-47d4-9b66-60dd4dd11f20        PS           69
    #> 779 uuid:49e95e91-8ab3-4445-96b4-8ef6e056d825        PS            7
    #> 780 uuid:720c8314-04b6-4e58-8806-ad21f306454b        PS            7
    #> 781 uuid:5da22a6b-c473-4116-aaac-3e60b01384d2        PS           55
    #> 782 uuid:b7741bc0-b8a3-4f36-addb-5d0eb83d33ee        PS           70
    #> 783 uuid:f7977091-9a9d-467b-ad15-cf7e04d6ae5f        PS           59
    #> 784 uuid:2be374d6-ca21-4283-aa4c-ee6f0b70d82d        PS           71
    #>                          candidate_name   uid          cnic
    #> 1                           Hamid Iqbal 00014 6110111313447
    #> 2                       Amad Ullah Awan 00014 3120284749857
    #> 3                        Afifa Saddique 00034 3310087540799
    #> 4                       Farman Ali Khan 00034 3310092607361
    #> 5                   Muhammad Asim Nazir 00040 3310066683117
    #> 6                  Muhammad Azeem Aslam 00040 3310443610075
    #> 7                     Amad U Allah Awan 00041 3120284749857
    #> 8                       Amad Ullah Awan 00041 3120284749857
    #> 9                       Amad Ullha Awan 00041 3120284749857
    #> 10                   Amadeus Ullah Awam 00041 3120284749857
    #> 11                       Amadullah Awan 00041 3120284545555
    #> 12                        Amadullahawan 00041 3120284749857
    #> 13                         Furqan Saeed 00165 3330334368085
    #> 14                      Muhammad Furqan 00165 4130225685079
    #> 15                    M. Ajmal Siddique 00169 3330345260283
    #> 16                           Abrar Azam 00169 3110385472859
    #> 17                            M. Tareej 00174 3330347495477
    #> 18                    Abdul Sattar Khan 00174 4420298881481
    #> 19                           Nawaz Khan 00275 1320218236479
    #> 20                           Liaqat Ali 00275 3540403938467
    #> 21                  Muhammad Saeed Virk 00279 3540107631253
    #> 22                        Muhammad Syed 00279 4250178317185
    #> 23                       Sumaira Noreen 00326 3520242866724
    #> 24                       Raheel Hussain 00326 3520180925675
    #> 25                             Asif Ali 00495 3520218372130
    #> 26                                 Asif 00495 3520218372130
    #> 27                         Anis Qureshi 00499 9999999999999
    #> 28                  Dawood Anis Qureshi 00499 3520221522343
    #> 29                          Raiz Ul Haq 00569 3530219817439
    #> 30            Mian Mumtaz Ahmad Matyana 00569 3110283813905
    #> 31                            M.Hussain 00593 3510250024613
    #> 32                        Noor Ul Ameen 00593 3530181678807
    #> 33                           Sardar Ali 00595 3530120113119
    #> 34                           Sarder Ali 00595 1730184756427
    #> 35                    Shaukat Ali Bajwa 00620 3640108595153
    #> 36                         Waseem Zafar 00620 3520272169519
    #> 37        Syed Tanvir Ul Hassan Gillani 00740 3630212066609
    #> 38               Sayed Ali Moosa Gilani 00740 3520126954701
    #> 39                          Allah Nawaz 00750 3630476567089
    #> 40                          Ameerahmend 00750 4250152730333
    #> 41                 Nawab Amanullah Khan 00791 3630243048821
    #> 42                Nawab Amanullaha Khan 00791 3620242048821
    #> 43           Ch.Muhammad Akmal Siddique 00954 3130103394171
    #> 44                          Abdul Hamid 00954 4240127097801
    #> 45                  Muhammad Ahsan Abid 00972 3130171948879
    #> 46                         Zulfiqar Ali 00972 4410703163689
    #> 47                      Muhammad Kashif 01036 3230471936017
    #> 48                       Muhammad Naeem 01036 3230415516999
    #> 49                         Qayoom Nawaz 01042 3230498145989
    #> 50                          Rana Mustiq 01042 3230450595887
    #> 51                       Muhammad Bilal 01104 3520171156571
    #> 52                              M Bilal 01104 4250141315313
    #> 53                     Shehzad Ali Khan 01123 1620235394869
    #> 54         Sardar Ameer Badshah Qasrani 01123 3210323975735
    #> 55                         Abdul Rehman 01141 3210220990739
    #> 56                         Abdul Rehman 01141 3210286640313
    #> 57                 Dr Mian Abdul Rehman 01141 3210286640313
    #> 58                       Parveen Akhtar 01149 3210243408290
    #> 59                       Parveen Akthar 01149 3210243408290
    #> 60               Ali Ahmad Khan Leghari 01161 3520242198033
    #> 61                            Ali Ahmad 01161 3520267882385
    #> 62                          Syed Haroon 01189 3230253934111
    #> 63                  Shamsher Ali Mazari 01189 3520184166637
    #> 64                    Shoaib Khan Khoso 01217 4550416905489
    #> 65                          Shoaib Khan 01217 8888888888888
    #> 66                         Noveed Iqbal 01247 1560204673101
    #> 67                        Naveed Zamurd 01247 3740268066422
    #> 68                                 Faiz 01294 4130237344955
    #> 69                        Faiz Muhammad 01294 4230198503801
    #> 70                        Faiz Muhammad 01294 4130710713457
    #> 71                      Abdul Haq Alias 01304 4510146040719
    #> 72                          Abdul Haque 01304 4410498381949
    #> 73                 Abdul Razzaque Mahar 01315 4510214397443
    #> 74                       Abdul Razzaque 01315 4130557129437
    #> 75                           Hazoor Bux 01347 4130145859109
    #> 76                         Hazoor Buksh 01347 4550411092212
    #> 77                     Abdul Aleem Khan 01369 9999999999999
    #> 78                      Syed Asad Abbas 01369 4520378562105
    #> 79                 Farooq Tahir Chishti 01371 9999999999999
    #> 80                       Syed Nawaz Ali 01371 4520308205261
    #> 81                          Frzana Butt 01372 3520299143066
    #> 82                         Syed Parvaiz 01372 4230192688975
    #> 83                  Humayun Akhtar Khan 01373 3520116923329
    #> 84                   Syed Qaim Ali Shah 01373 4230384633667
    #> 85                  Iftikhar Shahid Adv 01374 3520296866923
    #> 86                           Veeru Maal 01374 4520318481615
    #> 87                         Kishwar Bano 01375 3520230485474
    #> 88                            Abdul Haq 01375 4520643564923
    #> 89                   M Tajjamul Hussain 01376 3520115829679
    #> 90                           Ghulam Ali 01376 4520697733639
    #> 91                             M Yousaf 01377 3840158406069
    #> 92                       Ghulam Mustafa 01377 4520674472307
    #> 93                 Sardar Ayaz Siddiqui 01378 3520226195201
    #> 94                          Ismail Shah 01378 4230193361273
    #> 95                  Sohail Shoukat Butt 01379 3520125764851
    #> 96                           Munwar Ali 01379 4520396405839
    #> 97                          I Ayatullah 01390 4430283137293
    #> 98                         Inayat Ullah 01390 4520415440319
    #> 99                         Amjad Masood 01394 9999999999999
    #> 100                            M.Ramzan 01394 4520203166847
    #> 101                    Hafeez Ur Rehman 01395 3520127886985
    #> 102                    Mukhtiar Hussain 01395 4520266097671
    #> 103                       Haroon Khwaja 01396 3520116751493
    #> 104                        Naimat Ullah 01396 4520415439963
    #> 105                Khawaja Ahmed Hassan 01397 3520230222061
    #> 106                      Syed Fahad Ali 01397 4520351240345
    #> 107                       Liaqat Bloach 01398 6110152868129
    #> 108                Syed Javeed Ali Shah 01398 6110152471235
    #> 109                     M Mumtaz Khalid 01400 3520248685935
    #> 110                   Syed Muharram Ali 01400 4230112546117
    #> 111                       M Yameen Awan 01401 3520223148165
    #> 112                 Abdul Ghaffar Alias 01401 4530278910227
    #> 113                     Saad Bin Hassan 01402 3520254550759
    #> 114                        Abdul Sattar 01402 4230106601065
    #> 115                     Shafqat Mahmood 01403 3520177273687
    #> 116                    Allalahando Shah 01403 4230107906385
    #> 117                        Asim Mahmood 01404 3520263350717
    #> 118                           Ameer Bux 01404 8888888888888
    #> 119             Faheem Ahmad Khan Lodhi 01405 3520124673781
    #> 120                          Asghar Ali 01405 4530221811559
    #> 121                 Hamayon Akhtar Khan 01406 3520116923329
    #> 122                     Ghazala Hussain 01406 4530262946774
    #> 123                          Jwad Ahmed 01408 3520268953227
    #> 124                       Khalid Masood 01408 4540269427893
    #> 125                 Khwaja Saad Rafique 01409 3520229866367
    #> 126                         Maqsood Ali 01409 4220129168135
    #> 127              Mian Zahid Islam Anjum 01410 3520116646707
    #> 128                      Mehmooda Begum 01410 4550466404330
    #> 129                      Noor Ul Rehman 01441 4220135924319
    #> 130                   M.Ikhtiar Hussain 01441 4530207757901
    #> 131                       Khaleel Ahmad 01446 3210209002373
    #> 132                         M.Qadir Bux 01446 4130614392399
    #> 133                             M.Qasim 01475 4420567645905
    #> 134                      Mumtaz Hussain 01475 4420383219817
    #> 135                       Ghulam Jaffar 01510 4420218088419
    #> 136                Ghulam Jaffar Junejo 01510 4420218088419
    #> 137              Per Aftab Hussain Shah 01516 8130238079213
    #> 138       Pir Aftab Hussain Shah Jelani 01516 8130238079213
    #> 139                    Shoaib Alam Khan 01537 1610253271611
    #> 140                         Shoaib Khan 01537 1610234576395
    #> 141                     M Younis Talpur 01543 4130685846907
    #> 142               Nawab M Yousaf Talpur 01543 8888888888888
    #> 143                  Arbab Anwer Jabbar 01553 4430283137293
    #> 144                            M Ramzan 01553 4430123630887
    #> 145                         Anwer Jabar 01553 4430283137293
    #> 146                         Arbab Anwer 01553 4430283137293
    #> 147          Arbab Togachi Fawad Razzak 01556 4430215770411
    #> 148                 Arbab Togachi Fawad 01556 4430218770411
    #> 149                        Kanwar Kumar 01560 4430391449923
    #> 150                      Kangaroo Kumar 01560 4430391449923
    #> 151      Makhdoom Fazal Hussain Qureshi 01568 4130168696387
    #> 152              Makhdoom Fazal Hussain 01568 4130168696387
    #> 153            Makhdoom Jameel Uz Zaman 01569 4230107181431
    #> 154             Makhdoom Jamil Uz Zaman 01569 4230107181431
    #> 155                       Altar Hussain 01578 4130785124618
    #> 156                       Altaf Hussain 01578 4130320150969
    #> 157                Pir Ghulam Nabi Shah 01586 4130398021727
    #> 158             Pir Ghulam Namibia Shah 01586 4130398021727
    #> 159                 Syed Ali Nawaz Shah 01588 8888888888888
    #> 160                      Aki Nawaz Shah 01588 4200082488213
    #> 161                         Khawand Bux 01595 4130315142423
    #> 162                  Khawand Bux Ghulam 01595 4130315142423
    #> 163                     Syed Azhar Abbs 01722 4250191329017
    #> 164              Syed Zahid Abbas Naqvi 01722 4220189135463
    #> 165                           Shahjahan 01786 4220113936129
    #> 166                   Syed Asif Hasnain 01786 4210141604663
    #> 167                  M Muzammil Qureshi 01828 4220104854095
    #> 168           Muhammad Muzammil Qureshi 01828 4220104854095
    #> 169               Muzammil Iqbal Hashmi 01830 3410125307923
    #> 170         Mian Vaqar Ahmad Pagaanwala 01830 6110169809055
    #> 171                             M Jamil 01866 4220106816075
    #> 172                      M Shafiq Paper 01866 4220126630445
    #> 173             Zulifqar Ali Qaim Khani 01872 4230107213549
    #> 174                 Abdul Shakoor Shaad 01872 4230124743733
    #> 175                      M. Salman Khan 01919 4230148403587
    #> 176                           M. Salman 01919 4230148403587
    #> 177                      Abdull Gahffar 02086 5610180255215
    #> 178                        Abdul Gaffar 02086 4220126221911
    #> 179                           Allah Dad 02089 5530297965283
    #> 180                            Allahdad 02089 4410746920319
    #> 181                         Mir Hussain 02155 5320113733023
    #> 182                    Muhammad Hussain 02155 5540314106631
    #> 183                     Bismillah Kakar 02184 5420262617867
    #> 184                     Busmillah Kakar 02184 9999999999999
    #> 185                     Arbab Amir Ayub 02365 1730115327497
    #> 186                          Arbab Amir 02365 4430218659051
    #> 187                             Ibrahim 02550 1560203445751
    #> 188                        Salah Ud Din 02550 2110499532243
    #> 189                         Maroof Khan 02611 9999999999999
    #> 190                         Ushair Khan 02611 4130410753195
    #> 191                       Jawad Hussain 02703 2160344084837
    #> 192                     Ghulam Muhammad 02703 5440090351209
    #> 193                    Malik Amin Aslam 02913 6110119093557
    #> 194                     Malik Mir Afzal 02913 3710116857051
    #> 195            Ch.Muhammad Zahid Farooq 03088 3420204648943
    #> 196                   Muhammad Ilyas Ch 03088 3520258859543
    #> 197                  Chaduary Abid Raza 03089 3420241959731
    #> 198                 Chaudhary Abid Raza 03089 3420241959731
    #> 199          Syed Mubashar Hussain Shah 03089 3420312887539
    #> 200                     Muhammad Fayyaz 03184 3410423503499
    #> 201             Muhammad Fayyaz Chattha 03184 3520014017353
    #> 202               Hamza Muteen Quraishi 03330 3840319054605
    #> 203                           Jamal Din 03330 3840321181271
    #> 204                  Hafiz Farhan Ahmed 03345 3840367932207
    #> 205                   Hafiz Talha Saeed 03345 3520228341335
    #> 206               Sheer Muhammad Gondal 03382 3440226254587
    #> 207                         Ameer Ullah 03382 3830118713275
    #> 208                   Farooq Ahmad Khan 03402 3810390717675
    #> 209                 Farrukh Hassan Khan 03402 3818313025101
    #> 210                  Zain Ejaz Ali Khan 03412 3810158176311
    #> 211                       Ejaz Ali Khan 03412 3810131382251
    #> 212                 Muhammad Afzal Khan 03415 3810106138803
    #> 213                      Muhammad Amjad 03415 9999999999999
    #> 214                        Abdul Raheem 03433 5650238439071
    #> 215                       Hameed Ahamad 03433 5640183364547
    #> 216                     Jala Ahmad Khan 03435 5630484926511
    #> 217                          Jalil Khan 03435 5630414313527
    #> 218                              A Ahad 03715 5440183009059
    #> 219                   Abdul Naeem Nasir 03715 5430320440065
    #> 220                          Barkat Ali 03760 5440004470637
    #> 221                         Barkat Khan 03760 5440027685693
    #> 222                            Wali Jan 03777 5440012842457
    #> 223                        Wilayat Khan 03777 5440005597055
    #> 224                Wali Muhammad Turabi 03778 5440054166165
    #> 225                       Wali Muhammad 03778 6110157436703
    #> 226                                 Esa 03909 5340314082341
    #> 227                                Essa 03909 5440005378917
    #> 228                      Zafrullah Khan 04002 9999999999999
    #> 229                 Mir Zafarullah Khan 04002 4410101580343
    #> 230                          Barket Ali 04191 5220410571723
    #> 231                              Bushra 04191 5220338016944
    #> 232                   Syed Taimoor Shah 04200 5440044048779
    #> 233               Syed Taimoor Ali Shah 04200 4220125040031
    #> 234                           Nawaz Ali 04222 5150353998463
    #> 235                           Nawab Ali 04222 4410156082903
    #> 236                             M Sabir 04245 5630193184679
    #> 237                             M Zahid 04245 8888888888888
    #> 238                     Israrullah Zaib 04351 5520216889723
    #> 239                        Abdul Gaffar 04351 1570116989403
    #> 240                      Muhammad Owais 04375 4220188003141
    #> 241                Abdul Wali Khan Abid 04375 1520106965539
    #> 242                         Abdul Wahid 04393 1570181212289
    #> 243                        Muhammad Ali 04393 1570368084493
    #> 244                    Bakht Jehan Khan 04491 1510182894887
    #> 245                     Bakhtjehan Khan 04491 1510132690077
    #> 246                       Rasheed Ahmad 04518 1550156861199
    #> 247                        Rashid Ahmad 04518 4130480810277
    #> 248                        Naseer U Din 04558 1340225578869
    #> 249                       Naseer Ud Din 04558 1340259052413
    #> 250               Syed Mazhar Ali Qasim 04604 1350149162469
    #> 251                 Malik Naveed Sarwar 04604 1310101815133
    #> 252                      Shahzad Haider 04661 1350310987803
    #> 253                      Farzana Ramzan 04661 9999999999999
    #> 254                   Inayat Ullah Khan 04772 1310160382629
    #> 255                   Malik Adeel Iqbal 04772 1330108517617
    #> 256                       Mohammad Ayaz 04829 1620178213737
    #> 257                        Shehram Khan 04829 1620205400595
    #> 258                          Maaz Ullah 04921 1710216476741
    #> 259                          Maza Ullah 04921 1710216476241
    #> 260                           Khan Sher 05010 1720184002017
    #> 261                      Muhammad Akram 05010 1730134300809
    #> 262                Arbab Saif Ur Haider 05136 1730105679365
    #> 263                      Atif Ur Rehman 05136 1730191716499
    #> 264                          Ajmal Khan 05160 1730114781667
    #> 265                         Arshad Khan 05160 1730135078501
    #> 266                    Irfan Ullah Khan 05324 1110109549245
    #> 267                         Shah M.Khan 05324 1110195963127
    #> 268                          Zahir Shah 05404 1120140959535
    #> 269                            Ali Khan 05404 1120103554353
    #> 270               Muhammad Dilawar Khan 05436 1210185025111
    #> 271                    Muhammad Dilawar 05436 4130315233951
    #> 272                           Asgir Ali 05532 9999999999999
    #> 273                Falik Sheer Sikandar 05532 9999999999999
    #> 274                        Nadeem Abbas 05555 3530218693495
    #> 275                    Rai Ijaz Hussain 05555 3310404721113
    #> 276                   Gulam Haider Bari 05556 3310422121835
    #> 277                 Rai Haider Ali Khan 05556 3310446365177
    #> 278               Syed Raza Ali Gillani 05583 3520233784561
    #> 279                    Mujeeb Ul Rehman 05583 3310403885785
    #> 280                     Muhammad Faheem 05612 3310583974889
    #> 281                         Zafar Iqbal 05612 3310365398843
    #> 282               Muhammad Qasim Farooq 05652 3310006098015
    #> 283                      Zunaira Rehman 05652 4220193858536
    #> 284                 Abdul Sataar Khalid 05653 4230135372107
    #> 285                          Abdulsitar 05653 4250113612475
    #> 286               Muhammad Qasim Farooq 05654 3310006098015
    #> 287                            Ahsan Ul 05654 4250116123203
    #> 288                    Ray Nadeem Hayat 05655 3310085453429
    #> 289                           Asif Khan 05655 8888888888888
    #> 290                          Rana Akram 05656 3310010339697
    #> 291             Haji Muzafar Ali Shujra 05656 4250115266845
    #> 292                           Sajid Ali 05657 3310061149443
    #> 293                        Haroon Rasid 05657 4220175502589
    #> 294                        Saffaqat Ali 05658 3310090057197
    #> 295                               Kamal 05658 4220144483309
    #> 296                             Shahbaz 05660 3410168391851
    #> 297                       Khursheed Ali 05660 4250114626353
    #> 298                           Shokt Ali 05661 3310006034667
    #> 299                              Shukat 05661 3310006034667
    #> 300                Muhammad Alam Jamood 05661 4250144847877
    #> 301                       Syed Ali Abbs 05662 3210071440025
    #> 302                  Malik Muhammad Taj 05662 4250198589319
    #> 303                         Umar Farooq 05663 3310024645317
    #> 304                Muhhmad Aslam Bhutto 05663 4220107800325
    #> 305                   Nazir Ahmed Bhuto 05663 4310405671445
    #> 306                     Adeel Nasrallah 05664 3310097063879
    #> 307                     Muhammad Haneef 05664 4250115155661
    #> 308                         Aftab Ahmed 05666 3310069009533
    #> 309                          Mumtaz Ali 05666 8888888888888
    #> 310                      Muhammad Ajmal 05668 3310040152401
    #> 311                     Raheem Dad Khan 05668 4250122196989
    #> 312                    Muhammad Maqsood 05669 3310028191959
    #> 313                     Muhammad Masood 05669 3310038191959
    #> 314                         Rasual Khan 05669 4250177146255
    #> 315                     Muhammad Saleem 05670 3310006532893
    #> 316                           Samiuaahl 05670 4250165147085
    #> 317               Doctor Muhammad Yasin 05671 3310009692279
    #> 318                       Sher Ali Khan 05671 1320283941593
    #> 319                         Najma Ajmal 05672 3310062058852
    #> 320                         Siraj Ahmed 05672 4250121953849
    #> 321                  Rana Anam Ul Allah 05673 3310305321865
    #> 322             Syed Imdad Hussain Shah 05673 4250118899983
    #> 323                        Hamid Sulman 05675 3310047552633
    #> 324                         Abduljilani 05675 4200006867573
    #> 325                        Basharat Ali 05676 3310087848745
    #> 326                            Aqibkhan 05676 4220145287663
    #> 327                    Hafiz Atta Ullah 05678 3310032246081
    #> 328                        Atif Mansoor 05678 4220102244719
    #> 329                 Muhammad Ijaz Hanif 05679 3310012226177
    #> 330                    Barket Ul Shtiqi 05679 4220198753839
    #> 331                Muhammad Azeem Tahir 05680 3310051993245
    #> 332                         Moiz Shazad 05680 4220138240865
    #> 333                        Nadeem Aftab 05682 8888888888888
    #> 334                Muhammad Asreef Khan 05682 4220155562915
    #> 335                Rana Asif Sadiq Khan 05683 3310084392169
    #> 336                     Muhammad Farooq 05683 4220174500529
    #> 337      Syed Abu Zar Muhammad Abdullah 05684 3310081636099
    #> 338               Muhammad Hussain Khan 05684 4220186405719
    #> 339                         Zafar Iqbal 05685 3310007423301
    #> 340                     Muhammad Kashif 05685 4220171315059
    #> 341                       Khalil Akhtar 05697 3310217829691
    #> 342                        Khayal Ahmad 05697 3310218168771
    #> 343                         Karim Buksh 05697 4220140039627
    #> 344                      Muhammad Nawaz 05701 3310062499253
    #> 345                  Rana Sajid Mehmood 05701 4220196115921
    #> 346                      Muhammad Sadam 05703 3310058421239
    #> 347                   Sanaullah Qureshi 05703 3410118480487
    #> 348                     Muhammad Saleem 05704 3310210537109
    #> 349                     Muhammad Shakil 05704 3310010250605
    #> 350                      Muhmmad Saleem 05704 4250115645979
    #> 351                            Shanawaz 05704 4250115900749
    #> 352                        Munwar Fayaz 05706 6110120126009
    #> 353              Shakeel Uddin Siddiqui 05706 4250187827261
    #> 354                     Shahzad Murtaza 05707 3310079913163
    #> 355              Syed Zahid Zafar Ahmed 05707 4220196861423
    #> 356                      Zulfiqar Ahmed 05708 3310060544021
    #> 357                      Syed Adnan Ali 05708 9999999999999
    #> 358                         Hamad Islam 05713 3310063951007
    #> 359                     Aurghenzab Khan 05713 4220196583775
    #> 360                        Imran Faisal 05714 3310083870975
    #> 361                         Fareed Ullh 05714 1540106957665
    #> 362            Muhammad Aqeel Ur Rehman 05715 3310056589538
    #> 363                   Gul E Rana Azahar 05715 4200003602584
    #> 364                Muhammad Fazal Elahi 05717 3310251447347
    #> 365                   Javeed Ali Sheikh 05717 4130214208375
    #> 366                      Muhammad Islam 05718 3310006822479
    #> 367                         Javid Iqbal 05718 4250191261827
    #> 368                      Mohammad Hanif 05718 4250128062177
    #> 369                       Najam Hussain 05719 3310023704141
    #> 370                 Marizq Farheen Bheg 05719 4220166926833
    #> 371                        Naveed Anwer 05720 3310217759803
    #> 372                      Muhammad Ahmad 05720 4220113841557
    #> 373                  Raja Zahid Pervaiz 05721 3310034503973
    #> 374       Muhammad Aslam Pervaiz Abbasi 05721 4220123299707
    #> 375                      Muhammad Irfan 05722 4220171486713
    #> 376                Muhhmad Irfan Waheed 05722 4220103994371
    #> 377                         Shah Faisal 05723 3310061274369
    #> 378                    Muhammad Rafuiqe 05723 4220170345899
    #> 379                      Shakeel Shahid 05724 3310094388303
    #> 380               Muhammad Saleeem Khan 05724 4220121526967
    #> 381                Umer Draz Ahmed Azad 05725 3310031614767
    #> 382           Muhammad Shoiab Ur Rehman 05725 4220103977277
    #> 383                      Ghulam Hussain 05728 3310022324799
    #> 384                     Raees Ur Rehman 05728 4210116435789
    #> 385                       Intazar Ahmed 05729 3310031375199
    #> 386                        Shamim Ahmed 05729 4220102662535
    #> 387                     Javaid Zulifqar 05730 3310082172287
    #> 388                         Uzma Farooq 05730 4210147050084
    #> 389                     Memoona Hussain 05734 3510002987340
    #> 390                               Bachu 05734 9999999999999
    #> 391                               Bachu 05734 4200005028765
    #> 392                        Muhammad Esa 05737 3310022932985
    #> 393           Muhammad Javed Hanif Khan 05737 4200005237621
    #> 394              Muhammad Tahir Pervaiz 05740 3310018086145
    #> 395            Muhammad Tanveer Qurashi 05740 4200003876453
    #> 396                      Rao Asif Iqbal 05741 3310096579017
    #> 397                      Muhammad Tariq 05741 4220102954185
    #> 398                Sarmad Fazal Hussain 05743 3310207770971
    #> 399                          Raiz Ahmad 05743 3660175334101
    #> 400                       Ahmad Shayiar 05744 3310007592117
    #> 401                        Sajid Hasnan 05744 4220107403124
    #> 402                        Ashan Ul Haq 05745 3310090902365
    #> 403                        Sharaz Wahid 05745 4220106027263
    #> 404                        Imran Javaid 05746 3310030570249
    #> 405                   Saeed Adnan Hasan 05746 8220330059585
    #> 406                             M Akbar 05749 3310008532623
    #> 407                        Asad Mujahid 05749 4220145246761
    #> 408                 M Farook Al Hassaim 05751 3130166031275
    #> 409                  Mohammad Abu Bakar 05751 4220146621651
    #> 410                         M Nasir Ali 05752 3310007520201
    #> 411                        Mohammad Ali 05752 4250112765451
    #> 412                      M Zafair Iqbal 05754 3310007094203
    #> 413                     Mohammad Kamran 05754 4200085928665
    #> 414                         Nabila Sana 05756 3310037353724
    #> 415                    Mohammad Wajahad 05756 4220174585247
    #> 416             Sahibzada M Husain Raza 05758 3310086552019
    #> 417                       Sajid Husnain 05758 8220330059585
    #> 418                 Sajad Haider Cheema 05759 3310045162829
    #> 419                Sajjad Haider Cheema 05759 3310045162829
    #> 420              Sheikh Mohammad Saleem 05759 4220199780129
    #> 421                       Maqsood Mashi 05761 3310059103169
    #> 422                    Syed Mehmood Ali 05761 4220149767251
    #> 423                     M Abdul Shakoor 05762 3310009899777
    #> 424                               -9999 05762 4220152733011
    #> 425                           M Shakoor 05765             0
    #> 426                    Abdul Azeem Khan 05765 4220178385057
    #> 427                        Abdul Hafeez 05765 4220151048551
    #> 428                         Raja Saleem 05766             0
    #> 429                     Hazar Khan Abro 05766 4220121213249
    #> 430                         Riyasat Ali 05767 3310050255355
    #> 431                         Imran Baghi 05767 4250106935043
    #> 432                  Tauseef Nawaz Khan 05769 3310090899823
    #> 433                       Maqbool Ahmed 05769 4220196970107
    #> 434                      Ali Abbas Khan 05770 3310009799225
    #> 435                Mian Mohammad Illyas 05770 4220124862873
    #> 436                         Asad Muazam 05771 3310009877335
    #> 437                       Mohamad Ahmad 05771 4220107805500
    #> 438                 Muhammad Akram Khan 05773 3310007298141
    #> 439                      Mohammad Naeem 05773 4220105980185
    #> 440                     Muhammad Fayyaz 05774 3310079915325
    #> 441                     Mohammad Rizwan 05774 4220105204723
    #> 442                      Muzamal Doggar 05775 3310273779409
    #> 443                       Saad Saddiqui 05775 4220123828203
    #> 444                       Faqir Hussain 05779 3310066320923
    #> 445                      Shakeel Ahmend 05779 4220175308309
    #> 446                        Mehboob Alam 05780 3540413074639
    #> 447                 Shyeed Gayas Ahmend 05780 4220106620595
    #> 448              Muhammad Yasir Hussain 05781 3310261542595
    #> 449                   Waqar Hussan Shah 05781 4220105647779
    #> 450                Muhammad Younas Shad 05782 3310034322493
    #> 451                           Abdul Haq 05782 4200005401593
    #> 452                           Andul Haq 05782 4200005401593
    #> 453              Muhammad Zahid Shahzad 05783 3310083938903
    #> 454                    Abdul Haq Usmani 05783 4220103668571
    #> 455                        Adnan Javaid 05785 3310057891831
    #> 456                            Asif Ali 05785 4220137711957
    #> 457                            Ali Raza 05786 3310259684951
    #> 458                         Bhari Kamal 05786 1550122366981
    #> 459                      Atta Ur Rehman 05787 3310006089355
    #> 460                       Fahmida Zahir 05787 8888888888888
    #> 461                        Hamid Rashid 05788 3310080981291
    #> 462                           Fazal Ali 05788 4220141983635
    #> 463                   Dr Hassain Masood 05789 3310005881521
    #> 464                        Kaleemuallah 05789 4220127786779
    #> 465                       Maher M Akram 05790 3320010322529
    #> 466                          Kareem Bux 05790 3630109816161
    #> 467             Mian Muhammad Raqafiluz 05791 3310028743497
    #> 468                      Masood Mahmood 05791 4230184044417
    #> 469                       M Azam Shazad 05792 3310007333689
    #> 470                        Mehtab Ahmed 05792 4220192299533
    #> 471                      M Hammad Ullah 05793 3310022206351
    #> 472                       M Haroon Khan 05793 4220169337823
    #> 473                             M Iqbal 05794 3310007244567
    #> 474                      Mohammad Iqbal 05794 4220102849933
    #> 475                          M Siddique 05796 3310009477367
    #> 476                       M Zafer Iqbal 05796 3310028373371
    #> 477                Mohammad Waqar Azeem 05796 4220176788453
    #> 478                 Munaver Fiyaz Sunny 05797 6110120126009
    #> 479                       Rehan Mansoor 05797 4203169897845
    #> 480                      Pervaiz Akhter 05798 3310097559035
    #> 481                          Roshan Ali 05798 4220105729179
    #> 482                           Sadiq Ali 05799 3310218050727
    #> 483                Syed Mohammad Zahoor 05799 4220150980307
    #> 484                          Safder Ali 05800 3310023667873
    #> 485                  Umar Ahmed Sadique 05800 8888888888888
    #> 486               Sahabzada Hasain Raza 05801 3310086447519
    #> 487                         Yasir Uddin 05801 4230129334791
    #> 488                 Rana Muhammed Naeem 05802 3310649064001
    #> 489                        Sajaid Numan 05802 3310204301713
    #> 490                         Zafar Iqbal 05802 4220108008935
    #> 491                          Asad Zaman 05803 3330155113369
    #> 492                  Ali Mohammad Gabol 05803 4240189554209
    #> 493                       Mansoor Ahmad 05806 3330164315203
    #> 494                          Deedar Ali 05806 4320392436081
    #> 495                              Diibar 05806 4250119192961
    #> 496                        Safdar Wahla 05808 3320242258127
    #> 497                 Jalil Ahmad Mugheri 05808 8888888888888
    #> 498                        Usman Haider 05809 3330121990795
    #> 499                        Jan Muhammad 05809 4250105640947
    #> 500                    Abdul Qadir Alvi 05810 3330142673979
    #> 501                      Manzoor Burfat 05810 8888888888888
    #> 502                   Atta Ullah Hameed 05811 3330120800079
    #> 503                    Mohi Uddin Ahmed 05811 4210176185711
    #> 504                 Haji Muhammad Ishaq 05812 3330121103423
    #> 505                      Mohammad Akram 05812 4250196145023
    #> 506                         Ehsan Ahsan 05814 3330120665835
    #> 507                Mohammad Aslam Bhuto 05814 4220107800328
    #> 508                       Khalid Bashir 05816 3310006425937
    #> 509                Mohammad Hammad Khan 05816 4250191207691
    #> 510                Mohammad Hammad Khan 05816 4200004706031
    #> 511                         Zahid Iqbal 05818 3330133691807
    #> 512                    Mohammad Hussain 05818 4240112917113
    #> 513                        Sharafat Ali 05819 3540312371813
    #> 514                     Mohammad Ismail 05819 4240119822893
    #> 515                        Zulfiqar Ali 05820 3330196875931
    #> 516                 Mohammad Jamil Gill 05820 4220147394999
    #> 517                        Abdul Waheed 05821 3740556288527
    #> 518                     Mohammad Marjan 05821 4200004553235
    #> 519                     Asfand Yar Raja 05822 3740527415911
    #> 520                     Mohammad Ramzan 05822 4250150141993
    #> 521                   Ch Nisar Ali Khan 05823 3740591042609
    #> 522                 Mohammad Zahid Awan 05823 8888888888888
    #> 523                  Faisal Qayum Malik 05824 3740586078997
    #> 524                    Mohammad Farrukh 05824 4250142339339
    #> 525                       Faryal Masood 05825 3740263087366
    #> 526                    Umar Uddin Zafat 05825 4230196914103
    #> 527                 Jaffar Hussain Shah 05826 3740554879439
    #> 528              Raees Anwar Ullah Khan 05826 8888888888888
    #> 529               Mudassar Hussain Khan 05828 3610330982491
    #> 530                           Sati Khan 05828 4250147082875
    #> 531                       Ali Raza Khan 05865 3330222742359
    #> 532                            Ali Raza 05865 4130380672229
    #> 533                   Abdul Jabbar Khan 05928 3320298294813
    #> 534                        Abdul Jabbar 05928 4130387542231
    #> 535                     Muhammad Muavia 05949 3320280607999
    #> 536                   Khuram Abbas Sial 05949 3320293192331
    #> 537                        Nadia Cheema 06012 3550301104566
    #> 538                       Shazia Cheema 06012 3540310121000
    #> 539                   Antique Ul Rehman 06036 3540273917763
    #> 540                Atta Ul Rehman Azher 06036 3540225026989
    #> 541                   Jamil Hassan Khan 06060 6110119103947
    #> 542                Rana Shaheen Ikhlaak 06060 3540296695941
    #> 543                Muhammad Umar Lagari 06078 4130774108385
    #> 544                 Muhammad Umar Afzal 06078 3540141229069
    #> 545                    Miraj Din Khalid 06219 3540433637929
    #> 546               Muhammad Kashif Miraj 06219 3540435177161
    #> 547                 Rana Asadullah Khan 06229 3540412327261
    #> 548                          Asad Ullha 06229 3510228211749
    #> 549                Sardar Sarfraz Ahmad 06256 3540403430355
    #> 550                  Sarfraz Ahmad Khan 06256 3520113130517
    #> 551                  Mian Hamza Shehbaz 06294 3520107131839
    #> 552                       Hamza Shahbaz 06294 3520107131839
    #> 553                       Tariq Sadakat 06304 3520265055635
    #> 554                 Robina Solehri Noor 06304 3520203799830
    #> 555                          Ch.M.Nawaz 06311 3520294475831
    #> 556                             Khurram 06311 3520224559727
    #> 557                   Abdul Kareem Khan 06338 3740504684429
    #> 558              Malik Abdul Karim Khan 06338 3740504084429
    #> 559                     M. Javaid Iqbal 06339 3740505064685
    #> 560                M. Javed Iqbal Malik 06339 3740505064685
    #> 561                      M. Tahir Iqbal 06342 3740516755477
    #> 562                         Tahir Iqbal 06342 3740516755477
    #> 563                        Umer Tanveer 06344 6110193237321
    #> 564                         Umer Tanvir 06344 6110193237321
    #> 565                         Bilal Yasin 06347 3520220079939
    #> 566                  Ch Muhammad Asghar 06347 3520222691965
    #> 567                  Ch M Mansha Prince 06349 3520261008701
    #> 568                 Waqas Mansha Prince 06349 3520285333831
    #> 569                         Aslam Iqbal 06367 3520293320863
    #> 570                             M.Fasal 06367 3520285824805
    #> 571                              Asif M 06374 3520211467799
    #> 572                        Hasan Irshad 06374 3520265590921
    #> 573                            M.Shazad 06404 3520114829809
    #> 574                      Shahzad Arshad 06404 3520221561831
    #> 575               Khawaja Salma Rafique 06470 3520277965077
    #> 576               Kjawaja Salma Rafique 06470 3520277965077
    #> 577                    Abdul Aleem Khan 06480 3520116596051
    #> 578                    Abdul Aleem Khan 06480 3520116596051
    #> 579                    Kiran Aleem Khan 06480 3520115309082
    #> 580                    Abdul Aleem Khan 06480 4130463875034
    #> 581                     M Mumtaz Khalid 06504 3520274763399
    #> 582              Muhammad Mumtaz Khalid 06504 3520248685935
    #> 583             Abdul Karim Rahi Kalwar 06616 3520209410145
    #> 584                          Abdul Krim 06616 4410426618167
    #> 585                         Abdul Hanan 06777 3510268607685
    #> 586                        Abdul Hannan 06777 3510268607685
    #> 587                               Ahmad 06806 3510243065419
    #> 588                    Malik Ahmad Syed 06806 3510243065419
    #> 589                    Amjad Ali Tufail 06929 9999999999999
    #> 590                               Amjad 06929 9999999999999
    #> 591                          Ilyas Khan 06937 3520297125927
    #> 592                            M.Illyas 06937 3520297121927
    #> 593                          Syed Ahmad 07185 3650212821271
    #> 594                    Syed Nazir Ahmad 07185 3650277208675
    #> 595                         Umair Javed 07212 3520255827129
    #> 596                       Waheed Asghar 07212 3650118353401
    #> 597                       Waheed Asghar 07212 3650118353405
    #> 598              Muhammad Arslan Masood 07222 3520241348107
    #> 599          Muhammad Daood Urf Alqasim 07222 3650189910639
    #> 600                        Khizar Hayat 07340 3630314008767
    #> 601               Malik Sikandar Hayyat 07340 3630309398157
    #> 602                  Malik Lal Muhammad 07465 3630105420671
    #> 603                        Lal Muhammad 07465 4410792675451
    #> 604                  Muhammad Asif Raza 07468 3630201300721
    #> 605                           Asif Raza 07468 4130352906503
    #> 606                Mian Amir Iqbal Shah 07489 3620105625631
    #> 607                 Muhammad Iqbal Shah 07489 3620186061797
    #> 608          Ahmad Khan Urf Ahmad Buksh 07529 3630223595245
    #> 609                 Syed M. Rafi Ud Din 07529 3620344054289
    #> 610                  Imran Khan Saldera 07578 3660167523401
    #> 611                          Imran Khan 07578 4430269329533
    #> 612                        Junaid Aslam 07865 3130243193931
    #> 613 Makhdoom Syed Muhammad Masood Aslam 07865 3130262548965
    #> 614                  Syed Ibrar Hussain 07925 3130378887023
    #> 615                         Sofia Sadiq 07925 9999999999999
    #> 616                Hafiz Muhammad Tahir 07943 3130115020583
    #> 617                         Junaid Riaz 07943 3130150583723
    #> 618                        Nazeer Ahmad 07965 3130323737493
    #> 619                         Nzeer Ahmad 07965 4410159397489
    #> 620                      Khalid Mehmood 08013 3130487945681
    #> 621                    Khalil Ur Rehman 08013 3130410838813
    #> 622                      Muhammad Balal 08092 3230416516063
    #> 623              Muhammad Javed Hussain 08092 3230472587391
    #> 624                Mohammad Zahid Naeem 08099 3330416456315
    #> 625                      Mumtaz Hussain 08099 3230484085341
    #> 626                        Mumtaz Saeed 08099 3740557620678
    #> 627                   Rao Atif Ali Khan 08130 3310520568597
    #> 628                            Atif Ali 08130 4130373572047
    #> 629                    Malik Abdul Aziz 08135 3230298191683
    #> 630                  Muhammad Jahangeer 08135 3630213827259
    #> 631              Muhammad Zaiullah Khan 08172 3630243823953
    #> 632            Muhammad Aon Hamid Dogar 08172 3230496332929
    #> 633                         Shafiq Alam 08322 3210502920955
    #> 634                        Shazad Hanif 08322 3210305110169
    #> 635                         Irfan Ullah 08352 3210209200013
    #> 636                   Mohsan Atta Khosa 08352 3210274827175
    #> 637                        Sajid Yousaf 08386 3420196118661
    #> 638                      Shaheena Karim 08386 3210236789214
    #> 639                          Amjad Khan 08402 3210209104981
    #> 640                        Gulam Sarwer 08402 3210267980130
    #> 641       Sardar Nasrullah Khan Dreshak 08433 3520229587905
    #> 642   Sardar Pervez Iqbal Khan Gorchani 08433 9999999999999
    #> 643            Sardar Zulfiqar Ali Khan 08436 3520229587975
    #> 644                 Sher Zaman Gorchani 08436 9999999999999
    #> 645                         Naeem Nawaz 08551 3420334855147
    #> 646                          Naeem Raza 08551 3420330314649
    #> 647                 Mirza Altaf Hussain 08613 3460216282263
    #> 648                    Muhammad Hussain 08613 8888888888888
    #> 649                        Noor Hussain 08616 3460368617403
    #> 650                         Munir Ahmed 08616 8888888888888
    #> 651                      Bushra Parveen 08745 3450164633002
    #> 652                              Bushra 08745 4410652059916
    #> 653                     Muhammad Rizwan 08803 3410425273181
    #> 654                       Rizwan Yousaf 08803 3410189301675
    #> 655                          Asim Anees 08838 3410152228557
    #> 656                           Asim Khan 08838 3410170903187
    #> 657                   Muhammad Saddique 08859 3410155471225
    #> 658                    Muhammas Sadique 08859 9999999999999
    #> 659                        Salman Nasir 08922 3410182902533
    #> 660                               Salma 08922 4410466088138
    #> 661                    Dr Sohail Zaffar 08924 3410170911269
    #> 662                 Sohail Zafar Cheema 08924 3410170911269
    #> 663                      Ch Azmat Ullah 09020 3440216037767
    #> 664                         Mirza Ikram 09020 3440216028479
    #> 665                        Sajjid Ahmad 09050 3520205453507
    #> 666                         Sajjid Khan 09050 3520205453507
    #> 667                Gulraiz Afzal Gondal 09098 3440156980065
    #> 668                       Fakkhar Abbas 09098 9999999999999
    #> 669                         Moazzam Ali 09167 3840371067253
    #> 670                 Sardar Kamil Shamim 09167 3840309687741
    #> 671                       M Aslam Arain 09184 9999999999999
    #> 672                        M Imran Baig 09184 3840322192551
    #> 673                       Zulafiqar Ali 09194 3840342881559
    #> 674                        Zulfiqar Ali 09194 3840322222975
    #> 675               Khalid Iiftikhar Shah 09304 9999999999999
    #> 676                     Muhammad Khalid 09304 9999999999999
    #> 677                      Rukhsana Begum 09317 3830363286744
    #> 678                            Rukhsana 09317 4410117939738
    #> 679                           Aijaz Ali 09348 4130666884621
    #> 680                       Ejaz Ali Khan 09348 3810131382251
    #> 681               Muhammad Abdul Jabaar 09359 3810471709261
    #> 682                     Muhammad Ramzan 09359 3810136748775
    #> 683                    Azad Ali Tabasum 09430 3310108049119
    #> 684                               Abbas 09430 4310299258233
    #> 685                      Muhammad Akbar 09487 4310224440301
    #> 686                 Muhammad Akram Abro 09487 4310224812922
    #> 687                       Abdul Rasheed 09696 4130364398983
    #> 688                  Syed Abdul Rasheed 09696 4230108147547
    #> 689                           Lutiullah 09727 8888888888888
    #> 690                         Moazzam Ali 09727 4320324294293
    #> 691                    Syed Adil Askari 09752 4220198651665
    #> 692                  Syed Farhan Askari 09752 4220136764525
    #> 693                        Jamshed Alam 09788 4130360992911
    #> 694                       Jamshaid Alam 09788 4240117034927
    #> 695                             Mubarak 09791 4430106704873
    #> 696                      Mubarak Baloch 09791 4240189700989
    #> 697                     Mohammad Saleem 09887 4200003798593
    #> 698        Syed Muhammad Saleem Ashrafi 09887 4240118594281
    #> 699                     Aman Ullah Khan 09902 4240122543821
    #> 700                      Amanullah Khan 09902 2170584413689
    #> 701                        Mehmood Khan 09964 4270115336725
    #> 702                     Muhammad Ismail 09964 4240120757351
    #> 703                         Qadir Baksh 09991 4240116977565
    #> 704                           Saqib Ali 09991 4240195177631
    #> 705              Malik Ahmad Ali Aulakh 09999 3220225644567
    #> 706                  Malik Allah Bakhsh 09999 3220315576555
    #> 707                   Ali Hassan Jhakar 09999 3220264205541
    #> 708                        Naveed Iqbal 09999 3220332417563
    #> 709                         Asad Ullaha 09999 3430218038589
    #> 710                       Dibag Jaffari 09999 9999999999999
    #> 711                     Fiaz Ahmad Awan 09999 3430117817363
    #> 712                          Gull Nawaz 09999 9999999999999
    #> 713                    M. Zia Ul Hassan 09999 3220225160495
    #> 714             Muhammad Duryab Mahwish 09999 3220218185481
    #> 715                          Sobia Inam 09999 3540117676792
    #> 716                  Haji Muhamad Ilyas 09999 4410720496465
    #> 717                       Babar Chandio 09999 4130435889605
    #> 718               Muhammad Hassan Elahi 09999 4200020933933
    #> 719               Muhammad Umar Qureshi 09999 4130373396357
    #> 720                   Qadeer Ahmed Khan 09999 4130355816413
    #> 721                 Qazi Muhammad Ilyas 09999 4130322492197
    #> 722                      Ghani Ul Fahad 09999 4210171011973
    #> 723                       Khalid Mumtaz 10042 8888888888888
    #> 724             Khawaja Izhar Ul Hassan 10042 4210145940583
    #> 725              M Arif Hussain Qureshi 10124 4210119993739
    #> 726                       M Arif Sheikh 10124 4210118937893
    #> 727                  Mansoor Ahmad Khan 10180 4210144178517
    #> 728                       Rehan Mansoor 10180 4230169897845
    #> 729                     Abdul Rabpitafi 10269 4510495253131
    #> 730                           Abdul Rub 10269 4130315476639
    #> 731                       Tahir Hussain 10294 4550484781321
    #> 732                     Zohaib Zulfiqar 10294 4230186870381
    #> 733                  Syed Ali Raza Shah 10342 4550272072131
    #> 734                    Syed Awais Qadir 10342 4550411270597
    #> 735                    Abdul Haq Buriro 10375 4250115832831
    #> 736                      Ghulam Mustafa 10375 4520674472707
    #> 737                         Nusrat Bano 10391 4520309185468
    #> 738             Syed Nasir Hussain Shah 10391 4550268335697
    #> 739                    Syed Nasim Ahmad 10401 4550484283685
    #> 740                        Bishar Ahmad 10401 9999999999999
    #> 741                         Nafisa Shah 10412 9999999999999
    #> 742                             Shahnan 10412 4520371723946
    #> 743                      Shahnaz Sheikh 10415 4520343644216
    #> 744                            Shahnwaz 10415 9999999999999
    #> 745                      As Advised Ali 10431 4130703435229
    #> 746                       Asad Ali Shar 10431 4520633710713
    #> 747                       Sajid Banbhan 10443 4520152055771
    #> 748                   Sajid Ali Banbhan 10443 4520152055771
    #> 749                       Abdul Ghaffar 10511 4530278910227
    #> 750                      Ghulam Murtaza 10511 4230189079923
    #> 751                        Abdul Waheed 10668 4540199921343
    #> 752                           Nasrullah 10668 4540286305223
    #> 753                Syed Imtiaz Ali Shah 10793 4410329751797
    #> 754                     Syed Imtiaz Ali 10793 4410329751797
    #> 755                Syed Imtiaz Ali Shah 10793 4410329751797
    #> 756                         Ahsan Ahmed 10824 4310316598249
    #> 757                    Ahsan Ali Mirani 10824 4220182739671
    #> 758                   Arbab Ghulam Rhem 10834 4220148315141
    #> 759                        Arbab Ghulam 10834 4220148315141
    #> 760              Syed Zulfiqar Ali Shah 10890 4200004626933
    #> 761                        Muhammad Jam 10890 4410425813177
    #> 762                           Ali Akbar 10908 4410714558247
    #> 763                           Ali Akber 10908 4410714558247
    #> 764                    Syed Ali Hussain 10978 4130422949959
    #> 765              Syed Jalal Shah Jomate 10978 4130579538741
    #> 766                         Qamber Khan 10981 4430450902067
    #> 767                   Syed Mir Ali Shah 10981 4130590866631
    #> 768                     Habibullah Palh 10988 4130705859789
    #> 769                    Habib Ullah Palh 10988 4130705859789
    #> 770                         Dodo Maheri 11020 4110249137449
    #> 771                     Habibur Rehaman 11020 4130657945501
    #> 772                            Jam Khan 11020 4130623122747
    #> 773                        Farhan Sabir 11077 4130483436759
    #> 774                     Muhammad Farhan 11077 4130402903035
    #> 775                      Muhammad Altaf 11138 4130816666135
    #> 776             Muhammad Altaf Nizamani 11138 4130816066135
    #> 777                        Zulfiqar Ali 11155 4410703163689
    #> 778              Syed Zulfiqar Ali Shah 11155 4130831152847
    #> 779                             Tanveer 11167 4330421750135
    #> 780                       Tanveer Ahmed 11167 4330421750135
    #> 781                       Guhlam Sarwar 11170 4430154070733
    #> 782               Ghulam Sarwar Leghari 11170 4110381989467
    #> 783                 Syed Ghulam Mustafa 11184 4130444162383
    #> 784                      Ghulam Mustafa 11184 4110444454013

    str(dup_by_uid$uid)

    #>  chr [1:784] "00014" "00014" "00034" "00034" "00040" "00040" "00041" ...

    write_csv(dup_by_uid,"data/rsols_corrections/duplicated_uids.csv", append = F)

#### 3. Assets (Immovable)

While there is an added variable that is supposedly the sum of all
im\_prop value variables, it seems they haven't controlled for when one
of the values in the sum is NA. So just to check how many total
immovable property value sums we actually have in our dataset:

    #>                im_prop_opk_missing
    #> im_prop_missing FALSE  TRUE
    #>           FALSE   160 10343
    #>           TRUE     33  6729

It appears there are 6762 candidates who have declared no immovable
assets in Pakistan. Similarly, only 193 candidates have declared
immovable asset ownership outside Pakistan.

There are 6729 candidates in total who have declared zero immovable
assets (both outside and inside Pakistan).

#### 4. Jewellery, Cash and others

In addition to immovable asset values and details, other measures of
wealth we have are: 1. Business Capital Ownership within and outside
Pakistan 2. Investments in varying forms (stocks, loans, mortages) 3.
Total value of owned motor cars (the motor car number and count ALL NA
majorly)

    #> # A tibble: 17,265 x 3
    #>    motor_cost motor_number motor_g_count
    #>         <dbl>        <dbl>         <dbl>
    #>  1        -99           NA            NA
    #>  2    1000000           NA            NA
    #>  3    1200000           NA            NA
    #>  4          0           NA            NA
    #>  5          0           NA            NA
    #>  6     430000           NA            NA
    #>  7          0           NA            NA
    #>  8     600000           NA            NA
    #>  9    2300000           NA            NA
    #> 10    2000000           NA            NA
    #> # ... with 17,255 more rows

    #> 
    #>  <NA> 
    #> 17265

    #> 
    #>  <NA> 
    #> 17265

    #> 
    #>       -99         0         1         2         3         4         5 
    #>      6850      3536       128        64        26         4         1 
    #>         8        99      1500      2000      5000      7000      7426 
    #>         7         2         1         1         3         1         1 
    #>      8000     10000     10500     12000     12500     14000     15000 
    #>         2         8         2         5         1         1        22 
    #>     16000     17000     18000     19000     20000     21000     22000 
    #>         2         3         4         1        68         2         5 
    #>     23000     25000     26000     27000     28000     30000     30008 
    #>         3        48         3         2         4        81         1 
    #>     30808     32000     35000     36000     38000     38500     39000 
    #>         1         3        29         5         4         1         3 
    #>     40000     42000     42500     43000     44000     45000     46000 
    #>        80         6         1         2         3        21         3 
    #>     47000     48000     49000     50000     50500     51000     52000 
    #>         5         3         2        51         1         1         3 
    #>     53000     55000     56000     58000     59000     60000     62000 
    #>         2         7         1         4         1        30         2 
    #>     63000     63200     63500     64000     65000     66000     67000 
    #>         2         1         4         2        11         1         2 
    #>     67500     69000     70000     71000     72000     72500     75000 
    #>         1         1        24         1         2         1        11 
    #>     78000     79500     79783     80000     85000     86000     86500 
    #>         1         1         1        22         3         1         3 
    #>     87000     87500     90000     94500     95000     98000     1e+05 
    #>         1         1        11         1         1         1        42 
    #>    102000    103000    103900    104000    104420    105000    106000 
    #>         4         1         1         2         1         6         1 
    #>    106500    107000    109000    110000    112000    112679    113000 
    #>         1         2         1        13         1         1         2 
    #>    115000    115300    117000    118000    120000    122000    124000 
    #>         1         1         1         1        10         2         1 
    #>    125000    126000    130000    134000    135000    137000    140000 
    #>         2         1        12         2         3         1         9 
    #>    144000    145000    147000    149000    150000    150002    155000 
    #>         1         2         2         1        34         1         1 
    #>    155500    160000    162000    165000    170000    176500    180000 
    #>         1         6         1         1         4         1        10 
    #>    182700    184000    190000    195000     2e+05    208000    210000 
    #>         1         1         2         1        41         1         5 
    #>    214283    220000    224000    230000    240000    245000    246000 
    #>         1         3         1         2         2         1         1 
    #>    250000    252000    260000    265000    270000    275000    276000 
    #>        31         1         2         2         2         1         2 
    #>    280000    288000    290000    295000    299400     3e+05    316000 
    #>         3         1         2         1         1        75         2 
    #>    320000    321200    325000    325349    330000    335000    340000 
    #>         4         1         3         1         4         3         2 
    #>    343500    345000    350000    360000    370000    380000    383000 
    #>         1         1        21         1         3         2         2 
    #>    390000    394000    395000    398000     4e+05    409600    410000 
    #>         1         1         1         1        78         1         3 
    #>    415000    417500    420000    422000    425000    430000    435000 
    #>         3         1         6         4         5         4         2 
    #>    440030    447760    450000    460000    466015    470000    470500 
    #>         1         1        31         2         1         3         1 
    #>    475000    480000    485000    490000    494000    495000     5e+05 
    #>         1         2         1         1         1         1       136 
    #>    500625    510000    514030    515000    520000    522468    525000 
    #>         1         1         1         3         4         1         3 
    #>    530000    540000    540500    542000    545000    550000    554000 
    #>         8         2         1         1         1        25         2 
    #>    560000    565000    570000    572000    575000    577285    580000 
    #>         4         1         2         1         4         1         2 
    #>    580030    585000    589000    590000     6e+05    610000    618000 
    #>         1         1         1         2       122         2         1 
    #>    621000    625000    631720    632831    636352    637000    640000 
    #>         1         3         1         1         1         2         4 
    #>    645000    650000    654000    657000    659973    660000    665000 
    #>         1        21         1         1         1         4         1 
    #>    670000    675000    676900    678000    680000    685000    690000 
    #>         4         6         1         1         2         1         2 
    #>    693000    693190     7e+05    702000    703168    704000    706000 
    #>         1         1       135         1         2         2         1 
    #>    708000    710000    715000    720000    724000    725000    730000 
    #>         1         3         1         4         1         5         3 
    #>    732000    735000    736000    737000    742000    742392    744000 
    #>         1         3         1         1         1         1         1 
    #>    745000    750000    760000    766000    770000    775000    775600 
    #>         1        27         2         1         4         2         1 
    #>    780000    784000    785000    790000     8e+05    810500    815751 
    #>         4         1         2         1       125         1         1 
    #>    825000    830000    830250    831060    833000    835000    835450 
    #>         2         7         1         1         1         2         5 
    #>    837650    840000    848000    849000    850000    852468    856000 
    #>         1         4         1         1        33         1         1 
    #>    859000    859973    860000    865000    870000    870500    876598 
    #>         1         1         1         1         2         1         1 
    #>    885000    885058    890000     9e+05    902000    909000    910000 
    #>         1         1         1        72         1         1         1 
    #>    912296    920000    921500    925000    930000    940000    945000 
    #>         1         2         1         4         1         3         2 
    #>    950000    955000    955361    960000    963000    963500    970000 
    #>        15         1         1         1         1         1         1 
    #>    975000    977000    980000    985000    990000     1e+06   1002000 
    #>         2         2         4         3         2       194         1 
    #>   1006900   1020000   1022000   1026574   1030000   1032500   1040000 
    #>         1         2         1         2         1         2         3 
    #>   1045000   1050000   1060000   1065000   1068000   1068976   1069000 
    #>         1        19         1         2         1         1         2 
    #>   1070000   1072000   1074999   1075000   1080000   1085000   1086250 
    #>         3         1         1         2         1         3         1 
    #>   1090000   1090804   1092000   1094000   1095000   1100000   1110000 
    #>         1         1         2         1         1        75         1 
    #>   1120000   1125000   1140000   1144000   1149000   1150000   1154000 
    #>         1         1         2         1         1        16         1 
    #>   1155000   1160000   1164000   1166879   1170000   1173274   1175000 
    #>         1         2         1         1         1         3         1 
    #>   1180000   1190000   1195000   1195500   1200000   1205000   1207000 
    #>         2         1         1         1       179         1         1 
    #>   1209345   1210000   1220000   1220160   1230000   1233264   1233764 
    #>         1         2         1         1         3         1         1 
    #>   1234272   1237000   1239000   1243000   1245000   1250000   1250300 
    #>         1         1         1         1         2        11         1 
    #>   1260000   1263000   1265000   1269000   1270000   1275000   1280000 
    #>         3         1         2         2         1         7         3 
    #>   1293880   1295000   1298660   1300000   1305000   1308778   1320000 
    #>         1         1         1        91         2         2         3 
    #>   1321393   1323880   1325000   1337000   1338000   1340000   1348635 
    #>         1         1         2         1         1         3         1 
    #>   1350000   1350464   1354000   1357660   1360000   1364000   1367075 
    #>        10         1         4         1         4         1         3 
    #>   1370000   1371702   1378000   1384000   1385000   1390000   1391000 
    #>         2         1         1         1         2         2         1 
    #>   1400000   1404000   1408000   1410000   1414000   1415000   1420000 
    #>        84         1         1         2         2         1         1 
    #>   1421000   1425000   1426000   1430000   1433500   1435000   1439000 
    #>         2         1         1         4         1         1         1 
    #>   1440000   1443000   1446154   1449991   1450000   1455000   1460000 
    #>         4         1         1         1        20         1         2 
    #>   1470000   1475000   1478000   1489000   1495000   1500000   1510000 
    #>         2         1         1         1         1       174         1 
    #>   1512620   1520000   1522000   1523476   1525000   1529000   1530000 
    #>         1         4         2         1         3         1         4 
    #>   1535000   1545000   1550000   1552000   1558800   1560000   1562500 
    #>         1         1        14         1         1         1         3 
    #>   1575000   1577000   1579000   1580000   1586362   1587000   1590000 
    #>         3         2         1         1         2         1         3 
    #>   1592000   1592500   1595000   1600000   1600088   1603384   1607500 
    #>         1         1         1       116         1         1         1 
    #>   1611450   1612500   1619000   1623000   1624000   1625000   1625520 
    #>         1         1         1         1         1         3         4 
    #>   1640000   1650000   1660000   1661000   1664000   1665000   1667800 
    #>         4        25         4         1         1         1         1 
    #>   1670000   1672500   1675000   1678000   1680000   1686000   1689500 
    #>         2         2         2         1         1         2         1 
    #>   1690000   1695000   1700000   1700980   1712500   1715000   1717500 
    #>         3         1        89         1         1         1         1 
    #>   1719000   1720000   1725500   1735000   1736000   1740000   1747000 
    #>         1         1         1         3         1         1         1 
    #>   1749000   1750000   1750500   1752000   1755000   1756946   1760000 
    #>         1        10         1         2         2         1         2 
    #>   1765000   1770000   1770500   1772000   1787500   1789500   1792500 
    #>         1         1         1         3         1         1         1 
    #>   1794000   1800000   1801440   1802000   1805000   1807500   1820000 
    #>         1       144         1         1         1         3         2 
    #>   1824000   1825000   1826000   1826650   1830000   1834000   1840000 
    #>         1         1         1         1         2         1         2 
    #>   1845500   1846000   1850000   1855602   1857000   1860000   1862000 
    #>         1         1        21         1         1         2         1 
    #>   1865000   1871500   1872000   1875000   1876000   1877000   1880000 
    #>         1         1         1         3         1         1         2 
    #>   1882000   1885000   1889000   1890000   1892448   1895000   1896000 
    #>         2         1         1         4         1         1         1 
    #>   1897620   1900000   1903000   1904260   1906500   1910000   1918000 
    #>         1        50         1         1         1         1         1 
    #>   1918233   1920000   1921500   1925000   1926000   1927500   1930000 
    #>         6         3         2         2         1         1         2 
    #>   1932000   1935000   1935230   1940000   1944000   1950000   1965000 
    #>         1         3         1         1         3        11         3 
    #>   1971500   1975000   1978000   1990000   1994553   1997500     2e+06 
    #>         1         2         1         2         1         1       154 
    #>   2010500   2012347   2019289   2025000   2027500   2040000   2050000 
    #>         1         1         1         5         1         1         3 
    #>   2056663   2060000   2065000   2074870   2075000   2080000   2090000 
    #>         2         1         2         1         2         2         1 
    #>   2091000   2100000   2103465   2109000   2110000   2110500   2114040 
    #>         1        50         1         1         3         1         1 
    #>   2115000   2117500   2121500   2125000   2130000   2135500   2138000 
    #>         1         1         2         1         2         1         1 
    #>   2140000   2150000   2160000   2165500   2170000   2172000   2175000 
    #>         2         5         1         1         2         2         1 
    #>   2196000   2200000   2215500   2225862   2230800   2240000   2248200 
    #>         1        70         1         1         2         1         1 
    #>   2250000   2254000   2255667   2260000   2265000   2275000   2280000 
    #>        15         2         1         1         1         1         3 
    #>   2290000   2300000   2310000   2320000   2322000   2325000   2331500 
    #>         3        42         1         1         1         1         2 
    #>   2335000   2337650   2341000   2346500   2350000   2357069   2360200 
    #>         1         1         1         1         7         1         1 
    #>   2360650   2375000   2375916   2380000   2383917   2390000   2394000 
    #>         1         1         3         1         1         2         1 
    #>   2398000   2400000   2403000   2403500   2405030   2406000   2409694 
    #>         2        39         1         1         1         1         1 
    #>   2414900   2415000   2429425   2430000   2431869   2441000   2445000 
    #>         1         1         1         4         1         1         3 
    #>   2450000   2460000   2468406   2470000   2474000   2478000   2478500 
    #>         7         1         1         1         1         2         1 
    #>   2483000   2500000   2503000   2515000   2520000   2525000   2527000 
    #>         1       103         2         1         5         1         1 
    #>   2535000   2545000   2550000   2553518   2555750   2556270   2557455 
    #>         2         1        10         1         2         1         5 
    #>   2560425   2570000   2572345   2578968   2579938   2580639   2585000 
    #>         1         5         1         1         1         1         1 
    #>   2591000   2600000   2605292   2610000   2620000   2625000   2626000 
    #>         1        35         1         1         1         2         1 
    #>   2630000   2634000   2640000   2645700   2650000   2652800   2665000 
    #>         1         3         1         1         7         1         1 
    #>   2680139   2690000   2700000   2710000   2712000   2714500   2720000 
    #>         1         1        42         1         2         1         2 
    #>   2722000   2732500   2737000   2740000   2740095   2745000   2747500 
    #>         1         1         2         1         1         3         1 
    #>   2749000   2750000   2756953   2759750   2766965   2777700   2780000 
    #>         1         8         1         2         1         1         1 
    #>   2785000   2795000   2800000   2800580   2810450   2810455   2820000 
    #>         1         1        35         1         5         1         1 
    #>   2838000   2839500   2850000   2854324   2874689   2899145   2900000 
    #>         1         1         2         3         1         1        21 
    #>   2908085   2914500   2917500   2920065   2924000   2940000   2950000 
    #>         1         1         1         1         1         1         2 
    #>   2955500   2965000     3e+06   3009329   3013080   3023218   3033280 
    #>         1         1        85         1         1         1         1 
    #>   3035000   3045000   3050000   3070000   3075900   3081000   3085000 
    #>         1         1         6         2         1         1         5 
    #>   3087000   3092000   3095000   3100000   3100962   3115000   3125000 
    #>         1         1         1        32         2         2         1 
    #>   3133834   3137000   3139800   3142000   3145000   3145008   3150000 
    #>         2         1         1         1         2         1         1 
    #>   3161000   3165000   3186000   3195000   3200000   3207500   3213000 
    #>         1         1         1         1        39         1         1 
    #>   3223800   3225000   3245000   3250000   3258000   3259061   3260000 
    #>         1         1         1        12         1         1         1 
    #>   3269000   3272700   3274699   3275272   3280000   3285000   3288000 
    #>         1         1         1         1         2         1         1 
    #>   3290000   3291000   3292000   3297500   3300000   3320000   3324000 
    #>         1         1         1         1        30         2         1 
    #>   3325000   3330000   3334088   3350000   3360000   3370000   3370500 
    #>         1         1         1         4         1         1         1 
    #>   3389000   3392000   3398800   3400000   3404500   3408000   3409000 
    #>         1         1         2        15         1         1         1 
    #>   3412000   3430000   3450000   3451000   3452153   3469012   3472000 
    #>         1         1         4         1         1         1         2 
    #>   3475000   3480000   3490000   3500000   3507000   3520200   3533147 
    #>         2         1         1        55         1         1         1 
    #>   3534000   3550000   3555200   3566655   3580000   3595500   3600000 
    #>         1         1         1         1         1         1        25 
    #>   3626840   3630000   3645500   3646000   3650000   3700000   3710000 
    #>         1         1         2         1         4        18         2 
    #>   3710180   3725000   3750000   3782679   3790000   3799762   3800000 
    #>         2         3         2         2         1         1        17 
    #>   3802940   3839200   3850000   3858000   3861000   3875000   3895000 
    #>         1         1         2         1         1         1         1 
    #>   3900000   3911900   3915000   3919276   3925349   3936757   3939500 
    #>        11         1         1         2         1         1         1 
    #>   3943000   3950000   3985500   3986000     4e+06   4008088   4050000 
    #>         2         2         1         1        48         1         3 
    #>   4055000   4075000   4080000   4091000   4100000   4118000   4120000 
    #>         1         1         2         3        12         1         1 
    #>   4130000   4136000   4145000   4156000   4170000   4200000   4211000 
    #>         1         1         1         1         1        24         1 
    #>   4227700   4245000   4265000   4278000   4290000   4300000   4350000 
    #>         1         1         1         1         2        12         6 
    #>   4352000   4355000   4400000   4405000   4405500   4421300   4438000 
    #>         1         1        13         1         2         1         2 
    #>   4445000   4450000   4480000   4500000   4525000   4527000   4546047 
    #>         1         1         1        47         2         1         1 
    #>   4575000   4577500   4587000   4597000   4600000   4609547   4650000 
    #>         1         2         1         1        11         2         3 
    #>   4700000   4704334   4750000   4750120   4756970   4756974   4785000 
    #>         7         1         1         1         1         1         1 
    #>   4800000   4846535   4850000   4860000   4885000   4900000   4932892 
    #>        12         1         7         1         2         4         2 
    #>   4949500   4950000   4950340   4965000   4969500   4977000     5e+06 
    #>         1         1         2         1         1         1        49 
    #>   5019428   5022650   5050000   5061000   5100000   5135000   5150000 
    #>         1         1         2         1        10         2         4 
    #>   5158000   5162500   5184000   5184619   5190500   5200000   5202000 
    #>         1         2         1         1         1        12         2 
    #>   5204700   5225000   5249000   5250000   5257000   5280500   5280800 
    #>         1         1         1         1         1         3         1 
    #>   5300000   5310000   5323500   5350000   5370000   5376000   5400000 
    #>         9         1         1         8         1         2         3 
    #>   5405000   5450000   5450180   5488980   5500000   5550000   5562000 
    #>         1         1         2         1        23         1         1 
    #>   5568000   5600000   5605000   5650000   5700000   5725000   5750000 
    #>         1         7         1         2         3         2         4 
    #>   5757000   5758858   5800000   5819500   5830000   5832500   5840000 
    #>         1         1        10         1         1         1         1 
    #>   5850000   5900000   5940000   5987940     6e+06   6007000   6020000 
    #>         1         4         1         1        31         1         1 
    #>   6023000   6050000   6100000   6102500   6107500   6116000   6137000 
    #>         1         2         6         1         1         1         1 
    #>   6150510   6164000   6165000   6200000   6207500   6210000   6213127 
    #>         1         1         1        14         1         1         1 
    #>   6240500   6252000   6300000   6349354   6378120   6400000   6448750 
    #>         3         1         5         1         2         5         1 
    #>   6460000   6488810   6498862   6499862   6500000   6530000   6531000 
    #>         2         1         1         2        14         2         2 
    #>   6537000   6550000   6570890   6597520   6600000   6600500   6603223 
    #>         1         1         1         1         7         1         1 
    #>   6610000   6629000   6633191   6650000   6685000   6700000   6738740 
    #>         4         1         1         2         1         5         1 
    #>   6750000   6758000   6760000   6791000   6800000   6810000   6860000 
    #>         2         1         3         1         8         1         1 
    #>   6880194   6894000   6900000   6950000   6968960   6988536   6994000 
    #>         2         2         3         2         1         3         1 
    #>     7e+06   7065000   7100000   7145000   7150000   7173000   7200000 
    #>        32         1         2         2         1         1         5 
    #>   7218080   7225000   7250000   7269500   7300000   7369366   7400000 
    #>         1         4         3         1         3         1         5 
    #>   7420200   7454244   7500000   7515000   7550000   7560000   7580000 
    #>         1         1        23         1         1         1         1 
    #>   7600000   7670000   7700000   7705500   7795000   7800000   7813500 
    #>         6         3         7         1         1         6         1 
    #>   7879905   7900000   7931823   7949000   7964500   7980770     8e+06 
    #>         1         4         1         1         1         1        21 
    #>   8030000   8035200   8050000   8080500   8087000   8100000   8150000 
    #>         1         1         2         1         2         3         2 
    #>   8160000   8200000   8250000   8255018   8283500   8300000   8340000 
    #>         1         5         1         2         1         8         1 
    #>   8400000   8455000   8494724   8500000   8525000   8571000   8600000 
    #>         2         1         1         8         1         1         3 
    #>   8683500   8695000   8700000   8729977   8785000   8800000   8802390 
    #>         1         1         2         1         1         2         1 
    #>   8820000   8888000   8899635   8900000   8932475   8960000     9e+06 
    #>         1         2         1         1         1         1        27 
    #>   9012000   9062500   9072401   9081000   9127000   9150000   9160500 
    #>         1         1         1         2         3         1         1 
    #>   9200000   9300000   9400000   9430000   9435000   9436240   9500000 
    #>         1         3         1         1         1         1         7 
    #>   9505500   9590230   9600000   9615000   9700000   9800000   9840000 
    #>         1         1         1         1         1         1         1 
    #>   9855800   9865300   9900000   9913500   9925000   9975000     1e+07 
    #>         1         1         2         1         1         2        26 
    #>  10000105  10001000  10047404  10095000  10210000  10220000  10316000 
    #>         1         1         1         2         2         1         1 
    #>  10400000  10429000  10505000  10700000  10760000  10800000  10900000 
    #>         1         2         1         1         2         2         2 
    #>  10910000  10925500  10960000   1.1e+07  11001000  11010938  11065000 
    #>         1         1         2        10         1         1         1 
    #>  11071000  11074000  11100000  11186740  11200000  11270000  11299000 
    #>         1         1         2         1         1         1         1 
    #>  11300000  11353000  11400000  11470000  11475000  11500000  11550000 
    #>         3         1         1         1         2         9         1 
    #>  11600000  11617000  11620500  11653000  11675000  11700000  11721980 
    #>         3         1         1         1         1         5         1 
    #>  11740065  11800000  11850000  11900000  11916120   1.2e+07  12011452 
    #>         1         1         1         2         1        11         1 
    #>  12102980  12118990  12140000  12200000  12252639  12300000  12400000 
    #>         1         1         1         4         1         1         1 
    #>  12442000  12480000  12500000  12520000  12620500  12650000  12700000 
    #>         2         1         5         1         1         1         2 
    #>  12755000  12800000  12822000  12873210   1.3e+07  13157500  13281000 
    #>         2         1         1         2         4         2         2 
    #>  13300000  13310000  13472000  13530000  13735000  13780000  13785000 
    #>         2         1         1         1         1         2         1 
    #>  13800000  13850000  13900000   1.4e+07  14100000  14250000  14312950 
    #>         3         1         2         3         1         1         1 
    #>  14442866  14450000  14500000  14530000  14609000  14700000  14800000 
    #>         1         1         2         1         1         1         1 
    #>   1.5e+07  15275000  15500000  15580000  15616130  15700000  15800000 
    #>        10         2         3         1         1         5         1 
    #>  15852000  15900000   1.6e+07  16100000  16200000  16250000  16312000 
    #>         1         1         4         1         4         1         1 
    #>  16448500  16500000  16525563  16738740  16779400  16900000   1.7e+07 
    #>         1         1         1         1         1         2         6 
    #>  17015000  17252639  17300000  17500000  17538150  17600000  17875000 
    #>         1         1         1         2         2         1         1 
    #>  17900000   1.8e+07  18094424  18400000  18431500  18500000  18600000 
    #>         1         3         2         3         4         6         1 
    #>  18700000  18750000  18920000   1.9e+07  19500000  19505018  19600000 
    #>         1         1         1         3         2         1         1 
    #>  19647700  19650000  19700000     2e+07  20200000  20300000  20492000 
    #>         1         2         1         6         1         1         1 
    #>  20500000  20504819  20740000  20750000  20900000  20939341  21067000 
    #>         1         2         1         1         1         1         1 
    #>  21300000  21500000  21600000  21763335  21771000  21771500  21814365 
    #>         1         1         1         1         1         1         1 
    #>  21870000  21880000   2.2e+07   2.3e+07  23333000  23500000  23700000 
    #>         1         1         4         1         1         1         1 
    #>  23729740  23800000   2.4e+07  24100000  24622720  24886500   2.5e+07 
    #>         2         1         2         1         1         1         3 
    #>  25500000   2.6e+07  26374671  26465000  26941500   2.7e+07  27052082 
    #>         1         7         1         1         1         1         2 
    #>  27700000   2.8e+07  28003000  28050000  28200000  28406000  28750000 
    #>         1         2         1         1         2         1         1 
    #>   2.9e+07  29400000     3e+07  30114981   3.1e+07  31500000   3.2e+07 
    #>         2         2         1         1         1         1         1 
    #>  32500000   3.3e+07  34115800  34521652  34900000  36369000   3.7e+07 
    #>         2         1         1         7         1         1         1 
    #>  37010180   3.8e+07   3.9e+07  39755500  40750000  40911000  41200000 
    #>         1         1         1         1         1         1         1 
    #>  41445500   4.3e+07   4.5e+07   4.6e+07  46800000   4.7e+07  48683000 
    #>         1         2         1         1         1         2         3 
    #>  49503470     5e+07   5.1e+07   5.2e+07  52500000  53402840  53500000 
    #>         1         1         1         3         1         1         3 
    #>   5.4e+07  54100000  54700000  54752840  54879481   5.5e+07  57290000 
    #>         1         1         1         2         1         2         2 
    #>  58500000   6.5e+07   6.7e+07   6.9e+07   7.2e+07  72250000  72500000 
    #>         1         2         1         1         1         1         1 
    #>     8e+07   8.3e+07  83502000   8.5e+07     9e+07   9.5e+07  96150000 
    #>         2         1         1         1         2         1         1 
    #>  97657595  98400000     1e+08 103530100  1.07e+08  1.09e+08   1.1e+08 
    #>         1         1         3         1         1         1         1 
    #> 116000008  1.17e+08 119176000   1.2e+08  1.32e+08  1.37e+08   1.4e+08 
    #>         1         1         1         1         1         1         1 
    #>   1.5e+08 153700000  1.56e+08   1.8e+08   2.3e+08  2.45e+08  2.55e+08 
    #>         1         2         1         2         1         1         1 
    #>   3.5e+08 368163510  4.35e+08   5.1e+08 600610000     7e+08   8.5e+08 
    #>         1         1         1         2         1         1         1

    #> 
    #>  -99    0    1    2    3    4    5    8   99 
    #> 6850 3536  128   64   26    4    1    7    2

    #> [1] "PP-271-Muhammad Imran-0"        "PS-125-Anwar Hussain-0"        
    #> [3] "Reserved-NA-Tehmina Faheem-0"   "Reserved-NA-Rukhsana Hassain-0"
    #> [5] "PS-23-Shahid Ali Shah-0"

1.  Jewellery: 8309 candidates have reported the value of jewellery
    owned, and of those for which value is not available, we have the
    weights of 7610 as shown in the table below. TODO get distribution
    of valuations of jewelry that people are using: rupees / weight for
    those that reported both, and then create imputed jewel\_val for
    those with missing jewelry using the mean of this valuation

<!-- -->

    #>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    #>        0    50000    50000    81110    55000 77777778

1.  Cash in hand and at bank are estimates of the total funds in cash
    available to any candidate. This may be important both in that we
    expect this to be the main pool from which expenditure for the
    campaign may take place, given it is not easy to sale off immovable
    property to fund an election at short notice (assuming the records
    are up to date till submission of nomination forms). We have total
    cash details available for 12141 candidates, which is a good sign.

<!-- -->

    #> 
    #> FALSE  TRUE 
    #>  5124 12141

1.  Agricultural and total income as declared in affidavits.
2.  Size of Agricultural land holdings. This is varying since some have
    reported in Acres, others in Kanals. We have such details for 1293
    candidates

<!-- -->

    #> 
    #> FALSE  TRUE  <NA> 
    #>   188  1293 15784

1.  Current fy and past fy net assets: we have reported values for both
    of these for 10987 candidates TODO chnage to case\_when for &gt;0,
    ==0, and ==-99 TODO ensure that 0s are actual 0s not blank spaces

<!-- -->

    #>                    fypast_assets_av
    #> fycurrent_assets_av Available (>0) Missing (-99) Zero (=0)  <NA>
    #>      Available (>0)          10162           287       410     0
    #>      Missing (-99)              55          2119        12     0
    #>      Zero (=0)                  73            10      4137     0
    #>      <NA>                        0             0         0     0

    #> Warning in log(delta_assets): NaNs produced

    #> Warning in log(-(delta_assets)): NaNs produced

![](00_generate_asset_checks_files/figure-markdown_strict/unnamed-chunk-14-1.png)

    #> Warning in log(current_fy_net_assets + 1): NaNs produced

![](00_generate_asset_checks_files/figure-markdown_strict/unnamed-chunk-14-2.png)

    #> Warning in log(current_fy_net_assets + 1): NaNs produced

    #> Warning in log(past_fy_net_assets + 1): NaNs produced

![](00_generate_asset_checks_files/figure-markdown_strict/unnamed-chunk-14-3.png)

    #> Warning in log(current_fy_net_assets + 1): NaNs produced

    #> Warning in log(current_fy_net_assets + 1): NaNs produced

![](00_generate_asset_checks_files/figure-markdown_strict/unnamed-chunk-14-4.png)

    #> Warning: Removed 2483 rows containing non-finite values (stat_binhex).

![](00_generate_asset_checks_files/figure-markdown_strict/unnamed-chunk-14-5.png)

    #> Warning: Removed 2483 rows containing missing values (geom_point).

![](00_generate_asset_checks_files/figure-markdown_strict/unnamed-chunk-14-6.png)

    #> Warning: Removed 2483 rows containing missing values (geom_point).

![](00_generate_asset_checks_files/figure-markdown_strict/unnamed-chunk-14-7.png)

### Affidavit data

TODO start cleaning and categorizing education, occupation, and amounts
paid to parties and amounts received from parties

##### Steps to consider going forward

-   CNICs: What to do about unmatched cnics (ones not in scrutiny)
-   Occupation and Education variables need to be cleaned and
    categorized
-   It would be useful to group asset types in a few broad categories
    such as:
-   Immovable Property Value Total
-   Agricultural Land Ownership Value
-   Agricultural Income
-   Movable Property Value Total (includes cash, automobile, jewellery)
-   To evaluate land at current prices, it might be useful to look at
    Land Revenue Annual *Mossat* *Bay* (reports land price value at
    mouza level)
-   For jewellery, I am unsure since the weight of jewellery does not
    capture type (silver, gold) whose prices may differ.

#### 5. Duplicated Rows?

Evaluating the data

-   Completely identical rows:

<!-- -->

    #> d
    #> FALSE 
    #> 17265

This shows that are **no rows** that are completely identical, i.e. each
row is unique when checked across all variables.

-   Rows duplicated with candidate identfiers:

<!-- -->

    #> d
    #> FALSE  TRUE 
    #> 17189    76

There are now only 76 duplicated rows with the main identifiers, a major
improvement from the earlier ~408 duplicates. There are still duplicates
but now we have a unique key identifier `key`.

-   Rows duplicated with candidate identfiers + enum\_name:

<!-- -->

    #> d
    #> FALSE  TRUE 
    #> 17224    41

-   Rows duplicated with candidate identfiers + cnic:

<!-- -->

    #> d
    #> FALSE  TRUE 
    #> 17260     5

If we add cnic we only get 5 duplicates. Much much better. And if we add
father's name:

    #> d
    #> FALSE  TRUE 
    #> 17264     1

Just one duplicated row. Which is great.

The data is completely identified by:

1.  "key"
2.  c("enum\_name", "type\_seat", "const\_number", "uid",
    "candidate\_name", "father\_name", "cnic")

#### 6. Checking for other errors

    errs%>%
      filter(!CNIC_val_error & !cnic_missing)%>%
      select(type_seat, const_number, candidate_name, cnic, contact_num, contact_num_num_val_error)

    #> # A tibble: 2 x 6
    #>   type_seat const_number candidate_name cnic  contact_num contact_num_num…
    #>   <chr>     <dbl+lbl>    <chr>          <chr> <chr>       <lgl>           
    #> 1 NA        23           Aftab Ahmad K… 1730… 091-5846091 TRUE            
    #> 2 NA        24           Asfndyar Wali… 1710… 091-6560560 TRUE

The only other error that is flagged is for two entries' contact number
errors. This however, isn't an issue because they've written their
residential phone numbers with a "-" in between.

#### 7. Is there anything we need to look at now?

In my opinion, just the missing cnics. If we do face problems ahead
it'll be with merging with the candidate lists and election results, but
I cannot exactly predict how big of a problem that will be. In any case,
we can expect to match 13000+ candidates cleanly.
