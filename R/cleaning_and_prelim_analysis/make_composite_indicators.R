# make composite indicators


make_rapid_market_composite_indicators<-function(df){
  
  ###############################
  
  edu_tbl<-tibble::tribble(
    ~old , ~new,
    "no_formal_education",    "None",
    "completed_primary",    "Low",
    "incomplete_primary",    "Low",
    "completed_secondary", "Middle",
    "incomplete_secondary",    "Low",
    "completed_university", "Higher",
    "incomplete_university", "Middle",
    "incomplete_prof_degree", "Middle",
    "completed_prof_degree", "Higher",
    "incomplete_voc_training", "Middle",
    "completed_voc_training", "Middle",
    "other",  "Other"
  )
  edu_lookup<- setNames(object = edu_tbl$new,nm = edu_tbl$old)
###############################
  df<- df %>% 
    mutate( 
      i.income_cat= case_when( monthly_y %in% c("less_equal_20000", "21000_50000") ~ "Very Poor",
                               monthly_y == "51001_100000"~ "Poor",
                               monthly_y %in% c("100001_300000", "300001_500000")~ "Middle",
                               monthly_y == "500001_1,000000"~ "High",
                               monthly_y == "1000001_and_above"~ "Higher",
                               TRUE~ "problemo"
      ),
      i.education_cat= recode(hoh_education,!!!edu_lookup),
      # i.education_cat= recode(hoh_education,!!!edu),
      i.animial_sustain_hh_tf= animal_sustain_hh %in% c("yes_through_animals", "no_through_other_sources"),
      i.land_sustain_hh_tf= agric_land_adequate %in% c("yes_met_hh_needs", "no_through_other_sources"),
      i.subsistence_livestock_tf = puropse_keep_animals %in% c("provide_food", "provide_food_y"),
      i.main_livelihood_agriculture = main_livelihood %in% c("farming_own_land",
                                                             "cash_crop_own_land",
                                                             "subsistance_hired_land",
                                                             "cash_crop_hired_land",
                                                             "paid_agric_labor",
                                                             "livestock",
                                                             "fishing",
                                                             "forestry"),
      i.hh_size_cat = case_when(hh_size<4 ~ "Small Household",
                                hh_size<7 ~ "Medium Household",
                                hh_size<11~ "Large Household",
                                TRUE~ "Very Large Houesehold"),
      
  i.number_earners_cat = case_when(hh_y_size==0 ~ "No Earners",
                            hh_y_size>0 ~ "One Earner",
                            hh_y_size>1~ "Multple Earners"
  ),
  i.yr_displaced_cat =  case_when(yr_displaced %in% c("less_1_yr","1_2_yrs","2_3_yrs")~ "0_3_yrs",
                                  yr_displaced %in% c("3_4_yrs","4_5_yrs")~ "3_5_yrs",
                                  yr_displaced %in% c("5_6_yrs","6_7_yrs", "7_8_yrs", "8_9_yrs","9_10_yrs")~"5_10_yrs",
                                  yr_displaced %in% c("greater_10_yrs")~"gte_10_yrs",
                                  TRUE ~yr_displaced
                                  )
  
      
    ) 
  
  
  
  
  
  seeking_work<-c("males_6_12_s", "females_6_12_s", "males_13_17_s", "females_13_17_s", 
                  "males_18_59_s", "females_18_59_s", "males_60_above_s", "females_60_above_s")
  
  males_seeking_work<-seeking_work[str_detect(seeking_work, "^males")]
  females_seeking_work<-seeking_work[str_detect(seeking_work, "^females")]
  
  
  working<-c("males_6_12_w", "females_6_12_w", "males_13_17_w", "females_13_17_w", 
             "males_18_59_w", "females_18_59_w", "males_60_above_w", "females_60_above_w")
  
  males_working<-working[str_detect(working, "^males")]
  females_working<-working[str_detect(working, "^females")]
  
  minors_seeking_work <- c("males_6_12_s", "females_6_12_s", "males_13_17_s", "females_13_17_s")
  minors_working<- c("males_6_12_w", "females_6_12_w", "males_13_17_w", "females_13_17_w")
  
  
  
  
  df<- df %>% 
    rowwise() %>% 
    mutate(
      # seeking work intermediates
      int.total_seeking_work=sum(c_across(all_of(seeking_work)),na.rm=T),
      int.men_seeking_work=sum(c_across(all_of(males_seeking_work)),na.rm=T),
      int.females_seeking_work=sum(c_across(all_of(females_seeking_work)),na.rm=T),
      # working intermediates
      int.total_working=sum(c_across(all_of(working)),na.rm=T),
      int.men_working=sum(c_across(all_of(males_working)),na.rm=T),
      int.females_working=sum(c_across(all_of(females_working)),na.rm=T),
      
      # number minors working
      int.minors_working=sum(c_across(all_of(minors_working)),na.rm=T),
      int.minors_seeking_work=sum(c_across(all_of(minors_seeking_work)),na.rm=T)
      
    ) %>% 
    ungroup() %>% 
    # seeking work gender
    mutate(
      i.seek_work_gender= case_when(
        int.total_seeking_work== 0 ~ "none",
        int.men_seeking_work>0 & int.females_seeking_work>0 ~ "both",
        int.men_seeking_work>0 & int.females_seeking_work==0 ~ "men",
        int.men_seeking_work==0 & int.females_seeking_work>0 ~ "women",
        
      ),
      # working gender
      i.working_gender= case_when(
        int.total_working== 0 ~ "none",
        int.men_working>0 & int.females_working>0 ~ "both",
        int.men_working>0 & int.females_working==0 ~ "men",
        int.men_working==0 & int.females_working>0 ~ "women"
        
      ),
      # minors working
      i.minors_working = int.minors_working>0,
      # minors seeking work
      i.minors_seeking_work = int.minors_seeking_work>0
      
    ) %>% 
    select(-all_of(starts_with('int.')))
  return(df)
}


