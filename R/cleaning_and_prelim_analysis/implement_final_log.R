# implement final log
library(tidyverse)
library(lubridate)
# koboAPI::addStartCols_sm
# cleaning_log<-read_csv("outputs/20210310_cleaning_log.csv")
cleaning_log<-read_csv("inputs/auto_cl/20210315_cleaning_log_DvZ.csv")
# raw_data<- read_csv("inputs/raw_data/20210304_rapid_markets_data.csv")
raw_data<- read_csv("inputs/raw_data/final_raw_data_from_kobo_server.csv")
ks<- readxl::read_xlsx(path = "inputs/tool/rapid_markets_tool.xlsx",sheet = "survey")
kc<- readxl::read_xlsx(path = "inputs/tool/rapid_markets_tool.xlsx",sheet = "choices")

# since i removed the enumerator column from the dataset i should not clean that column
cleaning_log<-cleaning_log %>%
  filter(name!="enumerator")

# These are all new choices that we want to add as options in the data set
new_vars<-tibble::tribble(
  ~name,              ~choice, 
  "agric_y_not",                  "no_land",
  "support_agric_land",   "facilitate_access_land",
  "support_agric_land", "facilitate_access_market",
  "support_agric_land",                 "manpower",
  "sector_appeal",        "personal_interest",
  "work_support",   "facilitate_access_land",
  "work_support",        "no_support_needed",
  "support_animal_husbandry",   "facilitate_access_land",
  "chall_find_job",             "lack_network",
  "chall_find_job",           "lack_equipment",
  "livestock_type",                  "rabbits",
  "mkt_trade_chall",           "health_reasons",
  "mkt_trade_chall", "lack_creditor_repayments",
  "additional_livelihood",              "bee_keeping",
  "main_livelihood",              "bee_keeping",
  "mkt_trade_chall",              "lack_credit",
  "mkt_trade_chall", "lack_mrkt_infrastructure",
  "additional_livelihood",                  "hunting",
  "support_agric_land",  "transport_access_market",
  "reason_less_y",               "high_costs",
  "sector_pref", "bee_keeping"
)
# make sure there are no duplicates
new_vars<- distinct(new_vars) %>% arrange(name)

# create kobold object
kbo<- kobold::kobold(survey = ks,
                     choices = kc,
                     data = raw_data,
                     cleaning = cleaning_log)


# made function to add choices to xlsform tool 
kc_modified<-butteR:::xlsform_add_choices(kobold =kbo,new_choices =  new_vars)

# if the new variables are select multiple we need to add those columns to the data set itself
new_vars_lu<- new_vars %>% 
  left_join(ks %>% 
              mutate(
                q_type= ifelse(str_detect(type,"select_one|select one"),"so","sm")
              ) %>% 
              select(name,q_type), by= c("name"="name")
  )
new_vars_lu_sm <-new_vars_lu %>% 
  filter(q_type=="sm") %>% 
  mutate(new_cols=paste0(name,"/",choice))

# make them default to FALSE, the kobold cleaner will impute relevant skip logic, and 
# when options are added in the cleaning log they will switch from F->T
df_modified<-raw_data %>% 
  butteR:::mutate_batch(nm=new_vars_lu_sm$new_cols,value = F)

# just need to add this column to create a unique pt id 
df_modified<- df_modified %>% 
  mutate(
    status_loc=ifelse(status=="refugee_settlement", paste0(refugee_settle_name,"_refugee"),
                      paste0(district_name,"_host")) %>% snakecase::to_snake_case(),
    unique_pt_id= paste0(status_loc,"_",point_number),
    start_date= as_date(start)
    
  )

# remake kobold object with new choices sheet, data set, and cleaning log
kbo<- kobold::kobold(survey = ks %>%
                       filter(name %in% colnames(df_modified)),
                     choices = kc_modified,
                     data = df_modified,
                     cleaning = cleaning_log)
kbo_cleaned<- kobold::kobold_cleaner(kbo)

#write out clean data set
write_csv(kbo_cleaned$data,file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data.csv"))


