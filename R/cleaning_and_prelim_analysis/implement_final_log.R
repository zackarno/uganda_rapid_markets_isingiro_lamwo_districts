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
# copied these new options with data pasta from a googlesheet
mutate_batch<- function(df,nm, value=NA){
  df %>% 
    tibble::add_column(!!!set_names(as.list(rep(value, length(nm))),nm=nm))
  
}

new_vars<-tibble::tribble(
  ~vars,              ~choice, 
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

new_vars<- distinct(new_vars) %>% arrange(vars)
# look up table
name_list_name<-ks %>% 
  mutate(
    list_name= str_replace_all(string = type, pattern = "select_one|select one|select_multiple|select multiple","") %>% 
      trimws()
  ) %>% select(list_name, name)



new_vars$vars[new_vars$vars%in%name_list_name$name]
kc %>% 
  left_join(name_list_name, by="list_name")
new_vars_lu<-new_vars %>% 
  left_join(name_list_name, by = c("vars"="name"))

new_vars_split<- new_vars_lu %>%
  select(list_name,name=choice) %>% 
  mutate(label=name) %>% 
  split(.$list_name)

kcl<-kc %>% 
  split(.$list_name)
kcl_mod<- kcl %>% 
  keep(names(.) %in% new_vars_lu$list_name)

kc_new_list<-list()
for(i in names(kcl_mod)){
  kc_temp<-kcl[i]
  nv_temp<- new_vars_split[i]
  kc_new_list[i]<-bind_rows(kc_temp,nv_temp)
}
kcl[names(kc_new_list)]<-kc_new_list
kobo_choices_modified<- bind_rows(kcl)

kobo_choices_modified %>% 
  filter(list_name=="livestock_type_list")

new_vars_lu<- new_vars_lu %>% 
  left_join(ks %>% 
              mutate(
                q_type= ifelse(str_detect(type,"select_one|select one"),"so","sm")
              ) %>% 
              select(name,q_type), by= c("vars"="name")
  )
new_vars_lu_sm <-new_vars_lu %>% 
  filter(q_type=="sm") %>% 
  mutate(new_cols=paste0(vars,"/",choice))

df_modified<-raw_data %>% 
  mutate_batch(nm=new_vars_lu_sm$new_cols,value = F)




# hopefully status_loc wont cause any issues
df_modified<- df_modified %>% 
  mutate(
    status_loc=ifelse(status=="refugee_settlement", paste0(refugee_settle_name,"_refugee"),
                      paste0(district_name,"_host")) %>% snakecase::to_snake_case(),
    unique_pt_id= paste0(status_loc,"_",point_number),
    start_date= as_date(start)
    
  )


kbo<- kobold::kobold(survey = ks %>%
                       filter(name %in% colnames(df_modified)),
                     choices = kobo_choices_modified,
                     data = df_modified,
                     cleaning = cleaning_log)
kbo_cleaned<- kobold::kobold_cleaner(kbo)



kbo_cleaned$data<- kbo_cleaned$data %>% select(-contains("geopoint"))
kbo_cleaned$data %>% select(starts_with("chall_find_job")) %>% View()

write_csv(kbo_cleaned$data,file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data.csv"))



