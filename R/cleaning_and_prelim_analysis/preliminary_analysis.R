library(tidyverse)
library(srvyr)
library(janitor)
library(glue)
source("R/cleaning_and_prelim_analysis/make_weights.R")
source("R/cleaning_and_prelim_analysis/make_composite_indicators.R")


df<- read_csv("outputs/20210315_clean_data.csv")
# dap_sheet<- "https://docs.google.com/spreadsheets/d/14p0UdcIR8oPoa0YD-FOZNV30cgRUQXJdP-qQ60ljZA4/edit#gid=193229464"
# dap<-googlesheets4::read_sheet(ss = dap_sheet, sheet = "analysis_plan")
dap<- read_csv("inputs/dap/r_dap.csv")
dap<- dap %>% janitor::clean_names()


# run from here to the bottom of the script. It takes 21 minutes on my computer so go have coffee
# its calculating > 28k point estimates.

start<- Sys.time() 
# load in hh-level population data sets
host_pop<- read_csv("inputs/20210224_host_pop_cleaned.csv")
ref_pop<- read_csv("inputs/ref_pops.csv")


# make composite indicators -----------------------------------------------

df_with_composites<-make_rapid_market_composite_indicators(df)
# split into refugee & host
df_with_composites<- df_with_composites %>% 
  mutate(
    strata= case_when(
      status_loc %in% c("nakivale_refugee","oruchinga_refugee")~"isingiro_refugee",
      status_loc == "palabek_refugee"~ "lamwo_refugee",
      TRUE ~ status_loc
    )
  )

df_ref<-df_with_composites %>% filter(status=="refugee_settlement")
df_host<-df_with_composites %>% filter(status=="host_community")



# create weights ----------------------------------------------------------
#' The make weight table functions are just specific to this script,
#' I only made them to save space and make the script easier to follow.
#' to review the weighting methodology you can review the functions and for more
#' details refer to xxx

# create refugee only weights
refugee_weight_table<- make_refugee_weight_table(df = df_ref,ref_pop =ref_pop)
df_ref_with_weights<-df_ref %>% 
  left_join(refugee_weight_table, by= "strata")

# create host only weights for cluster design
df_host<- df_host %>% 
  mutate(
    district_sub_county= paste0(district_name, "_",sub_county_name),
    cluster_id= paste0(strata, "_", parse_number(unique_pt_id))
  )

host_weight_table<- make_host_weight_table(host_data = df_host,
                                           host_pop = host_pop)

df_host_with_weights<-df_host %>% 
  left_join(
    host_weight_table %>%
      select(district_name, district_weights), by ="district_name"
  ) 

# create combined refugee and host weights
combined_weight_table<-make_combined_weight_table(df = df_with_composites,
                           refugee_weight_table = refugee_weight_table, 
                           host_weight_table = host_weight_table)

#
df_with_composites<- df_with_composites %>% 
  mutate(
    cluster_id= case_when(
      status=="host_community"~
        paste0(strata, "_", parse_number(unique_pt_id)),
      TRUE ~strata),
    pop_group= case_when(
      status=="host_community"~glue("{district_name}_host"),
      TRUE~ strata
    )
  )


df_with_combined_weights<- df_with_composites %>% 
  left_join(
    combined_weight_table, by =c("pop_group")
  )


# set up design objects ---------------------------------------------------
refsvy<-survey::svydesign(ids = ~ 1,
                          strata =  ~strata,
                          weights= ~weights,
                          data = df_ref_with_weights)

hostsvy <- survey::svydesign(data = df_host_with_weights,
                                  ids = ~cluster_id, 
                                  strata = ~district_name,
                                  weights = ~district_weights
                                  # nest = T
)

refhostsvy <- survey::svydesign(data = df_with_combined_weights,
                                  ids = ~cluster_id, 
                                  strata = ~ district_name, 
                                  weights = ~weights,
                                  nest = T
)
#survey-> srvyr
refsvy<-as_survey(refsvy)
hostsvy<- as_survey(hostsvy)
refhostsvy<- as_survey(refhostsvy)



# split daps for different analyses ---------------------------------------
# dap to be implemented on refugee and host community data sets
dap_refugee<-dap %>% 
  filter( split %in%  c("All","ref_only"), !is.na(subset_1))

# remove analysis that needs subsetting from the DAP
refugee_variables_no_subsets<- dap_refugee %>% 
  pull(variable)



# dap refugee & host with subsets
dap_refugee_subset1<-dap %>% 
  filter( split %in%  c("All","ref_only"), !is.na(subset_1))




outputs<-list()
# refugee -----------------------------------------------------------------

outputs$ref_dist<-
  butteR::survey_collapse(df = refsvy,
                          vars_to_analyze = refugee_variables_no_subsets, 
                          disag="district_name") %>% 
  mutate(population="refugee")


# refugee overall, no additional subset
outputs$ref_overall<-
  butteR::survey_collapse(df = refsvy,
                  vars_to_analyze = refugee_variables_no_subsets, 
                  ) %>% 
  mutate(population="refugee")


# refugee overall, subset 1
dap_refugee_subset_split<-dap_refugee_subset1 %>% 
  split(.$subset_1)
ref_overall_subset1<-list()
for(i in seq_along(dap_refugee_subset_split)){ #seq_along(dap_single_subset_split)){
  print(i)
  subset_temp<-dap_refugee_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  ref_overall_subset1[[subset_value]]<- butteR::survey_collapse(df = refsvy,
                                                          vars_to_analyze =vars_temp ,
                                                          disag = c( subset_value) 
  )
  
}
outputs$ref_overall_subset1<- bind_rows(ref_overall_subset1) %>% 
  mutate(population= "refugee")

# refugee overall by district & subset 1
ref_district_subset1<-list()
for(i in seq_along(dap_refugee_subset_split)){ #seq_along(dap_single_subset_split)){
  print(i)
  subset_temp<-dap_refugee_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  ref_district_subset1[[subset_value]]<- butteR::survey_collapse(df = refsvy,
                                                        vars_to_analyze =vars_temp ,
                                                        disag = c( "district_name",subset_value) 
  )
  
}
outputs$ref_district_subset1<- bind_rows(ref_district_subset1) %>% 
  mutate(population= "refugee")

# host --------------------------------------------------------------------

dap_host<-dap %>% 
  filter( split %in%  c("All"), !is.na(subset_1))
host_variables_no_subsets<- dap_host %>% 
  pull(variable)
dap_host_subset1<-dap %>% 
  filter( split %in%  c("All"), !is.na(subset_1))
# host by district, no additional subsets
outputs$host_dist<-
  butteR::survey_collapse(df = hostsvy,
                  vars_to_analyze = host_variables_no_subsets, 
                  disag="district_name") %>% 
  mutate(population="host")

# host overall, no additional subset
outputs$host_overall<-
  butteR::survey_collapse(df = hostsvy,
                  vars_to_analyze = host_variables_no_subsets, 
                  ) %>% 
  mutate(population="host")


# HOST SINGLE SUBSET

# host district, subset 1
dap_host_subset_split<-dap_host_subset1 %>% 
  split(.$subset_1)

host_district_subset1<-list()

for(i in seq_along(dap_host_subset_split)){ #seq_along(dap_single_subset_split)){
  print(i)
  subset_temp<-dap_host_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  host_district_subset1[[subset_value]]<- butteR::survey_collapse(df = hostsvy,
                                                          vars_to_analyze =vars_temp ,
                                                          disag = c("district_name", subset_value) 
  )
  
}
outputs$host_subset1<- bind_rows(host_district_subset1) %>% 
  mutate(population="host")

# host overall, subset 1

host_overall_subset1<-list()

for(i in seq_along(dap_host_subset_split)){ #seq_along(dap_single_subset_split)){
  print(i)
  subset_temp<-dap_host_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  host_overall_subset1[[subset_value]]<- butteR::survey_collapse(df = hostsvy,
                                                               vars_to_analyze =vars_temp ,
                                                               disag = c(subset_value) 
  )
  
}
outputs$host_subset1<- bind_rows(host_overall_subset1) %>% 
  mutate(population="host")

# host and refugee combined -----------------------------------------------
# these will be the same, just duplicating for clarity
combined_variables_no_subset<- host_variables_no_subsets
# combined pops  by district, no additional subsets
outputs$combined_pops_dist<-
  butteR::survey_collapse(df = refhostsvy,
                  vars_to_analyze = combined_variables_no_subset, 
                  disag="district_name") %>% 
  mutate(population="combined")

# combined pops overall, no additional subset
outputs$combined_pops_overall<-
  butteR::survey_collapse(df = refhostsvy,
                  vars_to_analyze = combined_variables_no_subset, 
  ) %>% 
  mutate(population="combined")


# COMBINED POPS SINGLE SUBSET

# host district, subset 1
dap_combined_subset1<- dap_host_subset1 # its the same, just duplicating for readability
dap_combined_subset_split<-dap_combined_subset1 %>% 
  split(.$subset_1)

combined_district_subset1<-list()
for(i in seq_along(dap_combined_subset_split)){
  print(i)
  subset_temp<-dap_combined_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  combined_district_subset1[[subset_value]]<- butteR::survey_collapse(df = refhostsvy,
                                                          vars_to_analyze =vars_temp ,
                                                          disag = c("district_name", subset_value) 
  )
  
}
outputs$combined_district_subset1<- bind_rows(combined_district_subset1) %>% 
  mutate(population="combined")

# overall single subset
combined_overall_subset1<-list()
for(i in seq_along(dap_combined_subset_split)){
  print(i)
  subset_temp<-dap_combined_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  combined_overall_subset1[[subset_value]]<- butteR::survey_collapse(df = refhostsvy,
                                                              vars_to_analyze =vars_temp ,
                                                              disag = c( subset_value) 
  )
  
}
outputs$combined_overall_subset1<- bind_rows(combined_overall_subset1) %>% 
  mutate(population="combined")

full_analysis_long<- bind_rows(outputs)
end<- Sys.time()
end-start

full_analysis_long %>%
  write_csv(paste0(butteR::date_file_prefix(),"_full_analysis_long_format.csv"),na="")


# plots -------------------------------------------------------------------

analysis_groups<-c('refugee','host', 'combined')
district_level_plots<-list()

for(i in seq_along(analysis_groups)){
  district_level_plots[[analysis_groups[i]]]<-full_analysis_long %>% 
    filter( population %in% c(analysis_groups[i]),
            subset_1_name=="district_name",is.na(subset_2_name)) %>% 
    mutate(question_val= paste0(variable,".",variable_val)) %>% 
    ggplot(aes(x= question_val,y= `mean/pct`,color= subset_1_val))+
    geom_point(stat="identity", position = position_dodge(width = 0.3))+
    geom_errorbar(aes(ymin= `mean/pct_low`, ymax= `mean/pct_upp`), 
                  width=0.2,position = position_dodge(width = 0.3))+
    geom_text(aes(x=question_val,y=`mean/pct`, label=n_unweighted), nudge_x = 0.3)+
    scale_y_continuous(labels = scales::percent,breaks = seq(0,1,by=0.1))+
    labs(color= "District")+
    ggtitle(label = glue::glue("{analysis_groups[i]} population by district"))+
    coord_flip()+
    theme_bw()+
    theme(
      axis.title = element_blank(),
      legend.title= element_blank()
    ) 
  ggsave(glue::glue("outputs/graphs/{analysis_groups[i]}_pop_by_district.pdf"),
         height = 36,
         width = 11,
         units = "in",
         device = "pdf")
  }



# lamwo by pop
analysis_groups<-c('lamwo','isingiro')
status_level_plots<-list()
full_analysis_long$subset_1_val %>% tabyl()
for(i in seq_along(analysis_groups)){
  status_level_plots[[analysis_groups[i]]]<-
    full_analysis_long %>% 
  filter( population %in% c("refugee","host"),
          subset_1_name=="district_name",is.na(subset_2_name), subset_1_val==analysis_groups[i]) %>% 
  mutate(question_val= paste0(variable,".",variable_val)) %>% 
  ggplot(aes(x= question_val,y= `mean/pct`,color= population))+
  geom_point(stat="identity", position = position_dodge(width = 0.3))+
  scale_colour_manual(values = c( "grey", "black"))+
  geom_errorbar(aes(ymin= `mean/pct_low`, ymax= `mean/pct_upp`), 
                width=0.2,position = position_dodge(width = 0.3))+
  # geom_text(aes(x=question_val,y=`mean/pct`, label=n_unweighted), nudge_x = 0.3)+
  scale_y_continuous(labels = scales::percent,breaks = seq(0,1,by=0.1))+
    ggtitle(label = glue::glue("{analysis_groups[i]} by population status"))+
  coord_flip()+
  theme_bw()+
  theme(
    axis.title = element_blank(),
    legend.title= element_blank()
  ) 
  ggsave(glue::glue("outputs/graphs/{analysis_groups[i]}_pop_by_status.pdf"),
         height = 36,
         width = 11,
         units = "in",
         device = "pdf")
}


