make_refugee_weight_table<- function(df, ref_pop){
  ref_pop<- ref_pop %>% 
    mutate(strata= ifelse(strata=="Isingiro_ref",
                          "isingiro_refugee",
                          "lamwo_refugee"),
           pop_status= "refugee") 
  refugee_weight_table<-df_ref %>% 
    group_by(strata) %>% 
    summarise(
      sample_strata=  n()
    )%>% 
    mutate(
      sample_global= sum(sample_strata)) %>% 
    left_join(ref_pop) %>% 
    rename(pop_strata= "total_hh") %>% 
    mutate(pop_global=sum(pop_strata) ,
           weights = (pop_strata/pop_global)/(sample_strata/sample_global)
    )
  
  
}


make_host_weight_table<- function(host_data,host_pop){
  
  # some annoying recoding to make match data set
  
  host_pop_harmonized<- host_pop %>% 
    mutate(district_name= tolower(district),
           district_sub_county= glue::glue("{district}_{sub_county}"),
           district_sub_county = ifelse(district_sub_county=="isingiro_town_council",
                                        "isingiro_isingiro_tc",district_sub_county),
           sub_county_name= ifelse(sub_county=="isingiro_town_council",
                                   "isingiro_tc",sub_county)
           
    ) %>% 
    select(district_name,sub_county_name, district_sub_county, total_hh=hh)
  
  census_pop_stats_by_district<- host_pop_harmonized %>% 
    group_by(district_name) %>% 
    summarise(
      pop_strata= sum(total_hh)
    ) %>% 
    ungroup() %>% 
    mutate(
      pop_global= sum(pop_strata)
    )
  
  sample_pop_stats_by_district<-df_host %>% 
    group_by(district_name) %>% 
    summarise(
      sample_strata=  n()
    ) %>% 
    ungroup() %>% 
    mutate(
      sample_global = sum(sample_strata)
    ) 
  host_district_weight_table<- census_pop_stats_by_district %>% 
    left_join(sample_pop_stats_by_district, by="district_name") %>% 
    mutate(
      district_weights= (pop_strata/pop_global)/(sample_strata/sample_global)
    )
  
}



# make overall weight tables ----------------------------------------------


make_combined_weight_table<- function(df, refugee_weight_table, host_weight_table){
  weight_tables<- list(refugee_weight_table, host_weight_table %>% 
                         rename(strata= "district_name"))
  
  
  weight_table<-map_dfr(weight_tables, function(x)x %>% 
                          select(pop_group=strata, sample_strata, pop_strata)) %>% 
    mutate(pop_group= ifelse(!str_detect(pop_group,"_refugee$"),glue::glue("{pop_group}_host"),pop_group))
  
  weight_table %>% 
    mutate(
      pop_global= sum(pop_strata),
      sample_global = sum(sample_strata),
      weights = (pop_strata/pop_global)/(sample_strata/sample_global)
    )
  
}
