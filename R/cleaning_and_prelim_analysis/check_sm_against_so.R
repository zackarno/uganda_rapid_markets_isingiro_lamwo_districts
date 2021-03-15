
check_sm_against_so<- function(df,check_name,sm,so, report_cols){
  # report_cols<-stringr::str_replace_all(report_cols,"_uuid|X_uuid","uuid")
  
  violations<- df %>% 
    rowwise() %>% 
    mutate(
      sm_sum= sum(c_across(sm),na.rm=T),
    ) %>% 
    ungroup() %>% 
    mutate(
      !!check_name:= !!sym(so[1])==so[2] & sm_sum>0
    ) %>% 
    filter(!!sym(check_name))
  violations %>% 
    mutate(type= "change_response",
           current_value= so[2],
           name = so[1],
           value = so[3],
           issue_id= check_name
           ) %>% 
    select(report_cols)
  
}



