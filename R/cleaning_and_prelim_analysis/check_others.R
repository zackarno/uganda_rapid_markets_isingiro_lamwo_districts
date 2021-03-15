# check_other(
#   df=df, 
#   suffix="_other",
#   report_cols= report_params, 
#   kobo_survey_sheet=ks
#   )

parent_other_q<- function(df){
  df %>% 
    select(ends_with("_other")) %>% 
    colnames() %>% 
    tibble() %>% 
    mutate(other_parent=str_replace_all(.,"_other","")) %>% 
    rename(other_text=".")
}

check_others<-function(df, suffix="_other", report_cols,kobo_survey_sheet){
  # res<-list()
  # 
  # rep_cols_not_in_df<-report_cols[!report_cols %in% colnames(df)]
  # message(crayon::magenta(glue::glue("{rep_cols_not_in_df} - not in data frame")))
  # 
  # user_uuid<-grep(x = report_cols,pattern = "uuid", value=T)
  # if(length(user_uuid>0)){
  #   data_uuid<-df %>% select(contains("uuid")) %>% colnames()
  #   if(user_uuid != data_uuid){
  #     message(crayon::blue(glue::glue("did you mean {data_uuid} as the uuid column?")))
  #   }
  # }
  # 
  # assertthat::assert_that(length(rep_cols_not_in_df)==0,msg= "reporting cols listed above not in data frame")
  # 
  # report_cols<-str_replace(report_cols,user_uuid,data_uuid)
  
  report_cols_in_df<-report_cols[report_cols %in% names(df)]
  # once across verb has any_vars equialent filter_at can be replaced
  #a.
  df_other_filt<-df %>%
    filter_at(vars(ends_with(suffix)), any_vars(!is.na(.))) 
  
  #experimental
  # want to see if i can get other text into q along with other option
  df_parent_other_table<- parent_other_q(df_other_filt)
  

  
  # so_other<- ks %>% 
  #   # mutate(type=)
  #   filter(name %in% df_parent_other) %>% 
  #   filter(str_detect(type, c("select_one|select one"))) %>% 
  #   pull(name)
    
  
  #b.
  # df_parent_other<-df_other_filt %>% 
  #   purrr::discard(~all(is.na(.))) %>%
  #   select(ends_with(suffix)) %>% 
  #   colnames() %>%
  #   str_replace_all(suffix, "")
  
  so_other<- ks %>% 
    # mutate(type=)
    filter(name %in% df_parent_other_table$other_parent) %>% 
    filter(str_detect(type, c("select_one|select one"))) %>% 
    pull(name)
  sm_other<- ks %>% 
    # mutate(type=)
    filter(name %in% df_parent_other_table$other_parent) %>% 
    filter(str_detect(type, c("select_multiple|select multiple"))) %>% 
    pull(name)
  
  df_other_long<-df_other_filt %>% 
    purrr::discard(~all(is.na(.))) %>%
    select(report_cols_in_df,
           any_of(c(
             df_parent_other_table$other_text))) %>%
    mutate(across(as.character(.data))) %>%
    pivot_longer(-report_cols_in_df,
                 names_to="name",
                 values_to='other_text',
                 values_drop_na = TRUE) %>% 
    mutate(
      name=str_replace_all(name,"_other","")
           )
  sm_other_long<- df_other_long %>% 
    filter(name %in% sm_other) %>% 
    mutate(type="add_option")
  
  so_other_long<- df_other_long %>% 
    filter(name %in% so_other) %>% 
    mutate(
      type="change_response"
    )
  
  sm_other_long<- sm_other_long[rep(seq_len(nrow(sm_other_long)), each = 2), ]
  sm_other_long<- sm_other_long %>%
    group_by(uuid,name) %>%
    mutate(rep_num=row_number(),
           type=ifelse(rep_num==2,"remove_option",type) %>% as.character(),
           value=ifelse(rep_num==2, "other","")
    ) %>% ungroup()
  
  other_log<-bind_rows(sm_other_long,so_other_long) %>% 
    mutate(
      current_value="other",
      issue="other_regrouped"
    ) %>% 
    select(report_cols, other_text)
  
  return(other_log)
  

  # sm_other_table<-df_other_filt %>% 
  #   purrr::discard(~all(is.na(.))) %>%
  #   select(report_cols_in_df,sm_other) %>%
  #   mutate(across(as.character(.data))) %>%
  #   pivot_longer(-report_cols_in_df,
  #                names_to="other_col",
  #                values_to='prev_value',
  #                values_drop_na = TRUE) %>%
  #   mutate(type= "add_option",
  #          name=str_replace(other_col,suffix,"")
  #          ) %>%
  #   select(report_cols_in_df, everything())
  # 
  # 
  # so_other_table<-df_other_filt %>% 
  #   purrr::discard(~all(is.na(.))) %>%
  #   select(report_cols_in_df,any_of(so_other)) %>%
  #   mutate(across(as.character(.data))) %>%
  #   pivot_longer(-report_cols_in_df,
  #                names_to="other_col",
  #                values_to='prev_value',
  #                values_drop_na = TRUE) %>%
  #   mutate(type= "change_response",
  #          name=str_replace(other_col,suffix,""),
  #          value=NA) %>%
  #   select(report_cols_in_df, everything())
  # 
  # 
  
}
