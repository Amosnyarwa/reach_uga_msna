# load packages

library(tidyverse)
library(lubridate)
library(glue)
source("R/support_functions.R")

# read data

df_msna_data <- readxl::read_excel("inputs/REACH_UGA_Dataset_MSNA_data_20JUL2018.xlsx") %>% 
  mutate(i.check.uuid = `X_uuid`,
         i.check.start_date = as_date(start),
         i.check.point_number = point_number,
         i.check.enumerator = enumerator,
         start = as_datetime(start),
         end = as_datetime(end)) 

df_survey <- readxl::read_excel("inputs/REACH_UGA_UNHCR_MSNA_Questionnaire_1AUG2018.xlsx")  
df_choices <- readxl::read_excel("inputs/REACH_UGA_UNHCR_MSNA_Questionnaire_1AUG2018.xlsx")


# 

# output holder -----------------------------------------------------------

logic_output <- list()



# data not meeting minimum requirements -----------------------------------

# no_consent_not_hoh
df_no_consent_not_hoh <- df_msna_data %>% 
  filter(head_of_household == "no") %>% 
  mutate(i.check.type ="remove_survey",
         i.check.type = "head_of_household",
         i.check.current_value = as.character(head_of_household),
         i.check.value = "",
         i.check.issue_id = "logic_m_requirement_no_consent_not_hoh",
         i.check.issue = "no_consent_not_hoh",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_data = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "df_no_consent_not_hoh"))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_consent_not_hoh")

# age out of range

df_respondents_not_of_age <- df_msna_data %>% 
  filter(respondent_age < 18 | respondent_age > 100) %>% 
  mutate(i.check.type ="remove_survey",
         i.check.type = "respondent_age",
         i.check.current_value = as.character(respondent_age),
         i.check.value = "",
         i.check.issue_id = "logic_m_requirement_age_out_of_range",
         i.check.issue = "age_out_of_range",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_data = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "df_respondents_not_of_age"))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_respondents_not_of_age")


# Time checks -------------------------------------------------------------

# Time interval for the survey

min_time_of_survey <- 25
max_time_of_survey <- 120

df_c_survey_time <- df_msna_data %>% 
  mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
         int.survey_time_interval = ceiling(int.survey_time_interval),
         i.check.type = "remove_survey",
         i.check.name = "point_number",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = case_when(
           int.survey_time_interval < min_time_of_survey ~ "less_survey_time",
           int.survey_time_interval > max_time_of_survey ~ "more_survey_time",
           TRUE ~ "normal_survey_time"),
         i.check.issue = glue("{int.survey_time_interval} min taken to do the survey"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  filter(i.check.issue_id %in% c("less_survey_time", "more_survey_time"))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_survey_time")    
  
# check time between surveys

min_time_btn_surveys <- 5

df_c_survey_time <- df_msna_data %>% 
  group_by(i.check.start_date, i.check.enumerator) %>% 
  filter(n()>1) %>% 
  arrange(start, .by_group = TRUE) %>% 
  mutate(int.time_between_survey = lubridate::time_length(start - lag(end, default = first(start)), unit = "min"),
         int.time_between_survey = ceiling(int.time_between_survey)) %>% 
filter(int.time_between_survey != 0 & int.time_between_survey < min_time_btn_surveys) %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "point_number",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "less_time_btn_surveys",
         i.check.issue = glue("{int.time_between_survey} min taken between surveys"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_survey_time")     
  
  
    

