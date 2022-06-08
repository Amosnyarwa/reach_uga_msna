# load packages

library(tidyverse)
library(lubridate)

source("R/support_functions.R")

# read data

df_msna_data <- readxl::read_excel("inputs/REACH_UGA_Dataset_MSNA_data_20JUL2018.xlsx") %>% 
  mutate(i.check.uuid = `X_uuid`,
         i.check.start_date = as_date(start),
         i.check.point_number = point_number,
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



