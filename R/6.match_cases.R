# Match cases

clinical_data <- clinical_data %>% 
  mutate_at(("race"), ~ case_when(
    . == 1 ~ "white",
    . == 2 ~ "black",
    . == 3 ~ "biracial",
    TRUE ~ NA_character_
  ))

cases_match <- left_join(cases_match, 
                          clinical_data %>% select("suid", "race"),
                          by= "suid")

cases_match1 <- dcast(setDT(cases_match), pair_id ~ rowid(pair_id),
                      value.var = c("suid", "race"))
cases_match1 <- cases_match1 %>% drop_na(c("race1", "race2"))

marker <-  left_join(cases_match, 
                     marker,
                     by= "suid")


