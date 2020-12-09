# Create full dataframe from cleaned ID data

ROI_global <- 
  full_join(ROI_tumor, ROI_stroma %>% select(-c("intratumoral_i_vs_peripheral_p_", "suid")),
            by = "image_tag") %>% 
  full_join(., ROI_total %>% select(-c("intratumoral_i_vs_peripheral_p_", "suid")),
            by = "image_tag") %>% 
  mutate_at(("intratumoral_i_vs_peripheral_p_"), ~ case_when(
    intratumoral_i_vs_peripheral_p_ == "p" ~ "Peripheral",
    intratumoral_i_vs_peripheral_p_ == "i" ~ "Intratumoral")
  ) %>% 
  mutate(suid = as.character(suid)) %>% 
  mutate(slide_type = "ROI") %>% 
  select(suid, slide_type, everything())

TMA_global <- 
  full_join(TMA_tumor, TMA_stroma %>% select(-suid),
            by = "image_tag") %>% 
  full_join(., TMA_total %>% select(-suid),
            by = "image_tag") %>% 
  mutate(suid = as.character(suid)) %>% 
  mutate(slide_type = "TMA") %>% 
  select(suid, slide_type, everything())


Global_data <- bind_rows(ROI_global, TMA_global) %>% 
  select(-c("tma.x", "other_tissue.x", "block_id.x", "tma.y", "other_tissue.y", "block_id.y")) %>% 
  full_join(., clinical_data, by = "suid")

write_rds(Global_data, path = paste0(path, "/K99_R00/Image analysis data/Immune marker count data/Peres IF_AACES_NCOCS full data 12092020.rds"))


