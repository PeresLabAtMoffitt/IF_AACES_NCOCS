
# III-Remove the TMA IDs from patient excluded from the study
# Should only be done for TMAs
# Plus remove TMA with no IDs = controls images
uid <- paste(unique(TMAcases_remove$Subject_IDs), collapse = "|")
TMA_tumor <-
  TMA_tumor[(!grepl(uid, TMA_tumor$suid)), ] %>% 
  filter(!is.na(suid))
TMA_stroma <-
  TMA_stroma[(!grepl(uid, TMA_stroma$suid)),] %>% 
  filter(!is.na(suid))

# Did it for ROIs too in case  #------------------ All good so don't run anymore but still create suid 
ROI_tumor$suid <- str_match(ROI_tumor$image_tag, "Peres_P1_([:digit:]*)")[,2]
# ROI_tumor <-
#   ROI_tumor[(!grepl(uid, ROI_tumor$suid)), ]
ROI_stroma$suid <- str_match(ROI_stroma$image_tag, "Peres_P1_([:digit:]*)")[,2]
# ROI_stroma <-
#   ROI_stroma[(!grepl(uid, ROI_stroma$suid)), ]

# Cleaning
rm(uid, fct_name_repair, # roir_import,
   data_import, roit_import, rois_import, tmat_import, tmas_import,
   tma2t_import, tma2s_import, common_ROITMA_import, # tmar_import, 
   TMAcases_remove, case_remove_import)


#-----------------------------------------------------------------------------------------------------------------

# 1.	Calculate the % tumor and % stroma within each ROI/TMA core. 
# Provide the mean, median, and range of the % tumor and % stroma 
# for the TMA cores, intratumoral ROIs, and peripheral ROIs. 
# Also assess the variation by case in terms of the % tumor and % stroma. 

ROI_global <- merge.data.frame(ROI_tumor, ROI_stroma %>% select(-c("intratumoral_i_vs_peripheral_p_", "suid")),
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = tumor_total_cells + stroma_total_cells
         ) %>% 
  mutate(percent_tumor = round((tumor_total_cells / total_cell_number)*100, 2) # Calculate percent of tumor cell
         ) %>% 
  mutate(percent_stroma = round((stroma_total_cells / total_cell_number)*100, 2) # Calculate percent of stromal cell
         ) %>% 
  mutate_at(("intratumoral_i_vs_peripheral_p_"), ~ case_when(
    intratumoral_i_vs_peripheral_p_ == "p" ~ "Peripheral",
    intratumoral_i_vs_peripheral_p_ == "i" ~ "Intratumoral")
    ) %>% 
  mutate(suid = factor(suid)) %>% 
  select(suid, everything()) %>% 
  mutate(CD3_tumor_mm2 = (tumor_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>% # density of marker per mm2
  mutate(CD3_stroma_mm2 = (stroma_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>% 
  mutate(CD3_CD8_tumor_mm2 = (tumor_cd3plus_cd8plus_cells/tumor_area_analyzed_mm2_)) %>% 
  mutate(CD3_CD8_stroma_mm2 = (stroma_cd3plus_cd8plus_cells/stroma_area_analyzed_mm2_)) %>% 
  mutate(CD3_FoxP3_tumor_mm2 = (tumor_cd3plus_foxp3plus_cells/tumor_area_analyzed_mm2_)) %>% 
  mutate(CD3_FoxP3_stroma_mm2 = (stroma_cd3plus_foxp3plus_cells/stroma_area_analyzed_mm2_)) %>% 
  mutate(CD11b_tumor = (tumor_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>% 
  mutate(CD11b_stroma = (stroma_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>% 
  mutate(CD11b_CD15_tumor_mm2 = (tumor_cd11bplus_cd15plus_cells/tumor_area_analyzed_mm2_)) %>% 
  mutate(CD11b_CD15_stroma_mm2 = stroma_cd11bplus_cd15plus_cells/stroma_area_analyzed_mm2_) # %>% 
  
  # mutate(CD3perc_tumor_mm2 = (tumor_percent_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>% # percent of marker per mm2
  # mutate(CD3perc_stroma_mm2 = (stroma_percent_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD3_CD8perc_tumor_mm2 = (tumor_percent_cd3plus_cd8plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3_CD8perc_stroma_mm2 = (stroma_percent_cd3plus_cd8plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD3_FoxP3perc_tumor_mm2 = (tumor_percent_cd3plus_foxp3plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3_FoxP3perc_stroma_mm2 = (stroma_percent_cd3plus_foxp3plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD11bperc_tumor = (tumor_percent_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD11bperc_stroma = (stroma_percent_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD11b_CD15perc_tumor_mm2 = (tumor_percent_cd11bplus_cd15plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD11b_CD15perc_stroma_mm2 = stroma_percent_cd11bplus_cd15plus_positive_cells/stroma_area_analyzed_mm2_)


TMA_global <- merge.data.frame(TMA_tumor, TMA_stroma %>% select(-suid),
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = tumor_total_cells + stroma_total_cells
         ) %>% 
  mutate(percent_tumor = round((tumor_total_cells / total_cell_number)*100, 2)
         ) %>% 
  mutate(percent_stroma = round((stroma_total_cells / total_cell_number)*100, 2)
         ) %>% 
  mutate(suid = factor(suid)) %>% 
  select(suid, everything()) %>% 
  mutate(CD3_tumor_mm2 = (tumor_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>% 
  mutate(CD3_stroma_mm2 = (stroma_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>% 
  mutate(CD3_CD8_tumor_mm2 = (tumor_cd3plus_cd8plus_cells/tumor_area_analyzed_mm2_)) %>% 
  mutate(CD3_CD8_stroma_mm2 = (stroma_cd3plus_cd8plus_cells/stroma_area_analyzed_mm2_)) %>% 
  mutate(CD3_FoxP3_tumor_mm2 = (tumor_cd3plus_foxp3plus_cells/tumor_area_analyzed_mm2_)) %>% 
  mutate(CD3_FoxP3_stroma_mm2 = (stroma_cd3plus_foxp3plus_cells/stroma_area_analyzed_mm2_)) %>% 
  mutate(CD11b_tumor = (tumor_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>% 
  mutate(CD11b_stroma = (stroma_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>% 
  mutate(CD11b_CD15_tumor_mm2 = (tumor_cd11bplus_cd15plus_cells/tumor_area_analyzed_mm2_)) %>% 
  mutate(CD11b_CD15_stroma_mm2 = stroma_cd11bplus_cd15plus_cells/stroma_area_analyzed_mm2_) # %>% 

  # mutate(CD3perc_tumor_mm2 = (tumor_percent_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3perc_stroma_mm2 = (stroma_percent_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD3_CD8perc_tumor_mm2 = (tumor_percent_cd3plus_cd8plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3_CD8perc_stroma_mm2 = (stroma_percent_cd3plus_cd8plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD3_FoxP3perc_tumor_mm2 = (tumor_percent_cd3plus_foxp3plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3_FoxP3perc_stroma_mm2 = (stroma_percent_cd3plus_foxp3plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD11bperc_tumor = (tumor_percent_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD11bperc_stroma = (stroma_percent_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD11b_CD15perc_tumor_mm2 = (tumor_percent_cd11bplus_cd15plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD11b_CD15perc_stroma_mm2 = stroma_percent_cd11bplus_cd15plus_positive_cells/stroma_area_analyzed_mm2_)

# Look at the variation between each patient and the global mean # Should we mot compare Black and White?
# Here compare the mean of % cells to global study % cells
variations_TMA <- group_by(TMA_global, suid) %>% 
  summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma)) %>% 
  mutate(ID = seq(1:nrow(.)))
variations_TMA$tumor_variation <- variations_TMA$mean_tumor - mean(TMA_global$percent_tumor)
variations_TMA$stroma_variation <- variations_TMA$mean_stroma - mean(TMA_global$percent_stroma)

variations_ROIip <- group_by(ROI_global, suid, intratumoral_i_vs_peripheral_p_) %>% # mean of % cells separated by intra or perip
  summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma))
setDT(variations_ROIip)[, ID := .GRP, .(suid)]
variations_ROIip$tumor_variation <- variations_ROIip$mean_tumor - mean(ROI_global$percent_tumor)
variations_ROIip$stroma_variation <- variations_ROIip$mean_stroma - mean(ROI_global$percent_stroma)

variations_ROI <- group_by(variations_ROIip, suid) %>% 
  summarize(mean_tumor = mean(mean_tumor), mean_stroma = mean(mean_stroma)) %>% # mean of % cells merging intra or perip
  mutate(ID = seq(1:nrow(.)))
variations_ROI$tumor_variation <- variations_ROI$mean_tumor - mean(ROI_global$percent_tumor)
variations_ROI$stroma_variation <- variations_ROI$mean_stroma - mean(ROI_global$percent_stroma)

########################################################################################## IV ### recode clinical data
class(clinical_data$BMI_YA)
clinical_data <- clinical_data %>% 
  mutate_at(("casecon"), ~ case_when(
   . == 1 ~ "case",
   . == 2 ~ "control"
  )) %>% 
  mutate(vitalstatus = case_when(
    vitalstatus == 1 ~ "alive",
    vitalstatus == 2 ~ "deceased",
    TRUE ~ NA_character_
  )) %>% 
  mutate(cancersite = case_when(
    cancersite == 1 ~ "ovarian",
    cancersite == 2 ~ "tubal",
    cancersite == 3 ~ "peritoneal",
    cancersite == 4 ~ "ovarian or tubal, can't distinguish",
    cancersite == 5 ~ "ovarian, tubal or peritoneal, can't distinguish",
    TRUE ~ NA_character_
  )) %>% 
  mutate_at(c("timelastfu", "morphology"), 
            ~ case_when(
              . %in% c("8888","9998", "9999")          ~ NA_real_,
              TRUE                                     ~ as.numeric(.)
            )) %>% 
  mutate_at(c("refage",
              "height", "wt_recent", "wt_YA", "wtgain", "BMI_recent", "BMI_YA"), 
            ~ case_when(
              . %in% c("888","998", "999")             ~ NA_real_,
              TRUE                                     ~ as.numeric(.)
            )) %>% 
  mutate(histology = case_when(
    histology == 1 ~ "serous",
    histology == 2 ~ "endometrioid",
    histology == 3 ~ "clear cell",
    histology == 4 ~ "mucinous",
    histology == 5 ~ "carcinosarcoma",
    histology == 6 ~ "carcinoma, NOS",
    histology == 7 ~ "other specified epithelial ovarian cancer (e.g. Malignant Brenner, mixed)",
    histology == 8 ~ "epithelial, NOS",
    histology == 9 ~ "synchronous",
    TRUE ~ NA_character_
  )) %>% 
  mutate(behavior = case_when(
    behavior == 1 ~ "borderline",
    behavior == 2 ~ "invasive",
    TRUE ~ NA_character_
  )) %>% 
  mutate(stage = case_when(
    stage == 1 ~ "Localized",
    stage == 2 ~ "Regional",
    stage == 3 ~ "Distant",
    TRUE ~ NA_character_
  )) %>% 
  mutate(grade = case_when(
    grade == 1 ~ "well differentiated",
    grade == 2 ~ "moderately differentiated",
    grade == 3 ~ "poorly differentiated",
    grade == 4 ~ "undifferentiated",
    TRUE ~ NA_character_
  )) %>% 
  mutate(histotype = case_when(
    histotype == 1 ~ "high-grade serous",
    histotype == 2 ~ "low-grade serous",
    histotype == 3 ~ "endometrioid",
    histotype == 4 ~ "clear cell",
    histotype == 5 ~ "mucinous",
    histotype == 6 ~ "carcinosarcoma",
    histotype == 7 ~ "other epithelial ovarian cancer (e.g. Malignant Brenner, mixed, carcinoma, NOS)",
    histotype == 9 ~ "serous borderline",
    histotype == 10 ~ "mucinous borderline",
    histotype == 11 ~ "other epithelial borderline",
    histotype == 13 ~ "synchronous",
    TRUE ~ NA_character_
  )) %>%
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "biracial",
    TRUE ~ NA_character_
  )) %>% 
  mutate(hispanic = case_when(
    hispanic == 1 ~ "hispanic",
    hispanic == 2 ~ "non-hispanic",
    TRUE ~ NA_character_
  )) %>% 
  mutate(birthplace = case_when(
    birthplace == 1 ~ "born in United States",
    birthplace == 2 ~ "born outside of United States",
    TRUE ~ NA_character_
  )) %>% 
  mutate(education = case_when(
    education == 1 ~ "high school graduate/GED or less",
    education == 2 ~ "some college",
    education == 3 ~ "college graduate",
    education == 4 ~ "graduate/professional school",
    TRUE ~ NA_character_
  )) %>% 
  mutate(married = case_when(
    married == 1 ~ "single/never married",
    married == 2 ~ "married/living as married"
  )) %>% 
  mutate(pregever = case_when(
    pregever == 1 ~ "yes",
    pregever == 2 ~ "no",
    TRUE ~ NA_character_
  )) 
  












# End #
