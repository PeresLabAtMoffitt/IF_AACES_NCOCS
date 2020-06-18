
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
ROI_tumor$suid <- substr(ROI_tumor$image_tag, start = 10, stop = 15)
# ROI_tumor <-
#   ROI_tumor[(!grepl(uid, ROI_tumor$suid)), ]
ROI_stroma$suid <- substr(ROI_stroma$image_tag, start = 10, stop = 15)
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
  mutate(CD11b_CD15_stroma_mm2 = stroma_cd11bplus_cd15plus_cells/stroma_area_analyzed_mm2_) %>% 
  
  mutate(CD3perc_tumor_mm2 = (tumor_percent_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>% # percent of marker per mm2
  mutate(CD3perc_stroma_mm2 = (stroma_percent_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>%
  mutate(CD3_CD8perc_tumor_mm2 = (tumor_percent_cd3plus_cd8plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  mutate(CD3_CD8perc_stroma_mm2 = (stroma_percent_cd3plus_cd8plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  mutate(CD3_FoxP3perc_tumor_mm2 = (tumor_percent_cd3plus_foxp3plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  mutate(CD3_FoxP3perc_stroma_mm2 = (stroma_percent_cd3plus_foxp3plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  mutate(CD11bperc_tumor = (tumor_percent_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>%
  mutate(CD11bperc_stroma = (stroma_percent_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>%
  mutate(CD11b_CD15perc_tumor_mm2 = (tumor_percent_cd11bplus_cd15plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  mutate(CD11b_CD15perc_stroma_mm2 = stroma_percent_cd11bplus_cd15plus_positive_cells/stroma_area_analyzed_mm2_)


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
  mutate(CD11b_CD15_stroma_mm2 = stroma_cd11bplus_cd15plus_cells/stroma_area_analyzed_mm2_) %>% 

  mutate(CD3perc_tumor_mm2 = (tumor_percent_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>%
  mutate(CD3perc_stroma_mm2 = (stroma_percent_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>%
  mutate(CD3_CD8perc_tumor_mm2 = (tumor_percent_cd3plus_cd8plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  mutate(CD3_CD8perc_stroma_mm2 = (stroma_percent_cd3plus_cd8plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  mutate(CD3_FoxP3perc_tumor_mm2 = (tumor_percent_cd3plus_foxp3plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  mutate(CD3_FoxP3perc_stroma_mm2 = (stroma_percent_cd3plus_foxp3plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  mutate(CD11bperc_tumor = (tumor_percent_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>%
  mutate(CD11bperc_stroma = (stroma_percent_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>%
  mutate(CD11b_CD15perc_tumor_mm2 = (tumor_percent_cd11bplus_cd15plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  mutate(CD11b_CD15perc_stroma_mm2 = stroma_percent_cd11bplus_cd15plus_positive_cells/stroma_area_analyzed_mm2_)

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


# End #
