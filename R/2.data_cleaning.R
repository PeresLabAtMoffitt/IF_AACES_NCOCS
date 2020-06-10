
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
rm(TMA2_tumor, TMA2_stroma, uid, fct_name_repair, # roir_import,
   data_import, roit_import, rois_import, tmat_import, tmas_import,
   tma2t_import, tma2s_import, common_ROITMA_import, # tmar_import, 
   TMAcases_remove, case_remove_import)


#-----------------------------------------------------------------------------------------------------------------

# 1.	Calculate the % tumor and % stroma within each ROI/TMA core. 
# Provide the mean, median, and range of the % tumor and % stroma 
# for the TMA cores, intratumoral ROIs, and peripheral ROIs. 
# Also assess the variation by case in terms of the % tumor and % stroma. 



ROI_global <- merge.data.frame(ROI_tumor, ROI_stroma,
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = tumor__total_cells + stroma__total_cells
         ) %>% 
  mutate(percent_tumor = round((tumor__total_cells / total_cell_number)*100, 2)
         ) %>% 
  mutate(percent_stroma = round((stroma__total_cells / total_cell_number)*100, 2)
         ) %>% 
  mutate_at(("intratumoral__i__vs_peripheral__p_.x"), ~ case_when(
    intratumoral__i__vs_peripheral__p_.x == "p" ~ "Peripheral",
    intratumoral__i__vs_peripheral__p_.x == "i" ~ "Intratumoral")
    ) %>% 
  mutate(suid.x = factor(suid.x)) %>% 
  mutate(CD3_tumor_mm2 = (tumor__cd3__opal_650__positive_cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_stroma_mm2 = (stroma__cd3__opal_650__positive_cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_CD8_tumor_mm2 = (tumor__cd3__cd8__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_CD8_stroma_mm2 = (stroma__cd3__cd8__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_FoxP3_tumor_mm2 = (tumor__cd3__foxp3__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_FoxP3_stroma_mm2 = (stroma__cd3__foxp3__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD11b_tumor = (tumor__cd11b__opal_620__positive_cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD11b_stroma = (stroma__cd11b__opal_620__positive_cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD11b_CD15_tumor_mm2 = (tumor__cd11b__cd15__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD11b_CD15_stroma_mm2 = stroma__cd11b__cd15__cells/tumor__area_analyzed__mm__)

TMA_global <- merge.data.frame(TMA_tumor, TMA_stroma,
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = tumor__total_cells + stroma__total_cells
         ) %>% 
  mutate(percent_tumor = round((tumor__total_cells / total_cell_number)*100, 2)
         ) %>% 
  mutate(percent_stroma = round((stroma__total_cells / total_cell_number)*100, 2)
         ) %>% 
  mutate(suid.x = factor(suid.x)) %>% 
  mutate(CD3_tumor_mm2 = (tumor__cd3__opal_650__positive_cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_stroma_mm2 = (stroma__cd3__opal_650__positive_cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_CD8_tumor_mm2 = (tumor__cd3__cd8__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_CD8_stroma_mm2 = (stroma__cd3__cd8__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_FoxP3_tumor_mm2 = (tumor__cd3__foxp3__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD3_FoxP3_stroma_mm2 = (stroma__cd3__foxp3__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD11b_tumor = (tumor__cd11b__opal_620__positive_cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD11b_stroma = (stroma__cd11b__opal_620__positive_cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD11b_CD15_tumor_mm2 = (tumor__cd11b__cd15__cells/tumor__area_analyzed__mm__)) %>% 
  mutate(CD11b_CD15_stroma_mm2 = stroma__cd11b__cd15__cells/tumor__area_analyzed__mm__)

variations_TMA <- group_by(TMA_global, suid.x) %>% 
  summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma)) %>% 
  mutate(ID = seq(1:nrow(.)))# %>% 
# mutate(suid.x = factor(suid.x))

variations_ROIip <- group_by(ROI_global, suid.x, intratumoral__i__vs_peripheral__p_.x) %>% 
  summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma))
setDT(variations_ROIip)[, ID := .GRP, .(suid.x)]

variations_ROI <- group_by(variations_ROIip, suid.x) %>% 
  summarize(mean_tumor = mean(mean_tumor), mean_stroma = mean(mean_stroma)) %>% 
  mutate(ID = seq(1:nrow(.)))

variations_TMA$tumor_variation <- variations_TMA$mean_tumor - mean(TMA_global$percent_tumor)
variations_TMA$stroma_variation <- variations_TMA$mean_stroma - mean(TMA_global$percent_stroma)
variations_ROIip$tumor_variation <- variations_ROIip$mean_tumor - mean(ROI_global$percent_tumor)
variations_ROIip$stroma_variation <- variations_ROIip$mean_stroma - mean(ROI_global$percent_stroma)
variations_ROI$tumor_variation <- variations_ROI$mean_tumor - mean(ROI_global$percent_tumor)
variations_ROI$stroma_variation <- variations_ROI$mean_stroma - mean(ROI_global$percent_stroma)


# End #
