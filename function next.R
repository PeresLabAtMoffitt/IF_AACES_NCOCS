# II  ### Data Cleaning

# IIa ### TMA data

## 1-verify that core removed ARE removed from TMA data  #-------------- All good so not run anymore
# uid <- paste(unique(TMAremove_tumor[1]), collapse = "|")
# TMA_tumor <-
#   TMA_tumor[(!grepl(uid, TMA_tumor$image_tag)), ]
# TMA_stroma <-
#   TMA_stroma[(!grepl(uid, TMA_stroma$image_tag)),]
# 
# uid <- paste(unique(TMA2remove_tumor[1]), collapse = "|")
# TMA2_tumor <-
#   TMA2_tumor[(!grepl(uid, TMA2_tumor$image_tag)),]
# TMA2_stroma <-
#   TMA2_stroma[(!grepl(uid, TMA2_stroma$image_tag)),]


## 2-bind TMA together
TMA_tumor <- 
  bind_rows(TMA_tumor,TMA2_tumor, .id = "TMA")
TMA_stroma <- 
  bind_rows(TMA_stroma,TMA2_stroma, .id = "TMA")


# IIb ### ROI data
# 1-verify that core removed ARE removed from ROI data  #--------------- All good so not run anymore
# ROI_remove <- ROI_remove %>% 
#   filter(REMOVED == "REMOVE" | is.na(REMOVED)) # only 84 after removin the to keep tag
# uid <- paste(unique(ROI_remove[1]), collapse = "|")
# ROI_tumor <-
#   ROI_tumor[(!grepl(uid, ROI_tumor$image_tag)), ]
# ROI_stroma <-
#   ROI_stroma[(!grepl(uid, ROI_stroma$image_tag)),]


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

# Did it for ROIs too in case  #------------------ All good so not run anymore but still create suid 
ROI_tumor$suid <- substr(ROI_tumor$image_tag, start = 10, stop = 14)
# ROI_tumor <-
#   ROI_tumor[(!grepl(uid, ROI_tumor$suid)), ]
ROI_stroma$suid <- substr(ROI_stroma$image_tag, start = 10, stop = 14)
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
  mutate(total_cell_number = tumor__total_cells + stroma__total_cells) %>% 
  mutate(percent_tumor = round((tumor__total_cells / total_cell_number)*100, 2)) %>% 
  mutate(percent_stroma = round((stroma__total_cells / total_cell_number)*100, 2)) %>% 
  mutate_at(("intratumoral__i__vs_peripheral__p_.x"), ~ case_when(
    intratumoral__i__vs_peripheral__p_.x == "p" ~ "Peripheral",
    intratumoral__i__vs_peripheral__p_.x == "i" ~ "Intratumoral"
  )) %>% 
  mutate(suid.x = factor(suid.x))

TMA_global <- merge.data.frame(TMA_tumor, TMA_stroma,
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = tumor__total_cells + stroma__total_cells) %>% 
  mutate(percent_tumor = round((tumor__total_cells / total_cell_number)*100, 2)) %>% 
  mutate(percent_stroma = round((stroma__total_cells / total_cell_number)*100, 2)) #%>% 
  #mutate(suid.x = factor(suid.x))


variations_TMA <- group_by(TMA_global, suid.x) %>% 
  summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma)) %>% 
  mutate(ID = seq(1:nrow(.)))# %>% 
# mutate(suid.x = factor(suid.x))

variations_ROI <- group_by(ROI_global, suid.x) %>% 
  summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma)) %>% 
  mutate(ID = seq(1:nrow(.)))# %>% 
# mutate(suid.x = factor(suid.x))

variations_TMA$tumor_variation <- variations_TMA$mean_tumor - mean(TMA_global$percent_tumor)
variations_TMA$stroma_variation <- variations_TMA$mean_stroma - mean(TMA_global$percent_stroma)
variations_ROI$tumor_variation <- variations_ROI$mean_tumor - mean(ROI_global$percent_tumor)
variations_ROI$stroma_variation <- variations_ROI$mean_stroma - mean(ROI_global$percent_stroma)




