# II  ### Data Cleaning

# IIa ### TMA data
# Fisrt need to 
## 1-verify that core removed ARE removed from TMA data  ####### All good so not run anymore
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
# 1-verify that core removed ARE removed from ROI data  ####### All good so not run anymore
# ROI_remove <- ROI_remove %>% filter(REMOVED == "REMOVE" | is.na(REMOVED)) # only 84 after removin the to keep tag
# uid <- paste(unique(ROI_remove[1]), collapse = "|")
# ROI_tumor <-
#   ROI_tumor[(!grepl(uid, ROI_tumor$image_tag)), ]
# ROI_stroma <-
#   ROI_stroma[(!grepl(uid, ROI_stroma$image_tag)),]


# III-Remove the TMA IDs from patient excluded from the study
# Should only be done for TMAs
# Remove TMA with no IDs = controls images
uid <- paste(unique(TMAcases_remove$Subject_IDs), collapse = "|")
TMA_tumor <-
  TMA_tumor[(!grepl(uid, TMA_tumor$suid)), ] %>% 
  filter(!is.na(suid))
TMA_stroma <-
  TMA_stroma[(!grepl(uid, TMA_stroma$suid)),] %>% 
  filter(!is.na(suid))

# Did it for ROIs too in case  ####### All good so not run anymore but still create suid 
ROI_tumor$suid <- substr(ROI_tumor$image_tag, start = 10, stop = 14)
# ROI_tumor <-
#   ROI_tumor[(!grepl(uid, ROI_tumor$suid)), ]
ROI_stroma$suid <- substr(ROI_stroma$image_tag, start = 10, stop = 14)
# ROI_stroma <-
#   ROI_stroma[(!grepl(uid, ROI_stroma$suid)), ]

# Cleaning
rm(TMA2_tumor, TMA2_stroma, uid, fct_name_repair,
data_import, roit_import, rois_import, roir_import, tmat_import, tmas_import,
tma2t_import, tma2s_import, tmar_import, common_ROITMA_import,
case_remove_import)


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
  mutate(percent_stroma = round((stroma__total_cells / total_cell_number)*100, 2)) 

TMA_global <- merge.data.frame(TMA_tumor, TMA_stroma,
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = tumor__total_cells + stroma__total_cells) %>% 
  mutate(percent_tumor = round((tumor__total_cells / total_cell_number)*100, 2)) %>% 
  mutate(percent_stroma = round((stroma__total_cells / total_cell_number)*100, 2)) 

table <- matrix(c("", "Tumor", "Stroma",
                  "TMA", "", "",
                  "mean", round(mean(TMA_global$percent_tumor),2), round(mean(TMA_global$percent_stroma),2),
                  "median", median(TMA_global$percent_tumor), median(TMA_global$percent_stroma),
                  "range", 
                  paste(range(TMA_global$percent_tumor)[1],range(TMA_global$percent_tumor)[2], sep = "-"), 
                  paste(range(TMA_global$percent_stroma)[1],range(TMA_global$percent_stroma)[2], sep = "-"),
                  "variance", 
                  round(var(x= TMA_global$percent_tumor),2),
                  round(var(x= TMA_global$percent_stroma),2),
                  "sd",
                  round(sd(x= TMA_global$percent_tumor),2),
                  round(sd(x= TMA_global$percent_stroma),2),
                  "ROIs", "", "",
                  "intratumoral", "", "",
                  "mean",
                  round(mean(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor),2),
                  round(mean(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma),2),
                  "median",
                  median(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor),
                  median(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma),
                  "range",
                  paste(
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor)[1],
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor)[2],
                    sep = "-"
                  ),
                  paste(
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma)[1],
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma)[2],
                    sep = "-"
                  ),
                  "variance", 
                  round(var(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor),2),
                  round(var(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma),2),
                  "sd",
                  round(sd(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor),2),
                  round(sd(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma),2),
                  "peripheral", "", "",
                  "mean",
                  round(mean(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor),2),
                  round(mean(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma),2),
                  "median",
                  median(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor),
                  median(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma),
                  "range",
                  paste(
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor)[1],
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor)[2],
                    sep = "-"
                  ),
                  paste(
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma)[1],
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma)[2],
                    sep = "-"
                  ),
                  "variance", 
                  round(var(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor),2),
                  round(var(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma),2),
                  "sd",
                  round(sd(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor),2),
                  round(sd(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma),2)
), ncol = 3, byrow = TRUE)
write.csv(table, 
          paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Summary tumor, stroma in TMAs and ROIs.csv"))

# TMA
p1 <- ggplot(TMA_global) +
  geom_histogram(aes(tumor__total_cells), color = "grey", fill="darkred") +
  theme_minimal() +
  labs(x="Tumor", y="count", title="Cell type repartition in TMAs")
p2 <- ggplot(TMA_global) +
  geom_histogram(aes(stroma__total_cells), color = "grey", fill="steelblue") +
  theme_minimal() +
  labs(x="Stroma", y="count")
gridExtra::grid.arrange(p1, p2, ncol = 2)

# ROI
p1 <- ggplot(ROI_global) +
  geom_histogram(aes(tumor__total_cells), color = "grey", fill="darkred") +
  theme_minimal() +
  labs(x="Tumor", y="count", title="Cell type repartition in ROIs")
p2 <- ggplot(ROI_global) +
  geom_histogram(aes(stroma__total_cells), color = "grey", fill="steelblue") +
  theme_minimal() +
  labs(x="Stroma", y="count")
gridExtra::grid.arrange(p1, p2, ncol = 2)

# 
p1 <- ggplot(ROI_global) +
  geom_histogram(aes(tumor__total_cells), color = "grey", fill="darkred") +
  theme_minimal() +
  labs(x="Tumor", y="count", title="Cell type repartition in ROIs") +
  facet_grid(.~intratumoral__i__vs_peripheral__p_.x)
p2 <- ggplot(ROI_global) +
  geom_histogram(aes(stroma__total_cells), color = "grey", fill="steelblue") +
  theme_minimal() +
  labs(x="Stroma", y="count") +
  facet_grid(.~intratumoral__i__vs_peripheral__p_.x)
gridExtra::grid.arrange(p1, p2, nrow = 2)





