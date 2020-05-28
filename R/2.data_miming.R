# 1.	Calculate the % tumor and % stroma within each ROI/TMA core. 
# Provide the mean, median, and range of the % tumor and % stroma 
# for the TMA cores, intratumoral ROIs, and peripheral ROIs. 
# Also assess the variation by case in terms of the % tumor and % stroma. 



ROI_global <- merge.data.frame(ROI_tumor, ROI_stroma,
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = Tumor:_Total_Cells + Stroma:_Total_Cells) %>% 
  mutate(percent_tumor = round((Tumor:_Total_Cells / total_cell_number)*100, 2)) %>% 
  mutate(percent_stroma = round((Stroma:_Total_Cells / total_cell_number)*100, 2)) 

TMA_global <- merge.data.frame(TMA_tumor, TMA_stroma,
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = Tumor:_Total_Cells + Stroma:_Total_Cells) %>% 
  mutate(percent_tumor = round((Tumor:_Total_Cells / total_cell_number)*100, 2)) %>% 
  mutate(percent_stroma = round((Stroma:_Total_Cells / total_cell_number)*100, 2)) 


# ROI_global <- ROI_global %>% 
#   mutate(total_cell_number = Tumor:_Total_Cells + Stroma:_Total_Cells) %>% 
#   mutate(percent_tumor = round((Tumor:_Total_Cells / total_cell_number)*100, 2)) %>% 
#   mutate(percent_stroma = round((Stroma:_Total_Cells / total_cell_number)*100, 2)) 
# TMA_global <- TMA_global %>% 
#   mutate(total_cell_number = Tumor:_Total_Cells + Stroma:_Total_Cells) %>% 
#   mutate(percent_tumor = round((Tumor:_Total_Cells / total_cell_number)*100, 2)) %>% 
#   mutate(percent_stroma = round((Stroma:_Total_Cells / total_cell_number)*100, 2)) 


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
                  round(mean(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_tumor),2),
                  round(mean(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_stroma),2),
                  "median",
                  median(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_tumor),
                  median(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_stroma),
                  "range",
                  paste(
                    range(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_tumor)[1],
                    range(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_tumor)[2],
                    sep = "-"
                  ),
                  paste(
                    range(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_stroma)[1],
                    range(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_stroma)[2],
                    sep = "-"
                  ),
                  "variance", 
                  round(var(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_tumor),2),
                  round(var(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_stroma),2),
                  "sd",
                  round(sd(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_tumor),2),
                  round(sd(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "i")$percent_stroma),2),
                  "peripheral", "", "",
                  "mean",
                  round(mean(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_tumor),2),
                  round(mean(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_stroma),2),
                  "median",
                  median(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_tumor),
                  median(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_stroma),
                  "range",
                  paste(
                    range(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_tumor)[1],
                    range(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_tumor)[2],
                    sep = "-"
                  ),
                  paste(
                    range(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_stroma)[1],
                    range(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_stroma)[2],
                    sep = "-"
                  ),
                  "variance", 
                  round(var(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_tumor),2),
                  round(var(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_stroma),2),
                  "sd",
                  round(sd(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_tumor),2),
                  round(sd(x= filter(ROI_global, Intratumoral_(i)_vs_Peripheral_(p).x == "p")$percent_stroma),2)
), ncol = 3, byrow = TRUE)
write.csv(table, 
          paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Summary tumor, stroma in TMAs and ROIs.csv"))




