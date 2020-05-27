# 1.	Calculate the % tumor and % stroma within each ROI/TMA core. 
# Provide the mean, median, and range of the % tumor and % stroma 
# for the TMA cores, intratumoral ROIs, and peripheral ROIs. 
# Also assess the variation by case in terms of the % tumor and % stroma. 

ROI_tumor
ROI_stroma

ROI_global <- merge.data.frame(ROI_tumor, ROI_stroma,
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE)

TMA_global <- merge.data.frame(TMA_tumor, TMA_stroma,
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE)


table <- matrix(c("Tumor", "Stroma", "Total",
                  
  
), ncol = , byrow = TRUE
  
)

ROI_global <- ROI_global %>% 
  mutate(total_cell_number = ROI_global$`Tumor: Total Cells` + ROI_global$`Stroma: Total Cells`)
TMA_global <- TMA_global %>% 
  mutate(total_cell_number = `Tumor: Total Cells` + `Stroma: Total Cells`)



