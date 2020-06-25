# 2.	For each case, calculate the average cell density (number of cells positive #--------------- Do % first
# for each marker per mm2 of tumor/stroma) of the markers below across the TMA cores, 
# intratumoral ROIs, and peripheral ROIs (separately – overall, tumor, and stroma). 
# Potentially create some sort of variable denoting an immune hot or immune cold tumor – 
# or something along those lines?

# % Cell
colnames(TMA_stroma)
markers_TMA <- group_by(TMA_global, suid) %>% 
  summarize(percent_CD3_tumor = mean(tumor_percent_cd3_opal_650_positive_cells), 
            percent_CD3_stroma = mean(stroma_percent_cd3_opal_650_positive_cells),
            percent_CD3_CD8_tumor = mean(tumor_percent_cd3plus_cd8plus_positive_cells),
            percent_CD3_CD8_stroma = mean(stroma_percent_cd3plus_cd8plus_positive_cells),
            percent_CD3_FoxP3_tumor = mean(tumor_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD3_FoxP3_stroma = mean(stroma_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD11b_tumor = mean(tumor_percent_cd11b_opal_620_positive_cells), 
            percent_CD11b_stroma = mean(stroma_percent_cd11b_opal_620_positive_cells),
            percent_CD11b_CD15_tumor = mean(tumor_percent_cd11bplus_cd15plus_positive_cells), 
            percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells)) %>%
  mutate(ID = seq(1:nrow(.)))

markers_ROIip <- group_by(ROI_global, suid, intratumoral_i_vs_peripheral_p_) %>% 
  summarize(percent_CD3_tumor = mean(tumor_percent_cd3_opal_650_positive_cells), 
            percent_CD3_stroma = mean(stroma_percent_cd3_opal_650_positive_cells),
            percent_CD3_CD8_tumor = mean(tumor_percent_cd3plus_cd8plus_positive_cells),
            percent_CD3_CD8_stroma = mean(stroma_percent_cd3plus_cd8plus_positive_cells),
            percent_CD3_FoxP3_tumor = mean(tumor_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD3_FoxP3_stroma = mean(stroma_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD11b_tumor = mean(tumor_percent_cd11b_opal_620_positive_cells), 
            percent_CD11b_stroma = mean(stroma_percent_cd11b_opal_620_positive_cells),
            percent_CD11b_CD15_tumor = mean(tumor_percent_cd11bplus_cd15plus_positive_cells), 
            percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells))
setDT(markers_ROIip)[, ID := .GRP, .(suid)]

# Plots CD3 and CD11b in TMAs and ROIs
ggplot(markers_TMA, aes(x=suid, y=percent_CD3_tumor)) +
  geom_bar(aes(fill="CD3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD3_stroma, fill="CD3 stroma"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_stroma, fill="CD11b stroma"), stat="identity")+
  coord_flip()+
  labs(y="% cells")
markers_ROIip %>% filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
  ggplot(aes(x=suid)) +
  geom_bar(aes(y=percent_CD3_tumor, fill="CD3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD3_stroma, fill="CD3 stroma"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_stroma, fill="CD11b stroma"), stat="identity")+
  coord_flip()+
  labs(y="% cells")







