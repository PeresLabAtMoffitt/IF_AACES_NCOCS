# 2.	For each case, calculate the average cell density (number of cells positive 
# for each marker per mm2 of tumor/stroma) of the markers below across the TMA cores, 
# intratumoral ROIs, and peripheral ROIs (separately – overall, tumor, and stroma). 
# Potentially create some sort of variable denoting an immune hot or immune cold tumor – 
# or something along those lines?

# Cell density

markers_TMA <- group_by(TMA_global, suid) %>% 
  summarize(mean_CD3_tumor = mean(CD3_tumor_mm2), 
            mean_CD3_stroma = mean(CD3_stroma_mm2),
            mean_CD3_CD8_tumor = mean(CD3_CD8_tumor_mm2),
            mean_CD3_CD8_stroma = mean(CD3_CD8_stroma_mm2),
            mean_CD3_FoxP3 = mean(CD3_FoxP3_tumor_mm2),
            mean_stroma = mean(CD3_FoxP3_stroma_mm2),
            mean_CD11b = mean(CD11b_tumor), 
            mean_stroma = mean(CD11b_stroma),
            mean_CD11b_CD15 = mean(CD11b_CD15_tumor_mm2), 
            mean_stroma = mean(CD11b_CD15_stroma_mm2)) %>%
  mutate(ID = seq(1:nrow(.)))

markers_ROIip <- group_by(ROI_global, suid, intratumoral_i_vs_peripheral_p_) %>% 
  summarize(mean_CD3_tumor = mean(CD3_tumor_mm2), 
            mean_CD3_stroma = mean(CD3_stroma_mm2),
            mean_CD3_CD8_tumor = mean(CD3_CD8_tumor_mm2),
            mean_CD3_CD8_stroma = mean(CD3_CD8_stroma_mm2),
            mean_CD3_FoxP3_tumor = mean(CD3_FoxP3_tumor_mm2),
            mean_CD3_FoxP3_stroma = mean(CD3_FoxP3_stroma_mm2),
            mean_CD11b_tumor = mean(CD11b_tumor), 
            mean_CD11b_stroma = mean(CD11b_stroma),
            mean_CD11b_CD15_tumor = mean(CD11b_CD15_tumor_mm2), 
            mean_CD11b_CD15_stroma = mean(CD11b_CD15_stroma_mm2))
setDT(markers_ROIip)[, ID := .GRP, .(suid)]

markers_ROI <- group_by(markers_ROIip, suid) %>% 
  summarize(mean_CD3_tumor = mean(mean_CD3_tumor), 
            mean_CD3_stroma = mean(mean_CD3_stroma),
            mean_CD3_CD8_tumor = mean(mean_CD3_CD8_tumor),
            mean_CD3_CD8_stroma = mean(mean_CD3_CD8_stroma),
            mean_CD3_FoxP3_tumor = mean(mean_CD3_FoxP3_tumor),
            mean_CD3_FoxP3_stroma = mean(mean_CD3_FoxP3_stroma),
            mean_CD11b_tumor = mean(mean_CD11b_tumor), 
            mean_CD11b_stroma = mean(mean_CD11b_stroma),
            mean_CD11b_CD15_tumor = mean(mean_CD11b_CD15_tumor), 
            mean_CD11b_CD15_stroma = mean(mean_CD11b_CD15_stroma)) %>% 
  mutate(ID = seq(1:nrow(.)))

ggplot(markers_TMA, aes(x=suid, y=mean_CD3_tumor, color = "tumor")) +
  geom_bar(stat="identity") +
  geom_bar(aes(y=mean_CD3_stroma, color="stroma"), stat="identity", 
           position = position_stack()) +
  geom_bar(aes(y=mean_CD3_CD8_tumor, color="CD38"), stat="identity", 
           position = position_stack()) +
  geom_bar(aes(y=mean_CD3_CD8_stroma, color="CD38 str"), stat="identity", 
           position = position_stack())


markers_perc_TMA <- group_by(TMA_global, suid) %>%
  summarize(mean_CD3_tumor = mean(CD3perc_tumor_mm2),
            mean_CD3_stroma = mean(CD3perc_stroma_mm2),
            mean_CD3_CD8_tumor = mean(CD3_CD8perc_tumor_mm2),
            mean_CD3_CD8_stroma = mean(CD3_CD8perc_stroma_mm2),
            mean_CD3_FoxP3_tumor = mean(CD3_FoxP3perc_tumor_mm2),
            mean_CD3_FoxP3_stroma = mean(CD3_FoxP3perc_stroma_mm2),
            mean_CD11b_tumor = mean(CD11bperc_tumor),
            mean_CD11b_stroma = mean(CD11bperc_stroma),
            mean_CD11b_CD15_tumor = mean(CD11b_CD15perc_tumor_mm2),
            mean_CD11b_CD15_stroma = mean(CD11b_CD15perc_stroma_mm2)) %>%
  mutate(ID = seq(1:nrow(.)))

markers_perc_ROIip <- group_by(ROI_global, suid, intratumoral_i_vs_peripheral_p_) %>%
  summarize(mean_CD3_tumor = mean(CD3_tumor_mm2),
            mean_CD3_stroma = mean(CD3_stroma_mm2),
            mean_CD3_CD8_tumor = mean(CD3_CD8_tumor_mm2),
            mean_CD3_CD8_stroma = mean(CD3_CD8_stroma_mm2),
            mean_CD3_FoxP3_tumor = mean(CD3_FoxP3_tumor_mm2),
            mean_CD3_FoxP3_stroma = mean(CD3_FoxP3_stroma_mm2),
            mean_CD11b_tumor = mean(CD11b_tumor),
            mean_CD11b_stroma = mean(CD11b_stroma),
            mean_CD11b_CD15_tumor = mean(CD11b_CD15_tumor_mm2),
            mean_CD11b_CD15_stroma = mean(CD11b_CD15_stroma_mm2))
setDT(markers_ROIip)[, ID := .GRP, .(suid)]

ggplot(markers_perc_TMA, aes(x=suid, y=mean_CD3_tumor, color = " CD3 tumor")) +
  geom_bar(stat="identity") +
  geom_bar(aes(y=mean_CD3_stroma, color="CD3 stroma"), stat="identity",
           position = position_dodge()) +
  geom_bar(aes(y=mean_CD3_CD8_tumor, color="CD38"), stat="identity",
           position = position_dodge()) +
  geom_bar(aes(y=mean_CD3_CD8_stroma, color="CD38 str"), stat="identity",
           position = position_dodge())






