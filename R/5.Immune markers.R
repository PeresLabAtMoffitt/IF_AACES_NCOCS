# 2.	For each case, calculate the average cell density (number of cells positive #--------------- Do % first
# for each marker per mm2 of tumor/stroma) of the markers below across the TMA cores, 
# intratumoral ROIs, and peripheral ROIs (separately – overall, tumor, and stroma). 
# Potentially create some sort of variable denoting an immune hot or immune cold tumor – 
# or something along those lines?

# % Cell
colnames(TMA_global)
markers_TMA <- group_by(TMA_global, suid) %>% 
  summarize(percent_CD3_tumor = mean(tumor_percent_cd3_opal_650_positive_cells), 
            percent_CD3_stroma = mean(stroma_percent_cd3_opal_650_positive_cells),
            percent_CD8_tumor = mean(tumor_percent_cd8_opal_570_positive_cells), 
            percent_CD8_stroma = mean(stroma_percent_cd8_opal_570_positive_cells),
            percent_CD3_CD8_tumor = mean(tumor_percent_cd3plus_cd8plus_positive_cells),
            percent_CD3_CD8_stroma = mean(stroma_percent_cd3plus_cd8plus_positive_cells),
            percent_FoxP3_tumor = mean(tumor_percent_foxp3_opal_540_positive_cells), 
            percent_FoxP3_stroma = mean(stroma_percent_foxp3_opal_540_positive_cells),
            percent_CD3_FoxP3_tumor = mean(tumor_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD3_FoxP3_stroma = mean(stroma_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD11b_tumor = mean(tumor_percent_cd11b_opal_620_positive_cells), 
            percent_CD11b_stroma = mean(stroma_percent_cd11b_opal_620_positive_cells),
            percent_CD15_tumor = mean(tumor_percent_cd15_opal_520_positive_cells), 
            percent_CD15_stroma = mean(stroma_percent_cd15_opal_520_positive_cells),
            percent_CD11b_CD15_tumor = mean(tumor_percent_cd11bplus_cd15plus_positive_cells), 
            percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells))# %>%
  # mutate(ID = seq(1:nrow(.)))

markers_ROIi <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
  group_by(suid) %>% 
  summarize(percent_CD3_tumor = mean(tumor_percent_cd3_opal_650_positive_cells), 
            percent_CD3_stroma = mean(stroma_percent_cd3_opal_650_positive_cells),
            percent_CD8_tumor = mean(tumor_percent_cd8_opal_570_positive_cells), 
            percent_CD8_stroma = mean(stroma_percent_cd8_opal_570_positive_cells),
            percent_CD3_CD8_tumor = mean(tumor_percent_cd3plus_cd8plus_positive_cells),
            percent_CD3_CD8_stroma = mean(stroma_percent_cd3plus_cd8plus_positive_cells),
            percent_FoxP3_tumor = mean(tumor_percent_foxp3_opal_540_positive_cells), 
            percent_FoxP3_stroma = mean(stroma_percent_foxp3_opal_540_positive_cells),
            percent_CD3_FoxP3_tumor = mean(tumor_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD3_FoxP3_stroma = mean(stroma_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD11b_tumor = mean(tumor_percent_cd11b_opal_620_positive_cells), 
            percent_CD11b_stroma = mean(stroma_percent_cd11b_opal_620_positive_cells),
            percent_CD15_tumor = mean(tumor_percent_cd15_opal_520_positive_cells), 
            percent_CD15_stroma = mean(stroma_percent_cd15_opal_520_positive_cells),
            percent_CD11b_CD15_tumor = mean(tumor_percent_cd11bplus_cd15plus_positive_cells), 
            percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells))
# setDT(markers_ROIip)[, ID := .GRP, .(suid)]

# Plots CD3 and CD11b in TMAs and ROIs
cols <- 
  c("CD3 tumor" = "#99CCFF", "CD8 tumor" = "#33CCFF", "FoxP3 tumor" = "#33FFFF", "CD11b tumor" = "#99FFFF", "CD15 tumor" = "#CCFFFF",
    "CD3 stroma" = "#660000", "CD8 stroma" = "#990000", "FoxP3 stroma" = "#CC0000", "CD11b stroma" = "#FF0000", "CD15 stroma" = "#FF66CC")
ggplot(markers_TMA, aes(x=suid, y=percent_CD3_tumor)) +
  geom_bar(aes(fill="CD3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD8_tumor, fill="CD8 tumor"), stat="identity") +
  geom_bar(aes(y=percent_FoxP3_tumor, fill="FoxP3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD15_tumor, fill="CD15 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD3_stroma, fill="CD3 stroma"), stat="identity") +
  geom_bar(aes(y=percent_CD8_stroma, fill="CD8 stroma"), stat="identity") +
  geom_bar(aes(y=percent_FoxP3_stroma, fill="FoxP3 stroma"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_stroma, fill="CD11b stroma"), stat="identity") +
  geom_bar(aes(y=percent_CD15_stroma, fill="CD15 stroma"), stat="identity") +
  scale_fill_manual(values = cols, breaks = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor",
                                              "CD11b tumor", "CD15 tumor","CD3 stroma", 
                                              "CD8 stroma", "FoxP3 stroma", "CD11b stroma", "CD15 stroma")) +
  coord_flip()+
  labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')

markers_ROIi %>% 
  ggplot(aes(x=suid)) +
  geom_bar(aes(y=percent_CD3_tumor, fill="CD3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD8_tumor, fill="CD8 tumor"), stat="identity") +
  geom_bar(aes(y=percent_FoxP3_tumor, fill="FoxP3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD15_tumor, fill="CD15 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD3_stroma, fill="CD3 stroma"), stat="identity") +
  geom_bar(aes(y=percent_CD8_stroma, fill="CD8 stroma"), stat="identity") +
  geom_bar(aes(y=percent_FoxP3_stroma, fill="FoxP3 stroma"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_stroma, fill="CD11b stroma"), stat="identity") +
  geom_bar(aes(y=percent_CD15_stroma, fill="CD15 stroma"), stat="identity") +
  scale_fill_manual(values = cols, breaks = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor",
                                              "CD11b tumor", "CD15 tumor","CD3 stroma", 
                                              "CD8 stroma", "FoxP3 stroma", "CD11b stroma", "CD15 stroma")) +  coord_flip()+
  labs(y="% cells", title="Immune Markers Repartition in ROIs", fill='Immune \nMarkers')

cols <- 
  c("CD3 tumor" = "#99CCFF", "CD8 tumor" = "#33CCFF", "FoxP3 tumor" = "#33FFFF", "CD11b tumor" = "#99FFFF", "CD15 tumor" = "#CCFFFF")
ggplot(markers_TMA, aes(x=suid, y=percent_CD3_tumor)) +
  geom_bar(aes(fill="CD3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD8_tumor, fill="CD8 tumor"), stat="identity") +
  geom_bar(aes(y=percent_FoxP3_tumor, fill="FoxP3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD15_tumor, fill="CD15 tumor"), stat="identity") +
  scale_fill_manual(values = cols, breaks = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor",
                                              "CD11b tumor", "CD15 tumor")) +
  coord_flip()+
  labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')

ggplot(markers_ROIi, aes(x=suid, y=percent_CD3_tumor)) +
  geom_bar(aes(fill="CD3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD8_tumor, fill="CD8 tumor"), stat="identity") +
  geom_bar(aes(y=percent_FoxP3_tumor, fill="FoxP3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD15_tumor, fill="CD15 tumor"), stat="identity") +
  scale_fill_manual(values = cols, breaks = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor",
                                              "CD11b tumor", "CD15 tumor")) +
  coord_flip()+
  labs(y="% cells", title="Immune Markers Repartition in ROIs", fill='Immune \nMarkers')

markers_TMA %>% 
  summarise(count = n()) %>% 
  mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=suid, y=percent_CD3_tumor)) +
  geom_bar(aes(fill="CD3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD8_tumor, fill="CD8 tumor"), stat="identity") +
  geom_bar(aes(y=percent_FoxP3_tumor, fill="FoxP3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD15_tumor, fill="CD15 tumor"), stat="identity") +
  scale_fill_manual(values = cols, breaks = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor",
                                              "CD11b tumor", "CD15 tumor")) +
  coord_flip()+
  labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')

library(gplots)
library(heatmap.plus)
library(RColorBrewer)


df1 <- as.data.frame(markers_ROIi) %>% 
  `row.names<-`(markers_ROIi$suid) %>% 
  drop_na(.)
df1$suid <- NULL

df1 <- as.matrix(df1)
heatmap.2(df1, main = "Immune Marker Presentation",
          
          trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1, 
          margins = c(10,5), # bottom, right
          ColSideColors = ,
          scale = "row")

df2 <- t(scale(t(df1)))
df2 <- df2[complete.cases(df2 * 0), , drop=FALSE]

heatmap.2(df2, main = "Immune Marker Presentation",
          
          trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1, 
          margins = c(10,5), # bottom, right
          ColSideColors = ,
          scale = "row")


########################################################################################## II ### Immune hot vs cold----
# 2.1.Look difference between peripheral and intra.----

markers_ROIp <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  group_by(suid) %>% 
  summarize(percent_CD3_tumor = mean(tumor_percent_cd3_opal_650_positive_cells), 
            percent_CD3_stroma = mean(stroma_percent_cd3_opal_650_positive_cells),
            percent_CD8_tumor = mean(tumor_percent_cd8_opal_570_positive_cells), 
            percent_CD8_stroma = mean(stroma_percent_cd8_opal_570_positive_cells),
            percent_CD3_CD8_tumor = mean(tumor_percent_cd3plus_cd8plus_positive_cells),
            percent_CD3_CD8_stroma = mean(stroma_percent_cd3plus_cd8plus_positive_cells),
            percent_FoxP3_tumor = mean(tumor_percent_foxp3_opal_540_positive_cells), 
            percent_FoxP3_stroma = mean(stroma_percent_foxp3_opal_540_positive_cells),
            percent_CD3_FoxP3_tumor = mean(tumor_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD3_FoxP3_stroma = mean(stroma_percent_cd3plus_foxp3plus_positive_cells),
            percent_CD11b_tumor = mean(tumor_percent_cd11b_opal_620_positive_cells), 
            percent_CD11b_stroma = mean(stroma_percent_cd11b_opal_620_positive_cells),
            percent_CD15_tumor = mean(tumor_percent_cd15_opal_520_positive_cells), 
            percent_CD15_stroma = mean(stroma_percent_cd15_opal_520_positive_cells),
            percent_CD11b_CD15_tumor = mean(tumor_percent_cd11bplus_cd15plus_positive_cells), 
            percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells))

markers_ROI <- full_join(markers_ROIi, markers_ROIp,
                         by = "suid", suffixe = c("i", "p"))

markers_ROI <- markers_ROI %>% 
  mutate(ratio_CD3 = percent_CD3_tumor/percent_tumor)
















