# Plots Immune markers

cols <- 
  c("CD3 tumor" = "#99CCFF", "CD8 tumor" = "#33CCFF", "FoxP3 tumor" = "#33FFFF", "CD11b tumor" = "#99FFFF", "CD15 tumor" = "#CCFFFF",
    "CD3 stroma" = "#660000", "CD8 stroma" = "#990000", "FoxP3 stroma" = "#CC0000", "CD11b stroma" = "#FF0000", "CD15 stroma" = "#FF66CC")
# ggplot(markers_TMA, aes(x=suid, y=percent_CD3_tumor)) +
#   geom_bar(aes(fill="CD3 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD8_tumor, fill="CD8 tumor"), position="identity", stat="identity") +
#   geom_bar(aes(y=percent_FoxP3_tumor, fill="FoxP3 tumor"), position="identity", stat="identity") +
#   geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), position="identity", stat="identity") +
#   geom_bar(aes(y=percent_CD15_tumor, fill="CD15 tumor"), position="identity", stat="identity") +
#   geom_bar(aes(y=percent_CD3_stroma, fill="CD3 stroma"), position="identity", stat="identity") +
#   geom_bar(aes(y=percent_CD8_stroma, fill="CD8 stroma"), position="identity", stat="identity") +
#   geom_bar(aes(y=percent_FoxP3_stroma, fill="FoxP3 stroma"), position="identity", stat="identity") +
#   geom_bar(aes(y=percent_CD11b_stroma, fill="CD11b stroma"), position="identity", stat="identity") +
#   geom_bar(aes(y=percent_CD15_stroma, fill="CD15 stroma"), position="identity", stat="identity") +
#   scale_fill_manual(values = cols, breaks = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor",
#                                               "CD11b tumor", "CD15 tumor","CD3 stroma", 
#                                               "CD8 stroma", "FoxP3 stroma", "CD11b stroma", "CD15 stroma")) +
#   coord_flip()+
#   labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')

markers_TMA %>% gather("cell_type", "percent", 2:17) %>% 
  ggplot(aes(x=suid, y=percent, fill=cell_type)) +
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_manual(breaks = c("percent_CD3_tumor", "percent_CD8_tumor", "percent_FoxP3_tumor", "percent_CD11b_tumor",
                               "percent_CD15_tumor", "percent_CD3_stroma", "percent_CD8_stroma", "percent_FoxP3_stroma",
                               "percent_CD11b_stroma", "percent_CD15_stroma"),
                    labels = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor","CD11b tumor",
                               "CD15 tumor",
                               "CD3 stroma", "CD8 stroma", "FoxP3 stroma", "CD11b stroma", 
                               "CD15 stroma"),
                    values = viridis::cividis(n=16))+
  labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')


# markers_ROIi %>% 
#   ggplot(aes(x=suid)) +
#   geom_bar(aes(y=percent_CD3_tumor, fill="CD3 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD8_tumor, fill="CD8 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_FoxP3_tumor, fill="FoxP3 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD15_tumor, fill="CD15 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD3_stroma, fill="CD3 stroma"), stat="identity") +
#   geom_bar(aes(y=percent_CD8_stroma, fill="CD8 stroma"), stat="identity") +
#   geom_bar(aes(y=percent_FoxP3_stroma, fill="FoxP3 stroma"), stat="identity") +
#   geom_bar(aes(y=percent_CD11b_stroma, fill="CD11b stroma"), stat="identity") +
#   geom_bar(aes(y=percent_CD15_stroma, fill="CD15 stroma"), stat="identity") +
#   scale_fill_manual(values = cols, breaks = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor",
#                                               "CD11b tumor", "CD15 tumor","CD3 stroma", 
#                                               "CD8 stroma", "FoxP3 stroma", "CD11b stroma", "CD15 stroma")) +  coord_flip()+
#   labs(y="% cells", title="Immune Markers Repartition in ROIs", fill='Immune \nMarkers')
colnames(markers_ROI)
markers_ROI %>% gather("cell_type", "percent", 6:21) %>% 
  ggplot(aes(x=suid, y=percent, fill=cell_type)) +
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_manual(breaks = c("percent_CD3_tumor.i", "percent_CD8_tumor.i", "percent_FoxP3_tumor.i", "percent_CD11b_tumor.i",
                               "percent_CD15_tumor.i", "percent_CD3_stroma.i", "percent_CD8_stroma.i", "percent_FoxP3_stroma.i",
                               "percent_CD11b_stroma.i", "percent_CD15_stroma.i"),
                    labels = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor","CD11b tumor",
                               "CD15 tumor",
                               "CD3 stroma", "CD8 stroma", "FoxP3 stroma", "CD11b stroma", 
                               "CD15 stroma"),
                    values = viridis::cividis(n=16))+
  labs(y="% cells", title="Immune Markers Repartition in intratumoral ROIs", fill='Immune \nMarkers')

cols <- 
  c("CD3 tumor" = "#99CCFF", "CD8 tumor" = "#33CCFF", "FoxP3 tumor" = "#33FFFF", "CD11b tumor" = "#99FFFF", 
    "CD15 tumor" = "#CCFFFF", "CD3/8 tumor" = "pink", "CD3/FoxP3 tumor" = "orange", "CD11b/15 tumor" = "red")
# ggplot(markers_TMA, aes(x=suid, y=percent_CD3_tumor)) +
#   geom_bar(aes(fill="CD3 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD8_tumor, fill="CD8 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_FoxP3_tumor, fill="FoxP3 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD11b_tumor, fill="CD11b tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD15_tumor, fill="CD15 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD3_CD8_tumor, fill="CD3/8 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD3_FoxP3_tumor, fill="CD3/FoxP3 tumor"), stat="identity") +
#   geom_bar(aes(y=percent_CD11b_CD15_tumor, fill="CD11b/15 tumor"), stat="identity") +
#   scale_fill_manual(values = cols, breaks = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor",
#                                               "CD11b tumor", "CD15 tumor")) +
#   coord_flip()+
#   labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')
markers_TMA %>% gather("cell_type", "percent", 2:17) %>% 
  ggplot(aes(x=suid, y=percent, fill=cell_type)) +
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_manual(breaks = c("percent_CD3_tumor", "percent_CD8_tumor", "percent_FoxP3_tumor", "percent_CD11b_tumor",
                               "percent_CD15_tumor"),
                    labels = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor","CD11b tumor",
                               "CD15 tumor"),
                    values = viridis::cividis(n=16))+
  labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')

markers_ROI %>% gather("cell_type", "percent", 6:21) %>% 
  ggplot(aes(x=suid, y=percent, fill=cell_type)) +
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_manual(breaks = c("percent_CD3_tumor.i", "percent_CD8_tumor.i", "percent_FoxP3_tumor.i", "percent_CD11b_tumor.i",
                               "percent_CD15_tumor.i"),
                    labels = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor","CD11b tumor",
                               "CD15 tumor"),
                    values = viridis::cividis(n=16))+
  labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')

colnames(markers_TMA)
markers_TMA %>% 
  # summarise(count = n()) %>% 
  # mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=suid, y=percent_CD3_tumor_tma)) +
  geom_bar(aes(fill="CD3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD8_tumor_tma, fill="CD8 tumor"), stat="identity") +
  geom_bar(aes(y=percent_FoxP3_tumor_tma, fill="FoxP3 tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD11b_tumor_tma, fill="CD11b tumor"), stat="identity") +
  geom_bar(aes(y=percent_CD15_tumor_tma, fill="CD15 tumor"), stat="identity") +
  scale_fill_manual(values = cols, breaks = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor",
                                              "CD11b tumor", "CD15 tumor")) +
  coord_flip()+
  labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')


# heatmap----
library(gplots)
library(heatmap.plus)
library(RColorBrewer)
# Plot SQRT
df1 <- as.data.frame(markers[,c(104, 106, 108:109, 111:112, 114, 116:117, 119)]) %>% 
  `row.names<-`(markers$suid) %>% drop_na(.)
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

# Plot 
df1 <- as.data.frame(markers[,c(120, 122, 124:125, 127)]) %>% 
  `row.names<-`(markers$suid) %>% 
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

# SQRT only tumor
colnames(markers_ROIi)
df1 <- as.data.frame(markers_ROIi[,c(21, 23:26)]) %>% 
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

rm(df1, df2, cols)


########################################################################################## II ### Immune hot vs cold----
# 2.1.Exclusion: Immune markers difference between periph and intra.----
p1 <- ggplot(markers_ROI, aes(x=percent_CD3_tumor.p, y=percent_CD3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(label.y = 15)+
  stat_cor(label.y = 17, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = 1)
p2 <- ggplot(markers_ROI, aes(x=percent_CD8_tumor.p, y=percent_CD8_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 12.5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = 1)
p3 <- ggplot(markers_ROI, aes(x=percent_FoxP3_tumor.p, y=percent_FoxP3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 3.5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = 1)
p4 <- ggplot(markers_ROI, aes(x=percent_CD11b_tumor.p, y=percent_CD11b_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = 1)
p5 <- ggplot(markers_ROI, aes(x=percent_CD15_tumor.p, y=percent_CD15_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 4, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = 1)
p6 <- ggplot(markers_ROI, aes(x=percent_CD3_CD8_tumor.p, y=percent_CD3_CD8_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 11, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = 1)
p7 <- ggplot(markers_ROI, aes(x=percent_CD3_FoxP3_tumor.p, y=percent_CD3_FoxP3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 3, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = 1)
p8 <- ggplot(markers_ROI, aes(x=percent_CD11b_CD15_tumor.p, y=percent_CD11b_CD15_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 1.5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = 1)
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 5)


# 2.2.Ratio:Immune cells vs tumor cell----
ggplot(markers_ROI, aes(x=mean_tumor.i, y=percent_CD3_tumor.i))+
  geom_point()+ scale_y_continuous(trans = "log10")+
  annotation_logticks(sides="l")
ggplot(markers_ROI, aes(x=mean_tumor.i, y=percent_CD8_tumor.i))+
  geom_point()+ scale_y_continuous(trans = "log10")+
  annotation_logticks(sides="l")
ggplot(markers_ROI, aes(x=mean_tumor.i, y=percent_FoxP3_tumor.i))+
  geom_point()+ scale_y_continuous(trans = "log10")+
  annotation_logticks(sides="l")
ggplot(markers_ROI, aes(x=mean_tumor.i, y=percent_CD11b_tumor.i))+
  geom_point()+ scale_y_continuous(trans = "log10")+
  annotation_logticks(sides="l")
ggplot(markers_ROI, aes(x=mean_tumor.i, y=percent_CD15_tumor.i))+
  geom_point()+ scale_y_continuous(trans = "log10")+
  annotation_logticks(sides="l")
ggplot(markers_ROI, aes(x=mean_tumor.i, y=percent_CD3_CD8_tumor.i))+
  geom_point()+ scale_y_continuous(trans = "log10")+
  annotation_logticks(sides="l")
ggplot(markers_ROI, aes(x=mean_tumor.i, y=percent_CD3_FoxP3_tumor.i))+
  geom_point()+ scale_y_continuous(trans = "log10")+
  annotation_logticks(sides="l")
ggplot(markers_ROI, aes(x=mean_tumor.i, y=percent_CD11b_CD15_tumor.i))+
  geom_point()+ scale_y_continuous(trans = "log10")+
  annotation_logticks(sides="l")


markers_ROI <- markers_ROI %>% 
  mutate(ratio_CD8 = percent_CD8_tumor.i/mean_tumor.i) %>% 
  mutate(ratio_FoxP3 = percent_FoxP3_tumor.i/mean_tumor.i) %>% 
  mutate(ratio_CD11b = percent_CD11b_tumor.i/mean_tumor.i) %>% 
  mutate(ratio_CD15 = percent_CD15_tumor.i/mean_tumor.i) %>% 
  mutate(ratio_CD3_CD8 = percent_CD3_CD8_tumor.i/mean_tumor.i) %>% 
  mutate(ratio_CD3_FoxP3 = percent_CD3_FoxP3_tumor.i/mean_tumor.i) %>% 
  mutate(ratio_CD11b_CD15 = percent_CD11b_CD15_tumor.i/mean_tumor.i)

colnames(markers_ROI)
markers_ROI %>% 
  gather("ratio_type", "ratio", ratio_CD8:ratio_CD11b_CD15) %>% 
  select("suid", "ratio_type", "ratio") %>%
  ggplot(aes(x=suid, y=ratio, fill=ratio_type)) +
  geom_bar(stat="identity", position = position_dodge())+
  coord_flip()+
  labs(y="Ratio", title="", fill='Ratio \nType')
markers_ROI %>% 
  gather("ratio_type", "ratio", "ratio_CD8":"ratio_CD11b_CD15") %>% 
  ggplot(aes(x=mean_tumor.i, y=ratio, color=ratio_type, fill=ratio_type)) +
  geom_bar(stat="identity")+
  labs(y="Ratio", title="", fill='Ratio \nType')

markers_ROI %>% 
  ggplot(aes(x=percent_CD3_CD8_tumor.i, y=percent_CD3_FoxP3_tumor.i, color=percent_CD11b_CD15_tumor.i))+
  geom_point(aes(size=percent_CD3_FoxP3_tumor.i), size=3)+
  scale_color_gradient2(midpoint = .8, low = "#CCFFFF", mid = "#990000",
                        high = "#000000", space = "Lab" )+
  # scale_color_gradientn(colours = rainbow(5))+
  theme_minimal()+
  #geom_point(aes(x=percent_CD3_CD8_tumor.i, y=percent_CD3_FoxP3_tumor.i, ))+ 
  scale_y_continuous(trans = "log10")+
  scale_x_continuous(trans = "log10")+
  annotation_logticks(sides="lb")
colour=percent_CD3_FoxP3_tumor.i<1


#####

# 2.3.Clustering----
# Need to remove NAs
clust_markers <- markers %>% filter(!is.na( markers[,c(104:111)] ))
# Do not take CD3 alone for clustering beacuse contain effector, regulator, helper, gamma delta
# Do not take FoxP3 alone because tumor cells can express FoxP3
# Do not take CD11b alone because CD3CD11b can be NK cells
# CD11bCD15 are myeloid cells prevent other immune cells to traffic into the tumor


# 2.3.0.clusters 1_by_Brooke # She only took intratumoral but all markers simple and doubled staining
clust <- Mclust(clust_markers[,104:111], G = 5)
summary(clust)
clust_markers$clusters_Brooke <- clust$classification
clust_markers$clusters_Brooke <- factor(clust_markers$clusters_Brooke, 
                                        levels = c(4, 2, 3, 5, 1),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(104:111)) %>% 
  select(suid, clusters_Brooke, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_Brooke, color=clusters_Brooke))+
  geom_boxplot()+
  facet_grid(.~ markers_cat)

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_Brooke))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_Brooke)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_Brooke))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_Brooke)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_Brooke))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_Brooke)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)

# Cluster for tumor
# 2.3.1.clusters 1_by_CD3-CD8
clust <- Mclust(clust_markers[106], G = 5)
summary(clust)
clust_markers$clusters_CD3CD8 <- clust$classification
clust_markers$clusters_CD3CD8 <- factor(clust_markers$clusters_CD3CD8, 
                                        levels = c(1 , 2, 3, 4, 5),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(106, 108:111)) %>% 
  select(suid, clusters_CD3CD8, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD3CD8, color=clusters_CD3CD8))+
  geom_boxplot()+
  facet_grid(.~ markers_cat)

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_CD3CD8))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD3CD8)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_CD3CD8))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD3CD8)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_CD3CD8))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD3CD8)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)
  
# 2.3.2.clusters 2_by_all double positive
clust <- Mclust(clust_markers[,c(106, 108, 111)], G = 5)
summary(clust)
clust_markers$dbl_pos <- clust$classification
clust_markers$dbl_pos <- factor(clust_markers$dbl_pos, 
                                        levels = c(3 ,4,2, 5, 1),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(106, 108:111)) %>% 
  select(suid, dbl_pos, markers_cat, value) %>% 
  ggplot(aes(x=suid, group=dbl_pos, color=dbl_pos))+
  geom_boxplot(aes(y=value))+
  facet_grid(.~ markers_cat)

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(106, 108:111)) %>% 
  select(suid, dbl_pos, markers_cat, value) %>% 
  ggplot(aes(x=suid, group=dbl_pos, color=dbl_pos))+
  geom_boxplot(aes(y=value))

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=dbl_pos))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ dbl_pos)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=dbl_pos))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ dbl_pos)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=dbl_pos))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ dbl_pos)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)


# 2.3.3.clusters 1_CD3-CD8 then FoxP3
clust <- Mclust(clust_markers[106], G = 2)
summary(clust)
clust_markers$clusters_CD38 <- clust$classification
clust_markers$clusters_CD38 <- factor(clust_markers$clusters_CD38, 
                                levels = c(1, 2),
                                labels = c("low", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(106, 108:111)) %>% 
  select(suid, clusters_CD38, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD38, color=clusters_CD38))+
  geom_boxplot()+
  facet_grid(.~ markers_cat)

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_CD38))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD38)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_CD38))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD38)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_CD38))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD38)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)

#
a <- clust_markers %>% filter(clusters_CD38 == "high") # 2 is high in CD38
clust <- Mclust(a[108], G = 2) # clust on CD3-FoxP3
summary(clust)
a$clusters_FoxP3 <- clust$classification
a$clusters_FoxP3 <- factor(a$clusters_FoxP3, 
                                      levels = c(1, 2),
                                      labels = c("low", "high"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(106, 108:111)) %>% 
  select(suid, clusters_FoxP3, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3, color=clusters_FoxP3))+
  geom_boxplot()+
  facet_grid(.~ markers_cat)

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_FoxP3))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_FoxP3)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_FoxP3))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_FoxP3)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_FoxP3))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_FoxP3)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)

# Combine cluster CD38 and cluster FoxP3
clust_markers <- clust_markers %>% 
  mutate(special_cluster = case_when(
    clusters_CD38 == "low" ~ "cold", # lox CD8 aka cold
    clusters_FoxP3 == "low" ~ "hot", # low Fox aka hot
    clusters_FoxP3 == "high" ~ "immunosupressed", # high Fox aka immunosupp
  ))


# Survival by cluster
# clin_surv1 <- left_join(clin_surv, clust_markers[, c("suid", "clusters_CD3CD8", "dbl_pos", "special_cluster")], by="suid")
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_Brooke", "clusters_CD3CD8", "dbl_pos", "special_cluster")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu, event = clin_surv$surv_vital)

# clusters_Brooke
myplot <- survfit(mysurv~clin_surv$clusters_Brooke)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by Brooke", # legend.labs = c("mid-high", "mid-low", "high", "low"), # 4253
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~clin_surv$race+clin_surv$clusters_Brooke)

# cluster 1
myplot <- survfit(mysurv~clin_surv$clusters_CD3CD8)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD3+/CD8+", #legend.labs = c("mid-high", "mid-low", "high", "low"), # 4253
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~clin_surv$race+clin_surv$clusters_CD3CD8)
# cluster 2
myplot <- survfit(mysurv~clin_surv$dbl_pos)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by all double positive", 
           # legend.labs = c("mid", "mid-high", "low", "mid-low", "high"), # 52143 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$dbl_pos)
# special_cluster
myplot <- survfit(mysurv~clin_surv$special_cluster)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 then FoxP3", 
           legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster)

# Same clustering with Stroma----
# 2.3.1.clusters 1_by_CD3-CD8 stroma
clust_markers <- markers %>% filter(!is.na( markers[,c(112:119)] ))

clust <- Mclust(clust_markers[114], G = 5)
summary(clust)
clust_markers$clusters_CD3CD8 <- clust$classification
clust_markers$clusters_CD3CD8 <- factor(clust_markers$clusters_CD3CD8, 
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(114, 116:119)) %>% 
  select(suid, clusters_CD3CD8, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD3CD8, color=clusters_CD3CD8))+
  geom_boxplot()+
  facet_grid(.~ markers_cat)

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_CD3CD8))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD3CD8)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_CD3CD8))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD3CD8)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_CD3CD8))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD3CD8)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)

# 2.3.2.clusters 2_by_all double positive Stroma
clust <- Mclust(clust_markers[,c(114, 116, 119)], G = 5)
summary(clust)
clust_markers$dbl_pos <- clust$classification
clust_markers$dbl_pos <- factor(clust_markers$dbl_pos, 
                                        levels = c(4, 2, 3, 5, 1),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(114, 116:119)) %>% 
  select(suid, dbl_pos, markers_cat, value) %>% 
  ggplot(aes(x=suid, group=dbl_pos, color=dbl_pos))+
  geom_boxplot(aes(y=value))+
  facet_grid(.~ markers_cat)

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(114, 116:119)) %>% 
  select(suid, dbl_pos, markers_cat, value) %>% 
  ggplot(aes(x=suid, group=dbl_pos, color=dbl_pos))+
  geom_boxplot(aes(y=value))

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=dbl_pos))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ dbl_pos)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=dbl_pos))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ dbl_pos)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=dbl_pos))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ dbl_pos)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)


# 2.3.3.clusters 1_CD3-CD8 then FoxP3 Stroma
clust <- Mclust(clust_markers[114], G = 2)
summary(clust)
clust_markers$clusters_CD38 <- clust$classification
clust_markers$clusters_CD38 <- factor(clust_markers$clusters_CD38, 
                                levels = c(1, 2),
                                labels = c("low", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(114, 116:119)) %>% 
  select(suid, clusters_CD38, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD38, color=clusters_CD38))+
  geom_boxplot()+
  facet_grid(.~ markers_cat)

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_CD38))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD38)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_CD38))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD38)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_CD38))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_CD38)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)

#
a <- clust_markers %>% filter(clusters_CD38 == "high") # 2 is high in CD38
clust <- Mclust(a[116], G = 2)
summary(clust)
a$clusters_FoxP3 <- clust$classification
a$clusters_FoxP3 <- factor(a$clusters_FoxP3, 
                                      levels = c(1, 2),
                                      labels = c("low", "high"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(114, 116:119)) %>% 
  select(suid, clusters_FoxP3, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3, color=clusters_FoxP3))+
  geom_boxplot()+
  facet_grid(.~ markers_cat)

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_FoxP3))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_FoxP3)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_FoxP3))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_FoxP3)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_FoxP3))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_FoxP3)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)



# Combine cluster CD38 and cluster FoxP3 Stroma
clust_markers <- clust_markers %>% 
  mutate(special_cluster = case_when(
    clusters_CD38 == "low" ~ "cold", # lox CD8 aka cold
    clusters_FoxP3 == "low" ~ "hot", # low Fox aka hot
    clusters_FoxP3 == "high" ~ "immunosupressed", # high Fox aka immunosupp
  ))

# Survivals----
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_CD3CD8", "dbl_pos", "special_cluster")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu, event = clin_surv$surv_vital)

# cluster 1
myplot <- survfit(mysurv~clin_surv$clusters_CD3CD8)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD3+/CD8+", #legend.labs = c("mid-high", "mid-low", "high", "low"), # 4253
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~clin_surv$race+clin_surv$clusters_CD3CD8)
# cluster 2
myplot <- survfit(mysurv~clin_surv$dbl_pos)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by all double positive", 
           legend.labs = c("mid", "mid-high", "low", "mid-low", "high"), # 52143 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$dbl_pos)
# special_cluster
myplot <- survfit(mysurv~clin_surv$special_cluster)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 then FoxP3", 
           legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster)



# Cleaning
rm(markers_ROIi, markers_ROIp)
