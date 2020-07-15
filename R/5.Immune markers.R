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
markers_ROIi %>% gather("cell_type", "percent", 2:17) %>% 
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

markers_ROIi %>% gather("cell_type", "percent", 2:17) %>% 
  ggplot(aes(x=suid, y=percent, fill=cell_type)) +
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_manual(breaks = c("percent_CD3_tumor", "percent_CD8_tumor", "percent_FoxP3_tumor", "percent_CD11b_tumor",
                               "percent_CD15_tumor"),
                    labels = c("CD3 tumor", "CD8 tumor", "FoxP3 tumor","CD11b tumor",
                               "CD15 tumor"),
                    values = viridis::cividis(n=16))+
  labs(y="% cells", title="Immune Markers Repartition in TMAs", fill='Immune \nMarkers')








markers_TMA %>% 
  # summarise(count = n()) %>% 
  # mutate(percent=(count/sum(count)*100)) %>% 
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


# heatmap----
library(gplots)
library(heatmap.plus)
library(RColorBrewer)
# Plot SQRT
df1 <- as.data.frame(markers_ROIi[19:34]) %>% 
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

# Plot values
df1 <- as.data.frame(markers_ROIi[1:17]) %>% 
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
markers_ROI <- full_join(markers_ROIi, markers_ROIp,
                         by = "suid", suffix = c(".i", ".p"))
p1 <- ggplot(markers_ROI, aes(x=percent_CD3_tumor.p, y=percent_CD3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(label.y = 15)+
  stat_cor(label.y = 17, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation()
p2 <- ggplot(markers_ROI, aes(x=percent_CD8_tumor.p, y=percent_CD8_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
p3 <- ggplot(markers_ROI, aes(x=percent_FoxP3_tumor.p, y=percent_FoxP3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
p4 <- ggplot(markers_ROI, aes(x=percent_CD11b_tumor.p, y=percent_CD11b_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
p5 <- ggplot(markers_ROI, aes(x=percent_CD15_tumor.p, y=percent_CD15_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
p6 <- ggplot(markers_ROI, aes(x=percent_CD3_CD8_tumor.p, y=percent_CD3_CD8_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
p7 <- ggplot(markers_ROI, aes(x=percent_CD3_FoxP3_tumor.p, y=percent_CD3_FoxP3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
p8 <- ggplot(markers_ROI, aes(x=percent_CD11b_CD15_tumor.p, y=percent_CD11b_CD15_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
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

markers_ROI %>% 
  gather("ratio_type", "ratio", 36:43) %>% 
  select("suid", "ratio_type", "ratio") %>%
  ggplot(aes(x=suid, y=ratio, fill=ratio_type)) +
  geom_bar(stat="identity", position = position_dodge())+
  coord_flip()+
  labs(y="Ratio", title="", fill='Ratio \nType')
markers_ROI %>% 
  gather("ratio_type", "ratio", 36:43) %>% 
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

# Clustering----
clust <- Mclust(markers_ROIi[,c(21, 23:26)], G = 5)
summary(clust)
markers_ROIi$clusters <- clust$classification

colnames(markers_ROIi)
markers_ROIi %>% 
  ggplot(aes(x=suid, y=sqrt_CD3_CD8_tumor, group=clusters))+
  geom_boxplot()+
  facet_grid(.~ clusters)














# Cleaning
rm(markers_ROIi, markers_ROIp)
