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

markers_TMA %>% gather("cell_type", "percent", 4:19) %>% # wrong----
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
############################################################################################### Immunoscore calculation----
# https://jitc.biomedcentral.com/articles/10.1186/s40425-016-0161-x
# The best performing algorithm to compute the Immunoscore has been defined in the large international SITC -
#   led retrospective validation study [1, 2] conducted on more than 3800 St I-III colon cancer patients, 
# Briefly, for each marker (CD3 & CD8) and each zone (CT & IM), densities distributions have been established 
# on the study training set; for each parameter of a tested sample (CD3, CD8, CT, IM), a percentile is derived 
# from these distributions. An average percentile is calculated based on these 4 values. The Immunoscore® is 
# reported as IS-0, 1 – 2 – 3 – 4 based on the following average percentile classes respectively: 
#   [0 %; 10 %] - [>10 %; 25 %] - [>25 %; 70 %] - [>70 %; 95 %] - [>95 %; 100 %].
# CD3 and CD8 in two regions (CT and IM)

# distribution CD3 intra
quantile(markers$percent_CD3_total.i, c(.10, .25, .70, .95), na.rm = TRUE) 
# distribution CD3 periph
quantile(markers$percent_CD3_total.p, c(.10, .25, .70, .95), na.rm = TRUE) 
# distribution CD8 intra
quantile(markers$percent_CD8_total.i, c(.10, .25, .70, .95), na.rm = TRUE) 
# distribution CD8 periph
quantile(markers$percent_CD8_total.p, c(.10, .25, .70, .95), na.rm = TRUE) 
# Calculate percentile for each patient for CD3, 8, i, p
markers <- markers %>% 
  mutate(percentile_score_CD3_i = ntile(percent_CD3_total.i, 100) ) %>% 
  mutate(percentile_score_CD3_p = ntile(percent_CD3_total.p, 100) ) %>% 
  mutate(percentile_score_CD8_i = ntile(percent_CD8_total.i, 100) ) %>% 
  mutate(percentile_score_CD8_p = ntile(percent_CD8_total.p, 100) ) %>%
  mutate(percentile_score_mean = rowMeans(markers[c("percentile_score_CD3_i", "percentile_score_CD3_p", "percentile_score_CD8_i", "percentile_score_CD8_p")])) %>% 
  mutate(immunoscore_ = case_when(
    percentile_score_mean <= 10 ~ 0,
    percentile_score_mean <= 25 ~ 1,
    percentile_score_mean <= 70 ~ 2,
    percentile_score_mean <= 95 ~ 3,
    percentile_score_mean > 95 ~ 4 
  ))
# Survival
clin_surv <- markers
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$immunoscore_)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "Immunoscore", # legend.labs = c("mid-high", "mid-low", "high", "low"), # 4253
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~clin_surv$race+clin_surv$clusters_Brooke)


# 2.1.Exclusion: Immune markers difference between periph and intra.----
# Here
# https://dm5migu4zj3pb.cloudfront.net/manuscripts/96000/96313/cache/96313.3-20190319160739-covered-253bed37ca4c1ab43d105aefdf7b5536.pdf
# https://github.com/bhklab/EpiStromaImmune
# They estimate accumulation in the periph is the double of the intra numbers when exclusion
# That could be logical when looking at CD11b-CD15 are same intra and periph
# May even need to go 1.5 times nut lets see.

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

# Conclusions to take home
# More+ CD3-CD8 outside the tumor but good trend = 3 L out for 2 L in = not bad
# More++ CD3-FoxP outside the tumor = 4 L out for 2.2 L in
# Same CD11b-CD15 outside the tumor = 1.1 L out for 1 L in

# +

# FoxP3 alone - CD3-FoxP3
# Look at survival
# Look at CD8 presence








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
  annotation_logticks(sides="lb") +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = .6, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = .75)
colour=percent_CD3_FoxP3_tumor.i<1


#####

# 2.3.Clustering----
# Need to remove NAs
clust_markers <- markers %>% filter(!is.na(markers$sqrt_CD3_CD8_tumor.i ))
# Do not take CD3 alone for clustering beacuse contain effector, regulator, helper, gamma delta
# Do not take FoxP3 alone because tumor cells can express FoxP3
# Do not take CD11b alone because CD3CD11b can be NK cells
# CD11bCD15 are myeloid cells prevent other immune cells to traffic into the tumor


# 2.3.0.clusters 1_by_Brooke # She took only intratumoral but all markers simple and doubled staining
clust <- Mclust(clust_markers[,c(116:123)], G = 5)
summary(clust)
clust_markers$clusters_Brooke <- clust$classification
clust_markers$clusters_Brooke <- factor(clust_markers$clusters_Brooke, 
                                        levels = c(4, 2, 3, 5, 1),
                                        labels = c("mid-high", "mid-low", "high", "low", "mid"))
# clust_markers <- left_join(clust_markers, clust[, c("suid", "clusters_Brooke")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(116:123)) %>% 
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

# Cluster for tumor+stroma/intra+periph
# 2.3.1.clusters 1_tumor+stroma/intra+periph
clust_markers <- markers %>% filter( !is.na(markers[,c(116:131)]), !is.na(markers[,c(140:155)])  )

clust <- Mclust(clust_markers[,c(116:131,140:155)], G = 5)
summary(clust)
clust_markers$clusters_all_IandP <- clust$classification
clust_markers$clusters_all_IandP <- factor(clust_markers$clusters_all_IandP, 
                                        levels = c(1 , 2, 3, 4, 5),
                                        labels = c("mid", "mid-low", "high", "mid-high", "low"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(116:131)) %>% 
  select(suid, clusters_all_IandP, markers_cat, value) %>% 
  ggplot(aes(x=markers_cat, y=value,  color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_all_IandP)

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_all_IandP))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_all_IandP)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_all_IandP))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_all_IandP)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_all_IandP))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ clusters_all_IandP)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)

# Cluster for tumor
# 2.3.1.clusters 1_by_CD3-CD8
clust <- Mclust(clust_markers$sqrt_CD3_CD8_tumor.i, G = 5)
summary(clust)
clust_markers$clusters_CD3CD8 <- clust$classification
clust_markers$clusters_CD3CD8 <- factor(clust_markers$clusters_CD3CD8, 
                                        levels = c(1 , 2, 3, 4, 5),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(116:123)) %>% 
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
clust <- Mclust(clust_markers[,c("sqrt_CD3_CD8_tumor.i", "sqrt_CD3_FoxP3_tumor.i", "sqrt_CD11b_CD15_tumor.i")], G = 5)
summary(clust)
clust_markers$dbl_pos <- clust$classification
clust_markers$dbl_pos <- factor(clust_markers$dbl_pos, 
                                        levels = c(3 ,4,2, 5, 1),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(116:123)) %>% 
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


# 2.3.3.clusters_special
### 2.3.3.1_CD3-CD8 first
# clust_markers <- markers %>% filter(!is.na( sqrt_CD3_CD8_tumor.i ))
clust <- Mclust(clust_markers$sqrt_CD3_CD8_tumor.i, G = 2)
summary(clust)
clust_markers$clusters_CD38 <- clust$classification
clust_markers$clusters_CD38 <- factor(clust_markers$clusters_CD38, 
                                levels = c(1, 2),
                                labels = c("low", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", sqrt_CD3_total.i:sqrt_CD11b_CD15_total.i) %>%
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


### 2.3.3.2_ Cluster low into cold or excluded----
b <- clust_markers %>% filter(clusters_CD38 == "low")
b <- b %>% mutate(ratio_IP = percent_CD3_CD8_tumor.p / percent_CD3_CD8_tumor.i) %>%
  mutate(excluded_cluster = case_when(
    ratio_IP < 2 ~ "cold",
    ratio_IP >=2 ~ "excluded"
  ))

b %>% 
  gather(key = "markers_cat", value = "value", ratio_IP) %>% 
  select(suid, excluded_cluster, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=excluded_cluster, color=excluded_cluster))+ # Take home: bad separation, MORE outliers
  geom_boxplot()+
  facet_grid(.~ markers_cat)

# Try another by clustering ######################################### Need to compare ratio separation or ratio cluster
c <- b %>% filter(is.finite(ratio_IP))
clust <- Mclust(c$ratio_IP, G = 2)
summary(clust)
c$clusters_excluded <- clust$classification
c$clusters_excluded <- factor(c$clusters_excluded, 
                              levels = c(1, 2),
                              labels = c("cold", "excluded"))
clust_markers <- left_join(clust_markers, c[, c("suid", "excluded_cluster", "clusters_excluded")], by= "suid")

c %>% 
  gather(key = "markers_cat", value = "value", ratio_IP) %>% 
  select(suid, clusters_excluded, excluded_cluster, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_excluded, color=clusters_excluded))+
  geom_boxplot()






### 2.3.3.3_ then immunosuppressed

# https://www.nature.com/articles/s41573-018-0007-y.pdf
# Following neoadjuvant
# chemotherapy (NAC), these patients had an increased
# ratio of intratumoural CD8+ T cells to FOXP3+ cells200.
# This event was accompanied by a clonal expansion of
# antitumour T cells that correlated with response to NAC, 
# followed by a complete pathological response200. After
# chemotherapy, the absence of autophagy-related protein
# LC3B (LC3B+) puncta and a low CD8+ to FOXP3+ cells
# ratio were associated with a bad prognosis in patients
# with breast cancer201,202

# The decreased percentages of intratumour
# CD4+CD25+FOXP3+ T  cells were accompanied by
# increased CD8+ T cell infiltrates, CD8+ T cell proliferation, increased levels of effector memory T cells and
# overall enhanced antitumour activity268.

# CD11b
# The chemokine XC receptor 1 (XCR1) and its ligand
# lymphotactin (XCL1) regulate migration and function
# of CD103+CD11b− DCs182–184. 

# https://www.spandidos-publications.com/10.3892/mco.2013.107
# Although FOXP3 expression in tumor cells was of no prognostic significance, FOXP3+ lymphocytes were
# significantly associated with poor overall survival 
# FOXP3 exhibited a heterogeneous subcellular localization in tumor cells 
# (cytoplasm, 31%; nucleus, 26%; both, 6%) and, although cytoplasmic FOXP3 was associated with poor OS (P=0.058), 
# nuclear FOXP3 demonstrated a significant association with improved OS (p=0.016). Furthermore, 
# when patients were grouped according to their expression of tumor cytoplasmic FOXP3 and lymphocyte FOXP3, 
# there were notable differences in the Kaplan-Meier curves for OS (P<0.001), with a high infiltration of 
# FOXP3+ lymphocytes accompanied by a cytoplasmic FOXP3+ tumor being the most detrimental phenotype.
# FOXP3 plays a crucial role in the generation of immunosuppressive CD4+CD25+ regulatory T cells (Tregs), 
# which induce immune tolerance to antigens
# tumor-expressed FOXP3, Hinz et al previously reported that FOXP3 expression in a pancreatic cancer cell 
# line inhibited the proliferation of anti-CD3/anti-CD28-stimulated T cells without impeding their activation 

# Ovarian
# https://clincancerres.aacrjournals.org/content/24/22/5685.long
# In agreement with these findings, bulk Treg cells purified from ovarian tumors had enhanced suppressive 
# capacity when compared with melanoma-infiltrating Treg cells (Fig. 6D). These results showed that the 
# immunologic receptor expression pattern including increased 4-1BB, PD-1, and ICOS expression defines 
# highly activated and suppressive Treg cells that infiltrate ovarian tumors.
# https://iji.sums.ac.ir/?sid=Entrez:PubMed&id=pmid:24975967&key=2014.11.2.105
# A trend toward higher Treg cells was observed in higher stages of ovarian cancer (III+IV) in comparison 
# to lower stages (I+II) (6.5 ± 3.2% vs. 4.44 ± 2.7%, p=0.2). Higher percentage of Treg cells was also 
# observed in the patients with high CA125 (CA-125 >100 U/mL) in comparison to those with low CA-125 serum 
# level (CA-125 ≤100 U/mL) although the difference was not significant (6.44 versus 4.18%, p=0.19).
# https://pubmed.ncbi.nlm.nih.gov/24244610/
# The total numbers of CD4(+)CD25(+)FOXP3(+) Tregs, CD4(+)CD25(+)FOXP3(-), CD3(+) and CD8(+) cells were not 
# significantly different between the groups. However, higher ratios of CD8(+)/CD4(+)CD25(+)FOXP3(+) Treg, 
# CD8(+)/CD4(+) and CD8/CD4(+)CD25(+)FOXP3(-) cells were seen in the good outcome group when compared to the
# patients with poor outcome. These data show for the first time that the ratios of CD8(+) to both 
# CD4(+)CD25(+)FOXP3(+) Tregs and CD4(+)CD25(+)FOXP3(-) T cells are associated with disease outcome in 
# ovarian cancer. The association being apparent in ratios rather than absolute count of T cells suggests 
# that the effector/suppressor ratio may be a more important indicator of outcome than individual cell count. 
# Thus, immunotherapy strategies that modify the ratio of CD4(+)CD25(+)FOXP3(+) Tregs or CD4(+)CD25(+)FOXP3(-) 
# T cells to CD8(+) effector cells may be useful in improving outcomes in ovarian cancer.
# # In yet a more recent study, Milne and colleagues showed, in high grade serous ovarian cancer, that the count 
# of intraepithelial cells singly stained for FOXP3+ was associated with greatly improved survival [20]. However, 
# their study did not specifically define the cell type expressing FOXP3. Given that other cells in addition to T 
# cells express FOXP3 (e.g., tumor cells and B cells), the significance of that work, while interesting, 
# remains unclear [21,22].


# let's take a look at survival between FoxP3_all, FoxP3_alone, FoxP3_CD3?






# cluster ratio CD3CD8/CD3Foxp3
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_CD3_FoxP3_tumor.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) # clust on ratio
summary(clust)
a$clusters_immsuppr <- clust$classification
a$clusters_immsuppr <- factor(a$clusters_immsuppr, 
                           levels = c(1, 2),
                           labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_immsuppr")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_immsuppr, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_immsuppr, color=clusters_immsuppr))+
  geom_boxplot()


# clustering
# a <- clust_markers %>% filter(clusters_CD38 == "high") # 2 is high in CD38
clust <- Mclust(a$percent_CD3_FoxP3_tumor.i, G = 2) # clust on CD3-FoxP3
summary(clust)
a$clusters_FoxP3 <- clust$classification
a$clusters_FoxP3 <- factor(a$clusters_FoxP3, 
                                      levels = c(1, 2),
                                      labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD3_FoxP3_tumor.i) %>% 
  select(suid, markers_cat, clusters_FoxP3, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3, color=clusters_FoxP3))+
  geom_boxplot()

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
gridExtra::grid.arrange(p1, p2, p3, ncol=3)############################ Need compare ration 8/fox and fox high lox cluster----

# Combine all special cluster CD38 and cluster FoxP3
clust_markers <- clust_markers %>% 
  mutate(special_cluster = case_when(
    clusters_CD38 == "low" ~ "cold", # lox CD8 aka cold
    clusters_FoxP3 == "hot" ~ "hot", # low Fox aka hot
    clusters_FoxP3 == "immunosuppressed" ~ "immunosuppressed", # high Fox aka immunosupp
  )) %>% 
  mutate(special_cluster2 = case_when(
    clusters_CD38 == "low" &
      clusters_excluded == "cold" ~ "cold",
    clusters_CD38 == "low" &
      clusters_excluded == "excluded" ~ "excluded",
    clusters_CD38 == "high" &
      clusters_immsuppr == "immunosupressed" ~ "immunosupressed",
    clusters_CD38 == "high" &
      clusters_immsuppr == "hot" ~ "hot"
  )) %>% 
  mutate(special_cluster3 = case_when(
    clusters_CD38 == "low" &
      excluded_cluster == "cold" ~ "cold",
    clusters_CD38 == "low" &
      excluded_cluster == "excluded" ~ "excluded",
    clusters_CD38 == "high" &
      clusters_immsuppr == "immunosupressed" ~ "immunosupressed",
    clusters_CD38 == "high" &
      clusters_immsuppr == "hot" ~ "hot"
  )) %>% 
  mutate(special_cluster4 = case_when(
    clusters_CD38 == "low" &
      clusters_excluded == "cold" ~ "cold",
    clusters_CD38 == "low" &
      clusters_excluded == "excluded" ~ "excluded",
    clusters_CD38 == "high" &
      clusters_FoxP3 == "hot" ~ "hot",
    clusters_CD38 == "high" &
      clusters_FoxP3 == "immunosuppressed" ~ "immunosuppressed"
  )) %>% 
  mutate(special_cluster5 = case_when(
    clusters_CD38 == "low" &
      excluded_cluster == "cold" ~ "cold",
    clusters_CD38 == "low" &
      excluded_cluster == "excluded" ~ "excluded",
    clusters_CD38 == "high" &
      clusters_FoxP3 == "hot" ~ "hot",
    clusters_CD38 == "high" &
      clusters_FoxP3 == "immunosuppressed" ~ "immunosuppressed"
  ))



######################################################################################## II ### Survival by cluster---
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_Brooke", "clusters_all_IandP", "clusters_CD3CD8", "dbl_pos",
                                         "clusters_CD38", "excluded_cluster",
                                         "clusters_excluded", "clusters_immsuppr", "clusters_FoxP3",
                                         "special_cluster", "special_cluster2", "special_cluster3", 
                                         "special_cluster4", "special_cluster5")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
# 2.1.Does Intratumoral tumor+stroma markers are predictive?----
# clusters_Brooke
myplot <- survfit(mysurv~clin_surv$clusters_Brooke)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by Brooke",
           legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~clin_surv$race+clin_surv$clusters_Brooke)

# 2.2.Does Intratumoral+Peripheral tumor+stroma markers are predictive?----
# clusters_all_IandP
myplot <- survfit(mysurv~clin_surv$clusters_all_IandP)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by Brooke",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~clin_surv$race+clin_surv$clusters_Brooke)

# 2.3.Does Peripheral tumor+stroma markers are predictive?-------------------------------------------------------


# 2.4.Does Intratumoral tumor CD3CD8 are predictive?----
# cluster 1
myplot <- survfit(mysurv~clin_surv$clusters_CD3CD8)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD3+/CD8+", 
           legend.labs = c("high", "mid", "mid-high", "mid-low"), # 4253
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~clin_surv$race+clin_surv$clusters_CD3CD8)

# 2.5.Does Intratumoral tumor double positive are predictive?----
# cluster dbl_pos
myplot <- survfit(mysurv~clin_surv$dbl_pos)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by all double positive", 
           legend.labs = c("high", "low", "mid", "mid-high", "mid-low"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$dbl_pos)

# 2.6.Does special clustering (excluded,immunosupp, hot, cold) is predictive?----
# special_cluster
myplot <- survfit(mysurv~clin_surv$special_cluster)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 then FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster)
# special_cluster2
myplot <- survfit(mysurv~clin_surv$special_cluster2)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 -ratioCD8<2 -ratioCD8/FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster2)
# special_cluster3
myplot <- survfit(mysurv~clin_surv$special_cluster3)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by  CD8 -ratioCD8cluster -ratioCD8/FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster3)
# special_cluster4
myplot <- survfit(mysurv~clin_surv$special_cluster4)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 -ratioCD8<2 -FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster4)
# special_cluster5
myplot <- survfit(mysurv~clin_surv$special_cluster5)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 -ratioCD8clustaer -FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster5)




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
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)

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
