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
  mutate(percentile_score_CD8_p = ntile(percent_CD8_total.p, 100) ) 
markers <- markers %>%
  mutate(percentile_score_mean = rowMeans(markers[c("percentile_score_CD3_i", "percentile_score_CD3_p", 
                              "percentile_score_CD8_i", "percentile_score_CD8_p")])
         ) %>% 
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
survdiff(mysurv~clin_surv$race+clin_surv$immunoscore_)


# 2.1.Exclusion: Immune markers difference between periph and intra.----
# Here
# https://dm5migu4zj3pb.cloudfront.net/manuscripts/96000/96313/cache/96313.3-20190319160739-covered-253bed37ca4c1ab43d105aefdf7b5536.pdf
# https://github.com/bhklab/EpiStromaImmune
# They estimate accumulation in the periph is the double of the intra numbers when exclusion
# That could be logical when looking at CD11b-CD15 are same intra and periph
# May even need to go 1.5 times nut lets see.

p1 <- ggplot(markers, aes(x=percent_CD3_tumor.p, y=percent_CD3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(label.y = 15)+
  stat_cor(label.y = 17, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,30) + ylim(0,30) +
stat_regline_equation(label.y.npc = 1)
p2 <- ggplot(markers, aes(x=percent_CD8_tumor.p, y=percent_CD8_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 12.5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,15.5) + ylim(0,15.5) +
stat_regline_equation(label.y.npc = 1)
p3 <- ggplot(markers, aes(x=percent_FoxP3_tumor.p, y=percent_FoxP3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 10, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(-0.05,12.5) + ylim(-0.05,12.5) +
  stat_regline_equation(label.y.npc = 1) +
  geom_rect(mapping=aes(xmin=-0.05, xmax=.5, ymin=-0.05, ymax=0.3, fill="red"), color="red", alpha=0.01)+
  theme(legend.position = "none")
p4 <- ggplot(markers, aes(x=percent_CD11b_tumor.p, y=percent_CD11b_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,11) + ylim(0,11) +
stat_regline_equation(label.y.npc = 1)
p5 <- ggplot(markers, aes(x=percent_CD15_tumor.p, y=percent_CD15_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 4, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,4) + ylim(0,4) +
stat_regline_equation(label.y.npc = 1)
p6 <- ggplot(markers, aes(x=percent_CD3_CD8_tumor.p, y=percent_CD3_CD8_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 11, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,15) + ylim(0,15) +
stat_regline_equation(label.y.npc = 1)
p7 <- ggplot(markers, aes(x=percent_CD3_FoxP3_tumor.p, y=percent_CD3_FoxP3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 3, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,4) + ylim(0,4) +
  stat_regline_equation(label.y.npc = 1)
p8 <- ggplot(markers, aes(x=percent_CD11b_CD15_tumor.p, y=percent_CD11b_CD15_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 1.5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,1.75) +
  stat_regline_equation(label.y.npc = 1)
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 5)

# Conclusions to take home
# More+ CD3-CD8 outside the tumor but good trend = 3 L out for 2 L in = not bad
# More++ CD3-FoxP outside the tumor = 4 L out for 2.2 L in
# Same CD11b-CD15 outside the tumor = 1.1 L out for 1 L in

# 2.1.Exclusion: Immune markers difference between tumor and stroma----
p1 <- ggplot(markers, aes(x=percent_CD3_stroma.i, y=percent_CD3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(label.y = 15)+
  stat_cor(label.y = 17, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,50) + ylim(0,50) +
  stat_regline_equation(label.y.npc = 1)
p2 <- ggplot(markers, aes(x=percent_CD8_stroma.i, y=percent_CD8_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 12.5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,45) + ylim(0,45) +
  stat_regline_equation(label.y.npc = 1)
p3 <- ggplot(markers, aes(x=percent_FoxP3_stroma.i, y=percent_FoxP3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 10, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(-0.05,12.5) + ylim(-0.05,12.5) +
  stat_regline_equation(label.y.npc = 1) +
  geom_rect(mapping=aes(xmin=-0.05, xmax=.5, ymin=-0.05, ymax=0.3, fill="red"), color="red", alpha=0.01)+
  theme(legend.position = "none")
p4 <- ggplot(markers, aes(x=percent_CD11b_stroma.i, y=percent_CD11b_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,25) + ylim(0,25) +
  stat_regline_equation(label.y.npc = 1)
p5 <- ggplot(markers, aes(x=percent_CD15_stroma.i, y=percent_CD15_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 4, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,11) + ylim(0,11) +
  stat_regline_equation(label.y.npc = 1)
p6 <- ggplot(markers, aes(x=percent_CD3_CD8_stroma.i, y=percent_CD3_CD8_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 11, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,35) + ylim(0,35) +
  stat_regline_equation(label.y.npc = 1)
p7 <- ggplot(markers, aes(x=percent_CD3_FoxP3_stroma.i, y=percent_CD3_FoxP3_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 3, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,12) + ylim(0,12) +
  stat_regline_equation(label.y.npc = 1)
p8 <- ggplot(markers, aes(x=percent_CD11b_CD15_stroma.i, y=percent_CD11b_CD15_tumor.i))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = 1.5, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  xlim(0,11) + ylim(0,11) +
  stat_regline_equation(label.y.npc = 1)
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 5)






# 2.2.Ratio:Immune cells vs tumor cell---- Doesn't seem as important
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

