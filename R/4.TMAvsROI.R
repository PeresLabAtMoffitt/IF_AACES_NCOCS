# Limiting to the 28 patients we have ROIs and TMAs

uid <- paste(unique(common_ROITMA_IDs$Subject_IDs), collapse = '|')
variations_TMA <- variations_TMA[(grepl(uid, variations_TMA$suid)),]
variations_ROI <- variations_ROI[(grepl(uid, variations_ROI$suid)),]
variations_ROIi <- variations_ROIip[(grepl(uid, variations_ROIip$suid)),] %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral")
variations_ROIp <- variations_ROIip[(grepl(uid, variations_ROIip$suid)),] %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral")

variations <- merge.data.frame(variations_TMA %>% 
                                 select(-ID), 
                               variations_ROI %>% 
                                 select(-ID),
                               by.x = "suid", by.y = "suid",
                               all = TRUE, suffixes = c("_tma", "_roi"))
variations_ROIip <- merge.data.frame(variations_ROIi %>% 
                                       select(-ID),
                                     variations_ROIp %>% 
                                       select(-ID),
                                     by.x = "suid", by.y = "suid",
                                     all = TRUE, suffixes = c("_roi_i", "_roi_p"))
variations <- merge.data.frame(variations, variations_ROIip,
                               by.x = "suid", by.y = "suid",
                               all = TRUE) %>% 
  mutate(ID = seq(1:nrow(.)))


# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_1.jpg"))
ggplot(variations, aes(x=suid, y=mean_tumor_tma)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_tma), color="skyblue") +
  geom_point(color="blue", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# peg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_2.jpg"))
ggplot(variations, aes(x=suid, y=mean_tumor_roi)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi), color="skyblue") +
  geom_point(color="blue", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_1.jpg"))
ggplot(variations, aes(x=ID, y=tumor_variation_tma, colour = tumor_variation_tma>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation_tma)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_3.jpg"))
ggplot(variations, aes(x=ID, y=stroma_variation_tma, colour = stroma_variation_tma>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation_tma)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_4.jpg"))
ggplot(variations, aes(x=ID, y=tumor_variation_roi, colour = tumor_variation_roi>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation_roi)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in ROIs per Patient",
       subtitle = "Each point represent the mean of up to 6 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_5.jpg"))
ggplot(variations, aes(x=ID, y=stroma_variation_roi, colour = stroma_variation_roi>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation_roi)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in ROIs per Patient",
       subtitle = "Each point represent the mean of up to 6 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_6.jpg"))
ggplot(variations, aes(x=ID, y=tumor_variation_roi_i, colour = tumor_variation_roi_i>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation_roi_i)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_7.jpg"))
ggplot(variations, aes(x=ID, y=stroma_variation_roi_i, colour = stroma_variation_roi_i>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation_roi_i)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_8.jpg"))
ggplot(variations, aes(x=ID, y=tumor_variation_roi_p, colour = tumor_variation_roi_p>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation_roi_p)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Peripheral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_9.jpg"))
ggplot(variations, aes(x=ID, y=stroma_variation_roi_p, colour = stroma_variation_roi_p>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation_roi_p)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Peripheral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()


#
# Back to back 
  # geom_segment(aes(x = 2, y = 15, xend = 2, yend = 25),
  #                arrow = arrow(length = unit(0.5, "cm")))

ggplot(variations, aes(x=suid)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_tma, color="TMA"))+
  geom_point(aes(y=mean_tumor_tma), color="blue", size=1, alpha=0.6) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi, color="ROI"), 
                position = position_nudge(x = 0.5, y = 0))+
  geom_point(aes(y=mean_tumor_roi) , color="red", size=1, alpha=0.6, 
             position = position_nudge(x = 0.5, y = 0)) + 
  theme_minimal() + 
  coord_flip()+
  scale_color_discrete(name="Samples from")+
  scale_color_manual(values = c("#FF9999", "skyblue")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs vs ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values for TMA, 6 values fro ROI")


variations %>% mutate(mean_tumor_roi = -1*mean_tumor_roi) %>% 
  ggplot(aes(x=suid)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_tma, color="TMA")) +
  geom_point(aes(y=mean_tumor_tma), color="blue", size=1, alpha=0.6) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi, color="ROI")) +
  geom_point(aes(y=mean_tumor_roi) , color="red", size=1, alpha=0.6) +
  scale_y_continuous(breaks = c(-100, -50, 0, 50, 100),
                     labels = abs(c(-100, -50, 0, 50, 100))) +
  geom_hline(yintercept = 0) +
  theme_minimal() + 
  coord_flip()+
  scale_color_discrete(name="Samples from")+
  scale_color_manual(values = c("#FF9999", "skyblue")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs vs ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values for TMA, 6 values fro ROI")


# TMA vs ROI vs I vs P
par(mar=c(5, 5, 20, 3.1)) # bottom left top right
ggplot(variations, aes(x=suid)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_tma, color="TMA")) +
  geom_point(aes(y=mean_tumor_tma), color="blue", size=1, alpha=0.6) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi_i, color="ROIi"), 
                position = position_nudge(x = 0.25, y = 0)) +
  geom_point(aes(y=mean_tumor_roi_i) , color="limegreen", size=1, alpha=0.6, 
             position = position_nudge(x = 0.25, y = 0)) + 
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi_p, color="ROIp"), 
                position = position_nudge(x = 0.5, y = 0)) +
  geom_point(aes(y=mean_tumor_roi_p) , color="purple", size=1, alpha=0.6, 
             position = position_nudge(x = 0.5, y = 0)) + 
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi, color="ROI"), 
                position = position_nudge(x = 0.75, y = 0)) +
  geom_point(aes(y=mean_tumor_roi) , color="red", size=1, alpha=0.6, 
             position = position_nudge(x = 0.75, y = 0)) + 
  theme_minimal() + 
  coord_flip()+
  scale_x_discrete(expand=c(0.05, 0)) +
  scale_color_manual(name="Samples \nfrom", values = c("skyblue", "palegreen", "plum1", "#FF9999"), breaks=c("TMA","ROIi", "ROIp","ROI")) +
  labs(x=paste(length(variations$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs vs ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values for TMA, 6 values fro ROI")


# Correlation

library(psych)

pairs.panels(variations)
pairs.panels(variations[c("mean_tumor_tma", "mean_tumor_roi_i")])

library(corrplot)
library(ggcorrplot)

mat <- cor(variations[, c("mean_tumor_tma", "mean_tumor_roi_i", "mean_tumor_roi_p", 
                          "mean_stroma_tma", "mean_stroma_roi_i", "mean_stroma_roi_p")], 
           use = "pairwise.complete.obs")
corrplot(mat)
corrplot.mixed(mat)
mat1 <- cor(variations[, c("tumor_variation_tma", "tumor_variation_roi_i", "tumor_variation_roi_p", 
                           "stroma_variation_tma", "stroma_variation_roi_i", "stroma_variation_roi_p")], 
            use = "pairwise.complete.obs")
variations$mean_stroma_roi_i
corrplot(mat1)
corrplot.mixed(mat1)
ggcorrplot(mat, hc.order = TRUE, method = "circle", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "lower", # show the top half panel
           lab = TRUE, # add correlation nbr
           title = "Correlation between collection type",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           colors = viridis::inferno(n=3),
           lab_col = "darkblue", lab_size = 3, # col and size of the correlation nbr
           # p.mat = pmat, # Add correlation significance
           # sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 270,
           digits = 1
)




# End

# Cleaning
rm(common_ROITMA_IDs, variations_TMA, variations_ROI, variations_ROIip)
