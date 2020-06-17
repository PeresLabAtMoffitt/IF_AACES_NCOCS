# Intraclass Correlation Coefficient

ROI_ICCi <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>%
  select(c("suid", "tumor_total_cells"))
ROI_ICCi <- dcast(setDT(ROI_ICCi), suid ~ rowid(suid), 
                 value.var = "tumor_total_cells") %>% 
  select(c(2:4))

ROI_ICCp <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>%
  select(c("suid", "tumor_total_cells"))
ROI_ICCp <- dcast(setDT(ROI_ICCp), suid ~ rowid(suid), 
                  value.var = "tumor_total_cells") %>% 
  select(c(2:4))

TMA_ICC <- TMA_global[, c("suid", "tumor_total_cells")] 
TMA_ICC <- dcast(setDT(TMA_ICC), suid ~ rowid(suid), 
                 value.var = "tumor_total_cells") %>% 
  select(c(2:4))

# If test for intra-rater (because for each patient the 3 TMA/ROI was done by the same person)
# If test for inter-rater (because not the same rater between patients)
# Two-way mixed effect, fixed raters are defined. Each subject is measured by the k raters.
library(psych) # Koo and Li (2016)
ICC(TMA_ICC)  # between 0.75 and 0.90: good
ICC(ROI_ICCi)
ICC(ROI_ICCp) #between 0.50 and 0.75: moderate
library(irr) # Does not count patient with NA
icc(
  TMA_ICC, model = "twoway", 
  type = "consistency", unit = "average"
)
icc(
  ROI_ICCi, model = "twoway", 
  type = "consistency", unit = "average"# Reliability applied where measures of k raters will be averaged for each subject.
)
icc(
  ROI_ICCp, model = "twoway", 
  type = "consistency", unit = "average" # Consistency: for repeated measurements by the same rater,
)




rm(TMA_ICC, ROI_ICCi, ROI_ICCp)


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
                  round(mean(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_tumor),2),
                  round(mean(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_stroma),2),
                  "median",
                  median(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_tumor),
                  median(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_stroma),
                  "range",
                  paste(
                    range(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_tumor)[1],
                    range(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_tumor)[2],
                    sep = "-"
                  ),
                  paste(
                    range(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_stroma)[1],
                    range(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_stroma)[2],
                    sep = "-"
                  ),
                  "variance", 
                  round(var(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_tumor),2),
                  round(var(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_stroma),2),
                  "sd",
                  round(sd(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_tumor),2),
                  round(sd(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Intratumoral")$percent_stroma),2),
                  "peripheral", "", "",
                  "mean",
                  round(mean(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_tumor),2),
                  round(mean(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_stroma),2),
                  "median",
                  median(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_tumor),
                  median(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_stroma),
                  "range",
                  paste(
                    range(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_tumor)[1],
                    range(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_tumor)[2],
                    sep = "-"
                  ),
                  paste(
                    range(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_stroma)[1],
                    range(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_stroma)[2],
                    sep = "-"
                  ),
                  "variance", 
                  round(var(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_tumor),2),
                  round(var(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_stroma),2),
                  "sd",
                  round(sd(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_tumor),2),
                  round(sd(x= filter(ROI_global, intratumoral_i_vs_peripheral_p_ == "Peripheral")$percent_stroma),2)
), ncol = 3, byrow = TRUE)
# write.csv(table, 
#           paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Summary tumor, stroma in TMAs and ROIs.csv"))
# rm(table)


# TMA
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/hist TMA tumor vs stroma-yaxis.jpg"),
#      width = 350, height = 350)
p1 <- ggplot(TMA_global) +
  geom_histogram(aes(tumor_total_cells), color = "darkgrey", fill="#8707A6FF") +
  theme_minimal() +
  ylim(0,100) +
  labs(x="Tumor", y="count", title="Cell Type Repartition in TMAs")
p2 <- ggplot(TMA_global) +
  geom_histogram(aes(stroma_total_cells), color = "darkgrey", fill="#00204DFF") +
  theme_minimal() +
  labs(x="Stroma", y="count")
gridExtra::grid.arrange(p1, p2, ncol = 2)
# dev.off()

# ROI
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/hist ROI tumor vs stroma.jpg")
#      , width = 350, height = 350)
p1 <- ggplot(ROI_global) +
  geom_histogram(aes(tumor_total_cells), color = "darkgrey", fill="#8707A6FF") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,175)) +
  labs(x="Tumor", y="count")
p2 <- ggplot(ROI_global) +
  geom_histogram(aes(stroma_total_cells), color = "darkgrey", fill="#00204DFF") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,175)) +
  labs(x="Stroma", y="count")
gridExtra::grid.arrange(p1, p2, ncol = 2,
                        top = "Cell Type Repartition in ROIs")
# dev.off()

# 
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/hist ROI tumor vs stroma facet location.jpg")
#      , width = 350, height = 350)
p1 <- ggplot(ROI_global) +
  geom_histogram(aes(tumor_total_cells), color = "darkgrey", fill="#8707A6FF") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,175)) +
  labs(x="Tumor", y="count") +
  facet_grid(.~intratumoral_i_vs_peripheral_p_)
p2 <- ggplot(ROI_global) +
  geom_histogram(aes(stroma_total_cells), color = "darkgrey", fill="#00204DFF") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,175)) +
  labs(x="Stroma", y="count") +
  facet_grid(.~intratumoral_i_vs_peripheral_p_)
gridExtra::grid.arrange(p1, p2, nrow = 2,
                        top = "Cell Type Repartition in ROIs")
# dev.off()


# Density plot

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density ROI tumor.jpg"))
ggplot(ROI_global, aes(tumor_total_cells, linetype = intratumoral_i_vs_peripheral_p_)) +
  geom_density(alpha=.3, color = "#8707A6FF") +
  theme_minimal() +
  scale_linetype_manual(name = "Sample \nPosition", values = 1:2) +
  labs(x="Tumor", y="Sample Count", title="Cell Type Repartition in ROIs")
# dev.off()
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density ROI stroma.jpg"))
ggplot(ROI_global, aes(stroma_total_cells, linetype = intratumoral_i_vs_peripheral_p_)) +
  geom_density(alpha=.3, color = "#00204DFF") +
  theme_minimal() +
  scale_linetype_manual(name = "Sample \nPosition", values = 1:2) +
  labs(x="Stroma", y="Sample Count", title="Cell Type Repartition in ROIs", color='Sample \nPosition')
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density ROI tumor vs stroma.jpg"))
ggplot(ROI_global, aes(tumor_total_cells, linetype=intratumoral_i_vs_peripheral_p_, color = "tumor")) +
  geom_density(fill = "rosybrown1", alpha = .2) +
  geom_density(aes(stroma_total_cells, linetype=intratumoral_i_vs_peripheral_p_, color = "stroma"),
               fill = "#00204DFF", alpha = .1) +
  theme_minimal() +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  scale_linetype_manual(name = "Sample \nPosition", labels = c("Intratumoral", "Peripheral"), values = 1:2) +
  labs(x="Amount of Cell", y="Sample Count", title="Cell Type Repartition in ROIs", color="Cell Type")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density TMA tumor vs stroma.jpg"))
ggplot(TMA_global, aes(tumor_total_cells, color = "Tumor")) +
  geom_density() +
  geom_density(aes(stroma_total_cells, color = "Stroma")) +
  theme_minimal() +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  scale_linetype_manual(name = "Sample \nPosition", values = 1:2) +
  labs(x="Amount of Cell", y="Sample Count", title="Cell Type Repartition in TMAs", color="Cell Type")
# dev.off()


# More about the variation per cases

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/tumor cell presence per case in TMAs.jpg"))
ggplot(variations_TMA, aes(x=suid, y=mean_tumor)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor), color="skyblue") +
  geom_point(color="blue", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x=paste(length(variations_TMA$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/tumor cell presence per case in ROIs.jpg"))
ggplot(variations_ROI, aes(x=suid, y=mean_tumor)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor), color="skyblue") +
  geom_point(color="blue", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x=paste(length(variations_ROI$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in TMAs.jpg"))
ggplot(variations_TMA, aes(x=ID, y=tumor_variation, colour = tumor_variation>0)) +
  annotate("text", x = 0.25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation)) +
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
  labs(x=paste(length(variations_TMA$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in TMAs.jpg"))
ggplot(variations_TMA, aes(x=ID, y=stroma_variation, colour = stroma_variation>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation)) +
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
  labs(x=paste(length(variations_TMA$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

#
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in ROI.jpg"))
ggplot(variations_ROI, aes(x=ID, y=tumor_variation, colour = tumor_variation>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation)) +
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
  labs(x=paste(length(variations_ROI$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in ROIs per Patient",
       subtitle = "Each point represent the mean of up to 6 values")
# dev.off()
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in ROI.jpg"))
ggplot(variations_ROI, aes(x=ID, y=stroma_variation, colour = stroma_variation>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation)) +
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
  labs(x=paste(length(variations_ROI$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in ROIs per Patient",
       subtitle = "Each point represent the mean of up to 6 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in intratumoral ROI.jpg"))
variations_ROIip %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
ggplot( aes(x=ID, y=tumor_variation, colour = tumor_variation>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation)) +
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
  labs(x=paste(length(variations_ROI$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in peripheral ROI.jpg"))
variations_ROIip %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  ggplot( aes(x=ID, y=tumor_variation, colour = tumor_variation>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation)) +
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
  labs(x=paste(length(variations_ROI$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Peripheral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in intratumoral ROI.jpg"))
variations_ROIip %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
  ggplot( aes(x=ID, y=stroma_variation, colour = stroma_variation>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation)) +
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
  labs(x=paste(nrow(variations_ROIip %>% filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral")), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in peripheral ROI.jpg"))
variations_ROIip %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  ggplot( aes(x=ID, y=stroma_variation, colour = stroma_variation>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation)) +
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
  labs(x=paste(nrow(variations_ROIip %>% filter(intratumoral_i_vs_peripheral_p_ == "Peripheral")), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Peripheral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# End



