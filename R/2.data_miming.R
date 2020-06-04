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
                  round(mean(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor),2),
                  round(mean(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma),2),
                  "median",
                  median(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor),
                  median(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma),
                  "range",
                  paste(
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor)[1],
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor)[2],
                    sep = "-"
                  ),
                  paste(
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma)[1],
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma)[2],
                    sep = "-"
                  ),
                  "variance", 
                  round(var(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor),2),
                  round(var(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma),2),
                  "sd",
                  round(sd(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_tumor),2),
                  round(sd(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "i")$percent_stroma),2),
                  "peripheral", "", "",
                  "mean",
                  round(mean(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor),2),
                  round(mean(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma),2),
                  "median",
                  median(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor),
                  median(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma),
                  "range",
                  paste(
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor)[1],
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor)[2],
                    sep = "-"
                  ),
                  paste(
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma)[1],
                    range(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma)[2],
                    sep = "-"
                  ),
                  "variance", 
                  round(var(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor),2),
                  round(var(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma),2),
                  "sd",
                  round(sd(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_tumor),2),
                  round(sd(x= filter(ROI_global, intratumoral__i__vs_peripheral__p_.x == "p")$percent_stroma),2)
), ncol = 3, byrow = TRUE)
# write.csv(table, 
#           paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Summary tumor, stroma in TMAs and ROIs.csv"))
# rm(table)
# TMA
jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/hist TMA tumor vs stroma-yaxis.jpg"),
     width = 350, height = 350)
p1 <- ggplot(TMA_global) +
  geom_histogram(aes(tumor__total_cells), color = "grey", fill="darkred") +
  theme_minimal() +
  ylim(0,100) +
  labs(x="Tumor", y="count", title="Cell Type Repartition in TMAs")
p2 <- ggplot(TMA_global) +
  geom_histogram(aes(stroma__total_cells), color = "grey", fill="steelblue") +
  theme_minimal() +
  labs(x="Stroma", y="count")
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()

# ROI
jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/hist ROI tumor vs stroma.jpg")
     , width = 350, height = 350)
p1 <- ggplot(ROI_global) +
  geom_histogram(aes(tumor__total_cells), color = "grey", fill="darkred") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,175)) +
  labs(x="Tumor", y="count")
p2 <- ggplot(ROI_global) +
  geom_histogram(aes(stroma__total_cells), color = "grey", fill="steelblue") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,175)) +
  labs(x="Stroma", y="count")
gridExtra::grid.arrange(p1, p2, ncol = 2,
                        top = "Cell Type Repartition in ROIs")
dev.off()

# 
jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/hist ROI tumor vs stroma facet location.jpg")
     , width = 350, height = 350)
p1 <- ggplot(ROI_global) +
  geom_histogram(aes(tumor__total_cells), color = "grey", fill="darkred") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,175)) +
  labs(x="Tumor", y="count") +
  facet_grid(.~intratumoral__i__vs_peripheral__p_.x)
p2 <- ggplot(ROI_global) +
  geom_histogram(aes(stroma__total_cells), color = "grey", fill="steelblue") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,175)) +
  labs(x="Stroma", y="count") +
  facet_grid(.~intratumoral__i__vs_peripheral__p_.x)
gridExtra::grid.arrange(p1, p2, nrow = 2,
                        top = "Cell Type Repartition in ROIs")
dev.off()

# Density plot
jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density ROI tumor.jpg"))
ggplot(ROI_global, aes(tumor__total_cells, colour = intratumoral__i__vs_peripheral__p_.x)) +
  geom_density(alpha=.3) +
  # scale_color_discrete(labels = c("intra", "periph")) +
  theme_minimal() +
  labs(x="Tumor", y="Sample Count", title="Cell Type Repartition in ROIs", color='Sample \nPosition')
dev.off()
jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density ROI stroma.jpg"))
ggplot(ROI_global, aes(stroma__total_cells, colour = intratumoral__i__vs_peripheral__p_.x)) +
  geom_density(alpha=.3) +
  # scale_color_discrete(labels = c("intra", "periph")) +
  theme_minimal() +
  labs(x="Stroma", y="Sample Count", title="Cell Type Repartition in ROIs", color='Sample \nPosition')
dev.off()

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density ROI tumor vs stroma.jpg"))
ggplot(ROI_global, aes(tumor__total_cells, linetype=intratumoral__i__vs_peripheral__p_.x, color = "tumor")) +
  geom_density(fill = "darkred", alpha = .3) +
  geom_density(aes(stroma__total_cells, linetype=intratumoral__i__vs_peripheral__p_.x), color = "steelblue") +
  theme_minimal() +
  scale_linetype_manual(name = "Sample \nPosition", labels = c("i", "p"), values = 1:2) +
  labs(x="Amount of Cell", y="Sample Count", title="Cell Type Repartition in ROIs", color="Cell Type")
dev.off()

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density TMA tumor vs stroma.jpg"))
ggplot(TMA_global, aes(tumor__total_cells, color = "Tumor")) +
  geom_density() +
  geom_density(aes(stroma__total_cells, color = "Stroma")) +
  theme_minimal() +
  scale_linetype_manual(name = "Sample \nPosition", values = 1:2) +
  labs(x="Amount of Cell", y="Sample Count", title="Cell Type Repartition in TMAs", color="Cell Type")
dev.off()


# More about the variation per cases

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/tumor cell presence per case in TMAs.jpg"))
ggplot(variations_TMA, aes(x=suid.x, y=mean_tumor)) +
  geom_segment( aes(x=suid.x, xend=suid.x, y=0, yend=mean_tumor), color="skyblue") +
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
dev.off()

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/tumor cell presence per case in ROIs.jpg"))
ggplot(variations_ROI, aes(x=suid.x, y=mean_tumor)) +
  geom_segment( aes(x=suid.x, xend=suid.x, y=0, yend=mean_tumor), color="skyblue") +
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
dev.off()

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in TMAs.jpg"))
ggplot(variations_TMA, aes(x=ID, y=tumor_variation, colour = tumor_variation>0)) +
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
  labs(x=paste(length(variations_TMA$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
dev.off()

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in TMAs.jpg"))
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
dev.off()

#
jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in ROI.jpg"))
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
dev.off()
jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in ROI.jpg"))
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
dev.off()

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in intratumoral ROI.jpg"))
variations_ROIip %>% 
  filter(intratumoral__i__vs_peripheral__p_.x == "Intratumoral") %>% 
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
dev.off()

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in peripheral ROI.jpg"))
variations_ROIip %>% 
  filter(intratumoral__i__vs_peripheral__p_.x == "Peripheral") %>% 
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
dev.off()

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in intratumoral ROI.jpg"))
variations_ROIip %>% 
  filter(intratumoral__i__vs_peripheral__p_.x == "Intratumoral") %>% 
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
  labs(x=paste(nrow(variations_ROIip %>% filter(intratumoral__i__vs_peripheral__p_.x == "Intratumoral")), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
dev.off()

jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in peripheral ROI.jpg"))
variations_ROIip %>% 
  filter(intratumoral__i__vs_peripheral__p_.x == "Peripheral") %>% 
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
  labs(x=paste(nrow(variations_ROIip %>% filter(intratumoral__i__vs_peripheral__p_.x == "Peripheral")), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Peripheral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
dev.off()

# End



