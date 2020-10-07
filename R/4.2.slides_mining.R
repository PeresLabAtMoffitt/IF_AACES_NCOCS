###################################################################################### III ### Create population table----
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
rm(table)


###################################################################################### III ### Plots data----
# 1. Investigate number of tumoral and stromal cell in TMAs and ROIs----

# TMA
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/hist TMA tumor vs stroma-yaxis.jpg"),
#      width = 350, height = 350)
p1 <- ggplot(markers) +
  geom_histogram(aes(mean_tumor_tma), color = "darkgrey", fill="darkred") +
  theme_minimal() +
  ylim(0,25) +
  labs(x="% Tumor", y="count")
p2 <- ggplot(markers) +
  geom_histogram(aes(mean_stroma_tma), color = "darkgrey", fill="orange") +
  theme_minimal() +
  ylim(0,25) +
  labs(x="% Stroma", y="")
gridExtra::grid.arrange(p1, p2, ncol = 2, top="Cell Type Repartition in patient's TMAs")
# dev.off()


# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/hist ROI tumor vs stroma facet location.jpg"))
p1 <- ggplot(markers) +
  geom_histogram(aes(mean_tumor.i), color = "darkgrey", fill="darkred") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,50)) +
  labs(x="% Tumor", y="count", title = "Intratumoral")
p2 <- ggplot(markers) +
  geom_histogram(aes(mean_tumor.p), color = "darkgrey", fill="darkred") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,50)) +
  coord_cartesian(xlim = c(0,100))+
  labs(x="% Tumor", y="", title = "Peripheral")
p3 <- ggplot(markers) +
  geom_histogram(aes(mean_stroma.i), color = "darkgrey", fill="orange") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,50)) +
  coord_cartesian(xlim = c(0,100))+
  labs(x="Stroma", y="count", title = "Intratumoral")
p4 <- ggplot(markers) +
  geom_histogram(aes(mean_stroma.p), color = "darkgrey", fill="orange") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,50)) +
  coord_cartesian(xlim = c(0,100))+
  labs(x="Stroma", y="", title = "Peripheral")
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2,
                        top = "Cell Type Repartition in patient's ROIs")
# dev.off()


# Density plot----------------------------------------------------------------------------------------------------------------------------
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density ROI tumor.jpg"))
ggplot(ROI_global, aes(tumor_total_cells, linetype = intratumoral_i_vs_peripheral_p_)) +
  geom_density(alpha=.3, color = "darkred") +
  theme_minimal() +
  scale_linetype_manual(name = "Sample \nPosition", values = 1:2) +
  labs(x="Tumor", y="Sample Count", title="Cell Type Repartition in ROIs")
# dev.off()
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density ROI stroma.jpg"))
ggplot(ROI_global, aes(stroma_total_cells, linetype = intratumoral_i_vs_peripheral_p_)) +
  geom_density(alpha=.3, color = "orange") +
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
  scale_color_manual(values = c("orange", "darkred")) +
  scale_linetype_manual(name = "Sample \nPosition", labels = c("Intratumoral", "Peripheral"), values = 1:2) +
  labs(x="Amount of Cell", y="Sample Count", title="Cell Type Repartition in ROIs", color="Cell Type")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/density TMA tumor vs stroma.jpg"))
ggplot(TMA_global, aes(tumor_total_cells, color = "Tumor")) +
  geom_density() +
  geom_density(aes(stroma_total_cells, color = "Stroma")) +
  theme_minimal() +
  scale_color_manual(values = c("orange", "darkred")) +
  scale_linetype_manual(name = "Sample \nPosition", values = 1:2) +
  labs(x="Amount of Cell", y="Sample Count", title="Cell Type Repartition in TMAs", color="Cell Type")
# dev.off()



# 2. Investigate variation per cases. Does TMAs and ROIs give similar results?----
p1 <- TMA_global %>% 
  mutate(Tumor = "Tumor") %>% 
  mutate(Stroma = "Stroma") %>% 
  ggplot()+
  geom_violin(aes(x=Tumor, y=tumor_total_cells), scale = "count", draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_violin(aes(x=Stroma, y=stroma_total_cells), scale = "count", draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(x="Cell type", y=NULL, title="TMAs")
p2 <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
  mutate(Tumor = "Tumor") %>% 
  mutate(Stroma = "Stroma") %>% 
  ggplot()+
  geom_violin(aes(x=Tumor, y=tumor_total_cells), scale = "count", draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_violin(aes(x=Stroma, y=stroma_total_cells), scale = "count", draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(x="Cell type", y=NULL, title="Intratumoral ROIs")
p3 <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  mutate(Tumor = "Tumor") %>% 
  mutate(Stroma = "Stroma") %>% 
  ggplot()+
  geom_violin(aes(x=Tumor, y=tumor_total_cells), scale = "count", draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_violin(aes(x=Stroma, y=stroma_total_cells), scale = "count", draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(x="Cell type", y=NULL, title="Peripheral ROIs")
gridExtra::grid.arrange(p1, p2, p3, ncol = 3,
                        top = "Nbr of Cell Present in", left = "Nbr Cell (mean)")

p1 <- TMA_global %>% 
  mutate(Tumor = "Tumor") %>% 
  mutate(Stroma = "Stroma") %>% 
  ggplot()+
  geom_violin(aes(x=Tumor, y=percent_tumor), scale = "count")+
  geom_boxplot(aes(x=Tumor, y=percent_tumor), width=.1) +
  geom_violin(aes(x=Stroma, y=percent_stroma), scale = "count") +
  geom_boxplot(aes(x=Stroma, y=percent_stroma), width=.1) +
  labs(x="Cell type", y=NULL, title=" \nTMAs")
p2 <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
  mutate(Tumor = "Tumor") %>% 
  mutate(Stroma = "Stroma") %>% 
  ggplot()+
  geom_violin(aes(x=Tumor, y=percent_tumor), scale = "count")+
  geom_boxplot(aes(x=Tumor, y=percent_tumor), width=.1) +
  geom_violin(aes(x=Stroma, y=percent_stroma), scale = "count") +
  geom_boxplot(aes(x=Stroma, y=percent_stroma), width=.1) +
  labs(x="Cell type", y=NULL, title="Intratumoral \nROIs")
p3 <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  mutate(Tumor = "Tumor") %>% 
  mutate(Stroma = "Stroma") %>% 
  ggplot()+
  geom_violin(aes(x=Tumor, y=percent_tumor), scale = "count")+
  geom_boxplot(aes(x=Tumor, y=percent_tumor), width=.1) +
  geom_violin(aes(x=Stroma, y=percent_stroma), scale = "count") +
  geom_boxplot(aes(x=Stroma, y=percent_stroma), width=.1) +
  labs(x="Cell type", y=NULL, title="Peripheral \nROIs")
gridExtra::grid.arrange(p1, p2, p3, ncol = 3,
                        top = "% of Cell Present in", left = "% Cell (mean)")

# Variation----
# Between patient
TMA_global %>% select("suid", "percent_tumor", "percent_stroma") %>% 
  tbl_summary(by= "suid") %>% 
  add_p()
ROI_global %>% filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
  select("suid", "percent_tumor", "percent_stroma") %>% 
  tbl_summary(by= "suid") %>% 
  add_p()
ROI_global %>% filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  select("suid", "percent_tumor", "percent_stroma") %>% 
  tbl_summary(by= "suid", statistic = list(all_continuous() ~ "{median} ({sd})"))%>% 
  add_p()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/tumor cell presence per case in TMAs.jpg"))
markers %>% mutate(suid = seq(nrow(markers))) %>%
ggplot(aes(x=suid, y=mean_tumor_tma)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_tma), color="skyblue") +
  geom_point(color="blue", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlim(1, sum(markers$mean_tumor_tma != "", na.rm = TRUE))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x=paste(sum(markers$mean_tumor_tma != "", na.rm = TRUE), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/tumor cell presence per case in ROIs.jpg"))
markers %>% filter(!is.na(whole_slide)) %>%  mutate(suid = seq(nrow(.))) %>% 
  ggplot(aes(x=suid, y=mean_tumor.i)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor.i), color="skyblue") +
  geom_point(color="blue", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlim(1, sum(markers$whole_slide != "", na.rm = TRUE))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x=paste(sum(markers$whole_slide != "", na.rm = TRUE), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in TMAs.jpg"))
var_markers %>% mutate(suid = seq(nrow(.))) %>%
ggplot(aes(x=suid, y=tumor_variation_tma, colour = tumor_variation_tma>0)) +
  # annotate("text", x = 0.25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  # annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=tumor_variation_tma)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  xlim(1, sum(markers$mean_tumor_tma != "", na.rm = TRUE))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  # scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(sum(markers$mean_tumor_tma != "", na.rm = TRUE), "Patients IDs"), y="% Tumor Cell (mean)", 
       title="% Change in Tumor Cell Present in TMAs per Patient compare to the patient mean",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in TMAs.jpg"))
var_markers %>% mutate(suid = seq(nrow(.))) %>%
  ggplot(aes(x=suid, y=stroma_variation_tma, colour = stroma_variation_tma>0)) +
  # annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  # annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=stroma_variation_tma)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  xlim(1, sum(markers$mean_tumor_tma != "", na.rm = TRUE))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(sum(markers$mean_tumor_tma != "", na.rm = TRUE), "Patients IDs"), y="% Stromal Cell (mean)", 
       title="% Change in Stromal Cell Present in TMAs per Patient compare to the patient mean",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

#
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in ROI.jpg"))
# ggplot(variations_ROI, aes(x=ID, y=tumor_variation, colour = tumor_variation>0)) +
#   annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
#   annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
#   geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation)) +
#   geom_point(size=1, alpha=0.6) +
#   theme_minimal() +
#   coord_flip() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.y = element_blank()
#   ) +
#   scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
#   labs(x=paste(length(variations_ROI$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in ROIs per Patient",
#        subtitle = "Each point represent the mean of up to 6 values")
# dev.off()
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in ROI.jpg"))
# ggplot(variations_ROI, aes(x=ID, y=stroma_variation, colour = stroma_variation>0)) +
#   annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
#   annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
#   geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation)) +
#   geom_point(size=1, alpha=0.6) +
#   theme_minimal() +
#   coord_flip() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.y = element_blank()
#   ) +
#   scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
#   labs(x=paste(length(variations_ROI$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in ROIs per Patient",
#        subtitle = "Each point represent the mean of up to 6 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in intratumoral ROI.jpg"))
var_markers %>% filter(!is.na(tumor_variation.i)) %>%  mutate(suid = seq(nrow(.))) %>% 
  ggplot( aes(x=suid, y=tumor_variation.i, colour = tumor_variation.i>0)) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=tumor_variation.i)) +
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
  labs(x=paste(sum(markers$whole_slide != "", na.rm = TRUE), "Patients IDs"), y="% Tumor Cell (mean)", 
       title="% Change in Tumor Cell Present in Intratumoral ROIs per Patient compare to the patient mean",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation tumor cell per case in peripheral ROI.jpg"))
var_markers %>% filter(!is.na(tumor_variation.p)) %>%  mutate(suid = seq(nrow(.))) %>% 
  ggplot( aes(x=suid, y=tumor_variation.p, colour = tumor_variation.p>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=tumor_variation.p)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  xlim(1, sum(markers$whole_slide != "", na.rm = TRUE))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(sum(markers$whole_slide != "", na.rm = TRUE), "Patients IDs"), y="% Tumor Cell (mean)", 
       title="% Change in Tumor Cell Present in Peripheral ROIs per Patient compare to the patient mean",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in intratumoral ROI.jpg"))
var_markers %>% filter(!is.na(stroma_variation.i)) %>%  mutate(suid = seq(nrow(.))) %>% 
  ggplot( aes(x=suid, y=stroma_variation.i, colour = stroma_variation.i>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=stroma_variation.i)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  xlim(1, sum(markers$whole_slide != "", na.rm = TRUE))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(sum(markers$whole_slide != "", na.rm = TRUE), "Patients IDs"), y="% Stromal Cell (mean)", 
       title="% Change in Stromal Cell Present in Intratumoral ROIs per Patient compare to the patient mean",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation stromal cell per case in peripheral ROI.jpg"))
var_markers %>% filter(!is.na(stroma_variation.p)) %>%  mutate(suid = seq(nrow(.))) %>% 
  ggplot( aes(x=suid, y=stroma_variation.p, colour = stroma_variation.p>0)) +
  annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=stroma_variation.p)) +
  geom_point(size=1, alpha=0.6) +
  theme_minimal() +
  coord_flip() +
  xlim(1, sum(markers$whole_slide != "", na.rm = TRUE))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
  labs(x=paste(sum(markers$whole_slide != "", na.rm = TRUE), "Patients IDs"), y="% Stromal Cell (mean)", 
       title="% Change in Stromal Cell Present in Peripheral ROIs per Patient compare to the patient mean",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()


# Lymphocytes----
p1 <- markers %>%
  gather(key = "markers_cat", value = "value", c("percent_CD3_stroma.i", "percent_CD3_total.i", "percent_CD3_tumor.i")) %>% 
  select(suid, markers_cat, value) %>% 
  ggplot()+
  geom_violin(aes(x=markers_cat, y=value), scale = "count")+
  geom_boxplot(aes(x=markers_cat, y=value), width=.1) +
  labs(x="Cell type", y="percent", title="CD3+")+
  scale_x_discrete(labels=c("Stroma","Total",
                            "Tumor"))
p2 <- markers %>% 
  gather(key = "markers_cat", value = "value", c("percent_CD3_CD8_stroma.i", "percent_CD3_CD8_total.i", "percent_CD3_CD8_tumor.i")) %>% 
  select(suid, markers_cat, value) %>% 
  ggplot()+
  geom_violin(aes(x=markers_cat, y=value), scale = "count")+
  geom_boxplot(aes(x=markers_cat, y=value), width=.1) +
  labs(x="Cell type", y=NULL, title="CD3+CD8+")+
  scale_x_discrete(labels=c("Stroma","Total",
                            "Tumor"))
p3 <- markers %>%
  gather(key = "markers_cat", value = "value", c("percent_CD3_FoxP3_stroma.i", "percent_CD3_FoxP3_total.i", "percent_CD3_FoxP3_tumor.i")) %>% 
  select(suid, markers_cat, value) %>% 
  ggplot()+
  geom_violin(aes(x=markers_cat, y=value), scale = "count")+
  geom_boxplot(aes(x=markers_cat, y=value), width=.1) +
  labs(x="Cell type", y=NULL, title="CD3+FoxP3+")+
  scale_x_discrete(labels=c("Stroma","Total",
                            "Tumor"))
p4 <- markers %>% 
  gather(key = "markers_cat", value = "value", c("percent_CD11b_stroma.i", "percent_CD11b_total.i", "percent_CD11b_tumor.i")) %>% 
  select(suid, markers_cat, value) %>% 
  ggplot()+
  geom_violin(aes(x=markers_cat, y=value), scale = "count")+
  geom_boxplot(aes(x=markers_cat, y=value), width=.1) +
  labs(x="Cell type", y="percent", title="CD11b+")+
  scale_x_discrete(labels=c("Stroma","Total",
                            "Tumor"))
p5 <- markers %>% 
  gather(key = "markers_cat", value = "value", c("percent_CD11b_CD15_stroma.i", "percent_CD11b_CD15_total.i", "percent_CD11b_CD15_tumor.i")) %>% 
  select(suid, markers_cat, value) %>% 
  ggplot()+
  geom_violin(aes(x=markers_cat, y=value), scale = "count")+
  geom_boxplot(aes(x=markers_cat, y=value), width=.1) +
  labs(x="Cell type", y=NULL, title="CD11b+CD15+")+
  scale_x_discrete(labels=c("Stroma","Total",
                            "Tumor"))
gridExtra::grid.arrange(p1,p2,p3,p4,p5,  nrow = 2,
                        top = "Immune markers Repartition in patient's intratumoral ROIs")


# Survival Tumor, Stroma, overall
clin_surv <- markers %>% 
  gather(key = "location", value = "value", c("percent_CD3_tumor.i", "percent_CD3_stroma.i", "percent_CD3_total.i")) %>% 
  select(c("suid", "location", "value", "timelastfu_new", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~location, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by CD3+ lymphocyte location",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "location",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2600, .52),
           conf.int = TRUE,
           # Color
           palette = c("blue", "lightgrey", "yellow"),
           linetype = c("solid", "dashed", "dotted")
)

clin_surv <- markers %>% 
  gather(key = "location", value = "value", c("percent_CD3_CD8_tumor.i", "percent_CD3_CD8_stroma.i", "percent_CD3_CD8_total.i")) %>% 
  select(c("suid", "location", "value", "timelastfu_new", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~location, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by CD3+CD8+ lymphocyte location",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "location",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2600, .52),
           conf.int = TRUE,
           # Color
           palette = c("blue", "lightgrey", "yellow"),
           linetype = c("solid", "dashed", "dotted"))

clin_surv <- markers %>% 
  gather(key = "location", value = "value", c("sqrt_CD3_FoxP3_tumor.i", "sqrt_CD3_FoxP3_stroma.i", "sqrt_CD3_FoxP3_total.i")) %>% 
  select(c("suid", "location", "value", "timelastfu_new", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~location, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by CD3+FoxP3+ lymphocyte location",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "location",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2600, .52),
           conf.int = TRUE,
           # Color
           palette = c("blue", "lightgrey", "yellow"),
           linetype = c("solid", "dashed", "dotted"))

clin_surv <- markers %>% 
  gather(key = "location", value = "value", c("sqrt_CD11b_tumor.i", "sqrt_CD11b_stroma.i", "sqrt_CD11b_total.i")) %>% 
  select(c("suid", "location", "value", "timelastfu_new", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~location, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by CD11b+ lymphocyte location",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "location",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2600, .52),
           conf.int = TRUE,
           # Color
           palette = c("blue", "lightgrey", "yellow"),
           linetype = c("solid", "dashed", "dotted"))

clin_surv <- markers %>% 
  gather(key = "location", value = "value", c("sqrt_CD11b_CD15_tumor.i", "sqrt_CD11b_CD15_stroma.i", "sqrt_CD11b_CD15_total.i")) %>% 
  select(c("suid", "location", "value", "timelastfu_new", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~location, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by CD11b+CD15 lymphocyte location",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "location",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2600, .52),
           conf.int = TRUE,
           # Color
           palette = c("blue", "lightgrey", "yellow"),
           linetype = c("solid", "dashed", "dotted"))








###################################################################################### IV ### Limiting 28 patients we have ROIs, TMAs----

venn.diagram(
  x = list(ROI_global$suid, TMA_global$suid),
  category.names = c("ROI" , "TMA"),
  filename = 'Patient who had Germline sequenced.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Title
  main = "Common patients in ROIs and TMA",
  main.pos = c(0.5, 1.05), main.fontface = "plain",
  main.fontfamily = "serif", main.col = "black",
  main.cex = .8, 
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("darkgrey", "#FEA873FF"), 
  margin = 0.03,
  
  # Numbers
  cex = .5,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(-50, 120), # Position legend arounf circle
  cat.dist = c(0.035, 0.045), # Distance legend from circle
  #ext.percent = 2,
  rotation.degree = -90,
  # cex = 3.5,
  cat.cex = .7
  # cat.fontface = "bold",
  # cat.default.pos = "outer",
  # cat.fontfamily = "sans",
)



# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_1.jpg"))
ggplot(var_markers28, aes(x=suid, y=tumor_variation_tma)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=tumor_variation_tma), color="skyblue") +
  geom_point(color="blue", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x=paste(length(var_markers28$suid), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# peg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_2.jpg"))
# ggplot(variation, aes(x=suid, y=mean_tumor_roi)) +
#   geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi), color="skyblue") +
#   geom_point(color="blue", size=1, alpha=0.6) +
#   theme_light() +
#   coord_flip() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.y = element_blank()
#   ) +
#   labs(x=paste(length(variation$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in ROIs per Patient",
#        subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_1.jpg"))
ggplot(var_markers28, aes(x=suid, y=tumor_variation_tma, colour = tumor_variation_tma>0)) +
  # annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  # annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=tumor_variation_tma)) +
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
  labs(x=paste(length(var_markers28$suid), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_3.jpg"))
ggplot(var_markers28, aes(x=suid, y=stroma_variation_tma, colour = stroma_variation_tma>0)) +
  # annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  # annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=stroma_variation_tma)) +
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
  labs(x=paste(length(var_markers28$suid), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_4.jpg"))
# ggplot(variation, aes(x=ID, y=tumor_variation_roi, colour = tumor_variation_roi>0)) +
#   annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
#   annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
#   geom_segment(aes(x=ID, xend=ID, y=0, yend=tumor_variation_roi)) +
#   geom_point(size=1, alpha=0.6) +
#   theme_minimal() +
#   coord_flip() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.y = element_blank()
#   ) +
#   scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
#   labs(x=paste(length(variation$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in ROIs per Patient",
#        subtitle = "Each point represent the mean of up to 6 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_5.jpg"))
# ggplot(variation, aes(x=ID, y=stroma_variation_roi, colour = stroma_variation_roi>0)) +
#   annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
#   annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
#   geom_segment(aes(x=ID, xend=ID, y=0, yend=stroma_variation_roi)) +
#   geom_point(size=1, alpha=0.6) +
#   theme_minimal() +
#   coord_flip() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.y = element_blank()
#   ) +
#   scale_color_manual(values = c("#00204DFF", "#8707A6FF")) +
#   labs(x=paste(length(variation$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in ROIs per Patient",
#        subtitle = "Each point represent the mean of up to 6 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_6.jpg"))
ggplot(var_markers28, aes(x=suid, y=tumor_variation.i, colour = tumor_variation.i>0)) +
  # annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  # annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=tumor_variation.i)) +
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
  labs(x=paste(length(var_markers28$suid), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_7.jpg"))
ggplot(var_markers28, aes(x=suid, y=stroma_variation.i, colour = stroma_variation.i>0)) +
  # annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  # annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=stroma_variation.i)) +
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
  labs(x=paste(length(var_markers28$suid), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_8.jpg"))
ggplot(var_markers28, aes(x=suid, y=tumor_variation.p, colour = tumor_variation.p>0)) +
  # annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  # annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=tumor_variation.p)) +
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
  labs(x=paste(length(var_markers28$suid), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Peripheral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_9.jpg"))
ggplot(var_markers28, aes(x=suid, y=stroma_variation.p, colour = stroma_variation.p>0)) +
  # annotate("text", x = .25, y = "mean", label = "High", color = "#8707A6FF", size = 3, hjust = 0, vjust = 1) +
  # annotate("text", x = .25, y = "", label = "Low", color = "#00204DFF", size = 3, hjust = 2, vjust = 1) +
  geom_segment(aes(x=suid, xend=suid, y=0, yend=stroma_variation.p)) +
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
  labs(x=paste(length(var_markers28$suid), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Peripheral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()


#
# Back to back ----
# geom_segment(aes(x = 2, y = 15, xend = 2, yend = 25),
#                arrow = arrow(length = unit(0.5, "cm")))

# ggplot(variation, aes(x=suid)) +
#   geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor, color="TMA"))+
#   geom_point(aes(y=mean_tumor), color="blue", size=1, alpha=0.6) +
#   geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi, color="ROI"),
#                 position = position_nudge(x = 0.5, y = 0))+
#   geom_point(aes(y=mean_tumor_roi) , color="red", size=1, alpha=0.6,
#              position = position_nudge(x = 0.5, y = 0)) +
#   theme_minimal() + 
#   coord_flip()+
#   scale_color_discrete(name="Samples from")+
#   scale_color_manual(values = c("#FF9999", "skyblue")) +
#   labs(x=paste(length(variation$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs vs ROIs per Patient",
#        subtitle = "Each point represent the mean of up to 3 values for TMA, 6 values fro ROI")


# variation %>% mutate(mean_tumor_roi = -1*mean_tumor_roi) %>% 
#   ggplot(aes(x=suid)) +
#   geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_tma, color="TMA")) +
#   geom_point(aes(y=mean_tumor_tma), color="blue", size=1, alpha=0.6) +
#   # geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi, color="ROI")) +
#   # geom_point(aes(y=mean_tumor_roi) , color="red", size=1, alpha=0.6) +
#   scale_y_continuous(breaks = c(-100, -50, 0, 50, 100),
#                      labels = abs(c(-100, -50, 0, 50, 100))) +
#   geom_hline(yintercept = 0) +
#   theme_minimal() + 
#   coord_flip()+
#   scale_color_discrete(name="Samples from")+
#   scale_color_manual(values = c("#FF9999", "skyblue")) +
#   labs(x=paste(length(variation$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs vs ROIs per Patient",
#        subtitle = "Each point represent the mean of up to 3 values for TMA, 6 values fro ROI")


# TMA vs ROI vs I vs P----
par(mar=c(5, 5, 20, 3.1)) # bottom left top right
ggplot(markers_28, aes(x=suid)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_tma, color="TMA")) +
  geom_point(aes(y=mean_tumor_tma), color="blue", size=1, alpha=0.6) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor.i, color="ROIi"), 
                position = position_nudge(x = 0.25, y = 0)) +
  geom_point(aes(y=mean_tumor.i) , color="red", size=1, alpha=0.6, 
             position = position_nudge(x = 0.25, y = 0)) + 
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor.p, color="ROIp"), 
                position = position_nudge(x = 0.5, y = 0)) +
  geom_point(aes(y=mean_tumor.p) , color="limegreen", size=1, alpha=0.6, 
             position = position_nudge(x = 0.5, y = 0)) + 
  theme_minimal() + 
  coord_flip()+
  scale_x_discrete(expand=c(0.05, 0)) +
  scale_color_manual(name="Samples \nfrom", values = c("skyblue", "#FF9999", "palegreen", "plum1"), breaks=c("TMA","ROIi", "ROIp","ROI")) +
  labs(x=paste(length(markers_28$suid), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs vs ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values for TMA, ROIi, Roip, 6 values fro ROI")

ggplot(markers_28, aes(x=suid)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_stroma_tma, color="TMA")) +
  geom_point(aes(y=mean_stroma_tma), color="blue", size=1, alpha=0.6) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_stroma.i, color="ROIi"), 
                position = position_nudge(x = 0.25, y = 0)) +
  geom_point(aes(y=mean_stroma.i) , color="red", size=1, alpha=0.6, 
             position = position_nudge(x = 0.25, y = 0)) + 
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_stroma.p, color="ROIp"), 
                position = position_nudge(x = 0.5, y = 0)) +
  geom_point(aes(y=mean_stroma.p) , color="limegreen", size=1, alpha=0.6, 
             position = position_nudge(x = 0.5, y = 0)) + 
  theme_minimal() + 
  coord_flip()+
  scale_x_discrete(expand=c(0.05, 0)) +
  scale_color_manual(name="Samples \nfrom", values = c("skyblue", "#FF9999", "palegreen", "plum1"), breaks=c("TMA","ROIi", "ROIp","ROI")) +
  labs(x=paste(length(markers_28$suid), "Patients IDs"), y="% Stroma Cell (mean)", title="% Stroma Cell Present in TMAs vs ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values for TMA, ROIi, Roip, 6 values fro ROI")

markers_28 %>% drop_na(mean_tumor_tma, mean_tumor.i) %>% 
  gather(key = "markers", value = "value", c("mean_tumor_tma", "mean_tumor.i")) %>% 
  select("markers", "value") %>% 
  ggplot(aes(x=markers, y=value)) +
  geom_boxplot()+
  stat_compare_means()

markers_28 %>% drop_na(mean_tumor_tma, mean_tumor.i) %>% 
  gather(key = "markers", value = "value", c("mean_tumor_tma", "mean_tumor.i")) %>% 
  select("markers", "value") %>% 
  ggpaired(x = "markers", y = "value",
         color = "markers", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)

markers_28 %>% drop_na(mean_tumor_tma, mean_tumor.i, mean_tumor.p) %>% 
  gather(key = "markers", value = "value", c("mean_tumor_tma", "mean_tumor.i", "mean_tumor.p")) %>% 
  select("markers", "value") %>% 
  ggplot(aes(x=markers, y=value, color=markers)) +
  geom_boxplot()+
  theme_minimal()+
  stat_compare_means(comparisons = 
                       list(c("mean_tumor_tma", "mean_tumor.i"), 
                            c("mean_tumor_tma", "mean_tumor.p")))

markers_28 %>% select("suid", "mean_tumor_tma", "mean_tumor.i", "mean_tumor.p") %>% 
  tbl_summary(by = suid, statistic = list(all_continuous() ~ "{median} ({sd})")) %>% 
  add_p()

markers_28 %>% gather(key = "markers", value = "value", c("mean_tumor_tma", "mean_tumor.i", "mean_tumor.p")) %>% 
  select("markers", "value") %>% 
  tbl_summary(by = markers, statistic = list(all_continuous() ~ "{median} ({sd})")) %>% 
  add_p()

markers_28 %>% gather(key = "markers", value = "value", c("mean_tumor_tma", "mean_tumor.i")) %>% 
  select("suid", "markers", "value") %>% 
  tbl_summary(by = value, statistic = list(all_continuous() ~ "{median} ({sd})")) %>% 
  add_p()
markers_28 %>% gather(key = "markers", value = "value", c("percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i")) %>% 
  select("markers", "value") %>% 
  tbl_summary(by = markers, statistic = list(all_continuous() ~ "{median} ({sd})")) %>% 
  add_p()
markers_28 %>% gather(key = "markers", value = "value", c("percent_CD3_CD8_total_tma", "percent_CD3_CD8_total.i")) %>% 
  select("markers", "value") %>% 
  tbl_summary(by = markers, statistic = list(all_continuous() ~ "{median} ({sd})")) %>% 
  add_p()

# TMAs silmilar to intratumoral ROIs?----
