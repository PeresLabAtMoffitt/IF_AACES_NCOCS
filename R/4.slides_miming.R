###################################################################################### I ### Intraclass Correlation Coefficient----
# The ICC is a measure of the reliability of measurements or ratings, for the purpose of assessing inter-rater reliability
# How to choose the correct ICC forms
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2628443/
# https://www.datanovia.com/en/lessons/intraclass-correlation-coefficient-in-r/#:~:text=Generally%20speaking%2C%20the%20ICC%20determines,all%20ratings%20and%20all%20individuals.
ICC_ROIi <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>%
  select(c("suid", "tumor_total_cells"))
ICC_ROIi <- dcast(setDT(ICC_ROIi), suid ~ rowid(suid), 
                 value.var = "tumor_total_cells") %>% 
  select(c(2:4))

ICC_ROIp <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>%
  select(c("suid", "tumor_total_cells"))
ICC_ROIp <- dcast(setDT(ICC_ROIp), suid ~ rowid(suid), 
                  value.var = "tumor_total_cells") %>% 
  select(c(2:4))

ICC_TMA <- TMA_global[, c("suid", "tumor_total_cells")] 
ICC_TMA <- dcast(setDT(ICC_TMA), suid ~ rowid(suid), 
                 value.var = "tumor_total_cells") %>% 
  select(c(2:4))
# Check density
# ICC_d_ROIi <- ROI_global %>% 
#   filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>%
#   select(c("suid", "CD3_tumor_mm2", "CD3_stroma_mm2"))
# ICC_d_ROIi <- dcast(setDT(ICC_d_ROIi), suid ~ rowid(suid), 
#                   value.var = c("CD3_tumor_mm2", "CD3_stroma_mm2")) %>% 
#   select(c(2:4, 14:16))
# vs %
ICC_p_ROIi <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>%
  select(c("suid", "tumor_percent_cd3_opal_650_positive_cells", 
           "stroma_percent_cd3_opal_650_positive_cells"))
ICC_p_ROIi <- dcast(setDT(ICC_p_ROIi), suid ~ rowid(suid), 
                    value.var = c("tumor_percent_cd3_opal_650_positive_cells", 
                    "stroma_percent_cd3_opal_650_positive_cells")) %>% 
  select(c(2:4, 14:16))
# If test for intra-rater (because for each patient the 3 TMA/ROI was done by the same person)
# If test for inter-rater (because not the same rater between patients)
# Two-way mixed effect, fixed raters are defined. Each subject is measured by the k raters.
library(psych) # Koo and Li (2016)
ICC(ICC_TMA)  # between 0.75 and 0.90: good
ICC(ICC_ROIi)
ICC(ICC_ROIp) #between 0.50 and 0.75: moderate
# ICC(ICC_d_ROIi)
# ICC(ICC_p_ROIi)
library(irr) # Does not count patient with NA
icc(
  ICC_TMA, model = "twoway", 
  type = "consistency", unit = "average"
)
icc(
  ICC_ROIi, model = "twoway", 
  type = "consistency", unit = "average"# Reliability applied where measures of k raters will be averaged for each subject.
)
icc(
  ICC_ROIp, model = "twoway", 
  type = "consistency", unit = "average" # Consistency: for repeated measurements by the same rater,
)

# ICC immune markers- look at ppt for more
ICC_ROIi_CD11bCD15_ov <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>%
  select(c("suid", "cd11bplus_cd15plus_cells"))
ICC_ROIi_CD11bCD15_ov <- dcast(setDT(ICC_ROIi_CD11bCD15_ov), suid ~ rowid(suid), 
                  value.var = "cd11bplus_cd15plus_cells") %>% 
  select(c(2:4))

ICC(ICC_ROIi_CD11bCD15_ov)
icc(
  ICC_ROIi_CD11bCD15_ov, model = "twoway", 
  type = "consistency", unit = "average"
)


# Cleaning
rm(ICC_TMA, ICC_ROIi, ICC_ROIp, ICC_d_ROIi, ICC_p_ROIi)


###################################################################################### II ### TMA vs ROI----

# Kappa stat
# https://www.datanovia.com/en/lessons/cohens-kappa-in-r-for-two-categorical-variables/#:~:text=Cohen's%20Kappa%20in%20R%3A%20For%20Two%20Categorical%20Variables,-20%20mins&text=Cohen's%20kappa%20(Jacob%20Cohen%201960,methods%20rating%20on%20categorical%20scales.&text=The%20Cohen's%20kappa%20is%20a,that%20removes%20this%20chance%20agreement.

# To get categorical var, create tertile
df.tertile <- data.frame(
  suid = markers_28$suid,
  tumor_tert_tma = ntile(markers_28$mean_tumor_tma, 4),
  stroma_tert_tma = ntile(markers_28$mean_stroma_tma, 4),
  tert_CD3_tumor_tma = ntile(markers_28$percent_CD3_tumor_tma, 4),
  tert_CD8_tumor_tma = ntile(markers_28$percent_CD8_tumor_tma, 4),
  tert_CD3_CD8_tumor_tma = ntile(markers_28$percent_CD3_CD8_tumor_tma, 4),
  tert_FoxP3_tumor_tma = ntile(markers_28$percent_FoxP3_tumor_tma, 4),
  tert_CD3_FoxP3_tumor_tma = ntile(markers_28$percent_CD3_FoxP3_tumor_tma, 4),
  tert_CD11b_tumor_tma = ntile(markers_28$percent_CD11b_tumor_tma, 4),
  tert_CD15_tumor_tma = ntile(markers_28$percent_CD15_tumor_tma, 4),
  tert_CD11b_CD15_tumor_tma = ntile(markers_28$percent_CD11b_CD15_tumor_tma, 4),
  tert_CD3_stroma_tma = ntile(markers_28$percent_CD3_stroma_tma, 4),
  tert_CD8_stroma_tma = ntile(markers_28$percent_CD8_stroma_tma, 4),
  tert_CD3_CD8_stroma_tma = ntile(markers_28$percent_CD3_CD8_stroma_tma, 4),
  tert_FoxP3_stroma_tma = ntile(markers_28$percent_FoxP3_stroma_tma, 4),
  tert_CD3_FoxP3_stroma_tma = ntile(markers_28$percent_CD3_FoxP3_stroma_tma, 4),
  tert_CD11b_stroma_tma = ntile(markers_28$percent_CD11b_stroma_tma, 4),
  tert_CD15_stroma_tma = ntile(markers_28$percent_CD15_stroma_tma, 4),
  tert_CD11b_CD15_stroma_tma = ntile(markers_28$percent_CD11b_CD15_stroma_tma, 4),
  tert_CD3_total_tma = ntile(markers_28$percent_CD3_total_tma, 4),
  tert_CD8_total_tma = ntile(markers_28$percent_CD8_total_tma, 4),
  tert_CD3_CD8_total_tma = ntile(markers_28$percent_CD3_CD8_total_tma, 4),
  tert_FoxP3_total_tma = ntile(markers_28$percent_FoxP3_total_tma, 4),
  
  tumor_tert.i = ntile(markers_28$mean_tumor.i, 4),
  stroma_tert.i = ntile(markers_28$mean_stroma.i, 4),
  tert_CD3_tumor.i = ntile(markers_28$percent_CD3_tumor.i, 4),
  tert_CD8_tumor.i = ntile(markers_28$percent_CD8_tumor.i, 4),
  tert_CD3_CD8_tumor.i = ntile(markers_28$percent_CD3_CD8_tumor.i, 4),
  tert_FoxP3_tumor.i = ntile(markers_28$percent_FoxP3_tumor.i, 4),
  tert_CD3_FoxP3_tumor.i = ntile(markers_28$percent_CD3_FoxP3_tumor.i, 4),
  tert_CD11b_tumor.i = ntile(markers_28$percent_CD11b_tumor.i, 4),
  tert_CD15_tumor.i = ntile(markers_28$percent_CD15_tumor.i, 4),
  tert_CD11b_CD15_tumor.i = ntile(markers_28$percent_CD11b_CD15_tumor.i, 4),
  tert_CD3_stroma.i = ntile(markers_28$percent_CD3_stroma.i, 4),
  tert_CD8_stroma.i = ntile(markers_28$percent_CD8_stroma.i, 4),
  tert_CD3_CD8_stroma.i = ntile(markers_28$percent_CD3_CD8_stroma.i, 4),
  tert_FoxP3_stroma.i = ntile(markers_28$percent_FoxP3_stroma.i, 4),
  tert_CD3_FoxP3_stroma.i = ntile(markers_28$percent_CD3_FoxP3_stroma.i, 4),
  tert_CD11b_stroma.i = ntile(markers_28$percent_CD11b_stroma.i, 4),
  tert_CD15_stroma.i = ntile(markers_28$percent_CD15_stroma.i, 4),
  tert_CD11b_CD15_stroma.i = ntile(markers_28$percent_CD11b_CD15_stroma.i, 4),
  tert_CD3_total.i = ntile(markers_28$percent_CD3_total.i, 4),
  tert_CD8_total.i = ntile(markers_28$percent_CD8_total.i, 4),
  tert_CD3_CD8_total.i = ntile(markers_28$percent_CD3_CD8_total.i, 4),
  tert_FoxP3_total.i = ntile(markers_28$percent_FoxP3_total.i, 4))

xtab <- table(df.tertile$tumor_tert_tma, df.tertile$tumor_tert.i)
diagonal.counts <- diag(xtab)
N <- sum(xtab)
row.marginal.props <- rowSums(xtab)/N
col.marginal.props <- colSums(xtab)/N
Po <- sum(diagonal.counts)/N
Pe <- sum(row.marginal.props*col.marginal.props)
kappa <- (Po - Pe)/(1 - Pe)
kappa # 0.2595978

library(irr)
# If the data is ordinal, then it may be appropriate to use a weighted Kappa. 
# For example, if the possible values are low, medium, and high, then if a case were rated 
# medium and high by the two coders, they would be in better agreement than if the ratings were low and high.
#   VALUE OF K	    STRENGTH OF AGREEMENT
#   < 0	            Poor
#   0.01 - 0.20	    Slight
#   0.21-0.40	      Fair
#   0.41-0.60	      Moderate
#   0.61-0.80	      Substantial
#   0.81 - 1.00	    Almost perfect
kappa2(df.tertile[,c("tumor_tert_tma","tumor_tert.i")], "unweighted") # Higher with unweighted, probably bc 1 can be 4

kappa2(df.tertile[,c("tumor_tert_tma","tumor_tert.i")], "unweighted")$value

df.tertile_kappa <- data.frame(
  tumor_tert = kappa2(df.tertile[,c("tumor_tert_tma","tumor_tert.i")], "unweighted")$value,
  stroma_tert = kappa2(df.tertile[,c("stroma_tert_tma","stroma_tert.i")], "unweighted")$value,
  tert_CD3_tumor = kappa2(df.tertile[,c("tert_CD3_tumor_tma","tert_CD3_tumor.i")], "unweighted")$value,
  tert_CD8_tumor = kappa2(df.tertile[,c("tert_CD8_tumor_tma","tert_CD8_tumor.i")], "unweighted")$value,
  tert_CD3_CD8_tumor = kappa2(df.tertile[,c("tert_CD3_CD8_tumor_tma","tert_CD3_CD8_tumor.i")], "unweighted")$value,
  tert_FoxP3_tumor = kappa2(df.tertile[,c("tert_FoxP3_tumor_tma","tert_FoxP3_tumor.i")], "unweighted")$value,
  tert_CD3_FoxP3_tumor = kappa2(df.tertile[,c("tert_CD3_FoxP3_tumor_tma","tert_CD3_FoxP3_tumor.i")], "unweighted")$value,
  tert_CD11b_tumor = kappa2(df.tertile[,c("tert_CD11b_tumor_tma","tert_CD11b_tumor.i")], "unweighted")$value,
  tert_CD15_tumor = kappa2(df.tertile[,c("tert_CD15_tumor_tma","tert_CD15_tumor.i")], "unweighted")$value,
  tert_CD11b_CD15_tumor = kappa2(df.tertile[,c("tert_CD11b_CD15_tumor_tma","tert_CD11b_CD15_tumor.i")], "unweighted")$value,
  tert_CD3_stroma = kappa2(df.tertile[,c("tert_CD3_stroma_tma","tert_CD3_stroma.i")], "unweighted")$value,
  tert_CD8_stroma = kappa2(df.tertile[,c("tert_CD8_stroma_tma","tert_CD8_stroma.i")], "unweighted")$value,
  tert_CD3_CD8_stroma = kappa2(df.tertile[,c("tert_CD3_CD8_stroma_tma","tert_CD3_CD8_stroma.i")], "unweighted")$value,
  tert_FoxP3_stroma = kappa2(df.tertile[,c("tert_FoxP3_stroma_tma","tert_FoxP3_stroma.i")], "unweighted")$value,
  tert_CD3_FoxP3_stroma = kappa2(df.tertile[,c("tert_CD3_FoxP3_stroma_tma","tert_CD3_FoxP3_stroma.i")], "unweighted")$value,
  tert_CD11b_stroma = kappa2(df.tertile[,c("tert_CD11b_stroma_tma","tert_CD11b_stroma.i")], "unweighted")$value,
  tert_CD15_stroma = kappa2(df.tertile[,c("tert_CD15_stroma_tma","tert_CD15_stroma.i")], "unweighted")$value,
  tert_CD11b_CD15_stroma = kappa2(df.tertile[,c("tert_CD11b_CD15_stroma_tma","tert_CD11b_CD15_stroma.i")], "unweighted")$value,
  tert_CD3_total = kappa2(df.tertile[,c("tert_CD3_total_tma","tert_CD3_total.i")], "unweighted")$value,
  tert_CD8_total = kappa2(df.tertile[,c("tert_CD8_total_tma","tert_CD8_total.i")], "unweighted")$value,
  tert_CD3_CD8_total = kappa2(df.tertile[,c("tert_CD3_CD8_total_tma","tert_CD3_CD8_total.i")], "unweighted")$value,
  tert_FoxP3_total = kappa2(df.tertile[,c("tert_FoxP3_total_tma","tert_FoxP3_total.i")], "unweighted")$value
) %>% 
  t(.) %>% as.data.frame(.)



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
p1 <- ggplot(markers_TMA) +
  geom_histogram(aes(mean_tumor_tma), color = "darkgrey", fill="darkred") +
  theme_minimal() +
  ylim(0,25) +
  labs(x="% Tumor", y="count")
p2 <- ggplot(markers_TMA) +
  geom_histogram(aes(mean_stroma_tma), color = "darkgrey", fill="orange") +
  theme_minimal() +
  ylim(0,25) +
  labs(x="% Stroma", y="")
gridExtra::grid.arrange(p1, p2, ncol = 2, top="Cell Type Repartition in patient's TMAs")
# dev.off()


# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/hist ROI tumor vs stroma facet location.jpg"))
p1 <- ggplot(markers_ROI) +
  geom_histogram(aes(mean_tumor.i), color = "darkgrey", fill="darkred") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,50)) +
  labs(x="% Tumor", y="count", title = "Intratumoral")
p2 <- ggplot(markers_ROI) +
  geom_histogram(aes(mean_tumor.p), color = "darkgrey", fill="darkred") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,50)) +
  coord_cartesian(xlim = c(0,100))+
  labs(x="% Tumor", y="", title = "Peripheral")
p3 <- ggplot(markers_ROI) +
  geom_histogram(aes(mean_stroma.i), color = "darkgrey", fill="orange") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,50)) +
  coord_cartesian(xlim = c(0,100))+
  labs(x="Stroma", y="count", title = "Intratumoral")
p4 <- ggplot(markers_ROI) +
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
  labs(x="Cell type", y=NULL, title="TMAs")
p2 <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
  mutate(Tumor = "Tumor") %>% 
  mutate(Stroma = "Stroma") %>% 
  ggplot()+
  geom_violin(aes(x=Tumor, y=percent_tumor), scale = "count")+
  geom_boxplot(aes(x=Tumor, y=percent_tumor), width=.1) +
  geom_violin(aes(x=Stroma, y=percent_stroma), scale = "count") +
  geom_boxplot(aes(x=Stroma, y=percent_stroma), width=.1) +
  labs(x="Cell type", y=NULL, title="Intratumoral ROIs")
p3 <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  mutate(Tumor = "Tumor") %>% 
  mutate(Stroma = "Stroma") %>% 
  ggplot()+
  geom_violin(aes(x=Tumor, y=percent_tumor), scale = "count")+
  geom_boxplot(aes(x=Tumor, y=percent_tumor), width=.1) +
  geom_violin(aes(x=Stroma, y=percent_stroma), scale = "count") +
  geom_boxplot(aes(x=Stroma, y=percent_stroma), width=.1) +
  labs(x="Cell type", y=NULL, title="Peripheral ROIs")
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
  labs(x=paste(length(variations_TMA$suid), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in TMAs per Patient",
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
  labs(x=paste(length(variations_ROI$suid), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in ROIs per Patient",
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
# variations_ROIip %>% 
#   filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
# ggplot( aes(x=ID, y=tumor_variation, colour = tumor_variation>0)) +
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
#   labs(x=paste(length(variations_ROI$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Intratumoral ROIs per Patient",
#        subtitle = "Each point represent the mean of up to 3 values")
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
  labs(x=paste(length(variations_ROIip$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Peripheral ROIs per Patient",
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


# Lymphocytes----
p1 <- markers %>%
  gather(key = "markers_cat", value = "value", c(104, 112, 120)) %>% 
  select(suid, markers_cat, value) %>% 
  ggplot()+
  geom_violin(aes(x=markers_cat, y=value), scale = "count")+
  geom_boxplot(aes(x=markers_cat, y=value), width=.1) +
 labs(x="Cell type", y="percent", title="CD3+")+
  scale_x_discrete(labels=c("Stroma","Total",
                            "Tumor"))
p2 <- markers %>% 
  gather(key = "markers_cat", value = "value", c(106, 114, 122)) %>% 
  select(suid, markers_cat, value) %>% 
  ggplot()+
  geom_violin(aes(x=markers_cat, y=value), scale = "count")+
  geom_boxplot(aes(x=markers_cat, y=value), width=.1) +
  labs(x="Cell type", y=NULL, title="CD3+CD8+")+
  scale_x_discrete(labels=c("Stroma","Total",
                            "Tumor"))
p3 <- markers %>%
  gather(key = "markers_cat", value = "value", c(108, 116, 124)) %>% 
  select(suid, markers_cat, value) %>% 
  ggplot()+
  geom_violin(aes(x=markers_cat, y=value), scale = "count")+
  geom_boxplot(aes(x=markers_cat, y=value), width=.1) +
  labs(x="Cell type", y=NULL, title="CD3+FoxP3+")+
  scale_x_discrete(labels=c("Stroma","Total",
                            "Tumor"))
p4 <- markers %>% 
  gather(key = "markers_cat", value = "value", c(109, 117, 125)) %>% 
  select(suid, markers_cat, value) %>% 
  ggplot()+
  geom_violin(aes(x=markers_cat, y=value), scale = "count")+
  geom_boxplot(aes(x=markers_cat, y=value), width=.1) +
  labs(x="Cell type", y="percent", title="CD11b+")+
  scale_x_discrete(labels=c("Stroma","Total",
                            "Tumor"))
p5 <- markers %>% 
  gather(key = "markers_cat", value = "value", c(111, 119, 127)) %>% 
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
  gather(key = "location", value = "value", c("sqrt_CD3_tumor.i", "sqrt_CD3_stroma.i", "sqrt_CD3_total.i")) %>% 
  select(c("suid", "location", "value", "timelastfu", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~location, data = clin_surv) 
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
  gather(key = "location", value = "value", c("sqrt_CD3_CD8_tumor.i", "sqrt_CD3_CD8_stroma.i", "sqrt_CD3_CD8_total.i")) %>% 
  select(c("suid", "location", "value", "timelastfu", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~location, data = clin_surv) 
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
  select(c("suid", "location", "value", "timelastfu", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~location, data = clin_surv) 
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
  select(c("suid", "location", "value", "timelastfu", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~location, data = clin_surv) 
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
  select(c("suid", "location", "value", "timelastfu", "surv_vital"))
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~location, data = clin_surv) 
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
  main.cex = 1, 

  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("darkgrey", "#FEA873FF"), 
  margin = 0.05,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(-20, 160), # Position legend arounf circle
  cat.dist = c(0.055, 0.055), # Distance legend from circle
  #ext.percent = 2,
  rotation.degree = -90
  # cex = 3.5,
  # cat.cex = 3,
  # cat.fontface = "bold",
  # cat.default.pos = "outer",
  # cat.fontfamily = "sans",
)



# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_1.jpg"))
ggplot(variation, aes(x=suid, y=mean_tumor)) +
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
  labs(x=paste(length(variation$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% of Tumor Cell Present in TMAs per Patient",
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
ggplot(variation, aes(x=ID, y=tumor_variation, colour = tumor_variation>0)) +
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
  labs(x=paste(length(variation$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_3.jpg"))
ggplot(variation, aes(x=ID, y=stroma_variation, colour = stroma_variation>0)) +
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
  labs(x=paste(length(variation$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in TMAs per Patient",
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
ggplot(variation, aes(x=ID, y=tumor_variation_roi_i, colour = tumor_variation_roi_i>0)) +
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
  labs(x=paste(length(variation$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_7.jpg"))
ggplot(variation, aes(x=ID, y=stroma_variation_roi_i, colour = stroma_variation_roi_i>0)) +
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
  labs(x=paste(length(variation$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Intratumoral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_8.jpg"))
ggplot(variation, aes(x=ID, y=tumor_variation_roi_p, colour = tumor_variation_roi_p>0)) +
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
  labs(x=paste(length(variation$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in Peripheral ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values")
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/variation 28 patients_9.jpg"))
ggplot(variation, aes(x=ID, y=stroma_variation_roi_p, colour = stroma_variation_roi_p>0)) +
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
  labs(x=paste(length(variation$ID), "Patients IDs"), y="% Stromal Cell (mean)", title="% Stromal Cell Present in Peripheral ROIs per Patient",
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
ggplot(variation, aes(x=suid)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor, color="TMA")) +
  geom_point(aes(y=mean_tumor), color="blue", size=1, alpha=0.6) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi_i, color="ROIi"), 
                position = position_nudge(x = 0.25, y = 0)) +
  geom_point(aes(y=mean_tumor_roi_i) , color="red", size=1, alpha=0.6, 
             position = position_nudge(x = 0.25, y = 0)) + 
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi_p, color="ROIp"), 
                position = position_nudge(x = 0.5, y = 0)) +
  geom_point(aes(y=mean_tumor_roi_p) , color="limegreen", size=1, alpha=0.6, 
             position = position_nudge(x = 0.5, y = 0)) + 
  # geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_tumor_roi, color="ROI"), 
  #               position = position_nudge(x = 0.75, y = 0)) +
  # geom_point(aes(y=mean_tumor_roi) , color="purple", size=1, alpha=0.6, 
  #            position = position_nudge(x = 0.75, y = 0)) + 
  theme_minimal() + 
  coord_flip()+
  scale_x_discrete(expand=c(0.05, 0)) +
  scale_color_manual(name="Samples \nfrom", values = c("skyblue", "#FF9999", "palegreen", "plum1"), breaks=c("TMA","ROIi", "ROIp","ROI")) +
  labs(x=paste(length(variation$ID), "Patients IDs"), y="% Tumor Cell (mean)", title="% Tumor Cell Present in TMAs vs ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values for TMA, ROIi, Roip, 6 values fro ROI")

ggplot(variation, aes(x=suid)) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_stroma, color="TMA")) +
  geom_point(aes(y=mean_stroma), color="blue", size=1, alpha=0.6) +
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_stroma_roi_i, color="ROIi"), 
                position = position_nudge(x = 0.25, y = 0)) +
  geom_point(aes(y=mean_stroma_roi_i) , color="red", size=1, alpha=0.6, 
             position = position_nudge(x = 0.25, y = 0)) + 
  geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_stroma_roi_p, color="ROIp"), 
                position = position_nudge(x = 0.5, y = 0)) +
  geom_point(aes(y=mean_stroma_roi_p) , color="limegreen", size=1, alpha=0.6, 
             position = position_nudge(x = 0.5, y = 0)) + 
  # geom_segment( aes(x=suid, xend=suid, y=0, yend=mean_stroma_roi, color="ROI"), 
  #               position = position_nudge(x = 0.75, y = 0)) +
  # geom_point(aes(y=mean_stroma_roi) , color="purple", size=1, alpha=0.6, 
  #            position = position_nudge(x = 0.75, y = 0)) + 
  theme_minimal() + 
  coord_flip()+
  scale_x_discrete(expand=c(0.05, 0)) +
  scale_color_manual(name="Samples \nfrom", values = c("skyblue", "#FF9999", "palegreen", "plum1"), breaks=c("TMA","ROIi", "ROIp","ROI")) +
  labs(x=paste(length(variation$ID), "Patients IDs"), y="% Stroma Cell (mean)", title="% Stroma Cell Present in TMAs vs ROIs per Patient",
       subtitle = "Each point represent the mean of up to 3 values for TMA, ROIi, Roip, 6 values fro ROI")

variation %>% select("suid", "mean_tumor", "mean_tumor_roi_i", "mean_tumor_roi_p") %>% 
  tbl_summary(by = suid, statistic = list(all_continuous() ~ "{median} ({sd})")) %>% 
  add_p()


# TMAs silmilar to intratumoral ROIs?----
# Test normality
qqnorm(markers$mean_tumor_tma)
qqnorm(markers$mean_tumor.i)
pairs.panels(markers[c("mean_tumor_tma", "mean_stroma_tma", # "tumor_variation_tma", "stroma_variation_tma",
                         "mean_tumor.i", "mean_stroma.i", # "tumor_variation_roi_i", "stroma_variation_roi_i",
                         "mean_tumor.p", "mean_stroma.p"# , "tumor_variation_roi_p", "stroma_variation_roi_p"
)])
pairs.panels(markers[c("mean_tumor_tma", "mean_tumor.i", "tumor_variation_tma", "variance_tumor.i")])

# Correlation----
var_cor <- markers[, c("mean_tumor", "mean_tumor_roi_i", "mean_tumor_roi_p")]

mat <- cor(markers[, c("mean_tumor_tma", "mean_tumor.i", "mean_tumor.p", 
                         "mean_stroma_tma", "mean_stroma.i", "mean_stroma.p")], 
           use = "pairwise.complete.obs")
corrplot(mat)
corrplot.mixed(mat)

mat1 <- cor(markers[, c("tumor_variation", "tumor_variation_roi_i", "tumor_variation_roi_p", 
                          "stroma_variation", "stroma_variation_roi_i", "stroma_variation_roi_p")], 
            use = "pairwise.complete.obs")
corrplot(mat1)
corrplot.mixed(mat1)
ggcorrplot(mat, hc.order = TRUE, method = "circle", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "lower", # show the top half panel
           lab = TRUE, # add correlation nbr
           title = "Correlation between collection type",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           # colors = viridis::inferno(n=3),
           lab_col = "darkblue", lab_size = 3, # col and size of the correlation nbr
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)

mat <- cor(markers[, c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
                       "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
                       "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i", "percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                       "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                       "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
                       "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
                       "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i", "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                       "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i")],
           use = "pairwise.complete.obs")
corrplot(mat)
corrplot.mixed(mat)
ggcorrplot(mat, hc.order = FALSE, method = "circle", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 3, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = FALSE,
           # colors = viridis::inferno(n=3),
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)

# For the 28 patients----
mat <- cor(markers_28[, c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
                       "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
                       "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i", "percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                       "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                       "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
                       "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
                       "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i", "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                       "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i")],
           use = "pairwise.complete.obs")
corrplot(mat)
corrplot.mixed(mat)

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation Immune markers for 28 patients.jpg"),
#      width = 1300, height = 800, quality = 100)
ggcorrplot(mat, hc.order = FALSE, method = "square", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs Intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           # colors = viridis::inferno(n=3),
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)
# dev.off()

mat <- cor(markers_28[, c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
                          "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
                          "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i",
                          "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
                          "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
                          "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i")],
           use = "pairwise.complete.obs")
mat1 <- cor(markers_28[, c("percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                          "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                          "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                          "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i")],
           use = "pairwise.complete.obs")

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation Immune markers for 28 patients separated.jpg"),
#      width = 1300, height = 800, quality = 100)
p1 <- ggcorrplot(mat, hc.order = FALSE, method = "square", 
                 # outline.col = "darkblue", # the outline of the circle or sqare
                 # hc.method = "complete",
                 type = "upper", # show the top half panel
                 lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
                 title = "CD3/CD8/FoxP3: TMA vs Intratumoral ROI",
                 show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
                 # colors = viridis::inferno(n=3),
                 # p.mat = pmat, # Add correlation significance
                 sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
                 tl.cex = 10, tl.col = "red", tl.srt = 40,
                 digits = 2
)
p2 <- ggcorrplot(mat1, hc.order = FALSE, method = "square", 
                 # outline.col = "darkblue", # the outline of the circle or sqare
                 # hc.method = "complete",
                 type = "upper", # show the top half panel
                 lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
                 title = "CD11b/CD15: TMA vs Intratumoral ROI",
                 show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
                 # colors = viridis::inferno(n=3),
                 # p.mat = pmat, # Add correlation significance
                 sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
                 tl.cex = 10, tl.col = "red", tl.srt = 40,
                 digits = 2
)
gridExtra::grid.arrange(p1, p2, ncol = 2, top="Immune markers correlation in TMA vs Intratumoral ROI")
# dev.off()

mat <- cor(markers_28[, c("percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                           "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                           "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                           "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i")],
            use = "pairwise.complete.obs")
mat1 <- cor(markers_28[, c("percent_CD11b_tumor_tma", "percent_CD11b_tumor.p",
                           "percent_CD15_tumor_tma", "percent_CD15_tumor.p", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.p",
                           "percent_CD11b_stroma_tma", "percent_CD11b_stroma.p", 
                           "percent_CD15_stroma_tma", "percent_CD15_stroma.p", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.p")],
            use = "pairwise.complete.obs")

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation CD11bCD15 for 28 patients intra or periph vs TMA.jpg"),
#      width = 1300, height = 800, quality = 100)
p1 <- ggcorrplot(mat, hc.order = FALSE, method = "square", 
                 # outline.col = "darkblue", # the outline of the circle or sqare
                 # hc.method = "complete",
                 type = "upper", # show the top half panel
                 lab = TRUE, lab_col = "darkblue", lab_size = 3, # add correlation nbr, col and size of the correlation nbr
                 title = "CD11b/CD15: TMA vs Intratumoral ROI",
                 show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
                 # colors = viridis::inferno(n=3),
                 # p.mat = pmat, # Add correlation significance
                 sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
                 tl.cex = 10, tl.col = "red", tl.srt = 40,
                 digits = 2
)
p2 <- ggcorrplot(mat1, hc.order = FALSE, method = "square", 
                 # outline.col = "darkblue", # the outline of the circle or sqare
                 # hc.method = "complete",
                 type = "upper", # show the top half panel
                 lab = TRUE, lab_col = "darkblue", lab_size = 3, # add correlation nbr, col and size of the correlation nbr
                 title = "CD11b/CD15: TMA vs Peripheral ROI",
                 show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
                 # colors = viridis::inferno(n=3),
                 # p.mat = pmat, # Add correlation significance
                 sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
                 tl.cex = 10, tl.col = "red", tl.srt = 40,
                 digits = 2
)
gridExtra::grid.arrange(p1, p2, ncol = 2, top="Immune markers correlation in TMA vs Intratumoral or Peripheral ROI")
# dev.off()

# Pairwise
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Pairwise tumor stromal cells for 28 patients.jpg"),
#      width = 1300, height = 800, quality = 100)
pairs.panels(markers_28[c("mean_tumor_tma", "mean_stroma_tma",
                       "mean_tumor.i", "mean_stroma.i",
                       "mean_tumor.p", "mean_stroma.p"
)])
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Pairwise Immune markers CD3CD8FoxP3 for 28 patients.jpg"),
#      width = 1300, height = 800, quality = 100)
pairs.panels(markers_28[c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
                       "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
                       "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i",
                       "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
                       "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
                       "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i"
)])
# dev.off()
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Pairwise Immune markers CD11bCD15 for 28 patients.jpg"),
#      width = 1300, height = 800, quality = 100)
pairs.panels(markers_28[c("percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                             "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                             "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                             "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i"
)])
# dev.off()

# Why is there a difference?
table <- as.data.table(markers_28[c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
          "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
          "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i",
          "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
          "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
          "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i", 
          "percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
          "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
          "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
          "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i"
)])
table

# Correlation with total cells
mat <-
  cor(markers_28[, c("percent_CD3_total_tma", "percent_CD8_total_tma", "percent_CD3_CD8_total_tma", "percent_FoxP3_total_tma",
    "percent_CD3_FoxP3_total_tma", "percent_CD11b_total_tma", "percent_CD15_total_tma", "percent_CD11b_CD15_total_tma",
    "percent_CD3_total.i", "percent_CD8_total.i", "percent_CD3_CD8_total.i", "percent_FoxP3_total.i", 
    "percent_CD3_FoxP3_total.i", "percent_CD11b_total.i", "percent_CD15_total.i", "percent_CD11b_CD15_total.i")],
  use = "pairwise.complete.obs")
corrplot.mixed(mat)

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation .jpg"),
#      width = 1300, height = 800, quality = 100)
ggcorrplot(mat, hc.order = FALSE, method = "square", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs Intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           # colors = viridis::inferno(n=3),
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)
# dev.off()

# sqrt
mat <-
  cor(markers_28[, c("sqrt_CD3_total_tma", "sqrt_CD8_total_tma", "sqrt_CD3_CD8_total_tma", "sqrt_FoxP3_total_tma",
                     "sqrt_CD3_FoxP3_total_tma", "sqrt_CD11b_total_tma", "sqrt_CD15_total_tma", "sqrt_CD11b_CD15_total_tma",
                     "percent_CD3_total.i", "percent_CD8_total.i", "percent_CD3_CD8_total.i", "percent_FoxP3_total.i",
                     "percent_CD3_FoxP3_total.i", "percent_CD11b_total.i", "percent_CD15_total.i", "percent_CD11b_CD15_total.i")],
      use = "pairwise.complete.obs")
corrplot.mixed(mat)

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation .jpg"),
#      width = 1300, height = 800, quality = 100)
ggcorrplot(mat, hc.order = FALSE, method = "square", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs Intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           # colors = viridis::inferno(n=3),
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)
# dev.off()

# var
colnames(var_markers28)
mat <-
  cor(var_markers28[, c("var_CD3_tumor_tma", "var_CD8_tumor_tma",
                        "var_CD3_CD8_tumor_tma", "var_FoxP3_tumor_tma", "var_CD3_FoxP3_tumor_tma", "var_CD11b_tumor_tma",
                        "var_CD15_tumor_tma", "var_CD11b_CD15_tumor_tma", "var_CD3_stroma_tma", "var_CD8_stroma_tma",
                        "var_CD3_CD8_stroma_tma", "var_FoxP3_stroma_tma", "var_CD3_FoxP3_stroma_tma", "var_CD11b_stroma_tma",
                        "var_CD15_stroma_tma", "var_CD11b_CD15_stroma_tma", "var_CD3_total_tma", "var_CD8_total_tma",
                        "var_CD3_CD8_total_tma", "var_FoxP3_total_tma",
                        
                        "var_CD3_tumor.i", "var_CD8_tumor.i", "var_CD3_CD8_tumor.i",
                        "var_FoxP3_tumor.i", "var_CD3_FoxP3_tumor.i", "var_CD11b_tumor.i", "var_CD15_tumor.i",
                        "var_CD11b_CD15_tumor.i", "var_CD3_stroma.i", "var_CD8_stroma.i", "var_CD3_CD8_stroma.i",
                        "var_FoxP3_stroma.i", "var_CD3_FoxP3_stroma.i", "var_CD11b_stroma.i", "var_CD15_stroma.i",
                        "var_CD11b_CD15_stroma.i", "var_CD3_total.i", "var_CD8_total.i", "var_CD3_CD8_total.i",
                        "var_FoxP3_total.i")],
      use = "pairwise.complete.obs")
corrplot.mixed(mat)

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation .jpg"),
#      width = 1300, height = 800, quality = 100)
ggcorrplot(mat, hc.order = FALSE, method = "square", 
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs Intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)
# dev.off()

# Cleaning
rm(p1, p2, p3, p4, p5, clin_surv, curv_facet, ggsurv, myplot,
   mat, mat1, variations_TMA, variations_ROI, variations_ROIip, var_cor, table)
# End ----

