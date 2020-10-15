# Survivals on TMA----
tma_clust_markers <- markers %>% filter( !is.na(markers$percent_CD3_CD8_tumor_tma) )
clin_surv <- tma_clust_markers
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$race)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "Race", # legend.labs = c("mid-high", "mid-low", "high", "low"), # 4253
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf_tmant = FALSE
)


# 1.1. Clustering----
tma_clust_markers <- markers %>% filter( !is.na(markers$percent_CD3_CD8_tumor_tma) )

# Immunoscore "Simplified"----
# distribution CD3 intra
quantile(tma_clust_markers$percent_CD3_total_tma, c(.10, .25, .70, .95), na.rm = TRUE) 
# distribution CD8 intra
quantile(tma_clust_markers$percent_CD8_total_tma, c(.10, .25, .70, .95), na.rm = TRUE) 
# Calculate percentile for each patient for CD3, 8
tma_clust_markers <- tma_clust_markers %>% 
  mutate(percentile_score_CD3_tma = ntile(percent_CD3_total_tma, 100) ) %>% 
  mutate(percentile_score_CD8_tma = ntile(percent_CD8_total_tma, 100) )
tma_clust_markers <- tma_clust_markers %>%
  mutate(percentile_score_mean = rowMeans(tma_clust_markers[c("percentile_score_CD3_tma", 
                                                    "percentile_score_CD8_tma")])
  ) %>% 
  mutate(immunoscore_ = case_when(
    percentile_score_mean <= 10 ~ 0,
    percentile_score_mean <= 25 ~ 1,
    percentile_score_mean <= 70 ~ 2,
    percentile_score_mean <= 95 ~ 3,
    percentile_score_mean > 95 ~ 4 
  ))
# CD3CD8
clust <- Mclust(tma_clust_markers$percent_CD3_CD8_tumor_tma, G = 2)
summary(clust)
tma_clust_markers$clusters_CD38 <- clust$classification
tma_clust_markers$clusters_CD38 <- factor(tma_clust_markers$clusters_CD38, 
                                      levels = c(1, 2),
                                      labels = c("low", "high"))

tma_clust_markers %>% 
  gather(key = "markers_cat", value = "value", c("percent_CD3_tumor_tma":"percent_CD11b_CD15_tumor_tma")) %>%
  select(suid, clusters_CD38, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_CD38)
# clusters_excluded_IP
# c <- b %>% filter(is.finite(ratio_IP))
# clust <- Mclust(c$ratio_IP, G = 2)
# summary(clust)
# c$clusters_excluded_IP <- clust$classification
# c$clusters_excluded_IP <- factor(c$clusters_excluded_IP, 
#                                  levels = c(1, 2),
#                                  labels = c("cold", "excluded"))
# tma_clust_markers <- left_join(tma_clust_markers, c[, c("suid", "clusters_excluded_IP")], by= "suid")


# clusters_excluded_ST
e <- tma_clust_markers %>% filter(clusters_CD38 == "low")
e <- e %>% 
  mutate(ratio_ST = percent_CD3_CD8_stroma.i / percent_CD3_CD8_tumor.i) %>% 
  mutate(ratio_ST = case_when(
    ratio_ST ==  "NaN" ~ 0,
    TRUE ~ ratio_ST
  )) %>%
  filter(is.finite(ratio_ST))

clust <- Mclust(e$ratio_ST, G = 2)
summary(clust)
e$clusters_excluded_ST <- clust$classification
e$clusters_excluded_ST <- factor(e$clusters_excluded_ST, 
                                 levels = c(1, 2),
                                 labels = c("cold", "excluded"))
tma_clust_markers <- left_join(tma_clust_markers, e[, c("suid", "clusters_excluded_ST")], by= "suid")

e %>% 
  gather(key = "markers_cat", value = "value", ratio_ST) %>% 
  select(suid, clusters_excluded_ST, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_excluded_ST, color=clusters_excluded_ST))+
  geom_boxplot()

# clusters_R_FoxP3_tum
highCD38 <- tma_clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor_tma / percent_FoxP3_tumor_tma) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(highCD38$ratio_eff_suppr, G = 2) 
summary(clust)
highCD38$clusters_R_FoxP3_tum <- clust$classification
highCD38$clusters_R_FoxP3_tum <- factor(highCD38$clusters_R_FoxP3_tum, 
                                 levels = c(1, 2),
                                 labels = c("immunosuppressed", "hot"))
tma_clust_markers <- left_join(tma_clust_markers, highCD38[, c("suid", "clusters_R_FoxP3_tum")], by= "suid")

highCD38 %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_FoxP3_tum, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_FoxP3_tum, color=clusters_R_FoxP3_tum))+
  geom_boxplot()
# clusters_CD11b_tot
highCD38 <- tma_clust_markers %>% filter(clusters_CD38 == "high")
clust <- Mclust(highCD38$percent_CD11b_total_tma, G = 2)
summary(clust)
highCD38$clusters_CD11b_tot <- clust$classification
highCD38$clusters_CD11b_tot <- factor(highCD38$clusters_CD11b_tot, 
                               levels = c(1, 2),
                               labels = c("low CD11b", "high CD11b"))
tma_clust_markers <- left_join(tma_clust_markers, highCD38[, c("suid", "clusters_CD11b_tot")], by= "suid")

tma_clust_markers %>% 
  gather(key = "markers_cat", value = "value", c("percent_FoxP3_total_tma", "percent_CD11b_total_tma",
                                                 "percent_CD3_CD8_total_tma")) %>% 
  select(suid, markers_cat, clusters_CD11b_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD11b_tot, color=clusters_CD11b_tot))+
  geom_boxplot()
# clusters_CD15_tot
highCD38 <- tma_clust_markers %>% filter(clusters_CD38 == "high") 
clust <- Mclust(highCD38$percent_CD15_total_tma, G = 2) 
summary(clust)
highCD38$clusters_CD15_tot <- clust$classification
highCD38$clusters_CD15_tot <- factor(highCD38$clusters_CD15_tot, 
                              levels = c(1, 2),
                              labels = c("low CD15", "high CD15"))
tma_clust_markers <- left_join(tma_clust_markers, highCD38[, c("suid", "clusters_CD15_tot")], by= "suid")
tma_clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor_tma) %>% 
  select(suid, markers_cat, clusters_CD15_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD15_tot, color=clusters_CD15_tot))+
  geom_boxplot()
# clusters_CD11bCD15_tot
highCD38 <- tma_clust_markers %>% filter(clusters_CD38 == "high") 
clust <- Mclust(highCD38$percent_CD11b_CD15_total_tma, G = 2) 
summary(clust)
highCD38$clusters_CD11bCD15_tot <- clust$classification
highCD38$clusters_CD11bCD15_tot <- factor(highCD38$clusters_CD11bCD15_tot, 
                                   levels = c(1, 2),
                                   labels = c("lowCD11bCD15", "highCD11bCD15"))
tma_clust_markers <- left_join(tma_clust_markers, highCD38[, c("suid", "clusters_CD11bCD15_tot")], by= "suid")

tma_clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor_tma) %>% 
  select(suid, markers_cat, clusters_CD11bCD15_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD11bCD15_tot, color=clusters_CD11bCD15_tot))+
  geom_boxplot()




# 1.2. Survivals----
# Immunoscore----
clin_surv <- tma_clust_markers
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$immunoscore_)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "Immunoscore", # legend.labs = c("mid-high", "mid-low", "high", "low"), # 4253
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf_tmant = FALSE
)
# CD3CD8
myplot <- survfit(mysurv~clin_surv$clusters_CD38)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD38",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf_tmant = FALSE
)
# Excluded
myplot <- survfit(mysurv~clin_surv$clusters_excluded_ST)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by clusters_excluded_ST",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf_tmant = FALSE
)
# Immunosuppressed
myplot <- survfit(mysurv~clin_surv$clusters_R_FoxP3_tum)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters ratio CD3CD8/FoxP3 in intratumoral tumor",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf_tmant = FALSE
)
# CD11b
myplot <- survfit(mysurv~clin_surv$clusters_CD11b_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD11b intratumoral tumor and stroma",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf_tmant = FALSE
)
# CD15
myplot <- survfit(mysurv~clin_surv$clusters_CD15_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD15 intratumoral tumor and stroma",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf_tmant = FALSE
)
# CD11bCD15
myplot <- survfit(mysurv~clin_surv$clusters_CD11bCD15_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD11bCD15 intratumoral tumor and stroma",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf_tmant = FALSE
)
