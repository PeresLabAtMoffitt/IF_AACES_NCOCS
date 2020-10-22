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


##################################################################### II ### Clustering----

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
tma_clust_markers$tmaclusters_CD38 <- clust$classification
tma_clust_markers$tmaclusters_CD38 <- factor(tma_clust_markers$tmaclusters_CD38, 
                                      levels = c(1, 2),
                                      labels = c("low", "high"))

tma_clust_markers %>% 
  gather(key = "markers_cat", value = "value", c("percent_CD3_tumor_tma":"percent_CD11b_CD15_tumor_tma")) %>%
  select(suid, tmaclusters_CD38, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ tmaclusters_CD38)

# clusters_excluded_ST
excl_ST <- tma_clust_markers %>% filter(tmaclusters_CD38 == "low")
excl_ST <- excl_ST %>% 
  mutate(ratio_ST = percent_CD3_CD8_stroma.i / percent_CD3_CD8_tumor.i) %>% 
  mutate(ratio_ST = case_when(
    ratio_ST ==  "NaN" ~ 0,
    TRUE ~ ratio_ST
  )) %>%
  filter(is.finite(ratio_ST))

clust <- Mclust(excl_ST$ratio_ST, G = 2)
summary(clust)
excl_ST$clusters_excluded_ST <- clust$classification
excl_ST$clusters_excluded_ST <- factor(excl_ST$clusters_excluded_ST, 
                                 levels = c(1, 2),
                                 labels = c("cold", "excluded"))
tma_clust_markers <- left_join(tma_clust_markers, excl_ST[, c("suid", "clusters_excluded_ST")], by= "suid")

excl_ST %>% 
  gather(key = "markers_cat", value = "value", ratio_ST) %>% 
  select(suid, clusters_excluded_ST, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_excluded_ST, color=clusters_excluded_ST))+
  geom_boxplot()

# tmaclusters_R_FoxP3_tum
highCD38_tma <- tma_clust_markers %>% filter(tmaclusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor_tma / percent_FoxP3_tumor_tma) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(highCD38_tma$ratio_eff_suppr, G = 2) 
summary(clust)
highCD38_tma$tmaclusters_R_FoxP3_tum <- clust$classification
highCD38_tma$tmaclusters_R_FoxP3_tum <- factor(highCD38_tma$tmaclusters_R_FoxP3_tum, 
                                 levels = c(1, 2),
                                 labels = c("immunosuppressed", "hot"))
tma_clust_markers <- left_join(tma_clust_markers, highCD38_tma[, c("suid", "tmaclusters_R_FoxP3_tum")], by= "suid")

highCD38_tma %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, tmaclusters_R_FoxP3_tum, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=tmaclusters_R_FoxP3_tum, color=tmaclusters_R_FoxP3_tum))+
  geom_boxplot()

# tmaclusters_CD11b_tot
CD11b_tma <- tma_clust_markers %>% filter(!is.na(percent_CD11b_total_tma))
clust <- Mclust(CD11b_tma$percent_CD11b_total_tma, G = 2)
summary(clust)
CD11b_tma$tmaclusters_CD11b_tot <- clust$classification
CD11b_tma$tmaclusters_CD11b_tot <- factor(CD11b_tma$tmaclusters_CD11b_tot, 
                               levels = c(1, 2),
                               labels = c("low CD11b", "high CD11b"))
tma_clust_markers <- left_join(tma_clust_markers, CD11b_tma[, c("suid", "tmaclusters_CD11b_tot")], by= "suid")

CD11b_tma %>% 
  gather(key = "markers_cat", value = "value", c("percent_FoxP3_total_tma", "percent_CD11b_total_tma",
                                                 "percent_CD3_CD8_total_tma")) %>% 
  select(suid, markers_cat, tmaclusters_CD11b_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=tmaclusters_CD11b_tot, color=tmaclusters_CD11b_tot))+
  geom_boxplot()

# tmaclusters_CD15_tot
CD15_tma <- tma_clust_markers %>% filter(!is.na(percent_CD15_total_tma)) 
clust <- Mclust(CD11b_tma$percent_CD15_total_tma, G = 2) 
summary(clust)
CD15_tma$tmaclusters_CD15_tot <- clust$classification
CD15_tma$tmaclusters_CD15_tot <- factor(CD15_tma$tmaclusters_CD15_tot, 
                              levels = c(1, 2),
                              labels = c("low CD15", "high CD15"))
tma_clust_markers <- left_join(tma_clust_markers, CD15_tma[, c("suid", "tmaclusters_CD15_tot")], by= "suid")
CD15_tma %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor_tma) %>% 
  select(suid, markers_cat, tmaclusters_CD15_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=tmaclusters_CD15_tot, color=tmaclusters_CD15_tot))+
  geom_boxplot()

# tmaclusters_CD11bCD15_tot
CD11b15_tma <- tma_clust_markers %>% filter(!is.na(percent_CD11b_CD15_total_tma)) 
clust <- Mclust(CD11b15_tma$percent_CD11b_CD15_total_tma, G = 2) 
summary(clust)
CD11b15_tma$tmaclusters_CD11bCD15_tot <- clust$classification
CD11b15_tma$tmaclusters_CD11bCD15_tot <- factor(CD11b15_tma$tmaclusters_CD11bCD15_tot, 
                                   levels = c(1, 2),
                                   labels = c("lowCD11bCD15", "highCD11bCD15"))
tma_clust_markers <- left_join(tma_clust_markers, CD11b15_tma[, c("suid", "tmaclusters_CD11bCD15_tot")], by= "suid")

CD11b15_tma %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor_tma) %>% 
  select(suid, markers_cat, tmaclusters_CD11bCD15_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=tmaclusters_CD11bCD15_tot, color=tmaclusters_CD11bCD15_tot))+
  geom_boxplot()



##################################################################### II ### Survival analysis on immune cells clustering----
# Immunoscore----
clin_surv <- tma_clust_markers
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$immunoscore_)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "Immunoscore", 
           legend.labs = c("0", "1", "2", "3", "4"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf_tmant = FALSE
)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_ + refage + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_ + refage + stage + histotype, data = clin_surv)
summary(myplot)

# CD3CD8
myplot <- survfit(mysurv~clin_surv$tmaclusters_CD38)
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
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD38, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD38 + refage + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD38 + refage + stage + histotype, data = clin_surv)
summary(myplot)

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
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_ST, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_ST + refage + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_ST + refage + stage + histotype, data = clin_surv)
summary(myplot)

# Immunosuppressed
myplot <- survfit(mysurv~clin_surv$tmaclusters_R_FoxP3_tum)
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
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_R_FoxP3_tum, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_R_FoxP3_tum + refage + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_R_FoxP3_tum + refage + stage + histotype, data = clin_surv)
summary(myplot)

# CD11b
myplot <- survfit(mysurv~clin_surv$tmaclusters_CD11b_tot)
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
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD11b_tot, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD11b_tot + refage + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD11b_tot + refage + stage + histotype, data = clin_surv)
summary(myplot)

# CD15
myplot <- survfit(mysurv~clin_surv$tmaclusters_CD15_tot)
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
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD15_tot, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD15_tot + refage + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD15_tot + refage + stage + histotype, data = clin_surv)
summary(myplot)

# CD11bCD15
myplot <- survfit(mysurv~clin_surv$tmaclusters_CD11bCD15_tot)
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
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD11bCD15_tot, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD11bCD15_tot + refage + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~tmaclusters_CD11bCD15_tot + refage + stage + histotype, data = clin_surv)
summary(myplot)


##################################################################### III ### Survival analysis on immune cells tertiles----







