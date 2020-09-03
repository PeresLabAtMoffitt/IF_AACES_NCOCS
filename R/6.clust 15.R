# Intratumor----
# ratio /CD15 tumor
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_CD15_tumor.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) # clust on ratio
summary(clust)
a$clusters_R_CD15_tum <- clust$classification
a$clusters_R_CD15_tum <- factor(a$clusters_R_CD15_tum, 
                              levels = c(1, 2),
                              labels = c("highCD3 highCD15", "highCD3 lowCD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD15_tum")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD15_tum, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD15_tum, color=clusters_R_CD15_tum))+
  geom_boxplot()

# ratio /CD15 total
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_CD15_total.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) # clust on ratio
summary(clust)
a$clusters_R_CD15_tot <- clust$classification
a$clusters_R_CD15_tot <- factor(a$clusters_R_CD15_tot, 
                              levels = c(1, 2),
                              labels = c("highCD3 highCD15", "highCD3 lowCD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD15_tot")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD15_tot, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD15_tot, color=clusters_R_CD15_tot))+
  geom_boxplot()
# ratio /CD15 stroma
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_CD15_stroma.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_CD15_str <- clust$classification
a$clusters_R_CD15_str <- factor(a$clusters_R_CD15_str, 
                                  levels = c(1, 2),
                                  labels = c("highCD3 highCD15", "highCD3 lowCD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD15_str")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD15_str, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD15_str, color=clusters_R_CD15_str))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_R_CD15_tum", "clusters_R_CD15_tot", 
                                         "clusters_R_CD15_str")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD15_tum)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD15_tum",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD15_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD15_tot",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD15_str)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD15_str",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)


############################################################################################# With no ratio----
# CD15 tum
a <- clust_markers %>% filter(clusters_CD38 == "high")
clust <- Mclust(a$percent_CD15_tumor.i, G = 2) 
summary(clust)
a$clusters_CD15_tum <- clust$classification
a$clusters_CD15_tum <- factor(a$clusters_CD15_tum, 
                           levels = c(1, 2),
                           labels = c("low CD15", "high CD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD15_tum")], by= "suid")
clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.i) %>% 
  select(suid, markers_cat, clusters_CD15_tum, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD15_tum, color=clusters_CD15_tum))+
  geom_boxplot()

# CD15 tot----
a <- clust_markers %>% filter(clusters_CD38 == "high") 
clust <- Mclust(a$percent_CD15_total.i, G = 2) 
summary(clust)
a$clusters_CD15_tot <- clust$classification
a$clusters_CD15_tot <- factor(a$clusters_CD15_tot, 
                           levels = c(1, 2),
                           labels = c("low CD15", "high CD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD15_tot")], by= "suid")
clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.i) %>% 
  select(suid, markers_cat, clusters_CD15_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD15_tot, color=clusters_CD15_tot))+
  geom_boxplot()

# CD15 str
a <- clust_markers %>% filter(clusters_CD38 == "high")
clust <- Mclust(a$percent_CD15_stroma.i, G = 2)
summary(clust)
a$clusters_CD15_str <- clust$classification
a$clusters_CD15_str <- factor(a$clusters_CD15_str, 
                           levels = c(1, 2),
                           labels = c("low CD15", "high CD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD15_str")], by= "suid")
clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.i) %>% 
  select(suid, markers_cat, clusters_CD15_str, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD15_str, color=clusters_CD15_str))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_CD15_tum", "clusters_CD15_tot",
                                         "clusters_CD15_str")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_CD15_tum)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD15_tum",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD15_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD15_tot",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD15_str)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD15_str",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)



# Peripheral----

# ratio /CD15 tumor
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.p / percent_CD15_tumor.p) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2)
summary(clust)
a$clusters_R_CD15_tum.p <- clust$classification
a$clusters_R_CD15_tum.p <- factor(a$clusters_R_CD15_tum.p, 
                                   levels = c(1, 2),
                                   labels = c("highCD3 highCD15", "highCD3 lowCD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD15_tum.p")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD15_tum.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD15_tum.p, color=clusters_R_CD15_tum.p))+
  geom_boxplot()

# ratio /CD15 total
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.p / percent_CD15_total.p) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) # clust on ratio
summary(clust)
a$clusters_R_CD15_tot.p <- clust$classification
a$clusters_R_CD15_tot.p <- factor(a$clusters_R_CD15_tot.p, 
                                  levels = c(1, 2),
                                  labels = c("highCD3 highCD15", "highCD3 lowCD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD15_tot.p")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD15_tot.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD15_tot.p, color=clusters_R_CD15_tot.p))+
  geom_boxplot()
# ratio /CD15 stroma
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.p / percent_CD15_stroma.p) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_CD15_str.p <- clust$classification
a$clusters_R_CD15_str.p <- factor(a$clusters_R_CD15_str.p, 
                                  levels = c(1, 2),
                                  labels = c("highCD3 highCD15", "highCD3 lowCD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD15_str.p")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD15_str.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD15_str.p, color=clusters_R_CD15_str.p))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_R_CD15_tum.p", "clusters_R_CD15_tot.p", 
                                         "clusters_R_CD15_str.p")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD15_tum.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD15_tum.p",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD15_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD15_tot.p",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD15_str.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD15_str.p",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)


############################################################################################# With no ratio----
# CD15 tum
a <- clust_markers %>% filter(clusters_CD38 == "high", !is.na(percent_CD15_tumor.p))
clust <- Mclust(a$percent_CD15_tumor.p, G = 2) 
summary(clust)
a$clusters_CD15_tum.p <- clust$classification
a$clusters_CD15_tum.p <- factor(a$clusters_CD15_tum.p, 
                                levels = c(1, 2),
                                labels = c("low CD15", "high CD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD15_tum.p")], by= "suid")
clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.p) %>% 
  select(suid, markers_cat, clusters_CD15_tum.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD15_tum.p, color=clusters_CD15_tum.p))+
  geom_boxplot()

# CD15 tot----
a <- clust_markers %>% filter(clusters_CD38 == "high", !is.na(percent_CD15_tumor.p))
clust <- Mclust(a$percent_CD15_total.p, G = 2) 
summary(clust)
a$clusters_CD15_tot.p <- clust$classification
a$clusters_CD15_tot.p <- factor(a$clusters_CD15_tot.p, 
                                levels = c(1, 2),
                                labels = c("low CD15", "high CD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD15_tot.p")], by= "suid")
clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.p) %>% 
  select(suid, markers_cat, clusters_CD15_tot.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD15_tot.p, color=clusters_CD15_tot.p))+
  geom_boxplot()

# CD15 str
clust <- Mclust(a$percent_CD15_stroma.p, G = 2)
summary(clust)
a$clusters_CD15_str.p <- clust$classification
a$clusters_CD15_str.p <- factor(a$clusters_CD15_str.p, 
                                levels = c(1, 2),
                                labels = c("low CD15", "high CD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD15_str.p")], by= "suid")
clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.p) %>% 
  select(suid, markers_cat, clusters_CD15_str.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD15_str.p, color=clusters_CD15_str.p))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_CD15_tum.p", "clusters_CD15_tot.p",
                                         "clusters_CD15_str.p")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_CD15_tum.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD15_tum.p",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD15_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD15_tot.p",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD15_str.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD15_str.p",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)


