# Intratumor
# ratio /CD3FoxP3 tumor
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_CD3_FoxP3_tumor.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_CD3FoxP3_tum <- clust$classification
a$clusters_R_CD3FoxP3_tum <- factor(a$clusters_R_CD3FoxP3_tum, 
                              levels = c(1, 2),
                              labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD3FoxP3_tum")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD3FoxP3_tum, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD3FoxP3_tum, color=clusters_R_CD3FoxP3_tum))+
  geom_boxplot()

# ratio /CD3FoxP3 total
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_CD3_FoxP3_total.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_CD3FoxP3_tot <- clust$classification
a$clusters_R_CD3FoxP3_tot <- factor(a$clusters_R_CD3FoxP3_tot, 
                              levels = c(1, 2),
                              labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD3FoxP3_tot")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD3FoxP3_tot, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD3FoxP3_tot, color=clusters_R_CD3FoxP3_tot))+
  geom_boxplot()
# ratio /CD3FoxP3 stroma
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_CD3_FoxP3_stroma.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_CD3FoxP3_str <- clust$classification
a$clusters_R_CD3FoxP3_str <- factor(a$clusters_R_CD3FoxP3_str, 
                                  levels = c(1, 2),
                                  labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD3FoxP3_str")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD3FoxP3_str, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD3FoxP3_str, color=clusters_R_CD3FoxP3_str))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_R_CD3FoxP3_tum", "clusters_R_CD3FoxP3_tot", 
                                         "clusters_R_CD3FoxP3_str")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD3FoxP3_tum)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD3FoxP3_tum",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD3FoxP3_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD3FoxP3_tot",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD3FoxP3_str)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD3FoxP3_str",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)




############################################################################################# ratio Fox alone----
# ratio /FoxP3 tumor (use this) ----
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_FoxP3_tumor.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_FoxP3_tum <- clust$classification
a$clusters_R_FoxP3_tum <- factor(a$clusters_R_FoxP3_tum, 
                              levels = c(1, 2),
                              labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_FoxP3_tum")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_FoxP3_tum, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_FoxP3_tum, color=clusters_R_FoxP3_tum))+
  geom_boxplot()

# ratio /FoxP3 total
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_FoxP3_total.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_FoxP3_tot <- clust$classification
a$clusters_R_FoxP3_tot <- factor(a$clusters_R_FoxP3_tot, 
                                  levels = c(1, 2),
                                  labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_FoxP3_tot")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_FoxP3_tot, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_FoxP3_tot, color=clusters_R_FoxP3_tot))+
  geom_boxplot()
# ratio /FoxP3 stroma
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_FoxP3_stroma.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_FoxP3_str <- clust$classification
a$clusters_R_FoxP3_str <- factor(a$clusters_R_FoxP3_str, 
                                  levels = c(1, 2),
                                  labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_FoxP3_str")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_FoxP3_str, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_FoxP3_str, color=clusters_R_FoxP3_str))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_R_FoxP3_tum", "clusters_R_FoxP3_tot", 
                                         "clusters_R_FoxP3_str")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)# **********************************************
myplot <- survfit(mysurv~clin_surv$clusters_R_FoxP3_tum)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_FoxP3_tum",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_FoxP3_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_FoxP3_tot",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_FoxP3_str)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_FoxP3_str",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)


############################################################################################# With no ratio----
# CD3FoxP3
a <- clust_markers %>% filter(clusters_CD38 == "high") 
clust <- Mclust(a$percent_CD3_FoxP3_tumor.i, G = 2) 
summary(clust)
a$clusters_CD3FoxP3_tum <- clust$classification
a$clusters_CD3FoxP3_tum <- factor(a$clusters_CD3FoxP3_tum, 
                           levels = c(1, 2),
                           labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD3FoxP3_tum")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD3_FoxP3_tumor.i) %>% 
  select(suid, markers_cat, clusters_CD3FoxP3_tum, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD3FoxP3_tum, color=clusters_CD3FoxP3_tum))+
  geom_boxplot()

a <- clust_markers %>% filter(clusters_CD38 == "high") 
clust <- Mclust(a$percent_CD3_FoxP3_total.i, G = 2) 
summary(clust)
a$clusters_CD3FoxP3_tot <- clust$classification
a$clusters_CD3FoxP3_tot <- factor(a$clusters_CD3FoxP3_tot, 
                           levels = c(1, 2),
                           labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD3FoxP3_tot")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD3_FoxP3_tumor.i) %>% 
  select(suid, markers_cat, clusters_CD3FoxP3_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD3FoxP3_tot, color=clusters_CD3FoxP3_tot))+
  geom_boxplot()

a <- clust_markers %>% filter(clusters_CD38 == "high") 
clust <- Mclust(a$percent_CD3_FoxP3_stroma.i, G = 2) 
summary(clust)
a$clusters_CD3FoxP3_str <- clust$classification
a$clusters_CD3FoxP3_str <- factor(a$clusters_CD3FoxP3_str, 
                           levels = c(1, 2),
                           labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD3FoxP3_str")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD3_FoxP3_tumor.i) %>% 
  select(suid, markers_cat, clusters_CD3FoxP3_str, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD3FoxP3_str, color=clusters_CD3FoxP3_str))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_CD3FoxP3_tum", "clusters_CD3FoxP3_tot",
                                         "clusters_CD3FoxP3_str")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)

myplot <- survfit(mysurv~clin_surv$clusters_CD3FoxP3_tum)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD3FoxP3_tum",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD3FoxP3_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD3FoxP3_tot",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD3FoxP3_str)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD3FoxP3_str",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)



# FoxP3 alone
clust <- Mclust(a$percent_FoxP3_tumor.i, G = 2) 
summary(clust)
a$clusters_FoxP3_tum <- clust$classification
a$clusters_FoxP3_tum <- factor(a$clusters_FoxP3_tum, 
                            levels = c(1, 2),
                            labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3_tum")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(percent_FoxP3_total.i, percent_CD3_FoxP3_total.i,
                                                 percent_CD3_CD8_tumor.i)) %>% 
  select(suid, markers_cat, clusters_FoxP3_tum, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3_tum, color=clusters_FoxP3_tum))+
  geom_boxplot()

clust <- Mclust(a$percent_FoxP3_total.i, G = 2) 
summary(clust)
a$clusters_FoxP3_tot <- clust$classification
a$clusters_FoxP3_tot <- factor(a$clusters_FoxP3_tot, 
                             levels = c(1, 2),
                             labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3_tot")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(percent_FoxP3_total.i, percent_CD3_FoxP3_total.i,
                                                 percent_CD3_CD8_tumor.i)) %>% 
  select(suid, markers_cat, clusters_FoxP3_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3_tot, color=clusters_FoxP3_tot))+
  geom_boxplot()

clust <- Mclust(a$percent_FoxP3_stroma.i, G = 2) 
summary(clust)
a$clusters_FoxP3_str <- clust$classification
a$clusters_FoxP3_str <- factor(a$clusters_FoxP3_str, 
                             levels = c(1, 2),
                             labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3_str")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(percent_FoxP3_total.i, percent_CD3_FoxP3_total.i,
                                                 percent_CD3_CD8_tumor.i)) %>% 
  select(suid, markers_cat, clusters_FoxP3_str, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3_str, color=clusters_FoxP3_str))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_FoxP3_tum", "clusters_FoxP3_tot",
                                         "clusters_FoxP3_str")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)

myplot <- survfit(mysurv~clin_surv$clusters_FoxP3_tum)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_FoxP3_tum",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_FoxP3_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_FoxP3_tot",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_FoxP3_str)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_FoxP3_str",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)



# Peripheral----


# ratio /CD3FoxP3 tumor
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.p / percent_CD3_FoxP3_tumor.p) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_CD3FoxP3_tum.p <- clust$classification
a$clusters_R_CD3FoxP3_tum.p <- factor(a$clusters_R_CD3FoxP3_tum.p, 
                                      levels = c(1, 2),
                                      labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD3FoxP3_tum.p")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD3FoxP3_tum.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD3FoxP3_tum.p, color=clusters_R_CD3FoxP3_tum.p))+
  geom_boxplot()

# ratio /CD3FoxP3 total
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.p / percent_CD3_FoxP3_total.p) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_CD3FoxP3_tot.p <- clust$classification
a$clusters_R_CD3FoxP3_tot.p <- factor(a$clusters_R_CD3FoxP3_tot.p, 
                                      levels = c(1, 2),
                                      labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD3FoxP3_tot.p")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD3FoxP3_tot.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD3FoxP3_tot.p, color=clusters_R_CD3FoxP3_tot.p))+
  geom_boxplot()
# ratio /CD3FoxP3 stroma
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.p / percent_CD3_FoxP3_stroma.p) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_CD3FoxP3_str.p <- clust$classification
a$clusters_R_CD3FoxP3_str.p <- factor(a$clusters_R_CD3FoxP3_str.p, 
                                      levels = c(1, 2),
                                      labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_CD3FoxP3_str.p")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_CD3FoxP3_str.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_CD3FoxP3_str.p, color=clusters_R_CD3FoxP3_str.p))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_R_CD3FoxP3_tum.p", "clusters_R_CD3FoxP3_tot.p", 
                                         "clusters_R_CD3FoxP3_str.p")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD3FoxP3_tum.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD3FoxP3_tum.p",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD3FoxP3_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD3FoxP3_tot.p",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_CD3FoxP3_str.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_CD3FoxP3_str.p",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)




############################################################################################# ratio Fox alone----
# ratio /FoxP3 tumor----
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.p / percent_FoxP3_tumor.p) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_FoxP3_tum.p <- clust$classification
a$clusters_R_FoxP3_tum.p <- factor(a$clusters_R_FoxP3_tum.p, 
                                   levels = c(1, 2),
                                   labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_FoxP3_tum.p")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_FoxP3_tum.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_FoxP3_tum.p, color=clusters_R_FoxP3_tum.p))+
  geom_boxplot()

# ratio /FoxP3 total
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.p / percent_FoxP3_total.p) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_FoxP3_tot.p <- clust$classification
a$clusters_R_FoxP3_tot.p <- factor(a$clusters_R_FoxP3_tot.p, 
                                   levels = c(1, 2),
                                   labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_FoxP3_tot.p")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_FoxP3_tot.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_FoxP3_tot.p, color=clusters_R_FoxP3_tot.p))+
  geom_boxplot()
# ratio /FoxP3 stroma
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.p / percent_FoxP3_stroma.p) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) 
summary(clust)
a$clusters_R_FoxP3_str.p <- clust$classification
a$clusters_R_FoxP3_str.p <- factor(a$clusters_R_FoxP3_str.p, 
                                   levels = c(1, 2),
                                   labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_R_FoxP3_str.p")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_FoxP3_str.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_FoxP3_str.p, color=clusters_R_FoxP3_str.p))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_R_FoxP3_tum.p", "clusters_R_FoxP3_tot.p", 
                                         "clusters_R_FoxP3_str.p")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_R_FoxP3_tum.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_FoxP3_tum.p",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_FoxP3_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_FoxP3_tot.p",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_FoxP3_str.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_R_FoxP3_str.p",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)


############################################################################################# With no ratio----
# CD3FoxP3
a <- clust_markers %>% filter(clusters_CD38 == "high", !is.na(percent_CD3_FoxP3_tumor.p)) 
clust <- Mclust(a$percent_CD3_FoxP3_tumor.p, G = 2) 
summary(clust)
a$clusters_CD3FoxP3_tum.p <- clust$classification
a$clusters_CD3FoxP3_tum.p <- factor(a$clusters_CD3FoxP3_tum.p, 
                                    levels = c(1, 2),
                                    labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD3FoxP3_tum.p")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD3_FoxP3_tumor.p) %>% 
  select(suid, markers_cat, clusters_CD3FoxP3_tum.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD3FoxP3_tum.p, color=clusters_CD3FoxP3_tum.p))+
  geom_boxplot()

# a <- clust_markers %>% filter(clusters_CD38 == "high") 
clust <- Mclust(a$percent_CD3_FoxP3_total.p, G = 2) 
summary(clust)
a$clusters_CD3FoxP3_tot.p <- clust$classification
a$clusters_CD3FoxP3_tot.p <- factor(a$clusters_CD3FoxP3_tot.p, 
                                    levels = c(1, 2),
                                    labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD3FoxP3_tot.p")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD3_FoxP3_tumor.p) %>% 
  select(suid, markers_cat, clusters_CD3FoxP3_tot.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD3FoxP3_tot.p, color=clusters_CD3FoxP3_tot.p))+
  geom_boxplot()

# a <- clust_markers %>% filter(clusters_CD38 == "high") 
clust <- Mclust(a$percent_CD3_FoxP3_stroma.p, G = 2) 
summary(clust)
a$clusters_CD3FoxP3_str.p <- clust$classification
a$clusters_CD3FoxP3_str.p <- factor(a$clusters_CD3FoxP3_str.p, 
                                    levels = c(1, 2),
                                    labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD3FoxP3_str.p")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD3_FoxP3_tumor.p) %>% 
  select(suid, markers_cat, clusters_CD3FoxP3_str.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD3FoxP3_str.p, color=clusters_CD3FoxP3_str.p))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_CD3FoxP3_tum.p", "clusters_CD3FoxP3_tot.p",
                                         "clusters_CD3FoxP3_str.p")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)

myplot <- survfit(mysurv~clin_surv$clusters_CD3FoxP3_tum.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD3FoxP3_tum.p",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD3FoxP3_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD3FoxP3_tot.p",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD3FoxP3_str.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD3FoxP3_str.p",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)



# FoxP3 alone
clust <- Mclust(a$percent_FoxP3_tumor.p, G = 2) 
summary(clust)
a$clusters_FoxP3_tum.p <- clust$classification
a$clusters_FoxP3_tum.p <- factor(a$clusters_FoxP3_tum.p, 
                                 levels = c(1, 2),
                                 labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3_tum.p")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(percent_FoxP3_total.p, percent_CD3_FoxP3_total.p,
                                                 percent_CD3_CD8_tumor.p)) %>% 
  select(suid, markers_cat, clusters_FoxP3_tum.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3_tum.p, color=clusters_FoxP3_tum.p))+
  geom_boxplot()

clust <- Mclust(a$percent_FoxP3_total.p, G = 2) 
summary(clust)
a$clusters_FoxP3_tot.p <- clust$classification
a$clusters_FoxP3_tot.p <- factor(a$clusters_FoxP3_tot.p, 
                                 levels = c(1, 2),
                                 labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3_tot.p")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(percent_FoxP3_total.p, percent_CD3_FoxP3_total.p,
                                                 percent_CD3_CD8_tumor.p)) %>% 
  select(suid, markers_cat, clusters_FoxP3_tot.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3_tot.p, color=clusters_FoxP3_tot.p))+
  geom_boxplot()

clust <- Mclust(a$percent_FoxP3_stroma.p, G = 2) 
summary(clust)
a$clusters_FoxP3_str.p <- clust$classification
a$clusters_FoxP3_str.p <- factor(a$clusters_FoxP3_str.p, 
                                 levels = c(1, 2),
                                 labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3_str.p")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(percent_FoxP3_total.p, percent_CD3_FoxP3_total.p,
                                                 percent_CD3_CD8_tumor.p)) %>% 
  select(suid, markers_cat, clusters_FoxP3_str.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3_str.p, color=clusters_FoxP3_str.p))+
  geom_boxplot()

# Surv
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_FoxP3_tum.p", "clusters_FoxP3_tot.p",
                                         "clusters_FoxP3_str.p")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)

myplot <- survfit(mysurv~clin_surv$clusters_FoxP3_tum.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_FoxP3_tum.p",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_FoxP3_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_FoxP3_tot.p",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_FoxP3_str.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_FoxP3_str.p",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)

