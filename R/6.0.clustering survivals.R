######################################################################################## I ### Survivals by clusters----
clin_surv <- markers

mysurv <- Surv(time = markers$timelastfu_new, event = markers$surv_vital)

# 1. Quick cluster----

# 1.1.Does Intratumoral tumor+stroma markers are predictive?----
# clusters_Brooke
myplot <- survfit(mysurv~markers$clusters_Brooke)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by Brooke",
           legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~markers$clusters_Brooke)

# 1.2.Does Intratumoral+Peripheral tumor+stroma markers are predictive?----
# clusters_all_IandP
myplot <- survfit(mysurv~markers$clusters_all_IandP)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by Brooke",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~+markers$clusters_all_IandP)

# 1.3.Does double positive markers are predictive?----
# clusters 
myplot <- survfit(mysurv~markers$dbl_pos)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by Brooke",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~markers$race+markers$dbl_pos)

p1 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=dbl_pos))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ dbl_pos)+
  stat_compare_means(label = "p.format")
p2 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=dbl_pos))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ dbl_pos)+
  stat_compare_means(label = "p.format")
p3 <- clust_markers %>% filter(!is.na(race)) %>% 
  ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=dbl_pos))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(.~ dbl_pos)+
  stat_compare_means(label = "p.format")  
gridExtra::grid.arrange(p1, p2, p3, ncol=3)

# 1.4.Does tumoral CD3CD8 markers are predictive?----
# clusters_CD3CD8
myplot <- survfit(mysurv~markers$clusters_CD3CD8)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD3CD8",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
# p1 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_CD3CD8))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_CD3CD8)+
#   stat_compare_means(label = "p.format")
# p2 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_CD3CD8))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_CD3CD8)+
#   stat_compare_means(label = "p.format")
# p3 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_CD3CD8))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_CD3CD8)+
#   stat_compare_means(label = "p.format")  
# gridExtra::grid.arrange(p1, p2, p3, ncol=3)



# 2. Specific cluster----

# 2.1.Does tumoral CD3CD8 markers are predictive?----
# clusters_CD38
myplot <- survfit(mysurv~markers$clusters_CD38)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_CD38",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
# p1 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_CD38))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_CD38)+
#   stat_compare_means(label = "p.format")
# p2 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_CD38))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_CD38)+
#   stat_compare_means(label = "p.format")
# p3 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_CD38))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_CD38)+
#   stat_compare_means(label = "p.format")  
# gridExtra::grid.arrange(p1, p2, p3, ncol=3)


# 2.1.Does tumoral Low_CD38 can be separated into cold or excluded?----
# excluded
myplot <- survfit(mysurv~markers$excluded_double_ratioIP)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by excluded_double_ratioIP",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~markers$excluded_double_ratioST)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by excluded_double_ratioST",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~markers$clusters_excluded_IP)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by clusters_excluded_IP",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~markers$clusters_excluded_ST)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by clusters_excluded_ST",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(percent_CD11b_stroma.i, percent_CD11b_tumor.i,
                                                 percent_CD15_stroma.i, percent_CD15_tumor.i)) %>% 
  select(suid, clusters_excluded_ST, markers_cat, value) %>% 
  ggplot(aes(x=clusters_excluded_ST, y=value, color=markers_cat))+ 
  geom_boxplot()+
  ylim(0,.5)
# Take home: CD11b (myeloid which can prevent T cell trafficing) has same repartition in cold or excluded
# So may not play a role or level is too low anyway



# 2.2.Does tumoral High_CD38 can be separated into immunosuppressed and hot?----
# immunosuppressed
myplot <- survfit(mysurv~markers$clusters_immsuppr)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_immsuppr",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)

myplot <- survfit(mysurv~markers$clusters_FoxP3)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters_FoxP3",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)

# Other cluster on FoxP3 alone (total) and check if we should add it up to ration or cd3foxp3 cluster
myplot <- survfit(mysurv~markers$clusters_FoxP3_)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by clusters_FoxP3_",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)




# 2.3.Does special clustering (excluded,immunosupp, hot, cold) is predictive?----
# special_cluster
myplot <- survfit(mysurv~markers$special_cluster)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 then FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~markers$race+markers$special_cluster)
# special_cluster2
myplot <- survfit(mysurv~markers$special_cluster2)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 -ratioCD8<2 -ratioCD8/FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~markers$race+markers$special_cluster2)
# special_cluster3
myplot <- survfit(mysurv~markers$special_cluster3)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by  CD8 -ratioCD8cluster -ratioCD8/FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~markers$race+markers$special_cluster3)
# special_cluster4
myplot <- survfit(mysurv~markers$special_cluster4)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 -ratioCD8<2 -FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~markers$race+markers$special_cluster4)
# special_cluster5
myplot <- survfit(mysurv~markers$special_cluster5)
ggsurvplot(myplot, data = markers,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD8 -ratioCD8clustaer -FoxP3", 
           # legend.labs = c("cold", "hot", "immunosuppressed"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~markers$race+markers$special_cluster5)


# 2.5.Does special clustering (immunosupp, hot) is predictive?----
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

# 2.6.How CD11b affect survivals? Can it be predictive?----
myplot <- survfit(mysurv~clin_surv$clusters_CD11b_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(14, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD11b intratumoral tumor and stroma",
           legend.labs = c("high CD11b", "low CD11b"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD11b_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(14, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD11b peripheral tumor and stroma",
           legend.labs = c("high CD11b", "low CD11b"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)

# 2.7.How CD15 affect survivals? Can it be predictive?----
myplot <- survfit(mysurv~clin_surv$clusters_CD15_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(14, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD15 intratumoral tumor and stroma",
           legend.labs = c("high CD15", "low CD15"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD15_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(14, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD15 peripheral tumor and stroma",
           legend.labs = c("high CD15", "low CD15"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)

# 2.8.How CD11bCD15 affect survivals? Can it be predictive?----
myplot <- survfit(mysurv~clin_surv$clusters_CD11bCD15_tot)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(14, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD11bCD15 intratumoral tumor and stroma",
           legend.labs = c("high CD11bCD15", "low CD11bCD15"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD11bCD15_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(14, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD11bCD15 peripheral tumor and stroma",
           legend.labs = c("high CD11bCD15", "low CD11bCD15"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)








