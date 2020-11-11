# ########################################################################################## I ### Survivals----
# 1.1.Clustering----
set.seed(1234)
# Merge markers_match with the cluster previously created
markers_match <- left_join(markers_match, clust_markers[, c("suid", 
                                                            "clusters_CD38", 
                                                            "clusters_excluded_IP", "clusters_excluded_ST",
                                                            "clusters_R_FoxP3_tum", "clusters_R_FoxP3_tum.p",
                                                            "clusters_CD11b_tot", "clusters_CD11b_tot.p", 
                                                            "clusters_CD15_tot", "clusters_CD15_tot.p",
                                                            "clusters_CD11bCD15_tot", "clusters_CD11bCD15_tot.p"
                                                            )], by= "suid") %>% 
  left_join(., markers[, c("suid","immunoscore_")], by = "suid")

# 1.2.Survivals----
# Immunoscore----
clin_surv <- markers_match
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
           conf.int = FALSE
)

clin_surv <- markers_match
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$immunoscore_2018lancet)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "Immunoscore Lancet 2018", # legend.labs = c("mid-high", "mid-low", "high", "low"), # 4253
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
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
           conf.int = FALSE
)
# Excluded
myplot <- survfit(mysurv~clin_surv$clusters_excluded_IP)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
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
           conf.int = FALSE
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
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_R_FoxP3_tum.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters ration CD3CD8/FoxP3 in peripheral tumor",
           # legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
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
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD11b_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD11b peripheral tumor and stroma",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
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
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD15_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD15 peripheral tumor and stroma",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
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
           conf.int = FALSE
)
myplot <- survfit(mysurv~clin_surv$clusters_CD11bCD15_tot.p)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on case-matched population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clusters CD11bCD15 peripheral tumor and stroma",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)


# 1.3.Survivals Black/White----
clin_surv <- markers_match
# Immunoscore----
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_ + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "immunoscore_",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Immunoscore",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_ + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_ + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_ + race  + immunoscore_*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_ + refage + race + stage  + immunoscore_*race, data = clin_surv)
summary(myplot)

myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_2018lancet + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "immunoscore_2018lancet",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Immunoscore Lancet 2018",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_2018lancet + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_2018lancet + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_2018lancet + race  + immunoscore_2018lancet*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~immunoscore_2018lancet + refage + race + stage  + immunoscore_2018lancet*race, data = clin_surv)
summary(myplot)
# clusters_CD38
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD38 + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_CD38",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_CD38",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD38 + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD38 + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD38 + race  + clusters_CD38*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD38 + refage + race + stage  + clusters_CD38*race, data = clin_surv)
summary(myplot)
# clusters_excluded_IP
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_IP + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_excluded_IP",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_excluded_IP",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_IP + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_IP + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_IP + race  + clusters_excluded_IP*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_IP + refage + race + stage  + clusters_excluded_IP*race, data = clin_surv)
summary(myplot)
# clusters_excluded_ST
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_ST + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_excluded_ST",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_excluded_ST",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_ST + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_ST + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_ST + race  + clusters_excluded_ST*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_excluded_ST + refage + race + stage  + clusters_excluded_ST*race, data = clin_surv)
summary(myplot)
# clusters_R_FoxP3_tum
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_R_FoxP3_tum",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_R_FoxP3_tum",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum + race  + clusters_R_FoxP3_tum*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum + refage + race + stage  + clusters_R_FoxP3_tum*race, data = clin_surv)
summary(myplot)
# clusters_R_FoxP3_tum.p
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum.p + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_R_FoxP3_tum.p",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_R_FoxP3_tum.p",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum.p + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum.p + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum.p + race  + clusters_R_FoxP3_tum.p*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_R_FoxP3_tum.p + refage + race + stage  + clusters_R_FoxP3_tum.p*race, data = clin_surv)
summary(myplot)
# clusters_CD11b_tot
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_CD11b_tot",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_CD11b_tot",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot + race  + clusters_CD11b_tot*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot + refage + race + stage  + clusters_CD11b_tot*race, data = clin_surv)
summary(myplot)
# clusters_CD11b_tot.p
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot.p + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_CD11b_tot.p",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_CD11b_tot.p",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot.p + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot.p + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot.p + race  + clusters_CD11b_tot.p*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11b_tot.p + refage + race + stage  + clusters_CD11b_tot.p*race, data = clin_surv)
summary(myplot)
# clusters_CD15_tot
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_CD15_tot",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_CD15_tot",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot + race  + clusters_CD15_tot*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot + refage + race + stage  + clusters_CD15_tot*race, data = clin_surv)
summary(myplot)
# clusters_CD15_tot.p
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot.p + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_CD15_tot.p",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_CD15_tot.p",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot.p + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot.p + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot.p + race  + clusters_CD15_tot.p*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD15_tot.p + refage + race + stage  + clusters_CD15_tot.p*race, data = clin_surv)
summary(myplot)
# clusters_CD11bCD15_tot
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_CD11bCD15_tot",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_CD11bCD15_tot",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot + race  + clusters_CD11bCD15_tot*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot + refage + race + stage  + clusters_CD11bCD15_tot*race, data = clin_surv)
summary(myplot)
# clusters_CD11bCD15_tot.p
myplot <- survfit(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot.p + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "clusters_CD11bCD15_tot.p",
                 title = "Survival analysis on case-matched population Black vs White",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "clusters_CD11bCD15_tot.p",
                 surv.median.line = c("hv"))
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot.p + race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot.p + refage + race + stage, data = clin_surv)
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot.p + race  + clusters_CD11bCD15_tot.p*race, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu_new, event = surv_vital)~clusters_CD11bCD15_tot.p + refage + race + stage  + clusters_CD11bCD15_tot.p*race, data = clin_surv)
summary(myplot)


########################################################################################## II ### Plot Matched Cases----
# Black vs White
markers_ROI_CM <- markers_ROI %>% 
  left_join(cases_match, ., by = "suid") %>% 
  drop_na(percent_CD3_tumor.i) %>% group_by(pair_id) %>% filter( n() > 1 )

# markers_ROIp <- markers_ROIp %>% 
#   full_join(cases_match, ., by = "suid") %>% 
#   drop_na(.) %>% group_by(pair_id) %>% filter( n() > 1 )

# plots----
# plot(markers_ROI_CM)
ggplot(markers_match, aes(x=race, y=percent_CD3_tumor.i)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Race", y="CD3 tumor", title="CD3 in Race") + geom_jitter(shape=16, position=position_jitter(0.2))

library(ggpubr)
ggpaired(markers_match, x = "race", y = "percent_CD3_tumor.i",
         id = "pair_id",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco")+ # depends of the journal to publish on
  stat_compare_means(paired = TRUE)

ggpaired(markers_match, x = "race", y = "percent_FoxP3_tumor.i",
         id = "pair_id",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE)





markers_match %>% 
  gather(key = "race", value = c("percent_CD3_tumor.i", "percent_FoxP3_tumor.i"))
ggpaired(markers_ROI_CM, x = "race", y = "percent_CD3_tumor",
         id = "pair_id",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco",
         facet.by = "marker", short.panel.labs = FALSE)+
  stat_compare_means(paired = TRUE)




# Ratio on case_matched
ggplot(markers_match, aes(x=percent_CD3_tumor.p, y=percent_CD3_tumor.i))+
  geom_point()
ggplot(markers_match, aes(x=percent_CD8_tumor.p, y=percent_CD8_tumor.i))+
  geom_point()
ggplot(markers_match, aes(x=percent_FoxP3_tumor.p, y=percent_FoxP3_tumor.i))+
  geom_point()
ggplot(markers_match, aes(x=percent_CD11b_tumor.p, y=percent_CD11b_tumor.i))+
  geom_point()
ggplot(markers_match, aes(x=percent_CD15_tumor.p, y=percent_CD15_tumor.i))+
  geom_point()




# heatmaps----same as before


