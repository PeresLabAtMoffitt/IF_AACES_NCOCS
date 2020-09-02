# 2.3.Clustering----
set.seed(1234)
# Need to remove NAs
clust_markers <- markers %>% filter(!is.na(markers$percent_CD3_CD8_tumor.i ))
# Idea:
# Do not take CD3 alone for clustering because contain effector, regulator, helper, gamma delta
# Do not take FoxP3 alone because tumor cells can express FoxP3
# Do not take CD11b alone because CD3CD11b can be NK cells
# CD11bCD15 are myeloid cells prevent other immune cells to traffic into the tumor


# 2.3.0.clusters 1_by_Brooke # She took only intratumoral total but all markers simple and doubled staining written below 
# sqrt.percentages.intra <-"SQRT.%.FOXP3", "SQRT.%.CD3","SQRT.%.CD8","SQRT.%.CD11b","SQRT.%.CD15","SQRT.%.CD3.FOXP3","SQRT.%.CD3.CD8","SQRT.%.CD11b.CD15"

clust <- Mclust(clust_markers[c(132:139)], G = 5)# sqrt_CD3_total.i:sqrt_CD11b_CD15_total.i
summary(clust)
clust_markers$clusters_Brooke <- clust$classification
clust_markers$clusters_Brooke <- factor(clust_markers$clusters_Brooke, 
                                        levels = c(4, 3, 5, 2, 1),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", 
         c("sqrt_CD3_CD8_total.i","sqrt_CD3_FoxP3_total.i", "sqrt_CD11b_stroma.i":"sqrt_CD11b_CD15_stroma.i")) %>% 
  select(suid, clusters_Brooke, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_Brooke)

# p1 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_Brooke)+
#   stat_compare_means(label = "p.format")
# p2 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_Brooke))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_Brooke)+
#   stat_compare_means(label = "p.format")
# p3 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_Brooke))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_Brooke)+
#   stat_compare_means(label = "p.format")  
# gridExtra::grid.arrange(p1, p2, p3, ncol=3)

# Cluster for tumor+stroma/intra+periph
# 2.3.1.clusters 1_tumor+stroma/intra+periph
clust_markers <- markers %>% filter( !is.na(markers$percent_CD3_tumor.i), !is.na(markers$percent_CD3_tumor.p)  )

clust <- Mclust(clust_markers[,c(59:74,89:104)], G = 5)
summary(clust)
clust_markers$clusters_all_IandP <- clust$classification
clust_markers$clusters_all_IandP <- factor(clust_markers$clusters_all_IandP, 
                                           levels = c(5, 4, 2, 3, 1),
                                           labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(116:131)) %>% 
  select(suid, clusters_all_IandP, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat,  color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_all_IandP)

# p1 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_all_IandP))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_all_IandP)+
#   stat_compare_means(label = "p.format")
# p2 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_all_IandP))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_all_IandP)+
#   stat_compare_means(label = "p.format")
# p3 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_all_IandP))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_all_IandP)+
#   stat_compare_means(label = "p.format")  
# gridExtra::grid.arrange(p1, p2, p3, ncol=3)

# Cluster for tumor
# 2.3.1.clusters 1_by_CD3-CD8
clust_markers <- markers %>% filter( !is.na(markers$percent_CD3_CD8_tumor.i) )
clust <- Mclust(clust_markers$percent_CD3_CD8_tumor.i, G = 5)
summary(clust)
clust_markers$clusters_CD3CD8 <- clust$classification
clust_markers$clusters_CD3CD8 <- factor(clust_markers$clusters_CD3CD8, 
                                        levels = c(1 , 2, 3, 4, 5),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c("percent_CD3_tumor.i":"percent_CD11b_CD15_tumor.i")) %>% 
  select(suid, clusters_CD3CD8, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_CD3CD8)

clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_CD3CD8")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_CD3CD8)
ggsurvplot(myplot, data = clin_surv,
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

# 2.3.2.clusters 2_by_all double positive
clust <- Mclust(clust_markers[,c("percent_CD3_CD8_tumor.i", "percent_CD3_FoxP3_tumor.i", "percent_CD11b_CD15_tumor.i")], G = 5)
summary(clust)
clust_markers$dbl_pos <- clust$classification
clust_markers$dbl_pos <- factor(clust_markers$dbl_pos, 
                                levels = c(2, 4, 3, 5, 1),
                                labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c("percent_CD3_tumor.i":"percent_CD11b_CD15_tumor.i")) %>% 
  select(suid, dbl_pos, markers_cat, value) %>% 
  ggplot(aes(x=suid, group=markers_cat, color=markers_cat))+
  geom_boxplot(aes(y=value))+
  facet_grid(.~ dbl_pos)

clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "dbl_pos")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$dbl_pos)
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "dbl_pos",
           #legend.labs = c("high", "low", "mid", "mid-high", "mid-low"),
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.pnt = FALSE
)
# clust_markers %>% 
#   gather(key = "markers_cat", value = "value", c(106, 108:111)) %>% 
#   select(suid, dbl_pos, markers_cat, value) %>% 
#   ggplot(aes(x=suid, group=dbl_pos, color=dbl_pos))+
#   geom_boxplot(aes(y=value))

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


# 2.3.3.clusters_special----
### 2.3.3.1_CD3-CD8 first
clust_markers <- markers %>% filter(!is.na( percent_CD3_CD8_tumor.i ))
# clust_markers <- markers %>% filter( !is.na(markers[,c(116:131)]), !is.na(markers[,c(140:155)])  )
clust <- Mclust(clust_markers$percent_CD3_CD8_tumor.i, G = 2)
summary(clust)
clust_markers$clusters_CD38 <- clust$classification
clust_markers$clusters_CD38 <- factor(clust_markers$clusters_CD38, 
                                      levels = c(1, 2),
                                      labels = c("low", "high"))

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c("percent_CD3_tumor.i":"percent_CD11b_CD15_tumor.i")) %>%
  select(suid, clusters_CD38, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_CD38)

clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_CD38")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_CD38)
ggsurvplot(myplot, data = clin_surv,
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


### 2.3.3.2_ Cluster low into cold or excluded----
b <- clust_markers %>% filter(clusters_CD38 == "low")
b <- b %>% mutate(ratio_IP = percent_CD3_CD8_tumor.p / percent_CD3_CD8_tumor.i) %>% 
  mutate(ratio_IP = case_when(
    ratio_IP ==  "NaN" ~ 0,
    TRUE ~ ratio_IP
  )) %>% 
  mutate(excluded_double_ratioIP = case_when(
    ratio_IP < 2 | ratio_IP == "NaN"     ~ "cold",
    ratio_IP >=2 | is.infinite(ratio_IP) ~ "excluded"
  )) %>% mutate(ratio_ST = percent_CD3_CD8_stroma.i / percent_CD3_CD8_tumor.i) %>% 
  mutate(ratio_ST = case_when(
    ratio_ST ==  "NaN" ~ 0,
    TRUE ~ ratio_ST
  )) %>% 
  mutate(excluded_double_ratioST = case_when(
    ratio_ST < 2 | ratio_ST == "NaN"     ~ "cold",
    ratio_ST >=2 | is.infinite(ratio_ST) ~ "excluded"))
clust_markers <- left_join(clust_markers, b[, c("suid", "excluded_double_ratioIP", "excluded_double_ratioST")], by= "suid")


b %>% 
  gather(key = "markers_cat", value = "value", ratio_IP) %>% 
  select(suid, excluded_double_ratioIP, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=excluded_double_ratioIP, color=excluded_double_ratioIP))+ # Take home: bad separation, MORE outliers
  geom_boxplot()
b %>% 
  gather(key = "markers_cat", value = "value", ratio_ST) %>% 
  select(suid, excluded_double_ratioST, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=excluded_double_ratioST, color=excluded_double_ratioST))+ # Take home: bad separation, MORE outliers
  geom_boxplot()

# Try another by clustering ######################################### Need to compare ratio separation or ratio cluster
c <- b %>% filter(is.finite(ratio_IP))
clust <- Mclust(c$ratio_IP, G = 2)
summary(clust)
c$clusters_excluded_IP <- clust$classification
c$clusters_excluded_IP <- factor(c$clusters_excluded_IP, 
                              levels = c(1, 2),
                              labels = c("cold", "excluded"))
clust_markers <- left_join(clust_markers, c[, c("suid", "clusters_excluded_IP")], by= "suid")

d <- b %>% filter(is.finite(ratio_ST))
clust <- Mclust(d$ratio_ST, G = 2)
summary(clust)
d$clusters_excluded_ST <- clust$classification
d$clusters_excluded_ST <- factor(d$clusters_excluded_ST, 
                               levels = c(1, 2),
                               labels = c("cold", "excluded"))
clust_markers <- left_join(clust_markers, d[, c("suid", "clusters_excluded_ST")], by= "suid")


c %>% 
  gather(key = "markers_cat", value = "value", ratio_IP) %>% 
  select(suid, clusters_excluded_IP, excluded_double_ratioIP, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_excluded_IP, color=clusters_excluded_IP))+
  geom_boxplot()

d %>% 
  gather(key = "markers_cat", value = "value", ratio_ST) %>% 
  select(suid, clusters_excluded_ST, excluded_double_ratioST, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_excluded_ST, color=clusters_excluded_ST))+
  geom_boxplot()

clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "excluded_double_ratioIP", "excluded_double_ratioST",
                                         "clusters_excluded_IP", "clusters_excluded_ST")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)

myplot <- survfit(mysurv~clin_surv$excluded_double_ratioIP)
myplot
ggsurvplot(myplot, data = clin_surv,
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
myplot <- survfit(mysurv~clin_surv$excluded_double_ratioST)
myplot
ggsurvplot(myplot, data = clin_surv,
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
myplot <- survfit(mysurv~clin_surv$clusters_excluded_IP)
myplot
ggsurvplot(myplot, data = clin_surv,
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
myplot <- survfit(mysurv~clin_surv$clusters_excluded_ST)
myplot
ggsurvplot(myplot, data = clin_surv,
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




### 2.3.3.3_ then immunosuppressed----

# https://www.nature.com/articles/s41573-018-0007-y.pdf
# Following neoadjuvant
# chemotherapy (NAC), these patients had an increased
# ratio of intratumoural CD8+ T cells to FOXP3+ cells200.
# This event was accompanied by a clonal expansion of
# antitumour T cells that correlated with response to NAC, 
# followed by a complete pathological response200. After
# chemotherapy, the absence of autophagy-related protein
# LC3B (LC3B+) puncta and a low CD8+ to FOXP3+ cells
# ratio were associated with a bad prognosis in patients
# with breast cancer201,202

# The decreased percentages of intratumour
# CD4+CD25+FOXP3+ T  cells were accompanied by
# increased CD8+ T cell infiltrates, CD8+ T cell proliferation, increased levels of effector memory T cells and
# overall enhanced antitumour activity268.

# CD11b
# The chemokine XC receptor 1 (XCR1) and its ligand
# lymphotactin (XCL1) regulate migration and function
# of CD103+CD11b− DCs182–184. 

# https://www.spandidos-publications.com/10.3892/mco.2013.107
# Although FOXP3 expression in tumor cells was of no prognostic significance, FOXP3+ lymphocytes were
# significantly associated with poor overall survival 
# FOXP3 exhibited a heterogeneous subcellular localization in tumor cells 
# (cytoplasm, 31%; nucleus, 26%; both, 6%) and, although cytoplasmic FOXP3 was associated with poor OS (P=0.058), 
# nuclear FOXP3 demonstrated a significant association with improved OS (p=0.016). Furthermore, 
# when patients were grouped according to their expression of tumor cytoplasmic FOXP3 and lymphocyte FOXP3, 
# there were notable differences in the Kaplan-Meier curves for OS (P<0.001), with a high infiltration of 
# FOXP3+ lymphocytes accompanied by a cytoplasmic FOXP3+ tumor being the most detrimental phenotype.
# FOXP3 plays a crucial role in the generation of immunosuppressive CD4+CD25+ regulatory T cells (Tregs), 
# which induce immune tolerance to antigens
# tumor-expressed FOXP3, Hinz et al previously reported that FOXP3 expression in a pancreatic cancer cell 
# line inhibited the proliferation of anti-CD3/anti-CD28-stimulated T cells without impeding their activation 

# Ovarian
# https://clincancerres.aacrjournals.org/content/24/22/5685.long
# In agreement with these findings, bulk Treg cells purified from ovarian tumors had enhanced suppressive 
# capacity when compared with melanoma-infiltrating Treg cells (Fig. 6D). These results showed that the 
# immunologic receptor expression pattern including increased 4-1BB, PD-1, and ICOS expression defines 
# highly activated and suppressive Treg cells that infiltrate ovarian tumors.
# https://iji.sums.ac.ir/?sid=Entrez:PubMed&id=pmid:24975967&key=2014.11.2.105
# A trend toward higher Treg cells was observed in higher stages of ovarian cancer (III+IV) in comparison 
# to lower stages (I+II) (6.5 ± 3.2% vs. 4.44 ± 2.7%, p=0.2). Higher percentage of Treg cells was also 
# observed in the patients with high CA125 (CA-125 >100 U/mL) in comparison to those with low CA-125 serum 
# level (CA-125 ≤100 U/mL) although the difference was not significant (6.44 versus 4.18%, p=0.19).
# https://pubmed.ncbi.nlm.nih.gov/24244610/
# The total numbers of CD4(+)CD25(+)FOXP3(+) Tregs, CD4(+)CD25(+)FOXP3(-), CD3(+) and CD8(+) cells were not 
# significantly different between the groups. However, higher ratios of CD8(+)/CD4(+)CD25(+)FOXP3(+) Treg, 
# CD8(+)/CD4(+) and CD8/CD4(+)CD25(+)FOXP3(-) cells were seen in the good outcome group when compared to the
# patients with poor outcome. These data show for the first time that the ratios of CD8(+) to both 
# CD4(+)CD25(+)FOXP3(+) Tregs and CD4(+)CD25(+)FOXP3(-) T cells are associated with disease outcome in 
# ovarian cancer. The association being apparent in ratios rather than absolute count of T cells suggests 
# that the effector/suppressor ratio may be a more important indicator of outcome than individual cell count. 
# Thus, immunotherapy strategies that modify the ratio of CD4(+)CD25(+)FOXP3(+) Tregs or CD4(+)CD25(+)FOXP3(-) 
# T cells to CD8(+) effector cells may be useful in improving outcomes in ovarian cancer.
# # In yet a more recent study, Milne and colleagues showed, in high grade serous ovarian cancer, that the count 
# of intraepithelial cells singly stained for FOXP3+ was associated with greatly improved survival [20]. However, 
# their study did not specifically define the cell type expressing FOXP3. Given that other cells in addition to T 
# cells express FOXP3 (e.g., tumor cells and B cells), the significance of that work, while interesting, 
# remains unclear [21,22].


# let's take a look at survival between FoxP3_all, FoxP3_alone, FoxP3_CD3?






# cluster ratio CD3CD8/CD3Foxp3 tumor
a <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_CD3_FoxP3_tumor.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(a$ratio_eff_suppr, G = 2) # clust on ratio
summary(clust)
a$clusters_immsuppr <- clust$classification
a$clusters_immsuppr <- factor(a$clusters_immsuppr, 
                              levels = c(1, 2),
                              labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_immsuppr")], by= "suid")

a %>% 
  gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_immsuppr, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_immsuppr, color=clusters_immsuppr))+
  geom_boxplot()

clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_immsuppr")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)

myplot <- survfit(mysurv~clin_surv$clusters_immsuppr)
myplot
ggsurvplot(myplot, data = clin_surv,
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

# clustering
a <- clust_markers %>% filter(clusters_CD38 == "high") # 2 is high in CD38
clust <- Mclust(a$percent_CD3_FoxP3_tumor.i, G = 2) # clust on CD3-FoxP3
summary(clust)
a$clusters_FoxP3 <- clust$classification
a$clusters_FoxP3 <- factor(a$clusters_FoxP3, 
                           levels = c(1, 2),
                           labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", percent_CD3_FoxP3_tumor.i) %>% 
  select(suid, markers_cat, clusters_FoxP3, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3, color=clusters_FoxP3))+
  geom_boxplot()

clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_FoxP3")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)

myplot <- survfit(mysurv~clin_surv$clusters_FoxP3)
myplot
ggsurvplot(myplot, data = clin_surv,
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
# p1 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_FoxP3))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_FoxP3)+
#   stat_compare_means(label = "p.format")
# p2 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_FoxP3))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_FoxP3)+
#   stat_compare_means(label = "p.format")
# p3 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_FoxP3))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_FoxP3)+
#   stat_compare_means(label = "p.format")  
# gridExtra::grid.arrange(p1, p2, p3, ncol=3)############################ Need compare ration 8/fox and fox high lox cluster----

# Other cluster on FoxP3 alone (total) and check if we should add it up to ration or cd3foxp3 cluster
clust <- Mclust(a$percent_FoxP3_total.i, G = 2) # clust on FoxP3
summary(clust)
a$clusters_FoxP3_ <- clust$classification
a$clusters_FoxP3_ <- factor(a$clusters_FoxP3_, 
                            levels = c(1, 2),
                            labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3_")], by= "suid")

clust_markers %>% 
  gather(key = "markers_cat", value = "value", c(percent_FoxP3_total.i, percent_CD3_FoxP3_total.i,
                                                 percent_CD3_CD8_tumor.i)) %>% 
  select(suid, markers_cat, clusters_FoxP3_, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3_, color=clusters_FoxP3_))+
  geom_boxplot()


clin_surv <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_FoxP3_")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$clusters_FoxP3_)
ggsurvplot(myplot, data = clin_surv,
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



# Combine all special cluster CD38 and cluster FoxP3
clust_markers <- clust_markers %>% 
  mutate(special_cluster = case_when(
    clusters_CD38 == "low" ~ "cold", # lox CD8 aka cold
    clusters_FoxP3 == "hot" ~ "hot", # low Fox aka hot
    clusters_FoxP3 == "immunosuppressed" ~ "immunosuppressed", # high Fox aka immunosupp
  )) %>% 
  mutate(special_cluster2 = case_when(
    clusters_CD38 == "low" &
      clusters_excluded_IP == "cold" ~ "cold",
    clusters_CD38 == "low" &
      clusters_excluded_IP == "excluded" ~ "excluded",
    clusters_CD38 == "high" &
      clusters_immsuppr == "immunosupressed" ~ "immunosupressed",
    clusters_CD38 == "high" &
      clusters_immsuppr == "hot" ~ "hot"
  )) %>% 
  mutate(special_cluster3 = case_when(
    clusters_CD38 == "low" &
      excluded_double_ratioIP == "cold" ~ "cold",
    clusters_CD38 == "low" &
      excluded_double_ratioIP == "excluded" ~ "excluded",
    clusters_CD38 == "high" &
      clusters_immsuppr == "immunosupressed" ~ "immunosupressed",
    clusters_CD38 == "high" &
      clusters_immsuppr == "hot" ~ "hot"
  )) %>% 
  mutate(special_cluster4 = case_when(
    clusters_CD38 == "low" &
      clusters_excluded_IP == "cold" ~ "cold",
    clusters_CD38 == "low" &
      clusters_excluded_IP == "excluded" ~ "excluded",
    clusters_CD38 == "high" &
      clusters_FoxP3 == "hot" ~ "hot",
    clusters_CD38 == "high" &
      clusters_FoxP3 == "immunosuppressed" ~ "immunosuppressed"
  )) %>% 
  mutate(special_cluster5 = case_when(
    clusters_CD38 == "low" &
      excluded_double_ratioIP == "cold" ~ "cold",
    clusters_CD38 == "low" &
      excluded_double_ratioIP == "excluded" ~ "excluded",
    clusters_CD38 == "high" &
      clusters_FoxP3 == "hot" ~ "hot",
    clusters_CD38 == "high" &
      clusters_FoxP3 == "immunosuppressed" ~ "immunosuppressed"
  ))



######################################################################################## II ### Survival by cluster---
clin_surv <- left_join(markers, 
                       clust_markers[, c("suid",#  "clusters_Brooke", "clusters_all_IandP", "clusters_CD3CD8", "dbl_pos",
                                         # "clusters_CD38", 
                                         # "excluded_double_ratioIP",
                                         # "clusters_excluded_IP", "clusters_immsuppr", "clusters_FoxP3",
                                         "special_cluster", "special_cluster2", "special_cluster3", 
                                         "special_cluster4", "special_cluster5")], by="suid")
mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
# 2.1.Does Intratumoral tumor+stroma markers are predictive?----
# clusters_Brooke
myplot <- survfit(mysurv~clin_surv$clusters_Brooke)
myplot
ggsurvplot(myplot, data = clin_surv,
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
survdiff(mysurv~clin_surv$race+clin_surv$clusters_Brooke)

# 2.2.Does Intratumoral+Peripheral tumor+stroma markers are predictive?----
# clusters_all_IandP
myplot <- survfit(mysurv~clin_surv$clusters_all_IandP)
myplot
ggsurvplot(myplot, data = clin_surv,
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
survdiff(mysurv~clin_surv$race+clin_surv$clusters_Brooke)

# 2.3.Does Peripheral tumor+stroma markers are predictive?-------------------------------------------------------


# 2.4.Does Intratumoral tumor CD3CD8 are predictive?----
# cluster 1
myplot <- survfit(mysurv~clin_surv$clusters_CD3CD8)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by CD3+/CD8+", 
           #legend.labs = c("high", "mid", "mid-high", "mid-low"), # 4253
           pval = TRUE, # pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
survdiff(mysurv~clin_surv$race+clin_surv$clusters_CD3CD8)

# 2.5.Does Intratumoral tumor double positive are predictive?----
# cluster dbl_pos
myplot <- survfit(mysurv~clin_surv$dbl_pos)
myplot
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on whole population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "clustered by all double positive", 
           legend.labs = c("high", "low", "mid", "mid-high", "mid-low"), 
           conf.int = FALSE,
           pval = TRUE, # pval.coord = c(2100,.53), 
           surv.median.line = c("hv"),
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table"
)
survdiff(mysurv~clin_surv$race+clin_surv$dbl_pos)

# 2.6.Does special clustering (excluded,immunosupp, hot, cold) is predictive?----
# special_cluster
myplot <- survfit(mysurv~clin_surv$special_cluster)
myplot
ggsurvplot(myplot, data = clin_surv,
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
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster)
# special_cluster2
myplot <- survfit(mysurv~clin_surv$special_cluster2)
myplot
ggsurvplot(myplot, data = clin_surv,
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
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster2)
# special_cluster3
myplot <- survfit(mysurv~clin_surv$special_cluster3)
myplot
ggsurvplot(myplot, data = clin_surv,
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
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster3)
# special_cluster4
myplot <- survfit(mysurv~clin_surv$special_cluster4)
myplot
ggsurvplot(myplot, data = clin_surv,
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
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster4)
# special_cluster5
myplot <- survfit(mysurv~clin_surv$special_cluster5)
myplot
ggsurvplot(myplot, data = clin_surv,
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
survdiff(mysurv~clin_surv$race+clin_surv$special_cluster5)




# Same clustering with Stroma----
# 2.3.1.clusters 1_by_CD3-CD8 stroma
# clust_markers <- markers %>% filter(!is.na( markers$sqrt_CD3_CD8_stroma.i ))
# 
# clust <- Mclust(clust_markers$sqrt_CD3_CD8_stroma.i, G = 5)
# summary(clust)
# clust_markers$clusters_CD3CD8 <- clust$classification
# clust_markers$clusters_CD3CD8 <- factor(clust_markers$clusters_CD3CD8, 
#                                         levels = c(1, 2, 3, 4, 5),
#                                         labels = c("low", "mid-low", "mid", "mid-high", "high"))
# 
# clust_markers %>% 
#   gather(key = "markers_cat", value = "value", c("sqrt_CD3_CD8_stroma.i", "sqrt_CD3_CD8_tumor.i", "sqrt_CD3_FoxP3_tumor.i")) %>% 
#   select(suid, clusters_CD3CD8, markers_cat, value) %>% 
#   ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
#   geom_boxplot()+
#   facet_grid(.~ clusters_CD3CD8)
# 
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
# 
# # 2.3.2.clusters 2_by_all double positive Stroma
# clust <- Mclust(clust_markers[,c(126, 128, 131)], G = 5)
# summary(clust)
# clust_markers$dbl_pos <- clust$classification
# clust_markers$dbl_pos <- factor(clust_markers$dbl_pos, 
#                                 levels = c(4, 2, 3, 5, 1),
#                                 labels = c("low", "mid-low", "mid", "mid-high", "high"))
# 
# clust_markers %>% 
#   gather(key = "markers_cat", value = "value", c(126, 128, 131)) %>% 
#   select(suid, dbl_pos, markers_cat, value) %>% 
#   ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
#   geom_boxplot()+
#   facet_grid(.~ dbl_pos)
# 
# clust_markers %>% 
#   gather(key = "markers_cat", value = "value", c(114, 116:119)) %>% 
#   select(suid, dbl_pos, markers_cat, value) %>% 
#   ggplot(aes(x=suid, group=dbl_pos, color=dbl_pos))+
#   geom_boxplot(aes(y=value))
# 
# p1 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=dbl_pos))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ dbl_pos)+
#   stat_compare_means(label = "p.format")
# p2 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=dbl_pos))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ dbl_pos)+
#   stat_compare_means(label = "p.format")
# p3 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=dbl_pos))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ dbl_pos)+
#   stat_compare_means(label = "p.format")  
# gridExtra::grid.arrange(p1, p2, p3, ncol=3)
# 
# 
# # 2.3.3.clusters 1_CD3-CD8 then FoxP3 Stroma
# clust <- Mclust(clust_markers[114], G = 2)
# summary(clust)
# clust_markers$clusters_CD38 <- clust$classification
# clust_markers$clusters_CD38 <- factor(clust_markers$clusters_CD38, 
#                                       levels = c(1, 2),
#                                       labels = c("low", "high"))
# 
# clust_markers %>% 
#   gather(key = "markers_cat", value = "value", c(114, 116:119)) %>% 
#   select(suid, clusters_CD38, markers_cat, value) %>% 
#   ggplot(aes(x=suid, y=value, group=clusters_CD38, color=clusters_CD38))+
#   geom_boxplot()+
#   facet_grid(.~ markers_cat)
# 
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
# 
# #
# a <- clust_markers %>% filter(clusters_CD38 == "high") # 2 is high in CD38
# clust <- Mclust(a[116], G = 2)
# summary(clust)
# a$clusters_FoxP3 <- clust$classification
# a$clusters_FoxP3 <- factor(a$clusters_FoxP3, 
#                            levels = c(1, 2),
#                            labels = c("low", "high"))
# clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_FoxP3")], by= "suid")
# 
# clust_markers %>% 
#   gather(key = "markers_cat", value = "value", c(114, 116:119)) %>% 
#   select(suid, clusters_FoxP3, markers_cat, value) %>% 
#   ggplot(aes(x=suid, y=value, group=clusters_FoxP3, color=clusters_FoxP3))+
#   geom_boxplot()+
#   facet_grid(.~ markers_cat)
# 
# p1 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_CD8_tumor.i, color=clusters_FoxP3))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_FoxP3)+
#   stat_compare_means(label = "p.format")
# p2 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD3_FoxP3_tumor.i, color=clusters_FoxP3))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_FoxP3)+
#   stat_compare_means(label = "p.format")
# p3 <- clust_markers %>% filter(!is.na(race)) %>% 
#   ggplot(aes(x=race, y=sqrt_CD11b_CD15_tumor.i, color=clusters_FoxP3))+
#   geom_boxplot()+
#   geom_jitter(shape=16, position=position_jitter(0.2))+
#   facet_grid(.~ clusters_FoxP3)+
#   stat_compare_means(label = "p.format")  
# gridExtra::grid.arrange(p1, p2, p3, ncol=3)
# 
# 
# 
# # Combine cluster CD38 and cluster FoxP3 Stroma
# clust_markers <- clust_markers %>% 
#   mutate(special_cluster = case_when(
#     clusters_CD38 == "low" ~ "cold", # lox CD8 aka cold
#     clusters_FoxP3 == "low" ~ "hot", # low Fox aka hot
#     clusters_FoxP3 == "high" ~ "immunosupressed", # high Fox aka immunosupp
#   ))
# 
# # Survivals----
# clin_surv <- left_join(markers, 
#                        clust_markers[, c("suid", "clusters_CD3CD8", "dbl_pos", "special_cluster")], by="suid")
# mysurv <- Surv(time = clin_surv$timelastfu_new, event = clin_surv$surv_vital)
# 
# # cluster 1
# myplot <- survfit(mysurv~clin_surv$clusters_CD3CD8)
# myplot
# ggsurvplot(myplot, data = clin_surv,
#            title = "Survival analysis on matched patient",
#            font.main = c(16, "bold", "black"),
#            xlab = "Time (days)", legend.title = "clustered by CD3+/CD8+", #legend.labs = c("mid-high", "mid-low", "high", "low"), # 4253
#            pval = TRUE, # pval.coord = c(2100,.53),
#            surv.median.line = c("hv"),
#            risk.table = TRUE,
#            tables.height = 0.2,
#            risk.table.title = "Risk table",
#            conf.int = FALSE
# )
# survdiff(mysurv~clin_surv$race+clin_surv$clusters_CD3CD8)
# # cluster 2
# myplot <- survfit(mysurv~clin_surv$dbl_pos)
# myplot
# ggsurvplot(myplot, data = clin_surv,
#            title = "Survival analysis on matched patient",
#            font.main = c(16, "bold", "black"),
#            xlab = "Time (days)", legend.title = "clustered by all double positive", 
#            legend.labs = c("mid", "mid-high", "low", "mid-low", "high"), # 52143 
#            conf.int = FALSE,
#            pval = TRUE, # pval.coord = c(2100,.53), 
#            surv.median.line = c("hv"),
#            risk.table = TRUE,
#            tables.height = 0.2,
#            risk.table.title = "Risk table"
# )
# survdiff(mysurv~clin_surv$race+clin_surv$dbl_pos)
# # special_cluster
# myplot <- survfit(mysurv~clin_surv$special_cluster)
# myplot
# ggsurvplot(myplot, data = clin_surv,
#            title = "Survival analysis on matched patient",
#            font.main = c(16, "bold", "black"),
#            xlab = "Time (days)", legend.title = "clustered by CD8 then FoxP3", 
#            legend.labs = c("cold", "hot", "immunosuppressed"), 
#            conf.int = FALSE,
#            pval = TRUE, # pval.coord = c(2100,.53), 
#            surv.median.line = c("hv"),
#            risk.table = TRUE,
#            tables.height = 0.2,
#            risk.table.title = "Risk table"
# )
# survdiff(mysurv~clin_surv$race+clin_surv$special_cluster)



# Cleaning
rm(markers_ROIi, markers_ROIp)
