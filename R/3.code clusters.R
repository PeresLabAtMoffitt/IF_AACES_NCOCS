########################################################################################## I ### Immunoscore calculation----
# https://jitc.biomedcentral.com/articles/10.1186/s40425-016-0161-x
# The best performing algorithm to compute the Immunoscore has been defined in the large international SITC -
#   led retrospective validation study [1, 2] conducted on more than 3800 St I-III colon cancer patients, 
# Briefly, for each marker (CD3 & CD8) and each zone (CT & IM), densities distributions have been established 
# on the study training set; for each parameter of a tested sample (CD3, CD8, CT, IM), a percentile is derived 
# from these distributions. An average percentile is calculated based on these 4 values. The Immunoscore® is 
# reported as IS-0, 1 – 2 – 3 – 4 based on the following average percentile classes respectively: 
#   [0 %; 10 %] - [>10 %; 25 %] - [>25 %; 70 %] - [>70 %; 95 %] - [>95 %; 100 %].
# CD3 and CD8 in two regions (CT and IM)

# For a 3 immunosore group
# https://pubmed.ncbi.nlm.nih.gov/29754777/

# distribution CD3 intra
quantile(markers$percent_CD3_total.i, c(.10, .25, .70, .95), na.rm = TRUE) 
# distribution CD3 periph
quantile(markers$percent_CD3_total.p, c(.10, .25, .70, .95), na.rm = TRUE) 
# distribution CD8 intra
quantile(markers$percent_CD8_total.i, c(.10, .25, .70, .95), na.rm = TRUE) 
# distribution CD8 periph
quantile(markers$percent_CD8_total.p, c(.10, .25, .70, .95), na.rm = TRUE) 
# Calculate percentile for each patient for CD3, 8, i, p
markers <- markers %>% 
  mutate(percentile_score_CD3_i = ntile(percent_CD3_total.i, 100) ) %>% 
  mutate(percentile_score_CD3_p = ntile(percent_CD3_total.p, 100) ) %>% 
  mutate(percentile_score_CD8_i = ntile(percent_CD8_total.i, 100) ) %>% 
  mutate(percentile_score_CD8_p = ntile(percent_CD8_total.p, 100) ) 

markers <- markers %>%
  mutate(percentile_score_mean = rowMeans( markers[c("percentile_score_CD3_i", "percentile_score_CD3_p", 
                                                    "percentile_score_CD8_i", "percentile_score_CD8_p")] ))
markers <- markers %>%
  mutate(immunoscore_ = case_when(
    percentile_score_mean <= 10        ~ 0,
    percentile_score_mean <= 25        ~ 1,
    percentile_score_mean <= 70        ~ 2,
    percentile_score_mean <= 95        ~ 3,
    percentile_score_mean > 95         ~ 4 
  )) %>% 
  mutate(immunoscore_ = factor(immunoscore_)) %>% 
  mutate(immunoscore_2018lancet = case_when(
    percentile_score_mean <= 25        ~ "Low",
    percentile_score_mean <= 70        ~ "Intermediate",
    percentile_score_mean > 70         ~ "High"
    )) %>% 
  mutate(immunoscore_2018lancet = factor(immunoscore_2018lancet, levels = c("Low", "Intermediate", "High"))) 


######################################################################################## II ### Prepare clusters----
set.seed(1234)
# Need to remove NAs
clust_markers <- markers %>% filter(!is.na(markers$percent_CD3_CD8_tumor.i ))
# Idea:
# Do not take CD3 alone for clustering because contain effector, regulator, helper, gamma delta
# Test FoxP3 alone and CD3-FoxP3 because tumor cells can express FoxP3
# Do not take CD11b alone because CD3CD11b can be NK cells?
# CD11bCD15 are myeloid cells prevent other immune cells to traffic into the tumor

# 1. Quick cluster----

# 1.0.clusters 1_by_Brooke # She took only intratumoral total but all markers simple and doubled staining written below 
# sqrt.percentages.intra <-"SQRT.%.FOXP3", "SQRT.%.CD3","SQRT.%.CD8","SQRT.%.CD11b","SQRT.%.CD15","SQRT.%.CD3.FOXP3","SQRT.%.CD3.CD8","SQRT.%.CD11b.CD15"
clust <- Mclust(clust_markers[c(132:139)], G = 5)# sqrt_CD3_total.i:sqrt_CD11b_CD15_total.i
summary(clust)
clust_markers$clusters_Brooke <- clust$classification
clust_markers$clusters_Brooke <- factor(clust_markers$clusters_Brooke, 
                                        levels = c(4, 3, 5, 2, 1),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", 
         c("sqrt_CD3_CD8_total.i":"sqrt_CD11b_CD15_total.i")) %>% 
  select(suid, clusters_Brooke, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_Brooke)

# 1.1.clusters 1_tumor+stroma/intra+periph
clust_marker <- markers %>% filter( !is.na(markers$percent_CD3_tumor.i), !is.na(markers$percent_CD3_tumor.p)  )

clust <- Mclust(clust_marker[,c(59:74,89:104)], G = 5)
summary(clust)
clust_marker$clusters_all_IandP <- clust$classification
clust_marker$clusters_all_IandP <- factor(clust_marker$clusters_all_IandP, 
                                           levels = c(5, 4, 2, 3, 1),
                                           labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_marker %>% 
  tidyr::gather(key = "markers_cat", value = "value", 
         c("percent_CD3_tumor.i":"percent_CD11b_CD15_stroma.i")) %>% 
  select(suid, clusters_all_IandP, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat,  color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_all_IandP)

clust_marker %>% 
  tidyr::gather(key = "markers_cat", value = "value", 
         c("percent_CD3_tumor.p":"percent_CD11b_CD15_stroma.p")) %>% 
  select(suid, clusters_all_IandP, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat,  color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_all_IandP)

clust_markers <- 
  left_join(clust_markers, clust_marker[, c("suid", "clusters_all_IandP")], by= "suid")

# Cluster for tumor
# 1.2.clusters 2_by_all double positive
clust <- 
  Mclust(clust_markers[,c("percent_CD3_CD8_tumor.i", "percent_CD3_FoxP3_tumor.i", "percent_CD11b_CD15_tumor.i")], G = 5)
summary(clust)
clust_markers$dbl_pos <- clust$classification
clust_markers$dbl_pos <- factor(clust_markers$dbl_pos, 
                                levels = c(2, 4, 3, 5, 1),
                                labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", c("percent_CD3_tumor.i":"percent_CD11b_CD15_tumor.i")) %>% 
  select(suid, dbl_pos, markers_cat, value) %>% 
  ggplot(aes(x=suid, group=markers_cat, color=markers_cat))+
  geom_boxplot(aes(y=value))+
  facet_grid(.~ dbl_pos)

# 1.3.clusters 1_by_CD3-CD8
clust <- Mclust(clust_markers$percent_CD3_CD8_tumor.i, G = 5)
summary(clust)
clust_markers$clusters_CD3CD8 <- clust$classification
clust_markers$clusters_CD3CD8 <- factor(clust_markers$clusters_CD3CD8, 
                                        levels = c(1 , 2, 3, 4, 5),
                                        labels = c("low", "mid-low", "mid", "mid-high", "high"))

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", c("percent_CD3_tumor.i":"percent_CD11b_CD15_tumor.i")) %>% 
  select(suid, clusters_CD3CD8, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_CD3CD8)


# 2. Specific cluster----

### 2.1_Start by cluster CD3-CD8 in 2----
clust <- Mclust(clust_markers$percent_CD3_CD8_tumor.i, G = 2)
summary(clust)
clust_markers$clusters_CD38 <- clust$classification
clust_markers$clusters_CD38 <- factor(clust_markers$clusters_CD38, 
                                      levels = c(1, 2),
                                      labels = c("low", "high"))

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", c("percent_CD3_tumor.i":"percent_CD11b_CD15_tumor.i")) %>%
  select(suid, clusters_CD38, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=markers_cat, color=markers_cat))+
  geom_boxplot()+
  facet_grid(.~ clusters_CD38)


### 2.2_From Low_CD38, separate into Cold or Excluded using ratio Intra/Periph or Stroma?Tumor----
# With ratio difference = 2
low_CD38 <- clust_markers %>% filter(clusters_CD38 == "low")
low_CD38 <- low_CD38 %>% mutate(ratio_IP = percent_CD3_CD8_tumor.p / percent_CD3_CD8_tumor.i) %>% 
  mutate(ratio_IP = case_when(
    ratio_IP ==  "NaN" ~ 0,
    TRUE ~ ratio_IP
  )) %>% 
  mutate(excluded_double_ratioIP = case_when( # 2 is not a good cutoff
    ratio_IP < 2 | ratio_IP == "NaN"     ~ "cold",
    ratio_IP >=2 | is.infinite(ratio_IP) ~ "excluded"
  )) %>% mutate(ratio_ST = percent_CD3_CD8_stroma.i / percent_CD3_CD8_tumor.i) %>% 
  mutate(ratio_ST = case_when(
    ratio_ST ==  "NaN" ~ 0,
    TRUE ~ ratio_ST
  )) %>% 
  mutate(excluded_double_ratioST = case_when( # 2 is not a good cutoff
    ratio_ST < 2 | ratio_ST == "NaN"     ~ "cold",
    ratio_ST >=2 | is.infinite(ratio_ST) ~ "excluded"))
clust_markers <- 
  left_join(clust_markers, low_CD38[, c("suid", "excluded_double_ratioIP", "excluded_double_ratioST")], by= "suid")


low_CD38 %>% 
  tidyr::gather(key = "markers_cat", value = "value", ratio_IP) %>% 
  select(suid, excluded_double_ratioIP, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=excluded_double_ratioIP, color=excluded_double_ratioIP))+ # Take home: bad separation, MORE outliers
  geom_boxplot()
low_CD38 %>% 
  tidyr::gather(key = "markers_cat", value = "value", ratio_ST) %>% 
  select(suid, excluded_double_ratioST, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=excluded_double_ratioST, color=excluded_double_ratioST))+ # Take home: bad separation, MORE outliers
  geom_boxplot()

# With ratio difference = Mclust----better separation
ratio_IP <- low_CD38 %>% filter(is.finite(ratio_IP))
clust <- Mclust(ratio_IP$ratio_IP, G = 2)
summary(clust)
ratio_IP$clusters_excluded_IP <- clust$classification
ratio_IP$clusters_excluded_IP <- factor(ratio_IP$clusters_excluded_IP, 
                                 levels = c(1, 2),
                                 labels = c("cold", "excluded"))
clust_markers <- left_join(clust_markers, ratio_IP[, c("suid", "clusters_excluded_IP")], by= "suid")

ratio_ST <- low_CD38 %>% filter(is.finite(ratio_ST))
clust <- Mclust(ratio_ST$ratio_ST, G = 2)
summary(clust)
ratio_ST$clusters_excluded_ST <- clust$classification
ratio_ST$clusters_excluded_ST <- factor(ratio_ST$clusters_excluded_ST, 
                                 levels = c(1, 2),
                                 labels = c("cold", "excluded"))
clust_markers <- left_join(clust_markers, ratio_ST[, c("suid", "clusters_excluded_ST")], by= "suid")


ratio_IP %>% 
  tidyr::gather(key = "markers_cat", value = "value", ratio_IP) %>% 
  select(suid, clusters_excluded_IP, excluded_double_ratioIP, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_excluded_IP, color=clusters_excluded_IP))+
  geom_boxplot()

ratio_ST %>% 
  tidyr::gather(key = "markers_cat", value = "value", ratio_ST) %>% 
  select(suid, clusters_excluded_ST, excluded_double_ratioST, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_excluded_ST, color=clusters_excluded_ST))+
  geom_boxplot()


### 2.3_From high_CD38, cluster into Immunosuppressed and Hot----

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


# cluster with ratio CD3CD8/CD3Foxp3 in tumor
high_CD38 <- clust_markers %>% filter(clusters_CD38 == "high") %>%
  mutate(ratio_eff_suppr = percent_CD3_CD8_tumor.i / percent_CD3_FoxP3_tumor.i) %>% 
  filter(is.finite(ratio_eff_suppr))
clust <- Mclust(high_CD38$ratio_eff_suppr, G = 2) # clust on ratio
summary(clust)
high_CD38$clusters_immsuppr <- clust$classification
high_CD38$clusters_immsuppr <- factor(high_CD38$clusters_immsuppr, 
                              levels = c(1, 2),
                              labels = c("immunosuppressed", "hot"))
clust_markers <- left_join(clust_markers, high_CD38[, c("suid", "clusters_immsuppr")], by= "suid")

high_CD38 %>% 
  tidyr::gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_immsuppr, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_immsuppr, color=clusters_immsuppr))+
  geom_boxplot()

# cluster with CD3Foxp3 in tumor
high_CD38 <- clust_markers %>% filter(clusters_CD38 == "high") 
clust <- Mclust(high_CD38$percent_CD3_FoxP3_tumor.i, G = 2) # clust on CD3-FoxP3
summary(clust)
high_CD38$clusters_FoxP3 <- clust$classification
high_CD38$clusters_FoxP3 <- factor(high_CD38$clusters_FoxP3, 
                           levels = c(1, 2),
                           labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, high_CD38[, c("suid", "clusters_FoxP3")], by= "suid")

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", percent_CD3_FoxP3_tumor.i) %>% 
  select(suid, markers_cat, clusters_FoxP3, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3, color=clusters_FoxP3))+
  geom_boxplot()

# Other cluster on FoxP3 alone (total) and check if we should add it up to ration or cd3foxp3 cluster
clust <- Mclust(high_CD38$percent_FoxP3_total.i, G = 2) # clust on FoxP3
summary(clust)
high_CD38$clusters_FoxP3_ <- clust$classification
high_CD38$clusters_FoxP3_ <- factor(high_CD38$clusters_FoxP3_, 
                            levels = c(1, 2),
                            labels = c("hot", "immunosuppressed"))
clust_markers <- left_join(clust_markers, high_CD38[, c("suid", "clusters_FoxP3_")], by= "suid")

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", c(percent_FoxP3_total.i, percent_CD3_FoxP3_total.i,
                                                 percent_CD3_CD8_tumor.i)) %>% 
  select(suid, markers_cat, clusters_FoxP3_, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_FoxP3_, color=clusters_FoxP3_))+
  geom_boxplot()



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


markers <- left_join(markers, 
                       clust_markers[, c("suid", "clusters_Brooke", "clusters_all_IandP", "clusters_CD3CD8", "dbl_pos",
                                         "clusters_CD38",
                                         "excluded_double_ratioIP",
                                         "clusters_excluded_IP", "clusters_immsuppr", "clusters_FoxP3", "clusters_FoxP3_",
                                         "special_cluster", "special_cluster2", "special_cluster3", 
                                         "special_cluster4", "special_cluster5")], by="suid")

## Hot vs Immunosuppressed
# Look at the ratio between effector cells (CD3+CD8+) and suppressor (global FoxP3).

# ratio /FoxP3 tumor (use this) ----
# Intratumoral
clust_markers <- markers
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
  tidyr::gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_FoxP3_tum, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_FoxP3_tum, color=clusters_R_FoxP3_tum))+
  geom_boxplot()
# Peripheral
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
  tidyr::gather(key = "markers_cat", value = "value", ratio_eff_suppr) %>% 
  select(suid, clusters_R_FoxP3_tum.p, markers_cat, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_R_FoxP3_tum.p, color=clusters_R_FoxP3_tum.p))+
  geom_boxplot()


## What happened for CD11 or CD15
## CD11b
# Intratumoral
a <- clust_markers %>% filter(!is.na(percent_CD11b_total.i))
clust <- Mclust(a$percent_CD11b_total.i, G = 2)
summary(clust)
a$clusters_CD11b_tot <- clust$classification
a$clusters_CD11b_tot <- factor(a$clusters_CD11b_tot, 
                               levels = c(1, 2),
                               labels = c("low CD11b", "high CD11b"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD11b_tot")], by= "suid")

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", c("percent_FoxP3_total.i", "percent_CD11b_total.i",
                                                 "percent_CD3_CD8_total.i")) %>% 
  select(suid, markers_cat, clusters_CD11b_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD11b_tot, color=clusters_CD11b_tot))+
  geom_boxplot()
# Peripheral
a <- clust_markers %>% filter(!is.na(percent_CD11b_total.p))
clust <- Mclust(a$percent_CD11b_total.p, G = 2)
summary(clust)
a$clusters_CD11b_tot.p <- clust$classification
a$clusters_CD11b_tot.p <- factor(a$clusters_CD11b_tot.p, 
                                 levels = c(1, 2),
                                 labels = c("low CD11b", "high CD11b"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD11b_tot.p")], by= "suid")

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", c("percent_FoxP3_total.p", "percent_CD11b_total.p",
                                                 "percent_CD3_CD8_total.p")) %>% 
  select(suid, markers_cat, clusters_CD11b_tot.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD11b_tot.p, color=clusters_CD11b_tot.p))+
  geom_boxplot()


## CD15
# Intratumoral
a <- clust_markers %>% filter(!is.na(percent_CD15_total.i))
clust <- Mclust(a$percent_CD15_total.i, G = 2) 
summary(clust)
a$clusters_CD15_tot <- clust$classification
a$clusters_CD15_tot <- factor(a$clusters_CD15_tot, 
                              levels = c(1, 2),
                              labels = c("low CD15", "high CD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD15_tot")], by= "suid")
clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.i) %>% 
  select(suid, markers_cat, clusters_CD15_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD15_tot, color=clusters_CD15_tot))+
  geom_boxplot()
# Peripheral
a <- clust_markers %>% filter(!is.na(percent_CD15_total.p))
clust <- Mclust(a$percent_CD15_total.p, G = 2) 
summary(clust)
a$clusters_CD15_tot.p <- clust$classification
a$clusters_CD15_tot.p <- factor(a$clusters_CD15_tot.p, 
                                levels = c(1, 2),
                                labels = c("low CD15", "high CD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD15_tot.p")], by= "suid")
clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.p) %>% 
  select(suid, markers_cat, clusters_CD15_tot.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD15_tot.p, color=clusters_CD15_tot.p))+
  geom_boxplot()

## CD11b+CD15+
# Intratumoral
a <- clust_markers %>% filter(!is.na(percent_CD11b_CD15_total.i))
clust <- Mclust(a$percent_CD11b_CD15_total.i, G = 2) 
summary(clust)
a$clusters_CD11bCD15_tot <- clust$classification
a$clusters_CD11bCD15_tot <- factor(a$clusters_CD11bCD15_tot, 
                                   levels = c(1, 2),
                                   labels = c("lowCD11bCD15", "highCD11bCD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD11bCD15_tot")], by= "suid")

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.i) %>% 
  select(suid, markers_cat, clusters_CD11bCD15_tot, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD11bCD15_tot, color=clusters_CD11bCD15_tot))+
  geom_boxplot()

# Peripheral
a <- clust_markers %>% filter(!is.na(percent_CD11b_CD15_total.p))
clust <- Mclust(a$percent_CD11b_CD15_total.p, G = 2) 
summary(clust)
a$clusters_CD11bCD15_tot.p <- clust$classification
a$clusters_CD11bCD15_tot.p <- factor(a$clusters_CD11bCD15_tot.p, 
                                     levels = c(1, 2),
                                     labels = c("lowCD11bCD15", "highCD11bCD15"))
clust_markers <- left_join(clust_markers, a[, c("suid", "clusters_CD11bCD15_tot.p")], by= "suid")

clust_markers %>% 
  tidyr::gather(key = "markers_cat", value = "value", percent_CD11b_CD15_tumor.p) %>% 
  select(suid, markers_cat, clusters_CD11bCD15_tot.p, value) %>% 
  ggplot(aes(x=suid, y=value, group=clusters_CD11bCD15_tot.p, color=clusters_CD11bCD15_tot.p))+
  geom_boxplot()

markers <- left_join(markers, 
                     clust_markers[, c("suid", "clusters_R_FoxP3_tum", "clusters_R_FoxP3_tum.p", 
                                       "clusters_CD3CD8", "dbl_pos",
                                       "clusters_CD11b_tot", "clusters_CD11b_tot.p",
                                       "clusters_CD15_tot", "clusters_CD15_tot.p", 
                                       "clusters_CD11bCD15_tot", "clusters_CD11bCD15_tot.p")], by="suid")

saveRDS(markers, file = "markers.rds")



######################################################################################## III ### Create df for pair_id----
markers_match <-  markers %>% drop_na(pair_id) %>% 
  group_by(pair_id) %>% filter(n() > 1)

saveRDS(markers_match, file = "markers_match.rds")
# End Cluster coding----
