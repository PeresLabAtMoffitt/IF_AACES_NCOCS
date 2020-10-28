##################################################################### I ### Immune marker TMA

# Violin plot
tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_tumor_tma", "percent_CD3_total_tma", "percent_CD3_stroma_tma")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  facet_grid(. ~ markers)
tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_tumor_tma", "percent_CD3_total_tma", "percent_CD3_stroma_tma")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  ylim(0, 40)+
  facet_grid(. ~ markers)

tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_total_tma", "percent_CD11b_CD15_stroma_tma")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  facet_grid(. ~ markers)
tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_total_tma", "percent_CD11b_CD15_stroma_tma")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  ylim(0, 1)+
  facet_grid(. ~ markers)


# Paired plot----
tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_tumor_tma", "percent_CD3_total_tma", "percent_CD3_stroma_tma")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_total_tma", "percent_CD3_CD8_stroma_tma")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_FoxP3_tumor_tma", "percent_FoxP3_total_tma", "percent_FoxP3_stroma_tma")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_tumor_tma", "percent_CD11b_total_tma", "percent_CD11b_stroma_tma")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_CD15_tumor_tma", "percent_CD15_total_tma", "percent_CD15_stroma_tma")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

tma_clust_markers %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_total_tma", "percent_CD11b_CD15_stroma_tma")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

# Paired----
p1 <- tma_clust_markers %>% 
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  labs(x=NULL, y="percent_CD3_tumor_tma")
p2 <- tma_clust_markers %>% 
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  labs(x=NULL, y="percent_CD3_total_tma")
p3 <- tma_clust_markers %>% 
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  labs(x=NULL, y="percent_CD3_stroma_tma")
gridExtra::grid.arrange(p1,p2,p3, ncol=3)


# By Immunoscore----
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ immunoscore_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ immunoscore_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ immunoscore_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3)


# By cluster----
# tmaclusters_CD38
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Tumor CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Tumor CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Tumor CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Tumor CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Tumor CD3+CD8+ in TMA")

# tmaclusters_excluded_ST
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Stroma/Tumor Exclusion in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Stroma/Tumor Exclusion in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Stroma/Tumor Exclusion in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Stroma/Tumor Exclusion in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ tmaclusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Stroma/Tumor Exclusion in TMA")

# tmaclusters_R_FoxP3_tum
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Ratio of Tumor CD3+CD8+/FoxP3 in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Ratio of Tumor CD3+CD8+/FoxP3 in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Ratio of Tumor CD3+CD8+/FoxP3 in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Ratio of Tumor CD3+CD8+/FoxP3 in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ tmaclusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster Ratio of Tumor CD3+CD8+/FoxP3 in TMA")

# tmaclusters_CD11b_tot
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+ in TMA")

# tmaclusters_CD15_tot
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD15+ in TMA")

# tmaclusters_CD11bCD15_tot
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ tmaclusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Cluster CD11b+CD15+ in TMA")


# By tertiles----
# CD3_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+ in TMA")

# CD3t_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+ in TMA")

# CD3s_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+ in TMA")

# CD3_CD8_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD3_CD8_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+CD8+ in TMA")

# CD3_CD8t_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD3_CD8t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+CD8+ in TMA")

# CD3_CD8s_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+CD8+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD3_CD8s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+CD8+ in TMA")

# CD3_FoxP3_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD3+FoxP3+ in TMA")

# CD3_FoxP3t_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD3+FoxP3+ in TMA")

# CD3_FoxP3s_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+FoxP3+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD3_FoxP3s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD3+FoxP3+ in TMA")

# CD11b_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD11b_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+ in TMA")

# CD11bt_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD11bt_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+ in TMA")

# CD11bs_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD11bs_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+ in TMA")

# CD11b_CD15_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD11b_CD15_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group CD11b+CD15+ in TMA")

# CD11b_CD15t_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD11b_CD15t_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Tumor CD11b+CD15+ in TMA")

# CD11b_CD15s_grp_tma
p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+CD15+ in TMA")

p1 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p1$layers <- p1$layers[-2]
p2 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p2$layers <- p2$layers[-2]
p3 <- tma_clust_markers %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma_tma",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma_tma")+
  facet_grid(. ~ CD11b_CD15s_grp_tma)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Marker Expression per Tertiles Group Stroma CD11b+CD15+ in TMA")


# End----








