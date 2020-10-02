##################################################################### I ### Immune marker B/W

# Violin plot
markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_tumor.i", "percent_CD3_total.i", "percent_CD3_stroma.i")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  facet_grid(. ~ markers)
markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_tumor.i", "percent_CD3_total.i", "percent_CD3_stroma.i")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  ylim(0, 2.5)+
  facet_grid(. ~ markers)
  
markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor.i", "percent_CD11b_CD15_total.i", "percent_CD11b_CD15_stroma.i")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  facet_grid(. ~ markers)
markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor.i", "percent_CD11b_CD15_total.i", "percent_CD11b_CD15_stroma.i")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  ylim(0, .1)+
  facet_grid(. ~ markers)


# Paired plot----
# Intratumoral
markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_tumor.i", "percent_CD3_total.i", "percent_CD3_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_CD8_tumor.i", "percent_CD3_CD8_total.i", "percent_CD3_CD8_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_FoxP3_tumor.i", "percent_FoxP3_total.i", "percent_FoxP3_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_tumor.i", "percent_CD11b_total.i", "percent_CD11b_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD15_tumor.i", "percent_CD15_total.i", "percent_CD15_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor.i", "percent_CD11b_CD15_total.i", "percent_CD11b_CD15_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

# Pripheral
markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_tumor.p", "percent_CD3_total.p", "percent_CD3_stroma.p")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_CD8_tumor.p", "percent_CD3_CD8_total.p", "percent_CD3_CD8_stroma.p")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_FoxP3_tumor.p", "percent_FoxP3_total.p", "percent_FoxP3_stroma.p")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_tumor.p", "percent_CD11b_total.p", "percent_CD11b_stroma.p")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD15_tumor.p", "percent_CD15_total.p", "percent_CD15_stroma.p")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor.p", "percent_CD11b_CD15_total.p", "percent_CD11b_CD15_stroma.p")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

# Paired----
p1 <- markers_match %>% 
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  labs(x=NULL, y="percent_CD3_tumor.i")
p2 <- markers_match %>% 
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  labs(x=NULL, y="percent_CD3_total.i")
p3 <- markers_match %>% 
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  labs(x=NULL, y="percent_CD3_stroma.i")
gridExtra::grid.arrange(p1,p2,p3, ncol=3)


# By Immunoscore----
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ immunoscore_)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ immunoscore_)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ immunoscore_)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3)


# By cluster----
# clusters_CD38
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD38")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD38")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD38")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD38")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_CD38)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_CD38)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_CD38)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD38")


# clusters_excluded_IP
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_excluded_IP)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_excluded_IP)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_excluded_IP)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_IP")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_excluded_IP)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_excluded_IP)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_excluded_IP)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_IP")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_excluded_IP)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_excluded_IP)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_excluded_IP)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_IP")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_excluded_IP)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_excluded_IP)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_excluded_IP)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_IP")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_excluded_IP)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_excluded_IP)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_excluded_IP)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_IP")

# clusters_excluded_ST
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_ST")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_ST")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_ST")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_ST")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_excluded_ST)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_excluded_ST)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_excluded_ST)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_excluded_ST")

# clusters_R_FoxP3_tum
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum")

# clusters_R_FoxP3_tum.p
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_R_FoxP3_tum.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_R_FoxP3_tum.p")

# clusters_CD11b_tot
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot")

# clusters_CD11b_tot.p
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_CD11b_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11b_tot.p")

# clusters_CD15_tot
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_CD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot")

# clusters_CD15_tot.p
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_CD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD15_tot.p")

# clusters_CD11bCD15_tot
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot")

# clusters_CD11bCD15_tot.p
p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD3_CD8_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD3_CD8_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_FoxP3_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_FoxP3_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot.p")

p1 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_tumor.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_tumor.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p1$layers <- p1$layers[-2]
p2 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_total.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_total.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p2$layers <- p2$layers[-2]
p3 <- markers_match %>%
  ggpaired(x = "race", y = "percent_CD11b_CD15_stroma.i", id= "pair_id",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = FALSE)+
  labs(x=NULL, y="percent_CD11b_CD15_stroma.i")+
  facet_grid(. ~ clusters_CD11bCD15_tot.p)
p3$layers <- p3$layers[-2]
gridExtra::grid.arrange(p1,p2,p3, ncol=3, top = "Expression per cluster clusters_CD11bCD15_tot.p")
