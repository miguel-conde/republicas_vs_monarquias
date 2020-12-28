library(tidyverse)

library(readr)

rep_mon <- data.table::fread("data/rep_mon.csv",dec = ",")

vars_clust <- c("score", "const_form", "int_dollars", "gini_index", "score_ief")

scaled_rep_mon <- rep_mon %>% 
  select(all_of(vars_clust)) %>% 
  mutate(const_form = as.numeric(factor(const_form))) %>% 
  mutate_if(is.numeric, ~ as.numeric(scale(.)))

km_clust <- kmeans(scaled_rep_mon, centers = 4)

res <- tibble(clust = km_clust$cluster) %>% 
  bind_cols(rep_mon)


kmean_withinss <- function(in_data, k) {
  cluster <- kmeans(in_data, k)
  return (cluster$tot.withinss)
}

aux <- sapply(1:1000, 
              function(n) sapply(2:20, 
                                 function(x) kmean_withinss(scaled_rep_mon, x)))
aux_mean <- aux %>% rowMeans()
aux_sd <- aux %>% apply(1, sd)

plot(2:20, aux_mean, type = "o")
lines(2:20, aux_mean + 1.96 * aux_sd, lty = 2)
lines(2:20, aux_mean - 1.96 * aux_sd, lty = 2)


km_clust <- kmeans(scaled_rep_mon, centers = 8)

res <- tibble(clust = km_clust$cluster) %>% 
  bind_cols(rep_mon)

res %>% select(clust, country) %>% filter(clust == 1) %>% as.data.frame()
res %>% select(clust, country) %>% filter(clust == 2) %>% as.data.frame()
res %>% select(clust, country) %>% filter(clust == 3) %>% as.data.frame()
res %>% select(clust, country) %>% filter(clust == 4) %>% as.data.frame()
res %>% select(clust, country) %>% filter(clust == 5) %>% as.data.frame()
res %>% select(clust, country) %>% filter(clust == 6) %>% as.data.frame()
res %>% select(clust, country) %>% filter(clust == 7) %>% as.data.frame()
res %>% select(clust, country) %>% filter(clust == 8) %>% as.data.frame()
res %>% select(clust, country) %>% filter(clust == 9) %>% as.data.frame()
res %>% select(clust, country) %>% filter(clust == 10) %>% as.data.frame()

boxplot(score ~ clust, res)
boxplot(const_form ~ clust, res)
boxplot(int_dollars ~ clust, res)
boxplot(gini_index ~ clust, res)
boxplot(score_ief ~ clust, res)

barplot(t(km_clust$centers), beside = TRUE,xlab="cluster", ylab="value")
legend("topleft", legend = colnames(km_clust$centers), bty = "n")

ggplot(data = km_clust$centers %>% 
         as.data.frame() %>% 
         mutate(clust = 1:nrow(km_clust$centers)), 
       mapping = aes(x = score, y = const_form)) +
  # geom_point() + 
  geom_text(aes(label = clust), hjust = 0, vjust = 0)

ggplot(data = km_clust$centers %>% 
         as.data.frame() %>% 
         mutate(clust = 1:nrow(km_clust$centers)), 
       mapping = aes(x = score, y = int_dollars)) +
  # geom_point() + 
  geom_text(aes(label = clust), hjust = 0, vjust = 0)

ggplot(data = km_clust$centers %>% 
         as.data.frame() %>% 
         mutate(clust = 1:nrow(km_clust$centers)), 
       mapping = aes(x = score, y = gini_index)) +
  # geom_point() + 
  geom_text(aes(label = clust), hjust = 0, vjust = 0)

ggplot(data = km_clust$centers %>% 
         as.data.frame() %>% 
         mutate(clust = 1:nrow(km_clust$centers)), 
       mapping = aes(x = const_form, y = int_dollars)) +
  # geom_point() + 
  geom_text(aes(label = clust), hjust = 0, vjust = 0)

ggplot(data = km_clust$centers %>% 
         as.data.frame() %>% 
         mutate(clust = 1:nrow(km_clust$centers)), 
       mapping = aes(x = const_form, y = gini_index)) +
  # geom_point() + 
  geom_text(aes(label = clust), hjust = 0, vjust = 0)

ggplot(data = km_clust$centers %>% 
         as.data.frame() %>% 
         mutate(clust = 1:nrow(km_clust$centers)), 
       mapping = aes(x = int_dollars, y = gini_index)) +
  # geom_point() + 
  geom_text(aes(label = clust), hjust = 0, vjust = 0)

##
ggplot(data = res, 
       mapping = aes(x = score, y = const_form, col = factor(clust))) +
  geom_text(aes(label = clust), hjust = 0, vjust = 0)
ggplot(data = res, 
       mapping = aes(x = score, y = int_dollars, col = factor(clust))) +
  geom_text(aes(label = clust), hjust = 0, vjust = 0)
ggplot(data = res, 
       mapping = aes(x = score, y = gini_index, col = factor(clust))) +
  geom_text(aes(label = clust), hjust = 0, vjust = 0)
ggplot(data = res, 
       mapping = aes(x = const_form, y = int_dollars, col = factor(clust))) +
  geom_text(aes(label = clust), hjust = 0, vjust = 0)
ggplot(data = res, 
       mapping = aes(x = const_form, y = gini_index, col = factor(clust))) +
  geom_text(aes(label = clust), hjust = 0, vjust = 0)
ggplot(data = res, 
       mapping = aes(x = int_dollars, y = gini_index, col = factor(clust))) +
  geom_text(aes(label = clust), hjust = 0, vjust = 0)


# hclust ------------------------------------------------------------------

d <- dist(scaled_rep_mon)
hc <- hclust(d)
clust <- cutree(hc, k = 10)

plot(hc, hang = -0.01, cex = 0.5)

rect.hclust(hc, k = 10 , border="red")

dt <- dist(t(scaled_rep_mon))
hcc <- hclust(dt)

heatmap(scaled_rep_mon %>% as.matrix(), 
        Rowv = as.dendrogram(hc), Colv = as.dendrogram(hcc))

# Bivariate ---------------------------------------------------------------

library(cluster)

clusplot(scaled_rep_mon, km_clust$cluster, color=TRUE, shade=TRUE)
clusplot(scaled_rep_mon, clust, color=TRUE, shade=TRUE)
