library(tidyverse)

set.seed(786)
setwd("/Users/JackVoigt/Documents/MSIA Files/Datasets")
seeds_df = read.csv("seeds_dataset.txt",sep = '\t',header = FALSE)

head(seeds_df)

feature_name = c('area','perimeter','compactness','length.of.kernel','width.of.kernal','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')
colnames(seeds_df) = feature_name


str(seeds_df)
summary(seeds_df)
head(seeds_df)
sum(is.na(seeds_df))
colSums(is.na(seeds_df))

seeds_df = seeds_df %>% drop_na()

seeds_label = seeds_df$type.of.seed
seeds_df$type.of.seed = NULL

seeds_df2 = as.data.frame(scale(seeds_df))
head(seeds_df2)
summary(seeds_df2)

dis_matrix = dist(seeds_df2, method = 'euclidean')


hclust_avg <- hclust(dis_matrix, method = 'average')
plot(hclust_avg)


cut_avg <- cutree(hclust_avg, k = 3)
head(cut_avg,100)

plot(hclust_avg)
rect.hclust(hclust_avg , k = 3, border = 2:6)
abline(h = 2.95, col = 'red')

seeds_df2 = seeds_df2 %>%
	mutate(cluster = cut_avg)
count(seeds_df2, cluster)


library(ggplot2)

ggplot(seeds_df2, aes(x = area, y = perimeter, color = factor(cluster))) +
	geom_point() +
	labs( title = "Perimeter vs Area") +
	theme(plot.title = element_text(hjust = 0.5))

table(seeds_df2$cluster, seeds_label)
