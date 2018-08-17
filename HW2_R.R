####European employment
#install.packages("dummies")
#install.packages("mlr")
library(dplyr)
library(mlr)
library(dummies)
employ <- read.delim("europeanJobs.txt", header = TRUE, sep = "\t")
#View(employ)
dim(employ)
  str(employ)
names(employ)
summary(employ)
set.seed(12382228)

### create dummy variable


index <- sample(nrow(employ), nrow(employ) * 0.9)
euro.emp1 <- employ[index,]
dim(euro.emp1)
#names(euro.emp1)
euro.emp <- scale(euro.emp1[,2:10])
dumb <- createDummyFeatures(euro.emp1, cols="Country")
employment <- data.frame(dumb, euro.emp)
head(employment)



#View(euro.emp)

# K-Means Cluster Analysis
finding <- kmeans(euro.emp, 3) #5 cluster solution
#str(finding)
#Display number of clusters in each cluster
table(finding$cluster)

#Plot cluster in kmeans
#install.packages("fpc")
library(fpc)
plotcluster(euro.emp, finding$cluster)

employment[finding$cluster==5,]

aggregate(euro.emp1,by=list(finding$cluster),FUN=mean)
finding$centers

# Determine number of clusters Elbow method
wss <- (nrow(euro.emp)-1)*sum(apply(euro.emp,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(euro.emp,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

prediction.strength(employment, Gmin=2, Gmax=10, M=10,cutoff=0.5)

## silhoute

d = dist(euro.emp, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(euro.emp, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn   
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')

plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')
dim(euro.emp1)
fm9 <- mutate(euro.emp1, cluster = finding$cluster)
fm_3

count(fm9, cluster)
f <- fm9[,2:10] %>% group_by(fm9$cluster) %>%   summarise_all(funs(mean(.)))
View(f)


#########
#########Hierarchical clustering
#########

#Wards Method or Hierarchical clustering
#Calculate the distance matrix
employment.dist=dist(euro.emp)
#Obtain clusters using the Wards method
employment.hclust=hclust(employment.dist, method="ward")

# Plot Dendogram
plot(employment.hclust)
#Cut dendrogram at the 3 clusters level and obtain cluster membership
employment.2clust = cutree(employment.hclust,3)
length(employment.2clust)

library(dplyr)
hier <- mutate(euro.emp1, cluster = employment.2clust)
names(hier)
count(hier, cluster)
o <- hier[,2:11] %>% group_by(cluster) %>%   summarise_all(funs(mean(.)))
View(o)
#employment.3clust
plotcluster(euro.emp, employment.2clust)

## using single linkage

employment.hclust=hclust(employment.dist, method="single")
employment.hclust1 = employment.hclust
# Plot Dendogram

plot(employment.hclust1)
plot(employment.hclust)
#Cut dendrogram at the 3 clusters level and obtain cluster membership
class(employment.3clust) = cutree(employment.hclust,3)
length(employment.3clust)

library(dplyr)
hier <- mutate(euro.emp, cluster = employment.2clust)
count(hier, cluster)
hier %>% group_by(cluster) %>%   summarise_all(funs(mean(.)))

#employment.3clust
plotcluster(employment, employment.3clust)


#######Problem 2####
##### Clustering
library(mlr)

fm
dim(fm)
View(fm)
str(fm)
summary(fm)
names(fm)

dumb <- createDummyFeatures(fm_1, cols="NickName")
temp <- data.frame(fm_1[,2:7])
fm_2 <- kmeans(temp, 2)
table(fm_2$cluster)
print(fm_2$cluster)

fm_3 <- mutate(fm, cluster = fm_2$cluster)
fm_3

#install.packages("fpc")
library(fpc)
plotcluster(temp, fm_2$cluster)

# Determine number of clusters
wss <- (nrow(temp)-1)*sum(apply(temp,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(temp,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

d = dist(temp, method = "euclidean") 
result = matrix(nrow = 14, ncol = 3) 
for (i in 2:15){
  cluster_result = kmeans(temp, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width',
xlab = 'number of clusters')


library(dplyr)
dim(fm_3)
names(fm_3)
count(temp, cluster)
f <- fm_3[2:8] %>% group_by(cluster) %>%   summarise_all(funs(mean(.)))
View(f)

####Hierarchical Clustering

#Wards Method or Hierarchical clustering
#Calculate the distance matrix
employment.dist=dist(temp)
#Obtain clusters using the Wards method
employment.hclust=hclust(employment.dist, method="ward")

# Plot Dendogram
plot(employment.hclust)
#Cut dendrogram at the 3 clusters level and obtain cluster membership
employment.2clust = cutree(employment.hclust,2)
length(employment.2clust)

library(dplyr)
hier <- mutate(temp, cluster = employment.2clust)
count(hier, cluster)
j <- hier %>% group_by(cluster) %>% summarise_all(funs(mean(.)))

View(j)


#### Association ####


TransFood <- read.csv('http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv')
TransFood <- TransFood[, -1]
TransFood <- as(as.matrix(TransFood), "transactions")

dim(TransFood)


food_rules <- apriori(TransFood,parameter = list(sup = 0.003, conf = 0.5,target="rules"))
inspect(subset(food_rules, size(food_rules)>3))

summary(food_rules)

large_sup <- subset(food_rules, subset = support>0.01)
inspect(large_sup)

large_sup <- subset(food_rules, subset = lift>10)
inspect(large_sup)

install.packages("arulesViz")
library(arulesViz)
plot(food_rules)


plot(head(sort(food_rules, by="lift"), 10), method = "graph")

plot(food_rules, method="grouped")

#To find the very long transactions we can use the size() and select very long transactions (containing more than 30 items)
x2= Groceries[size(Groceries) > 28]
inspect(x2)

#To see which items are important in the data set we can use the itemFrequencyPlot(). 
#To reduce the number of items, we only plot the item frequency for items with a support greater than 10%. 
#The label size is reduced with the parameter cex.names.
itemFrequencyPlot(Groceries, support = 0.1, cex.names=1)


## Run the apriori algorithm
#Use apriori() algorithm to find all rules (the default association type for apriori()) with a minimum support of 0.3% and a confidence of 0.5

basket_rules <- apriori(Groceries, parameter = list(sup = 0.003, conf = 0.5,target="rules"))

summary(basket_rules)


# Check the generated rules using inspect
inspect(head(basket_rules))


#As typical for association rule mining, the number of rules found is huge. To analyze these rules, for example, subset() can be used to produce separate subsets of rules.

#Basket rules of size greater than 4
inspect(subset(basket_rules, size(basket_rules)>4))

#find the subset rules that has Yogurt in the right hand side. 
#Here we require lift measure exceeds 3.5
yogurt.rhs <- subset(basket_rules, subset = (rhs %in% "yogurt") &(lift>3.5))
inspect(yogurt.rhs)


meat.lhs <- subset(basket_rules, subset = (lhs%in% "meat") & (lift>1.5) )
inspect(meat.lhs)


large_sup <- subset(basket_rules, subset = support>0.01)
inspect(large_sup)

#install.packages('arulesViz')
library('arulesViz')

plot(basket_rules)

#interactive plot
#plot(basket_rules, interactive=TRUE)


#Graph-based visualization can be used for very small sets of rules. 
# The vertices are represented by items for the 10 rules with highest lift:
plot(head(sort(basket_rules, by="lift"), 10), method = "graph")


#The package comes with an approach to cluster association rules and itemsets:
plot(basket_rules, method="grouped")


#Case Starter Code
#data(iris)

TransFood <- read.csv('http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv')
TransFood <- TransFood[, -1]
TransFood <- as(as.matrix(TransFood), "transactions")


food_rules <- apriori(TransFood,parameter = list(sup = 0.003, conf = 0.5,target="rules"))
inspect(subset(food_rules, size(food_rules)>3))


#Load the data for clustering:
Food_by_month <- read.csv('http://homepages.uc.edu/~maifg/DataMining/data/qry_Food_by_Month.csv')

Food_by_month=Food_by_month[,2:7]
food_clust=kmeans(Food_by_month, 4)

food3$centers





#To find the very long transactions we can use the size() 
#and select very long transactions (containing more than 25 items).
data("food")
x = TransFood[size(TransFood) > 25]
inspect(x)

#To see which items are important in the data set we can use the
#itemFrequencyPlot(). To reduce the number of items, 
#we only plot the item frequency for items with a support greater than 10%. 
#The label size is reduced with the parameter cex.names.

#
itemFrequencyPlot(TransFood, support = 0.003, cex.names=0.8)

# Run the apriori algorithm
basket_rules <- apriori(TransFood,parameter = list(sup = 0.003, 
                                                   conf = 0.5,target="rules"))

summary(basket_rules)
inspect(head(basket_rules))

#Basket rules of size greater than 4
inspect(subset(basket_rules, size(basket_rules)>4))

#Find the subset of rules with lift greater than 5:
inspect(subset(basket_rules, lift>10))


#install.packages('arulesViz')
library('arulesViz')
plot(basket_rules)

plot(basket_rules, interactive=TRUE)

plot(head(sort(basket_rules, by="lift"), 10), method = "graph")

plot(basket_rules, method="grouped")










