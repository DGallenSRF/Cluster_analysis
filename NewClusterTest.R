require(flexclust)
require(rattle)
require(ggplot2)
require(cluster)
require(NbClust)
require(fMultivar)

inputdata <- read.csv("H:/Projects/10000/10682/TS/Scenario Manager/Cluster Analysis/Test Data4b.csv", fileEncoding="UTF-8-BOM")

#inputdata_setup <- inputdata[,-c(1,1)]
inputdata_scaled <- scale(inputdata)

#hierarchical clustering
d <- dist(inputdata_scaled)
fit_average <-  hclust(d,method = "average")
plot(fit_average, hang=-1, cex=.4, main="Average Linkage Clustering")

devAskNewPage(ask=T)
nc <-  NbClust(inputdata_scaled, distance = "euclidean", min.nc=2, max.nc=15,method = "average")
table(nc$Best.n[1,])

#best number of clusters is two??? no clustering structure
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")

clusters <- cutree(fit_average, k=9)
table(clusters)
aggregate(inputdata, by=list(cluster=clusters), median)
aggregate(as.data.frame(inputdata_scaled), by=list(cluster=clusters), median)

plot(fit_average, hang=-1, cex=.4, main="Average Linkage Clustering\n9 Cluster Solution")
rect.hclust(fit_average, k=9)

#partitioning cluster methods
wssplot <- function(inputdata_scaled, nc=15,seed=1){
  wss <- (nrow(inputdata_scaled)-1)*sum(apply(inputdata_scaled,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <-  sum(kmeans(inputdata_scaled,centers=i)$withinss)}
  plot(1:nc,wss,type="b", xlab = "Number of Clusters", ylab="Within groups sum of squares")
}

head(inputdata_scaled)

wssplot(inputdata_scaled)
set.seed(1)
devAskNewPage(ask=T)
nc <- NbClust(inputdata_scaled, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab = "Number of Criteria", main = "Number of Clusters Chosen by 26 Criteria")
set.seed(1)
fit_km <- kmeans(inputdata_scaled, 8, nstart=25)
fit_km$size
fit_km$centers
aggregate(inputdata[-1], by=list(cluster=fit_km$cluster), mean)
plot(Speed~Week, inputdata, col=fit_km$cluster)
# legend("bottomright", legend = c(1:7), col = fit_km$cluster)

# crosstab_km <- table(inputdata$Week) #don't have any existing data to cross tab to.
# crosstab_km

# partition around medoids
set.seed(1)
fit_pam <- pam(inputdata[-1], k=8, stand=T)
fit_pam$medoids
clusplot(fit_pam, main="Bivariate Cluster Plot")

#CCC to check for arbitrary clusters???
plot(nc$All.index[,4], type="o", ylab="CCC", xlab = "Number of clusters", col="blue")
