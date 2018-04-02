#Read CSV File
inputdata <- read.csv("H:/Projects/10000/10682/TS/Scenario Manager/Cluster Analysis/Test Data5.csv", fileEncoding="UTF-8-BOM")
attach(inputdata)
plot(inputdata)

#Normalization (subtract mean, divide by std)
z <- inputdata[,-c(1,1)]
m <- apply(z,2,mean)
s <- apply(z,2,sd)
z <- scale(z,m,s)

#K-Means Clustering
datalength <- length(Week)
newdf <- as.data.frame(matrix(c(1:datalength)))
for(i in 1:7){
  set.seed(i)
  kc <- kmeans(z,7) #specify number of clusters
  newdf[,i] <- kc$cluster
  plot(Speed~Week, inputdata, col=kc$cluster)
}
inputdata_cluster <- cbind(inputdata,newdf)
write.csv(inputdata_cluster, file = "MyData.csv")
 #try HoursofSnow~Week IncidentCount~Week Volume~Week Volume~HoursofSnow
#kc$centers #can uncomment this to see averages
#write.csv(kc$cluster, file = "MyData.csv")
#print(kc) #prints vector with group label
plot(inputdata, col = kc$cluster)

#individual tests
set.seed(1)
kc <- kmeans(z,7)
plot(Speed~Week, inputdata, col = kc$cluster)
boxplot(Speed~Week, inputdata, col=kc$cluster)
kc
#plot(Volume~Week, inputdata, col=kc$cluster)
#kc$centers


#choosing K
k <- list()
for(i in 1:10){
  set.seed(1)
  k[[i]] <- kmeans(z, i)
}

betweenss_totss <- list()

for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

#plot to determine number of K
plot(1:10, betweenss_totss, type = "b", ylab = "Between SS/Total SS", xlab = "Clusters(k)")

#plot for many k
for(i in 1:6){
  plot(inputdata, col = k[[i]]$cluster)
}
