Uni <- read.csv("C:/Users/india/Desktop/data science/assignments/kmeans/crime_data.csv")
Uni
normalized_data<-scale(Uni[,2:5])
View(normalized_data)


#kmeans
k_3 <- kmeans(normalized_data,3)
str(k_3)

wss <- (nrow(normalized_data)-1)*sum(apply(normalized_data,2,var))
for (i in 1:8) wss[i]=sum(kmeans(normalized_data,centers=i)$withinss)
plot(1:8, wss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

install.packages("cluster")
library(cluster)
xds<-rbind(cbind(rnorm(5000,0,8),rnorm(5000,0,8)),cbind(rnorm(5000,50,8),rnorm(5000,50,8)))
xcl<-clara(xds,3,sample=100)
clusplot(xcl)

xpm<-pam(xds,3)
clusplot(xpm)

#hcluster
d <- dist(as.matrix(normalized_data))   
hc <- hclust(d)                 
plot(hc)
plot(hc, hang=-1)
rect.hclust(hc, k=4, border="red")
groups <- cutree(hc, k=4) 
membership<-as.matrix(groups)
final1 <- data.frame(Uni, membership)
View(final1)
write.csv(final1, file="final1.csv",row.names = F)
aggregate(Uni[,-1],by=list(final1$membership),mean)

