#PCA
install.packages("xlsx")
library(xlsx)
install.packages("gdata")
library(gdata)
mydata<-read.csv("C:/Users/india/Desktop/data science/assignments/pca/wine.csv")
View(mydata)
attach(mydata)
cor(mydata)
pca1<-princomp(mydata,cor = TRUE,scores = TRUE,covmat = NULL)
summary(pca1)
str(pca1)
loadings(pca1)
plot(pca1)
biplot(pca1)
pca1$loadings
pca1$scores
plot(cumsum(pca1$sdev*pca1$sdev)*100/(sum(pca1$sdev*pca1$sdev)),type="b")
wine_score <- cbind(mydata,pca1$scores[,1:3])

#clustering
#hclustering
install.packages("factoextra")
library(factoextra)
install.packages("NbClust")
library(NbClust)
install.packages("dendextend")
library(dendextend)

clust_data1 <-mydata
norm_clust1 <- scale(clust_data1)
clust1 <- eclust(norm_clust1,"hclust",k=3,graph = FALSE)
fviz_dend(clust1,rect = TRUE)
groups1 <- cutree(clust1,k=3)
finalhclust1 <- data.frame(groups1,mydata)
aggregate(finalhclust1,by=list(clust1$cluster),FUN=mean)

clust_data <-wine_score[,14:16] 
norm_clust <- scale(clust_data)
clust <- eclust(norm_clust,"hclust",k=3,graph = FALSE)
fviz_dend(clust,rect = TRUE)
groups <- cutree(clust,k=3)
finalhclust <- data.frame(groups,mydata)
aggregate(finalhclust,by=list(clust$cluster),FUN=mean)

#kmeans
wss1 <- (nrow(norm_clust1)-1)*sum(apply(norm_clust1,2,var))
for(i in 1:10){
  wss1[i]=sum(kmeans(norm_clust1,centers = i)$withinss)
}
plot(1:10,wss1, type = "o") 
noofclust1 <- NbClust(clust_data1,distance = "euclidean",method = "kmeans",min.nc = 2,max.nc = 10,index = "all")
fviz_nbclust(noofclust1)                                  
km1 <- kmeans(norm_clust1,3)
fviz_cluster(km1,data = mydata)
final1 <- data.frame(km$cluster,mydata)                    
aggregate(final1,by=list(km1$cluster),FUN = mean)

wss <- (nrow(norm_clust)-1)*sum(apply(norm_clust,2,var))
for(i in 1:10){
  wss[i]=sum(kmeans(norm_clust,centers = i)$withinss)
}
plot(1:10,wss, type = "o") 
noofclust <- NbClust(clust_data,distance = "euclidean",method = "kmeans",min.nc = 2,max.nc = 10,index = "all")
fviz_nbclust(noofclust)                                  
km <- kmeans(norm_clust,3)
fviz_cluster(km,data = mydata)
final <- data.frame(km$cluster,mydata)                    
aggregate(final,by=list(km$cluster),FUN = mean)

