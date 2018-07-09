library(RMySQL)
library(cluster)
library(factoextra)
library(tidyr)
library(ggplot2)
library(DeducerExtras)
library(class)
library(NbClust)
library(mclust)

libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"//config.R",sep='')) # MAC and Windows
mydb = dbConnect(MySQL(), user=LAuserID, password=LAuserpass, dbname=LAdb, host=LAserver)
rs<-dbSendQuery(mydb, "call SSPWide()")
SSPWide<-fetch(rs, n=-1)
dbClearResult(dbListResults(mydb)[[1]])
dbDisconnect(mydb)

SSPWide <- SSPWide[SSPWide$nonsAct<=60,]

SSPscaled <- data.frame(scale(SSPWide[,4:20]))

WWS <- rep(NA,6)
for(i in 1:6){
  paritioning <- kmeans(SSPscaled,centers = i,nstart = 5)
  WWS[i] <- paritioning$tot.withinss
}
WWS
plot(1:6,WWS,type = 'b')

ch_index <- function(kmeans_obj){
  k <- length(kmeans_obj$centers)
  n <- sum(kmeans_obj$size)
  ch <- (kmeans_obj$betweenss/(k-1))/((kmeans_obj$tot.withinss)/(n-k))
  ch
}

CH <- rep(NA,10)
for(i in 1:10){
  paritioning <- kmeans(SSPscaled,centers = i,nstart = 5)
  CH[i] <- ch_index(paritioning)
}
CH
plot(1:10,CH,type = 'b')

fviz_nbclust(SSPscaled, kmeans, method = "silhouette")+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,0.12,0.01))+
  geom_text(aes(label=round(y,3)), vjust=2)+
  ggtitle('Average silhouette method for k-means')

clust <- kmeans(SSPscaled,centers = 2,nstart = 5)

fviz_cluster(clust,
             SSPscaled,
             geom='point',
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

clust$size
SSPclust <- data.frame(SSPscaled,'clustkmeans2'=clust$cluster)

dataLong <- gather(SSPclust, variable, score, Att2Edu:whyUni)
ggplot(data=dataLong,aes(x=variable,y=score,col=factor(clustkmeans2)))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.7),alpha=0.5)+
  scale_color_discrete(name='Cluster')+
  scale_size_continuous(name='Point count')+
  ggtitle('k-means')

clust3 <- kmeans(SSPscaled,centers = 3,nstart = 5)

fviz_cluster(clust3,
             SSPscaled,
             geom='point',
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

clust3$size
SSPclust$clustkmeans3 <- clust3$cluster

dataLong <- gather(SSPclust, variable, score, Att2Edu:whyUni)
ggplot(data=dataLong,aes(x=variable,y=score,col=factor(clustkmeans3)))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.7),alpha=0.5)+
  scale_color_discrete(name='Cluster')+
  ggtitle('kmeans 3 clusters')

#For allocation of groups to new data insert object=kmean-model and data=Newdata. Rememeber to standardize the data.
#predict(object=,data=)

#Using medoids instead
fviz_nbclust(SSPscaled, pam, method = "silhouette")+
  theme_classic()+
  scale_y_continuous(breaks=seq(0,0.12,0.01),limits = c(0,0.12))+
  geom_text(aes(label=round(y,3)), vjust=2)+
  ggtitle('Average silhouette method for k-medoids')



fit <- pam(SSPscaled,2)

fviz_cluster(fit, 
             geom='point',
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

SSPclust$clustermedoids <- fit$clustering

dataLong <- gather(SSPclust, variable, score, Att2Edu:whyUni)
ggplot(data=dataLong,aes(x=variable,y=score,col=factor(clustermedoids)))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.7),alpha=0.5)+
  scale_color_discrete(name='Cluster')+
  ggtitle('kmedoids')

#hierarchical
dmatrix <- dist(SSPscaled, upper = T,diag = T)

hcomplete <- hclust(dmatrix,method = 'complete')
plot(hcomplete)
rect.hclust(hcomplete, k=2, border="red")
rect.hclust(hcomplete, k=3, border="blue")
rect.hclust(hcomplete, k=4, border="green")

nc <- NbClust(SSPscaled, min.nc = 2, method = 'complete')
fviz_nbclust(SSPscaled, hcut, method = "silhouette", hc_func='hclust', hc_method='complete')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,0.12,0.01),limits = c(0,0.12))+
  geom_text(aes(label=round(y,3)), vjust=2)+
  ggtitle('Average silhouette method for hierarchical, complete linkage')




groupscomplete3 <- cutree(hcomplete,k=3)

hsingle <- hclust(dmatrix,method = 'single')
plot(hsingle)
rect.hclust(hsingle, k=2, border="red")
#not a good structure

haverage <- hclust(dmatrix,method = 'average')
plot(haverage)
rect.hclust(haverage, k=2, border="red")
#not a good structure

hward <- hclust(dmatrix,method = 'ward.D')
plot(hward)
rect.hclust(hward, k=2, border="red")
rect.hclust(hward, k=3, border="red")
rect.hclust(hward, k=4, border="red")

nc <- NbClust(SSPscaled, min.nc = 2, method = 'ward.D')
fviz_nbclust(SSPscaled, hcut, method = "silhouette", hc_func='hclust', hc_method='ward.D')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,0.12,0.01),limits = c(0,0.12))+
  geom_text(aes(label=round(y,3)), vjust=2)+
  ggtitle('Average silhouette method for hierarchical, Wards method')




groupsward2 <- cutree(hward,k=2)

SSPclust[,c('hclustcomp3','hclustward2')] <- cbind(groupscomplete3,groupsward2)
dataLong <- gather(SSPclust, variable, score, Att2Edu:whyUni)

ggplot(data=dataLong,aes(x=variable,y=score,col=factor(hclustcomp3)))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.7),alpha=0.5)+
  scale_color_discrete(name='Cluster')+
  ggtitle('hierarchical clustering, complete linkage')

ggplot(data=dataLong,aes(x=variable,y=score,col=factor(hclustward2)))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.7),alpha=0.5)+
  scale_color_discrete(name='Cluster')+
  scale_size_continuous(name='Point count')+
  ggtitle('Hierarchical clustering, Wards method')


#For allocation of groups to new data insert the scaled data and the desired grouping vektor into
#For instance for complete linkage
#knn(SSPscaled,newdata.scaled,groupscomplete3)


table(SSPclust$clustermedoids)/nrow(SSPclust)
table(SSPclust$clustkmeans2)/nrow(SSPclust)
table(SSPclust$hclustcomp3)/nrow(SSPclust)
table(SSPclust$hclustward2)/nrow(SSPclust)
