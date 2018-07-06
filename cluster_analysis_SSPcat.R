library(RMySQL)
library(cluster)
library(factoextra)
library(tidyr)
library(ggplot2)

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
  theme_classic()

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
SSPscaled$cluster <- clust$cluster
for(i in 1:17){
  plot(SSPscaled[,i]~factor(SSPscaled$cluster),ylab = names(SSPscaled)[i],xlab='Cluster')
}


dataLong <- gather(SSPscaled, variable, score, Att2Edu:whyUni)
ggplot(data=dataLong,aes(x=variable,y=score,col=factor(cluster)))+
  geom_boxplot()+
  scale_color_discrete(name='Cluster')

clust3 <- kmeans(SSPscaled,centers = 3,nstart = 5)

fviz_cluster(clust3,
             SSPscaled,
             geom='point',
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

clust3$size
SSPscaled$cluster3 <- clust3$cluster

dataLong <- gather(SSPscaled, variable, score, Att2Edu:whyUni)
ggplot(data=dataLong,aes(x=variable,y=score,col=factor(cluster3)))+
  geom_boxplot()+
  scale_color_discrete(name='Cluster')

#Using medioids instead
fviz_nbclust(SSPscaled, pam, method = "silhouette")+
  theme_classic()

fit <- pam(SSPscaled,2)

fviz_cluster(fit, 
             geom='point',
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

SSPscaled$clusterMedioids <- fit$clustering

dataLong <- gather(SSPscaled, variable, score, Att2Edu:whyUni)
ggplot(data=dataLong,aes(x=variable,y=score,col=factor(clusterMedioids)))+
  geom_boxplot()+
  scale_color_discrete(name='Cluster')
