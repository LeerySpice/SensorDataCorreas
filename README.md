---
title: "RClustering - Nivel1"
author: "EJA"
date: "21-07-2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(factoextra)
library(NbClust)
library(reshape2)
library(plotly)
```

Se carga la base de datos con fecha 19/07 importada en ImportBson.R usando *mongolite* y mongoexport en json y luego generando base *Rda*.
Se agrega Nivel1.csv con dirección MAC de los sensores asociados.

```{r data}
load("Data/data.Rda")
# Se quitan columnas innecesarias 
df <- na.omit(data.frame(key = substr(data[,2],11,27) ,data[,3][,-c(1,66,99)]))
# MIC out
df <- df[,-c(2:9,97:74)]
df <- df[,c(1,58:65,2:57)]
# Read Nivel1
nivel1 <- read.csv("Data/Nivel1.csv")
```

Se filtra por MAC los sensores asociados a Nivel1.
Se guargan estos valores en *df2*. Se genera vector de keys. Se genera dataframe escalando la base de datos completa para kmeans.

```{r df2}
# df2 filtrado por correa
df2 <- df[1,]
#b <- rep(0)
  
for (i in 1:length(nivel1$MAC)) {
  index <- which(as.character(df$key)==as.character(nivel1$MAC[i]))
  df2 <- rbind(df2,df[index,])
}  
df2 <- df2[-1,] 
row <- as.character(df2[,1])
df2 <- df2[,-1]
df2_s <- data.frame(scale(df2))
```

Se realiza test de clusterización via *NbClust*. Para agregar mayor clusterización de datos outlayer, se generan k=6 centros.

```{r Clustering}
# Revisar número de cluster via NBclust
#nb <- NbClust(df2_s, distance = "euclidean", min.nc = 2, max.nc = 8, method = "kmeans")
#fviz_nbclust(nb)
k = 6
set.seed(7)
km.res <- kmeans(df2_s,k,nstart=25)
fviz_cluster(km.res,data=df2_s,
             palette=c("#000099","#00AFBB","#E7B800","#FC4E07", "#23B439", "#A328C7" ),
             ellipse.type="euclid",#Concentrationellipse
             star.plot=TRUE,#Addsegmentsfromcentroidstoitems
             #ggtheme=theme_minimal()
)
```

Se vuelve a asignar keys a base. Se guardan las bases en csv y Rda. Se visualizan los sensores asociados al cluster 1. Se observan 61 mediciones con 3 sensores 

```{r Merge}
# Merge DB
df2 <- data.frame(key=row, df2, kmeans=as.factor(km.res$cluster))
table(data.frame(km.res$cluster))
a <- as.character(df2$key[which(df2$kmeans==4)])
b <- rep(0); Urg <- rep(0) 
for (z in 1:length(table(a))) {
  b[z] <- cbind(names(table(a)[z]))
  Urg[z] <- paste("M",nivel1$MESA[which(nivel1$MAC==b[z])],"-" ,    as.character(nivel1$ESTACION[which(nivel1$MAC==b[z])]), sep = "")
}
t(Urg)
```


```{r Means}
for (x in 1:k) {
  assign(paste0("i",x), which(df2$kmeans==x))
  assign(paste0("PDC",x), colMeans(df2[get(paste0("i",x)),][,-c(1,66)]))
}
PC <- data.frame(PDC1,PDC2,PDC3,PDC4,PDC5,PDC6,x=rownames(data.frame(PDC1)) )
```

```{r Plots, warning=FALSE}
PC.long <- melt(PC, id.vars="x", measure=c("PDC1", "PDC2", "PDC3","PDC4", "PDC5", "PDC6"))
ggplot(data = PC.long, aes(x,value, colour = variable, group=1)) +  
  geom_line(size=1) + geom_jitter() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust = 1, size=5)) 
ay <- list(tickfont = list(color = "red"), overlaying = "y", side = "right", title = "# Mean Cluster")
fig <- plot_ly()
for (x in 1:k) {
  fig <- fig %>% add_lines(x = PC$x, PC[[x]], name = names(PC[x]), yaxis = "Counts")
}
#viewer
fig
```

Se establecen los estados a partir de los clusters y se guarda en columna *state* como factores

```{r states, warning=FALSE}
df2 <- data.frame(df2, state=factor(rep(0), levels = c("3","2","1","0")))
df2$state[i4] <- as.factor("3")
df2$state[i3] <- as.factor("2")
df2$state[i6] <- as.factor("1")
df2$state[i5] <- as.factor("1")
df2$state[i1] <- as.factor("1")
df2$state[i2] <- as.factor("0")
table(df2$state)
head(df2[,-c(1:60)])
saveRDS(df2,"Cluster/Nivel1_cluster.Rda")
write.csv(df2, file="Cluster/Nivel1_cluster.csv")
```
