library(ggplot2)
library(factoextra)
library(NbClust)
library(reshape2)
library(plotly)

load("Data/data.Rda")

# Mediciones 
df <- na.omit(data.frame(key = substr(data[,2],11,27) ,data[,3][,-c(1,66,99)]))
# MIC out
df <- df[,-c(2:9,97:74)]
df <- df[,c(1,58:65,2:57)]
# Read Nivel1
nivel1 <- read.csv("Data/Nivel1.csv")

# df2 filtrado por correa
df2 <- df[1,]
  
for (i in 1:length(nivel1$MAC)) {
  index <- which(as.character(df$key)==as.character(nivel1$MAC[i]))
  df2 <- rbind(df2,df[index,])
}  

df2 <- df2[-1,] 
row <- as.character(df2[,1])
df2 <- df2[,-1]
df2_s <- data.frame(scale(df2))


# Revisar número de cluster via NBclust
set.seed(7)
nb <- NbClust(df2_s, distance = "euclidean", min.nc = 2, max.nc = 8, method = "kmeans")

jpeg("Cluster/optimz_nb.cluster.png", width = 800, height = 600)
fviz_nbclust(nb)
dev.off()

km.res <- kmeans(df2_s,6,nstart=25)

jpeg("Cluster/kmeans_cluster.png", width = 800, height = 600)
fviz_cluster(km.res,data=df2_s,
             palette=c("#000099","#00AFBB","#E7B800","#FC4E07", "#23B439", "#A328C7" ),
             ellipse.type="euclid",#Concentrationellipse
             star.plot=TRUE,#Addsegmentsfromcentroidstoitems
             #ggtheme=theme_minimal()
)
dev.off()


# Merge DB
df2 <- data.frame(key=row, df2, kmeans=as.factor(km.res$cluster))
table(data.frame(km.res$cluster))
saveRDS(df2,"Cluster/Nivel1_cluster.Rda")
write.csv(df2, file="Cluster/Nivel1_cluster.csv")
df2 <-df2[,-1] 

i <- which(df2$kmeans==1)
a <- df2$key[i]
a <- data.frame(a1="0013a200419a6833",a2="0013a200419a4d6a",a3="0013a200419a4e13")
nivel1$MESA[which(nivel1$MAC==as.character(a$a1))]
nivel1$MESA[which(nivel1$MAC==as.character(a$a2))]
nivel1$MESA[which(nivel1$MAC==as.character(a$a3))]

for (x in 1:4) {
  assign(paste0("i",x), which(df2$kmeans==x))
  assign(paste0("DC",x), df2[get(paste0("i",x)),][,-97])
  assign(paste0("PDC",x), colMeans(get(paste0("DC",x))))
}

PC <- data.frame(PDC1,PDC2,PDC3,PDC4,x=rownames(data.frame(PDC1)) )

jpeg("Cluster/Nivel1_main_cluster.png", width = 800, height = 600)
PC.long <- melt(PC, id.vars="x", measure=c("PDC1", "PDC2", "PDC3", "PDC4"))
ggplot(data = PC.long, aes(x,value, colour = variable, group=1)) +  
  geom_line(size=1) + geom_jitter() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust = 1, size=5)) 
dev.off()

ay <- list(tickfont = list(color = "red"), overlaying = "y", side = "right", title = "# TEST/ día")
fig <- plot_ly()

for (x in 1:4) {
  fig <- fig %>% add_lines(x = PC$x, PC[[x]], name = names(PC[x]), yaxis = "Confirmados acum")
}

#viewer
fig



