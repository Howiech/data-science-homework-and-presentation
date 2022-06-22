dataset=read.csv("/NTNUBA/R scripts/data/crime.csv")
head(dataset)

newData=dataset[,-c(1,2)] 
rownames(newData)=dataset[,1]  

E.dist <- dist(newData, method="euclidean") 
M.dist <- dist(newData, method="manhattan")


dev.new()
par(mfrow=c(1,2)) 
# Euclidean distance
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="Euclidean distance")

# Manhattan distance
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="Manhattan")
par(mfrow=c(1,1))

hclust(E.dist, method="single")   
hclust(E.dist, method="complete") 
hclust(E.dist, method="average")  
hclust(E.dist, method="centroid") 
hclust(E.dist, method="ward.D2")  

# Euclidean distance
E.dist <- dist(newData, method="euclidean")      
h.cluster <- hclust(E.dist, method="ward.D2") 
dev.new()
plot(h.cluster)
abline(h=2000, col="red")


cut.h.cluster <- cutree(h.cluster, k=4)  
cut.h.cluster                            
table(cut.h.cluster, dataset[,2]) 


# kmeans
kmeans.cluster <- kmeans(newData, centers=4) 


kmeans.cluster$withinss

table(kmeans.cluster$cluster, dataset[,1])   

#Visualization
require(factoextra)
fviz_cluster(kmeans.cluster,           
             data = newData,           
             geom = c("point","text"), 
             frame.type = "norm")      

require(cluster)


kmedoid.cluster <- pam(newData, k=4) 


kmedoid.cluster$objective


table(kmedoid.cluster$clustering, dataset[,2]) 


require(factoextra)
fviz_cluster(kmedoid.cluster,       
             data = newData,        
             geom = c("point","text"),     
             frame.type = "norm")   
