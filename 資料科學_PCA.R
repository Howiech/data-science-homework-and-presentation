dataset=read.csv("/NTNUBA/R scripts/data/crime.csv") 
head(dataset)

#Figure 1
newData=dataset[,-c(1,2)] 
.PC= princomp(newData ,cor=TRUE)
biplot(.PC) 

#Figure 2
rownames(newData)=dataset[,1] 
.PC= princomp(newData ,cor=TRUE)
dev.new();biplot(.PC,cex=0.8) 
abline(h=0,v=0,lty=3,col=("blue"))

plot(.PC, col=c("blue"))
screeplot(.PC, col=c("lightblue"))


summary(.PC, loadings=TRUE)
names(.PC)
.PC$sdev

.PC$scores 
predict(.PC) 

loadings(.PC)
round(unclass(loadings(.PC))[,1:2],4)


# visualization
library("FactoMineR")
library("factoextra")
dev.new();fviz_eig(.PC)

#PCA graph
dev.new();fviz_pca_ind(.PC,
                       col.ind = "cos2",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE    
)

#Graph 2
dev.new();fviz_pca_var(.PC,
                       col.var = "contrib", 
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE    
)

#Biplot 
dev.new();fviz_pca_biplot(.PC, repel = TRUE,
                          col.var = "#2E9FDF", # Variables color
                          col.ind = "#696969"  # Individuals color
)




