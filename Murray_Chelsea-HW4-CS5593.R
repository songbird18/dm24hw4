#Written for Data Mining Fall 2024 by Chelsea Murray
#Please update working directory as necessary for your machine.
setwd("C:/Users/Chelsea/Documents/FA24/Data Mining/Homework/homework 4")

dpath = paste(getwd(), "/seeds/seeds_dataset.txt", sep="")
df<-read.table(dpath, header=FALSE, sep="")

#3.2: K-Means and Elbow/Knee

#Cluster with k=2 through 6
k2 = kmeans(df, 2)
k3 = kmeans(df, 3)
k4 = kmeans(df, 4)
k5 = kmeans(df, 5)
k6 = kmeans(df, 6)

#Function for printing results
#k = number of clusters (ex. k=2)
#res = kmeans result (ex. res=k2)
printresult<-function(k, res){
  cat("K =", k, "\n")
  #Total SSE for all clusters
  cat("Total SSE:", res$tot.withinss, "\n")
  #convert center indexes to a data frame
  kc = data.frame(seq(1, 8*k, by=k))
  for (i in 2:k){
    col = toString(i)
    kc[col] <- data.frame(seq(i, 8*k, by=k))
  }
  for (i in 1:k) {
    cat("Cluster", i, "\n")
    cat("SSE:", res$withinss[i], "\n")
    cat("Centers for 8 columns in Cluster", i, ":", res$centers[kc[,i]], "\n")
    cat("Items in this cluster:", which(res$cluster == i), "\n\n")
  }
}

#K=x results
printresult(2, k2)
printresult(3, k3)
printresult(4, k4)
printresult(5, k5)
printresult(6, k6)

#Elbow/knee
library(ggplot2)
sses <- c(k2$tot.withinss, k3$tot.withinss, k4$tot.withinss, k5$tot.withinss, 
         k6$tot.withinss)
x <- 2:6
elb <- data.frame(x, sses)
ggplot(elb, aes(x=x, y=sses)) + geom_line()
ggsave("elbow.png")


