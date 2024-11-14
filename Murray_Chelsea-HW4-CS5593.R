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
sse <- c(k2$tot.withinss, k3$tot.withinss, k4$tot.withinss, k5$tot.withinss, 
         k6$tot.withinss)
k <- 2:6
elb <- data.frame(k, sse)
ggplot(elb, aes(x=k, y=sse)) + geom_line() +ggtitle("Elbow/Knee")
ggsave("elbow.png")

#Based on the output, k=3 seems to be an adequate K-value, since the decrease
#in SSE drops sharply at first but "evens out" at the "elbow" of k=3.


#3.3: Bisecting K-means
bisect_km <- function(k, clusters, sses, means=NULL){
  #find cluster with highest sse
  high = 0
  ind = 0
  for(i in 1:length(clusters)){
    if (sses[i] > high){
      ind = i
      high = sses[i]
    }
  }
  #set current cluster for bisection to highest sse cluster
  data = clusters[ind]
  
  #3 trials
  t1 = kmeans(data, 2)
  t2 = kmeans(data, 2)
  t3 = kmeans(data, 2)
  #determine ideal trial
  kept = t1
  if(t1$tot.withinss > t2$tot.withinss){
    if(t2$tot.withinss > t3$tot.withinss){
      kept = t3
    }
    else {
      kept = t2
    }
  }
  else{
    if(t1$tot.withinss > t3$tot.withinss){
      kept = t3
    }
    else {
      kept = t1
    }
  }
  #for identified ideal trial, split the data into the 2 clusters
  an <- which(kept$cluster == 1)
  bn <- which(kept$cluster == 2)
  a <- data[-bn]
  b <- data[-an]
  #update sse vector
  sses[ind] = kept$withinss[1]
  sses.append(kept$withinss[2])
  #update cluster set
  clusters[ind] = a
  clusters.append(b)
  
  if(k > 1){
    bisect_km(k-1, clusters, sses, kmres)
  }
  else{
    cat("K =", k, "\n")
    #Total SSE for all clusters
    cat("Total SSE:", sum(sses), "\n")
    for (i in 1:k) {
      cat("Cluster", i, "\n")
      cat("SSE:", sses[i], "\n")
    }
  }
}

cluster = list(df)
sses = list(0)

bisect_km(k, cluster, sses)

