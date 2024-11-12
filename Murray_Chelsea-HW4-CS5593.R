#Written for Data Mining Fall 2024 by Chelsea Murray
#Please update working directory as necessary for your machine.
setwd("C:/Users/Chelsea/Documents/FA24/Data Mining/Homework/homework 4")

dpath = paste(getwd(), "/seeds/seeds_dataset.txt", sep="")
df<-read.table(dpath, header=FALSE, sep="")