  library(plotrix)
#在RStudio中可以设置绝对路径，因此使用RGui时要注意修改路径。
  a=read.csv("USArrests.txt",header=T,sep="\t",na.strings = c("NA"))
  a=as.matrix(a)
  a=as.vector(a)
  AGNES(a,2)
#AGNES(a,4)