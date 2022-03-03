  library(psych)
  data=USArrests
  cortest.bartlett(cor(data))
  KMO(cor(data))	#输出的结果中，MSA越大，说明数据集越适合做主成分分析法
  data.pr <- princomp(data, cor = TRUE)
  summary(data.pr, loadings = TRUE)
#最后需要对USArrests数据集用降维后的公式进行计算，得到处理后的二维数据集。