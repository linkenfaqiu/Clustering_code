  library(R2SWF)
  library(animation)
#设置当前文件的目录为工作目录
#运行代码时需要替换以下数据集"USArrests.txt"所在的路径
  data=read.table("USArrests.txt",header=T,na.strings = c("NA"))
#下行需要用户输入最终聚类簇数
  K<-readline(prompt = "请输入数据集最终的聚类簇数：")
  output = dev2swf({
  #mar设置边距参数，mgp控制坐标轴的位置
    par(mar = c(3, 3, 1, 1.5), mgp = c(1.5, 0.5, 0))
  #由于K-Means算法需要人为输入聚类个数，因此想要
    X=kmeans.ani(x=data,centers=K,hints = c("Move centers!", "Find cluster?"), pch = 1:K, col = 1:K)
  #将聚类结果输出
    cat("样本点的聚类结果为：\n")
    print(X$cluster)
    cat("每个簇中心点的坐标为：\n")
    print(X$centers)
    cat("最终的聚类动画文件保存于当前工作目录，当前工作目录为：\n")
  }, output = "K-means.swf")
#将swf转换成html保存后输出
  swf2html(output)

