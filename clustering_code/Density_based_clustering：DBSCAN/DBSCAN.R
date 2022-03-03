#data为数据集或距离矩阵，eps为划分考察领域半径，minPts为密度阈值，scale为数据是否进行标准化
#method为数据集类型，"hybrid", 表示数据是距离矩阵，"raw",表示数据是原始数据，"dist"表示原始数据
#showplot为是否输出聚类结果示意图，arrow为是否画出箭头
DBSCAN<-function (data, eps, MinPts = 5, scale = FALSE, method = c("hybrid", 
    "raw", "dist"), seeds = TRUE, showplot = FALSE, arrow=FALSE,
    countmode = NULL) 
{
    distcomb <- function(x, data) {	#求距离
        data <- t(data)	
        temp <- apply(x, 1, function(x) {
            sqrt(colSums((data - x)^2))
        })
        if (is.null(dim(temp))) 
            matrix(temp, nrow(x), ncol(data))
        else t(temp)
    }

    method <- match.arg(method)
    data <- as.matrix(data)
    n <- nrow(data)
    if (scale) 
        data <- scale(data)
    classn <- cv <- integer(n)
    isseed <- logical(n)
    cn <- integer(1)
    for (i in 1:n) {	#遍历每个点,对一个点如果它是核心点，就直接判断其他点，因此还在当前的i值中

        if (i %in% countmode) 
            cat("Processing point ", i, " of ", n, ".\n")  
        unclass <- (1:n)[cv < 1]    #判断是否已分类，一开始每个点都以自己为聚类中心
        if (cv[i] == 0) {
            if (method == "dist") {
                reachables <- unclass[data[i, unclass] <= eps]    #找出以点i为中心，以eps为半径范围内的点，加入到可达集
                #print(reachables)
                #cat('\n')
                #for(i in 1:length(reachables)){
                #    draw.circle(data[reachables[i],1],data[reachables[i],2],radius=0.3,border="red")
                #    Sys.sleep(0.5)
                #}
            }
            else {
                reachables <- unclass[as.vector(distcomb(data[i, 
                  , drop = FALSE], data[unclass, , drop = FALSE])) <= 
                  eps]
                #for(i in 1:length(reachables)){
                #    draw.circle(data[reachables[i],1],data[reachables[i],2],radius=0.3,border="red")
                #    Sys.sleep(0.5)
                #}
            }
            if (length(reachables) + classn[i] < MinPts) 
                cv[i] <- (-1)
            else {
                cn <- cn + 1
                cv[i] <- cn
                isseed[i] <- TRUE
                reachables <- setdiff(reachables, i)
                unclass <- setdiff(unclass, i)
                classn[reachables] <- classn[reachables] + 1
                while (length(reachables)) {
                  if (showplot) 
                    plot(data, col = 1 + cv, pch = 1 + isseed)
                    Sys.sleep(0.3)
                    if(arrow==TRUE){
                        for(i in 1:length(reachables)){
                            draw.circle(data[reachables[i],1],data[reachables[i],2],radius=eps,border='red')
                            Sys.sleep(0.3)
                            if(i!=1){
                                #画箭头的函数，length用来控制箭头的头部大小
                                arrows(data[reachables[i-1],1],data[reachables[i-1],2],data[reachables[i],1],data[reachables[i],2],col='red',length=0.1)
                                Sys.sleep(0.3)
                            }
                        }  
                    }
                  cv[reachables] <- cn
                  ap <- reachables
                  reachables <- integer()
                  if (method == "hybrid") {
                    tempdist <- distcomb(data[ap, , drop = FALSE], 
                      data[unclass, , drop = FALSE])
                    frozen.unclass <- unclass
                  }
                  for (i2 in seq(along = ap)) {
                    j <- ap[i2]
                    if (showplot > 1) 
                      plot(data, col = 1 + cv, pch = 1 + isseed)
                    if (method == "dist") {
                      jreachables <- unclass[data[j, unclass] <= 
                        eps]
                    }
                    else if (method == "hybrid") {
                      jreachables <- unclass[tempdist[i2, match(unclass, 
                        frozen.unclass)] <= eps]
                    }
                    else {
                      jreachables <- unclass[as.vector(distcomb(data[j, 
                        , drop = FALSE], data[unclass, , drop = FALSE])) <= 
                        eps]
                    }
                    if (length(jreachables) + classn[j] >= MinPts) {
                      isseed[j] <- TRUE
                      cv[jreachables[cv[jreachables] < 0]] <- cn
                      reachables <- union(reachables, jreachables[cv[jreachables] == 
                        0])
                    }
                    classn[jreachables] <- classn[jreachables] + 1
                    unclass <- setdiff(unclass, j)
                  }
                }
            }
        }
        if (!length(unclass)) 
            break
    }
    rm(classn)
    if (any(cv == (-1))) {
        cv[cv == (-1)] <- 0
    }
    if (showplot) 
        plot(data, col = 1 + cv, pch = 1 + isseed)
        Sys.sleep(0.3)
    out <- list(cluster = cv, eps = eps, MinPts = MinPts)
    if (seeds && cn > 0) {
        out$isseed <- isseed
    }
    class(out) <- "dbscan"
    out
}