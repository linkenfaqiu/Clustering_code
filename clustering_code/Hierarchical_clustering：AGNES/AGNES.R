AGNES<-function(a,cluster_num)
{
zb_num=2
#a为样本数据向量，zb_num是指标的个数,cluster_num为最终的簇数
#cluster_num必须大于1，代码中对簇数为2时会直接合并最后两类。

yp_num=length(a)/zb_num
#yp_num是样本的个数。
x=a[1:yp_num]			#x是X轴
y=a[(yp_num+1):length(a)] 			#x是Y轴
dim(a)<-c(yp_num,zb_num)
#设置初始样本点的半径，每次合并两个样本点后半径会相应增加
radius= rep(0.05,times=length(x))

b<-matrix(ncol=yp_num,nrow=yp_num)
for (i in 1:yp_num)
{
for (j in 1:yp_num)
{
if(i!=j)
{
b[i,j]=abs(a[i,1]-a[j,1])+abs(a[i,2]-a[j,2])
#这里只考虑了绝对距离，采用其他距离对上式进行修改即可。
}
if (i==j)
{
b[i,j]=0}
}
}
Dian=0
choose=0
row_name=0
num_dian=1
x_min=min(x)
x_max=max(x)
y_min=min(y)
y_max=max(y)
color=rainbow(yp_num)	#生成样本个数个彩虹色
#color=c("#00AFBB","#FF5044","#FFEF00","#0099CC","#797979","#CC6666")
row_all=paste("G",1:(2*yp_num-cluster_num),sep="")
for(r in 1:dim(b)[1])
{
row_name[r]=row_all[r]
}
dimnames(b)=list(row_name,row_name)
b0=b
print("距离矩阵D(0)")
print(b0)
cat("\n")
#pch是画图的样式,用实心的点来画
plot(x,y,pch=1,lwd=radius,xlim=c(x_min-1.5,x_max+1.5),ylim=c(y_min-1.5,y_max+1.5),col="white")
print(x_min)
print(x_max)
for(i in 1:length(x))
{
draw.circle(x[i],y[i],radius=radius[i],border=color[i], col=color[i])
}
Sys.sleep(1)
Step=yp_num-cluster_num
#以下是找最小位置
for(step in 1:Step)
{
min=b[2,1]
ik=1:(dim(b)[1])
count=nrow(b)
for(i in 2:nrow(b))
{
for(j in 1:(ncol(b)-1))
{
if(i>j)
{
if(b[i,j]<min)
min=b[i,j]
}
}
}
#找出最小距离点的位置
dingwei=(which(b==min, arr.ind=TRUE))
row2=dingwei[1,1] #行
col2=dingwei[1,2] #列
b2=b[-row2,-row2]
b3=b2[-col2,-col2]
count=yp_num+step
ik=ik[-row2]
ik=ik[-col2]
#去掉ik中的两个合并的点

choose[1]=row_name[col2]
choose[2]=row_name[row2]
for(p in 1:2)
{
Juleidian=which(choose[p]==row_all)  #找出在row_all里对应的位置
if(Juleidian<=yp_num)
{
Dian[num_dian]=Juleidian
num_dian=num_dian+1
}
}
row_name=row_name[-row2]
row_name=row_name[-col2]

d1=0
for(k in 1:length(ik))
{
m=ik[k]
d1[k]=min(b[row2,m],b[col2,m])
}

d2=matrix(d1,nrow=1)
b4=rbind(b3,d2)
d1[length(d1)+1]=0
b4=cbind(b4,d1)
b=b4
row_name[dim(b)]=row_all[count]
dimnames(b)=list(row_name,row_name)
#重新给b的行列名赋值
if(dim(b)[1]==2){
last=which(row_name[1]==row_all)
Dian[length(Dian)+1]=last
}
print(sprintf('距离矩阵D(%d)',step)) 
print(b)
print("依次归并的点：")
print(Dian)

cat("\n")
u=0
#for (i in 1:length(Dian))
#{
#color[Dian[i]]="red"
#}
#pch是画图的样式,用实心的点来画
new= length(x)+1
x[new]=(x[row2]+x[col2])/2
y[new]=(y[row2]+y[col2])/2
radius[new]=sqrt(radius[row2]^2+radius[col2]^2)
color[new]=color[row2]
#下一行代码用来画框住两个样本点的大圆圈
draw.circle(x[new],y[new], radius= sqrt((x[row2]-x[col2])^2+(y[row2]-y[col2])^2)/2+0.3, border="red",lwd=2)
Sys.sleep(0.5) 
#下两行代码用来擦掉需要被合并的点
draw.circle(x[row2],y[row2],radius=radius[row2]+0.05,border="white", col="white")
draw.circle(x[col2],y[col2],radius=radius[col2]+0.05,border="white", col="white")
draw.circle(x[new],y[new], radius= sqrt((x[row2]-x[col2])^2+(y[row2]-y[col2])^2)/2+0.3, border="white",lwd=2)
draw.circle(x[new],y[new],radius=radius[new],col=color[new],border=color[new])
Sys.sleep(0.5) 
#最后两类的时候追加判断一下
x=x[-row2]
x=x[-col2]
y=y[-row2]
y=y[-col2]
radius=radius[-row2]
radius=radius[-col2]
color=color[-row2]
color=color[-col2]
if(dim(b)[1]==2){
print("将这两类聚为一类后聚类过程结束，所有类合并为一类")
new_x=(x[1]+x[2])/2
new_y=(y[1]+y[2])/2
new_r=sqrt(radius[1]^2+radius[2]^2)
draw.circle(new_x,new_y, radius= sqrt((x[1]-x[2])^2+(y[1]-y[2])^2)/2+0.3, border="red",lwd=2)
Sys.sleep(0.5) 
draw.circle(x[1],y[1],radius=radius[1]+0.05,border="white", col="white")
draw.circle(x[2],y[2],radius=radius[2]+0.05,border="white", col="white")
draw.circle(new_x, new_y, radius= sqrt((x[1]-x[2])^2+(y[1]-y[2])^2)/2+0.3, border="white",lwd=2)
draw.circle(new_x, new_y,radius=new_r,border=color[2], col=color[2])
Sys.sleep(0.5) 
}
}   #for STEP
}