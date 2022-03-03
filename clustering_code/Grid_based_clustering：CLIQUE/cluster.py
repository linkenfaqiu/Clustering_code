import numpy as np
# 选择聚类方法：clique 类
from pyclustering.cluster.clique import clique
# clique 可视化
from pyclustering.cluster.clique import clique_visualizer
# 构建训练数据
x = np.loadtxt(open("three_cluster.csv","rb"),usecols=(0),delimiter=",",skiprows=1,encoding='utf-8')     #读取数据文件，usecols=0表示取出第一列。
y = np.loadtxt(open("three_cluster.csv","rb"),usecols=(1),delimiter=",",skiprows=1,encoding='utf-8')
data = np.array([x, y]) #转换成np.array格式
data = data.T
data_M = np.array(data)
# 创建 CLIQUE 算法进行处理
# 定义每个维度中网格单元的数量，算法的重要参数之一
intervals = 14
# 密度阈值，算法的重要参数之二
threshold = 3
clique_instance = clique(data_M, intervals, threshold)

# 开始聚类过程并获得结果
clique_instance.process()
clique_cluster = clique_instance.get_clusters()

# 被认为是异常值的点（噪点）
noise = clique_instance.get_noise()
# CLIQUE形成的网格单元
cells = clique_instance.get_cells() 

length=len(clique_cluster)
print("Amount of clusters:", length)
for i in range(length):
    print('第',i+1,'个簇内样本点为：')
    print(clique_cluster[i])
# 显示由算法形成的网格
clique_visualizer.show_grid(cells, data_M) 
# 显示聚类结果
clique_visualizer.show_clusters(data_M, clique_cluster, noise) 