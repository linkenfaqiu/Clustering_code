#include<cstdio>
#include<iostream>
#include<vector>
#include<cstdlib>
#include<cmath>
using namespace std;
typedef struct Point
{
	double x;
	double y;
	int cluster_num;
	Point(double a, double b, int c)
	{
		x = a;
		y = b;
		cluster_num = c;
	}
}point;
double cal_dist(point a, point b)
{
	return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
}
int main()
{
	double eps_max = 1e-5;
	// 以下是要聚类的数据，这里以二维数组存放，仅用于测试，实际使用一般要读取文件中的数据集 
	double a[5][2] = {{1,2},{3,3},{6,6},{3,4},{4,3}};
	int k = 3;	// 人工选择聚类中心个数 
	vector<Point>p;
	int cnt = sizeof(a) / sizeof(double) / 2;
	cout << "共收集到有" << cnt << "个样本点\n";
	for(int i = 0; i < 5; i++)
	{
		int c = 0;
		p.push_back({a[i][0],a[i][1],0}); 
	}
	int temp_cluster = 1;
	cout << "---初始时随机选择k个样本点作为聚类中心---\n";
	for(int i = 1; i <= k; i++)
	{
		int r = rand() % cnt;
		p[r].cluster_num = temp_cluster;
		cout << "第" << i - 1 << "个聚类中心坐标为：(" << p[r].x << ", " << p[r].y << ")" << endl; 
		temp_cluster++;
	}
	int max_step = 100, step = 0;
	vector<Point>last;	// 记录上一次的聚类中心
	while(step < max_step)
	{
		vector<Point>cluster;
		if(step == 0)
		{
			for(int i = 0; i < p.size(); i++)
			{
				if(p[i].cluster_num != 0)cluster.push_back(p[i]);	// 第一次先把随机划分的聚类中心加进去 
			}
		}
		else	// 如果不是第一步的话，就需要求每个簇内所有点的均值 
		{ 
			cout << "\n---进行第" << step << "次聚类---" << endl;
			vector<int>cnt(k, 0);
			for(int i = 0; i < k; i++)
			{
				cluster.push_back({0, 0, i + 1});	// 初始化聚类中心，因为要重新算 
			} 
			for(int i = 0; i < p.size(); i++)
			{
				int now_cluster = p[i].cluster_num;
				cluster[now_cluster - 1].x += p[i].x;
				cluster[now_cluster - 1].y += p[i].y;
				cnt[now_cluster - 1]++;
			}
			cout << "当前的聚类中心分别为：\n";
			for(int i = 0; i < k; i++)
			{
				cluster[i].x /= cnt[i];
				cluster[i].y /= cnt[i];
				cout << "第" << i << "个: (" << cluster[i].x << "," << cluster[i].y << ")";
				cout << ", 其中包含" << cnt[i] << "个样本点" << endl;
			}
		}
		for(int i = 0; i < p.size(); i++)	// 对所有点中的第i个点 
		{
			int c = 0;
			if(step == 0 && p[i].cluster_num != 0)continue;
			double min_dist = 1e9;
			for(int j = 0; j < cluster.size(); j++)
			{
				double dist = cal_dist(p[i], cluster[j]);	// 是跟第j个聚类中心比，不要写成第i个了，否则全都聚类到一个去了 
				if(dist < min_dist)
				{
					c = cluster[j].cluster_num;
					// cout << "当前属于第" << c << "个簇" << endl;
					min_dist = dist;
				}
			}
			p[i].cluster_num = c;    // 将第i个点划分给第c个聚类中心 
		}
		if(last.size() != 0)
		{
			double eps = 0;
			for(int i = 0; i < cluster.size(); i++)
			{
				double d = cal_dist(cluster[i], last[i]);
				eps += d;
			}
			if(eps < eps_max)
			{
				cout << "\n!!!算法已经收敛，提前停止聚类!!!\n\n"; 
				break;
			}
		}
		step++;
		last = cluster;
	}
	cout << "------聚类结束------" << endl; 
	return 0;
} 
