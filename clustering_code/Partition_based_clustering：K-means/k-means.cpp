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
	// ������Ҫ��������ݣ������Զ�ά�����ţ������ڲ��ԣ�ʵ��ʹ��һ��Ҫ��ȡ�ļ��е����ݼ� 
	double a[5][2] = {{1,2},{3,3},{6,6},{3,4},{4,3}};
	int k = 3;	// �˹�ѡ��������ĸ��� 
	vector<Point>p;
	int cnt = sizeof(a) / sizeof(double) / 2;
	cout << "���ռ�����" << cnt << "��������\n";
	for(int i = 0; i < 5; i++)
	{
		int c = 0;
		p.push_back({a[i][0],a[i][1],0}); 
	}
	int temp_cluster = 1;
	cout << "---��ʼʱ���ѡ��k����������Ϊ��������---\n";
	for(int i = 1; i <= k; i++)
	{
		int r = rand() % cnt;
		p[r].cluster_num = temp_cluster;
		cout << "��" << i - 1 << "��������������Ϊ��(" << p[r].x << ", " << p[r].y << ")" << endl; 
		temp_cluster++;
	}
	int max_step = 100, step = 0;
	vector<Point>last;	// ��¼��һ�εľ�������
	while(step < max_step)
	{
		vector<Point>cluster;
		if(step == 0)
		{
			for(int i = 0; i < p.size(); i++)
			{
				if(p[i].cluster_num != 0)cluster.push_back(p[i]);	// ��һ���Ȱ�������ֵľ������ļӽ�ȥ 
			}
		}
		else	// ������ǵ�һ���Ļ�������Ҫ��ÿ���������е�ľ�ֵ 
		{ 
			cout << "\n---���е�" << step << "�ξ���---" << endl;
			vector<int>cnt(k, 0);
			for(int i = 0; i < k; i++)
			{
				cluster.push_back({0, 0, i + 1});	// ��ʼ���������ģ���ΪҪ������ 
			} 
			for(int i = 0; i < p.size(); i++)
			{
				int now_cluster = p[i].cluster_num;
				cluster[now_cluster - 1].x += p[i].x;
				cluster[now_cluster - 1].y += p[i].y;
				cnt[now_cluster - 1]++;
			}
			cout << "��ǰ�ľ������ķֱ�Ϊ��\n";
			for(int i = 0; i < k; i++)
			{
				cluster[i].x /= cnt[i];
				cluster[i].y /= cnt[i];
				cout << "��" << i << "��: (" << cluster[i].x << "," << cluster[i].y << ")";
				cout << ", ���а���" << cnt[i] << "��������" << endl;
			}
		}
		for(int i = 0; i < p.size(); i++)	// �����е��еĵ�i���� 
		{
			int c = 0;
			if(step == 0 && p[i].cluster_num != 0)continue;
			double min_dist = 1e9;
			for(int j = 0; j < cluster.size(); j++)
			{
				double dist = cal_dist(p[i], cluster[j]);	// �Ǹ���j���������ıȣ���Ҫд�ɵ�i���ˣ�����ȫ�����ൽһ��ȥ�� 
				if(dist < min_dist)
				{
					c = cluster[j].cluster_num;
					// cout << "��ǰ���ڵ�" << c << "����" << endl;
					min_dist = dist;
				}
			}
			p[i].cluster_num = c;    // ����i���㻮�ָ���c���������� 
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
				cout << "\n!!!�㷨�Ѿ���������ǰֹͣ����!!!\n\n"; 
				break;
			}
		}
		step++;
		last = cluster;
	}
	cout << "------�������------" << endl; 
	return 0;
} 
