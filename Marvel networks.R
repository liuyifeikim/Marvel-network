library(igraph)
library(ggplot2)
library(plyr)
library(dplyr)
library(sna)

#nodes
nodes<-read.csv('../../../Kaggle/Marvel networks/nodes.csv') #表明每个点是hero还是comic
nodes$node<-as.character(nodes$node)
str(nodes)
table(nodes$type) #6439个hero，12651个comic

#edges
edges<-read.csv('../../../Kaggle/Marvel networks/edges.csv')  #每个hero在什么comic中出现
edges$hero<-as.character(edges$hero)
edges$comic<-as.character(edges$comic)
str(edges)
length((unique(edges$hero)))
length((unique(edges$comic))) #hero和comic之和为nodes中的type，6439个hero，12651个comic

comic_size<-ddply(edges,.(comic),summarize,
                  size=length(hero),#每个comic的英雄数量
                  pair=length(hero)*(length(hero)-1)/2)#每个comic的英雄关系数量
head(comic_size)
sum(comic_size$pair) #以comic为边界，总关系之和理论值=579171

comic_size<-comic_size[order(-comic_size$size),] #按size排序
comic_size50<-head(comic_size,n=50L)
ggplot(comic_size50,aes(reorder(comic,size),size))+geom_bar(stat='identity')+coord_flip() 

hero_size<-ddply(edges,.(hero),summarize,size=length(comic)) #每个hero出现的comic数量
hero_size<-hero_size[order(-hero_size$size),] #按size排序
hero_size50<-head(hero_size,n=50L)
ggplot(hero_size50,aes(reorder(hero,size),size))+geom_bar(stat='identity')+coord_flip() 

#dplyr方法同组hero两两配对
edges_sum<-ddply(edges,~comic,summarise,size=length(hero))
two<-edges_sum[edges_sum$size>=2,]$comic #hero>=2的comic列表
edges_two<-edges[edges$comic %in% two,] #hero>=2的数据
by_comic<-edges_two%>%group_by(comic)
edges_pair<-do(by_comic,data.frame(t(combn(.$hero,2))))
edges_pair<-data.frame(edges_pair)
edges_pair$comic<-NULL
colnames(edges_pair)[1]<-'hero1'
colnames(edges_pair)[2]<-'hero2'
str(edges_pair) #579171,符合理论总数

#A smaller network:在不同comic中相同的关系只保留1个
edges_pair_s<-edges_pair
edges_pair_s$index<-strsplit(paste(edges_pair_s$hero1,edges_pair_s$hero2),'') #生成索引列,将hero的名称字符合并并分割为字母
edges_pair_s$index<-sapply(edges_pair_s$index,sort) #将字母排序,两个hero相同的对,字母的排列相同
edges_pair_s<-edges_pair_s[!(duplicated(edges_pair_s$index)),] #删除相同的配对
edges_pair_s<-edges_pair_s[,-ncol(edges_pair_s)] #删除索引列
nrow(edges_pair_s) #171593

#network
hero_network<-read.csv('../../../Kaggle/Marvel networks/hero-network.csv') #在同一个comic中出现的hero为一对，应该为无向关系
hero_network$hero1<-as.character(hero_network$hero1) #factor to character
hero_network$hero2<-as.character(hero_network$hero2)
str(hero_network) #574467个关系,和理论总数579171相比缺失了一部分关系

length(unique(hero_network$hero1)) #hero1=6211
length(unique(hero_network$hero2)) #hero2=6173
length(unique(hero_network$hero1))*(length(unique(hero_network$hero1))-1) #理论关系总数（不以comic为边界）=38570310

#A smaller network:在不同comic中相同的关系只保留1个
hero_network_s<-hero_network
hero_network_s$index<-strsplit(paste(hero_network_s$hero1,hero_network_s$hero2),'') #生成索引列,将hero的名称字符合并并分割为字母
hero_network_s$index<-sapply(hero_network_s$index,sort) #将字母排序,两个hero相同的对,字母的排列相同
hero_network_s<-hero_network_s[!(duplicated(hero_network_s$index)),] #删除相同的配对
hero_network_s<-hero_network_s[,-ncol(hero_network_s)] #删除索引列
nrow(hero_network_s) #167169


#describe
net<-graph_from_data_frame(edges_pair_s,directed=F) #转化为net对象，全部为对等关系

vcount(net) #number of nodes=6421
ecount(net) #number of edges=171593

head(V(net),10)
head(E(net),10)

#度中心性（degree centrality）
head(sort(degree(net,mode='in'),decreasing=TRUE)) #in-degree
head(sort(degree(net,mode='out'),decreasing=TRUE)) #out-degree
head(sort(degree(net,mode='all'),decreasing=TRUE)) #all-degree,无向网络以上三个值全部相同
head(sort(degree(net,mode='all',normalized=T),decreasing=TRUE)) #标准化degree，除以n-1

sum(degree(net,mode='in')) 
sum(degree(net,mode='out')) 
sum(degree(net,mode='all')) #无向网络以上三个值全部相同

(sum(max(degree(net))-degree(net)))/((vcount(net)-1)*(vcount(net)-2)) #群组度中心性
sd(degree(net)) #群组度中心性方差
mean(degree(net)) #度数平均值
mean(degree(net))/(vcount(net)-1) #标准化度数平均值=edge_density(net)
edge_density(net) #density，现存关系数与所有可能关系数比值

#接近中心性（closeness centrality）
head(sort(closeness(net),decreasing=TRUE),10) #closeness
head(sort(closeness(net,normalized=T),decreasing=TRUE),10) #标准化closeness，乘以n-1
(sum(max(closeness(net,normalized=T))-closeness(net,normalized=T)))/
  ((vcount(net)-2)*(vcount(net)-1)/((2*vcount(net)-3))) #群组接近中心性
sd(closeness(net,normalized=T)) #群组接近中心性方差

#中介中心性（betweeness centrality）
head(sort(betweenness(net),decreasing=TRUE),10) #betweenness
head(sort(betweenness(net,normalized=T),decreasing=TRUE),10) #标准化betweenness，Bnorm=2*B/(n-1)(n-2)
(2*sum(max(betweenness(net))-betweenness(net)))/
  (((vcount(net)-1)^2)*(vcount(net)-2)) #群组中介中心性
(sum(max(betweenness(net,normalized=T))-betweenness(net,normalized=T)))/
  (vcount(net)-1) #群组中介中心性简化版

#其他
distances(net) #最短距离矩阵
reciprocity(net) #reciprocity，双向关系数与全部关系数比值，全部为双向关系=1
diameter(net,directed=F) #diameter，两点之间最长距离
mean_distance(net,directed=F) #每个点到其余点最短距离均值

transitivity(net,type = 'globalundirected') #网络聚类系数,整体,无向图
transitivity(net,type = 'localundirected') #网络聚类系数,每个点,无向图
transitivity(net,type = 'localaverageundirected') #网络聚类系数,每个点,无向图,取均值

#==================================================================================================

#将edge_pair转化为有向图,假定为有向（实际无向）
edges_pair_d<-edges_pair[!duplicated(edges_pair),] #方向相同的人和关系保留1个
nrow(edges_pair_d) #171647

net_d<-graph_from_data_frame(edges_pair_d,directed = T)
net_d

reciprocity(net_d) #双向关系占所有关系比例

#度中心性（degree centrality）
head(sort(degree(net_d,mode='in',normalized = T),decreasing=TRUE)) #in-degree
head(sort(degree(net_d,mode='out',normalized = T),decreasing=TRUE)) #out-degree
(sum(max(degree(net_d))-degree(net_d)))/((vcount(net)-1)^2) #群组度中心性,分母为(n-1)平方

#接近中心性（closeness centrality）
head(sort(closeness(net_d,mode = 'in'),decreasing=TRUE),10) #closeness
head(sort(closeness(net_d,mode = 'out'),decreasing=TRUE),10) #closeness

head(sort(closeness(net_d,mode = 'in',normalized = T),decreasing=TRUE),10) #closeness
head(sort(closeness(net_d,mode = 'out',normalized = T),decreasing=TRUE),10) #closeness

#中介中心性（betweeness centrality）
head(sort(betweenness(net_d,directed = T),decreasing=TRUE),10) #betweenness
head(sort(betweenness(net_d,directed = T,normalized=T),decreasing=TRUE),10) #标准化betweenness，Bnorm=4*B/(n-1)(n-2)

#声望

#入度
head(sort(degree(net_d,mode='in',normalized = T),decreasing = TRUE),10) 

#邻近声望
edges_pair_d_matrix<-as.network(edges_pair_d,directed=TRUE)
head(sort(prestige(edges_pair_d_matrix,cmode = 'domain'),decreasing=TRUE),10)
