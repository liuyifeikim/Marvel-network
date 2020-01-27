#pair function
expand.grid(a183$hero,a183$hero)
combn(a183$hero,2)

set.seed(5)
s1<-sample(unique(edges$comic),5)
edges_s1<-edges[edges$comic %in% s1,]
edges_s1 #小样本用来测试配对函数

comic_list<-unique(edges$comic) #comic 列表
comic_size<-ddply(edges,.(comic),summarize,size=length(hero))#每个comic的英雄关系数量
comic_list_two<-comic_size[comic_size$size>=2,]$comic #hero>=2 的comic列表
length(comic_list) #12651
length(comic_list_two) #11171

pair_fun<-function(df){
  x=NA #生成一个空对象
  for (i in 1:length(comic_list_two)){
    df_s=df[df$comic %in% comic_list_two[i],]
    nrow_df_s=nrow(df_s)
    df_s=do.call('rbind',replicate(nrow_df_s,df_s,simplify=FALSE)) #按不同hero的顺序重复多次（次数为hero数）
    df_s$hero2=rep(unique(df_s$hero),each=nrow_df_s) #生成新列,按相同hero的顺序重复多次（次数为hero数）
    df_s=df_s[!(df_s$hero==df_s$hero2),] #删除相同的配对
    df_s$index=strsplit(paste(df_s$hero,df_s$hero2),'') #生成索引列,将hero的名称字符合并并分割为字母
    df_s$index=sapply(df_s$index,sort) #将字母排序,两个hero相同的对,字母的排列相同
    df_s=df_s[!duplicated(df_s$index),]#删除相同的配对
    df_s=df_s[,-ncol(df_s)] #删除索引列
    x=rbind(x,df_s)
  }
  return(x=na.omit(x))
}

pair_fun<-function(df){
  x=NA
  comic_list=unique(df$comic)
  for (i in 1:length(unique(df$comic))){
    df_s=df[df$comic %in% comic_list[i],]
    df_pair=expand.grid(df_s$hero,df_s$hero)
    df_pair=df_pair[df_pair[,1]!=df_pair[,2],]
    df_pair$comic=df_s$comic
    x=rbind(x,df_pair)
  }
  return(x=na.omit(x))
}

pair_fun(a183) 
pair_fun(edges_s1)

pair_fun <- function(df){
  x=NA
  comic_list = unique(df$comic)
  for (i in 1:length(unique(df$comic))){
    df_s=df[df$comic %in% comic_list[i],]
    df_pair=as.data.frame(t(combn(df_s$hero, 2)))
    df_pair$comic = df_s$comic
    x=rbind(x,df_pair)
  }
  return(x = na.omit(x))
}
pair_fun(a183) 
pair_fun(edges_s1)
