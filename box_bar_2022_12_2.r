rm(list=ls())
setwd('D:/desk/R作业/第一周R语言题目-郑福兴/题目3')
library(ggplot2)
library(reshape2)
df <- read.table('KM_data_BCLC17.xls',header = T)
df <- df[,-1]

df1 <- df[,c(1,2)]
df1$Type <- rep('PFS',dim(df1)[1])
colnames(df1)[c(1,2)] <- c('bool','value')
df2 <- df[,c(3,4)]
df2$Type <- rep('OS',dim(df1)[1])
colnames(df2)[c(1,2)] <- c('bool','value')
data <- rbind(df1,df2)
data$bool <- factor(data$bool,levels = c('YES','NO'))
data$Type <- factor(data$Type,levels = c('PFS','OS'))
p1 <- ggplot(data,mapping=aes(bool,fill=Type)) + 
  geom_bar(width = 0.4) + 
  labs(x = "",y="",fill = "") + # 删除图例 坐标轴名称
  scale_fill_manual(breaks = c('PFS', 'OS'), values = c("#0099B4FF","#AD002AFF")) +
  theme_bw() + 
  theme(panel.grid =element_blank())
ggsave(filename = "exercise_2_1.png",plot = p1,width = 8,height = 6,dpi = 420)

# 按样本分离统计频率分布后绘图 1
Type <- c()
for(num in df1$value){
  if(num > 0 & num <= 5){
    Type <- c(Type,' 0 < PFS <=  5')
  }else if(num > 5 & num <= 10){
    Type <- c(Type,' 5 < PFS <= 10')
  }else if(num > 10 & num <= 15){
    Type <- c(Type,'10 < PFS <= 15')
  }else if(num > 15 & num <= 21){
    Type <- c(Type,'15 < PFS <= 21')
  }
}
df1$Type <- Type
df1$bool <- factor(df1$bool,levels = c('YES','NO'))
df1$Type <- factor(df1$Type,levels = c(' 0 < PFS <=  5',' 5 < PFS <= 10', '10 < PFS <= 15', '15 < PFS <= 21'))
p2 <- ggplot(df1,mapping=aes(bool,fill=Type)) + 
  geom_bar(width = 0.4,position='fill') + 
  labs(x = "",y="",fill = "",title = "PFS") + # 删除图例 坐标轴名称
  # 调色
  scale_fill_manual(breaks = c(' 0 < PFS <=  5',' 5 < PFS <= 10', '10 < PFS <= 15', '15 < PFS <= 21'), values = c("#0099B4FF","#AD002AFF","#FDAF91FF","#00468BFF")) +
  theme_bw() + 
  theme(panel.grid =element_blank(),plot.title = element_text(hjust = 0.5))
ggsave(filename = "exercise_2_2_PFS.png",plot = p2,width = 8,height = 6,dpi = 420)

# 按样本分离统计频率分布后绘图 2
Type <- c()
for(num in df2$value){
  if(num > 0 & num <= 5){
    Type <- c(Type,' 0 < OS <=  5')
  }else if(num > 5 & num <= 10){
    Type <- c(Type,' 5 < OS <= 10')
  }else if(num > 10 & num <= 15){
    Type <- c(Type,'10 < OS <= 15')
  }else if(num > 15 & num <= 21){
    Type <- c(Type,'15 < OS <= 21')
  }
}
df2$Type <- Type
df2$bool <- factor(df2$bool,levels = c('YES','NO'))
df2$Type <- factor(df2$Type,levels = c(' 0 < OS <=  5',' 5 < OS <= 10', '10 < OS <= 15', '15 < OS <= 21'))
p3 <- ggplot(df2,mapping=aes(bool,fill=Type)) + 
  geom_bar(width = 0.4,position='fill') + 
  labs(x = "",y="",fill = "",title = "OS") + # 删除图例 坐标轴名称
  # 调色
  scale_fill_manual(breaks = c(' 0 < OS <=  5',' 5 < OS <= 10', '10 < OS <= 15', '15 < OS <= 21'), values = c("#0099B4FF","#AD002AFF","#FDAF91FF","#00468BFF")) +
  theme_bw() + 
  theme(panel.grid =element_blank(),plot.title = element_text(hjust = 0.5))
ggsave(filename = "exercise_2_2_OS.png",plot = p3,width = 8,height = 6,dpi = 420)
