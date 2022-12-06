#绘图文件：total_private.window.txt
#绘图示例：

#绘图数据解释说明：
#Ratio列为栽培特有的数目(Cultivar-specific)与野生特有的数目(Wild-specific)比值，比值大于1的区域，认为是Cultivar-like，比值小于1的，认为是Wild-like。
#绘图要求：
#1.纵坐标数值对Ratio值取log10；
#2.尽可能地还原示例图中的元素特征；
#3.对图片细节进行调整，使图片更为美观；
#两张图可在一个R脚本中完成，也可分别写两个R脚本完成；



rm(list=ls())
setwd('D:/desk/R作业/第一周R语言题目-郑福兴/题目1')
df <- read.table('total_private.window.txt',header = T,sep = '\t')
df$Ratio <- log10(df$Ratio)
library(ggplot2)
# 画第一张图
p1 <- ggplot(df,aes(x='',y=Ratio,fill = '#00468BFF')) +  # 总体
  geom_boxplot(size = 0.8, width = 0.2,linetype="dashed") +  # 画整体 并使用虚线
  stat_boxplot(aes(ymin=..lower..,ymax=..upper..),size = 1, width = 0.2) + # 绘制箱子整体 使用实线主题覆盖虚线
  scale_fill_manual(values = c("#ED0000FF"),breaks = '#00468BFF') + # 修改颜色
  guides(fill=FALSE) +  # 去除图例
  xlab("") +   # 删除 x 轴标题
  ylab("Wild-like                          Proportion of private SNPs                          Crop-like") +
  ylim(-4, 4) +  # 设置 Y 轴 范围
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),width=0.1,color="black") +  # 添加上方最大值横线
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),width=0.1,color="black") +  # 添加下方最小值横线
  theme_bw() +  # 主题
  geom_hline(yintercept=0, linetype="longdash", size=0.6) +  # 添加水平虚线
  theme(panel.grid =element_blank()) +   # 删掉网格线
  theme(axis.ticks.x = element_blank())  # 删除 x 轴 刻度
ggsave(filename = "exercise_1_1.png",plot = p1,width = 8,height = 6,dpi = 420)


# 画第二张图
# 删除 scaffold
df2 <- df[grep('Chr',df$Chromosome),]
df2$Chromosome <- gsub('Chr','',df2$Chromosome)
df2$Chromosome <- factor(df2$Chromosome, levels=unique(df2$Chromosome), ordered=TRUE)
p2 <- ggplot(df2,aes(x=Chromosome,y=Ratio,fill = 'black')) +  # 总体
  geom_boxplot(size = 0.8, width = 0.2,linetype="dashed") +  # 画整体 并使用虚线
  stat_boxplot(aes(ymin=..lower..,ymax=..upper..),size = 1, width = 0.2) + # 绘制箱子整体 使用实线主题覆盖虚线
  scale_fill_manual(values = c("#ED0000FF"),breaks = 'black') + # 修改颜色
  guides(fill=FALSE) +  # 去除图例
  xlab("Chromosome number") +   # 设置 x 轴标题
  ylab("log of private SNPs") +
  ylim(-4, 4) +  # 设置 Y 轴 范围
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),width=0.1,color="black") +  # 添加上方最大值横线
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),width=0.1,color="black") +  # 添加下方最小值横线
  theme_bw() +  # 主题
  geom_hline(yintercept=0, linetype="longdash", size=0.6) +  # 添加水平虚线
  geom_vline(xintercept=c(seq(1.5,13.5,1)), linetype=1, size=0.2) +  # 添加垂直实线
  theme(panel.grid =element_blank())    # 删掉网格线
ggsave(filename = "exercise_1_2.png",plot = p2,width = 8,height = 6,dpi = 420)
