###final project
## data structure
#nba = read.csv("D://BA data//nba.games.stats.csv")
nba = read.csv("~/Documents/大學課程/商業分析：行銷與決策/Data/nba.games.stats.csv")
#了解資料
str(nba)
#nba為原始資料
nbadata.df = nba


## data cleaning & 新增變數 & 處理資料

#加入防守籃板
DefRebounds=nbadata.df$TotalRebounds-nbadata.df$OffRebounds
nbadata.df=cbind(nbadata.df,DefRebounds)
#加入兩分球出手次數
X2PointShotsAttempted=nbadata.df$FieldGoalsAttempted-nbadata.df$X3PointShotsAttempted
nbadata.df=cbind(nbadata.df,X2PointShotsAttempted)
#加入兩分球出手次數佔Fieldgoal(總出手次數)的比例
X2PointsAttemptRatio=nbadata.df$X2PointShotsAttempted/nbadata.df$FieldGoalsAttempted
nbadata.df=cbind(nbadata.df,X2PointsAttemptRatio)
#加入三分球出手次數佔Fieldgoal(總出手次數)的比例
X3PointsAttemptRatio=nbadata.df$X3PointShotsAttempted/nbadata.df$FieldGoalsAttempted
nbadata.df=cbind(nbadata.df,X3PointsAttemptRatio)
#加入勝分差
Pointdif=abs(nbadata.df$TeamPoints-nbadata.df$OpponentPoints)
nbadata.df=cbind(nbadata.df,Pointdif)
#將贏視為1，將輸視為0
Result=ifelse(nbadata.df$WINorLOSS=="W",1,0)
nbadata.df$Result <- Result

#將資料根據年份分開(全部)
nba201415=nbadata.df[ 1:2460,]
nba201516=nbadata.df[ 2461:4920,]
nba201617=nbadata.df[ 4921:7380,]
nba201718=nbadata.df[ 7381:9840,]
#三分球出手分布圖
qplot(x=X3PointsAttemptRatio,                      
      data=nbadata.df,
      geom="histogram",   
      main = "201415-201718 3PointsAttemptRatio",  
      binwidth = 0.025,
      xlab="3PointsAttemptRatio"           
)
#三分出手佔所有出手比例的中位數呈現上升的趨勢
X3RatioMedian201415=median(nba201415$X3PointsAttemptRatio)
X3RatioMedian201516=median(nba201516$X3PointsAttemptRatio)
X3RatioMedian201617=median(nba201617$X3PointsAttemptRatio)
X3RatioMedian201718=median(nba201718$X3PointsAttemptRatio)
X3RatioMedian201415
X3RatioMedian201516
X3RatioMedian201617
X3RatioMedian201718
X3RatioMedian <- data.frame( 
  year = c("201415", "201516","201617","201718"), 
  Xratio= c(X3RatioMedian201415, X3RatioMedian201516,X3RatioMedian201617,X3RatioMedian201718))
library(ggplot2)
p1=ggplot(X3RatioMedian , aes(x =year, y = Xratio,group=1)) +geom_line()
p1
#2017-18相較於2014-15賽季提高的三分球出手比例大約25.4%
X3pointsratioUp=(X3RatioMedian201718-X3RatioMedian201415)/X3RatioMedian201415
X3pointsratioUp
#加上總出手次數提升了約兩次，一場比賽單一球隊201718相較於201415，每場約多投了3.16顆三分球
median(nba201718$FieldGoals)-median(nba201415$FieldGoals)
X3Pointnba201415=median(nba201415$FieldGoals)*X3RatioMedian201415
X3Pointnba201718=median(nba201718$FieldGoals)*X3RatioMedian201718
X3Pointnba201718-X3Pointnba201415
nba201415gsw=nba201415[which(nba201415[ ,"Team"]=="GSW"), ]
nba201516gsw=nba201516[which(nba201516[ ,"Team"]=="GSW"), ]
nba201617gsw=nba201617[which(nba201617[ ,"Team"]=="GSW"), ]
nba201718gsw=nba201718[which(nba201718[ ,"Team"]=="GSW"), ]
X3RatioMedian201415gsw=median(nba201415gsw$X3PointsAttemptRatio)
X3RatioMedian201516gsw=median(nba201516gsw$X3PointsAttemptRatio)
X3RatioMedian201617gsw=median(nba201617gsw$X3PointsAttemptRatio)
X3RatioMedian201718gsw=median(nba201718gsw$X3PointsAttemptRatio)
X3RatioMedian201415gsw
X3RatioMedian201516gsw
X3RatioMedian201617gsw
X3RatioMedian201718gsw
X3RatioMediangsw <- data.frame( 
  year = c("201415", "201516","201617","201718"), 
  Xratio= c(X3RatioMedian201415gsw, X3RatioMedian201516gsw,X3RatioMedian201617gsw,X3RatioMedian201718gsw))

library(ggplot2)
p2=ggplot(X3RatioMediangsw,aes(x =year, y = Xratio,group=1)) +geom_line()
p2
#將勇士和全聯盟折線圖疊加
team=c("All","All","All","All","GSW","GSW","GSW","GSW")
year=c(201415,201516,201617,201718,201415,201516,201617,201718)
X3RatioMedians=c(X3RatioMedian201415, X3RatioMedian201516,X3RatioMedian201617,X3RatioMedian201718,X3RatioMedian201415gsw, X3RatioMedian201516gsw,X3RatioMedian201617gsw,X3RatioMedian201718gsw)
x3pointratio.df=data.frame(team,year,X3RatioMedians)
ggplot(x3pointratio.df, aes(x=factor(year), y=X3RatioMedians,colour=team,group=team)) + geom_line()
#用標準差計算
SD201415=sd(nba201415$X3PointsAttemptRatio)
SD201516=sd(nba201516$X3PointsAttemptRatio)
(mean(nba201415gsw$X3PointsAttemptRatio)-mean(nba201415$X3PointsAttemptRatio))/SD201415
(mean(nba201516gsw$X3PointsAttemptRatio)-mean(nba201516$X3PointsAttemptRatio))/SD201516
#各賽季聯盟三分球出手比例排名與各賽季平均三分球出手比例排名
library(dplyr)
rankall=aggregate(x = nbadata.df$X3PointsAttemptRatio, by= list(nbadata.df$Team), FUN = median)
arrange(rankall,by=desc(x))

rank1415=aggregate(x = nba201415$X3PointsAttemptRatio, by= list(nba201415$Team), FUN = median)
arrange(rank1415,by=desc(x))

rank1516=aggregate(x = nba201516$X3PointsAttemptRatio, by= list(nba201516$Team), FUN = median)
arrange(rank1516,by=desc(x))

rank1617=aggregate(x = nba201617$X3PointsAttemptRatio, by= list(nba201617$Team), FUN = median)
arrange(rank1617,by=desc(x))

rank1718=aggregate(x = nba201718$X3PointsAttemptRatio, by= list(nba201718$Team), FUN = median)
arrange(rank1718,by=desc(x))

#全聯盟三分命中率趨勢
X3Shots.Median201415=median(nba201415$X3PointShots.)
X3Shots.Median201516=median(nba201516$X3PointShots.)
X3Shots.Median201617=median(nba201617$X3PointShots.)
X3Shots.Median201718=median(nba201718$X3PointShots.)
X3Shots.Median201415
X3Shots.Median201718
X3Shots.Median <- data.frame( 
  year = c("201415", "201516","201617","201718"), 
  XShots.= c(X3Shots.Median201415, X3Shots.Median201516,X3Shots.Median201617,X3Shots.Median201718))
library(ggplot2)
p3=ggplot(X3Shots.Median , aes(x =year, y = XShots.,group=1)) +geom_line()
p3
#勇士三分命中率趨勢
ssd=sd(nba201415$X3PointShots.)
(X3Shots.Median201415gsw-X3Shots.Median201415)/ssd
ssd2=sd(nba201516$X3PointShots.)
(X3Shots.Median201516gsw-X3Shots.Median201516)/ssd2
nba201415gsw=nba201415[which(nba201415[ ,"Team"]=="GSW"), ]
nba201516gsw=nba201516[which(nba201516[ ,"Team"]=="GSW"), ]
nba201617gsw=nba201617[which(nba201617[ ,"Team"]=="GSW"), ]
nba201718gsw=nba201718[which(nba201718[ ,"Team"]=="GSW"), ]
X3Shots.Median201415gsw=median(nba201415gsw$X3PointShots.)
X3Shots.Median201516gsw=median(nba201516gsw$X3PointShots.)
X3Shots.Median201617gsw=median(nba201617gsw$X3PointShots.)
X3Shots.Median201718gsw=median(nba201718gsw$X3PointShots.)
X3Shots.Median201415gsw
X3Shots.Median201516gsw
X3Shots.Median201617gsw
X3Shots.Median201718gsw
X3Shots.Mediangsw <- data.frame( 
  year = c("201415", "201516","201617","201718"), 
  XShots.= c(X3Shots.Median201415gsw, X3Shots.Median201516gsw,X3Shots.Median201617gsw,X3Shots.Median201718gsw))
library(ggplot2)
p4=ggplot(X3Shots.Mediangsw , aes(x =year, y = XShots.,group=1)) +geom_line()
p4

#疊加圖
team=c("All","All","All","All","GSW","GSW","GSW","GSW")
year=c(201415,201516,201617,201718,201415,201516,201617,201718)
X3Shots.Median=c(X3Shots.Median201415, X3Shots.Median201516,X3Shots.Median201617,X3Shots.Median201718,X3Shots.Median201415gsw, X3Shots.Median201516gsw,X3Shots.Median201617gsw,X3Shots.Median201718gsw)
x3shots.df=data.frame(team,year,X3Shots.Median)
library(ggplot2)
ggplot(x3shots.df, aes(x=factor(year), y=X3Shots.Median,colour=team,group=team)) + geom_line()
#各賽季聯盟三分球命中率排名與各賽季平均三分球命中率排名
library(dplyr)
x3shots.=aggregate(x = nbadata.df$X3PointShots., by= list(nbadata.df$Team), FUN = median)
arrange(x3shots.,by=desc(x))

x3shots.1415=aggregate(x = nba201415$X3PointShots., by= list(nba201415$Team), FUN = median)
arrange(x3shots.1415,by=desc(x))

x3shots.1516=aggregate(x = nba201516$X3PointShots., by= list(nba201516$Team), FUN = median)
arrange(x3shots.1516,by=desc(x))

x3shots.1617=aggregate(x = nba201617$X3PointShots., by= list(nba201617$Team), FUN = median)
arrange(x3shots.1617,by=desc(x))

x3shots.1718=aggregate(x = nba201718$X3PointShots., by= list(nba201718$Team), FUN = median)
arrange(x3shots.1718,by=desc(x))

# # 丟掉命中率的變數(三個完全相關的變數，選擇踢掉命中率)
# nbadata.df = nbadata.df[,-c(7,10,13,23,26,29)]

#加入助攻失誤比(可考慮拿來代替助攻) 只用前三年
nba1417 = nbadata.df[1:7380,]
AssistTurnoverRatio = nba1417$Assists/nba1417$Turnovers
OppAssistTurnoverRatio = nba1417$Opp.Assists/nba1417$Opp.Turnovers
nba1417 = cbind(nba1417, AssistTurnoverRatio)
nba1417 = cbind(nba1417, OppAssistTurnoverRatio)
mean(AssistTurnoverRatio)
max(AssistTurnoverRatio)
mean(OppAssistTurnoverRatio)
max(OppAssistTurnoverRatio)
nbadata.df[which(nbadata.df$AssistTurnoverRatio==12.5),]

nba201415=nbadata.df[ 1:2460,]
nba201516=nbadata.df[ 2461:4920,]
nba201617=nbadata.df[ 4921:7380,]
nba201718=nbadata.df[ 7381:9840,]
str(nba201415)
logit201415=glm(as.factor(Result)~FieldGoals+X3PointShots+FreeThrows+TotalRebounds+Assists+Steals+Blocks+Turnovers+TotalFouls
                , data=nba201415,family=binomial(link="logit"))
logit201516=glm(as.factor(Result)~FieldGoals+X3PointShots+FreeThrows+TotalRebounds+Assists+Steals+Blocks+Turnovers+TotalFouls
                , data=nba201516,family=binomial(link="logit"))
logit201617=glm(as.factor(Result)~FieldGoals+X3PointShots+FreeThrows+TotalRebounds+Assists+Steals+Blocks+Turnovers+TotalFouls
                , data=nba201617,family=binomial(link="logit"))
logit201718=glm(as.factor(Result)~FieldGoals+X3PointShots+FreeThrows+TotalRebounds+Assists+Steals+Blocks+Turnovers+TotalFouls
                , data=nba201718,family=binomial(link="logit"))
options(scipen=999) # 不要科學記號
summary(logit201415)
summary(logit201516)
summary(logit201617)
summary(logit201718)

# 準確率
per1415 = table(true=nba201415$Result, pred=round(fitted(logit201415)))
per1516 = table(true=nba201516$Result, pred=round(fitted(logit201516)))
per1617 = table(true=nba201617$Result, pred=round(fitted(logit201617)))
per1718 = table(true=nba201718$Result, pred=round(fitted(logit201718)))
per1415
per1516
per1617
per1718

# 四年的平均犯規數趨勢圖
fouls1415=mean(nba201415$TotalFouls)
fouls1516=mean(nba201516$TotalFouls)
fouls1617=mean(nba201617$TotalFouls)
fouls1718=mean(nba201718$TotalFouls)
foulsMean <- data.frame( 
  year = c("201415", "201516", "201617", "201718"),
  foulMean = c(fouls1415, fouls1516, fouls1617, fouls1718))
ggplot(foulsMean , aes(x = year, y = foulMean, group = 1)) + geom_line(size = 1.5, color = "deeppink4") + 
  theme(text = element_text(size=25), axis.text.x = element_text(angle=0, hjust=1),
        axis.text.y = element_text(angle=0))

# 如何打敗勇士隊 勇士平均 vs 聯盟平均、勇士贏 vs 勇士輸(用四年資料)
# 加入助攻失誤比
AssistTurnoverRatio = nbadata.df$Assists/nbadata.df$Turnovers
OppAssistTurnoverRatio = nbadata.df$Opp.Assists/nbadata.df$Opp.Turnovers
nbadata.df = cbind(nbadata.df, AssistTurnoverRatio)
nbadata.df = cbind(nbadata.df, OppAssistTurnoverRatio)

warriors.df = nbadata.df[which(nbadata.df$Team == "GSW"),]
AssistTurnoverRatio = warriors.df$Assists/warriors.df$Turnovers
OppAssistTurnoverRatio = warriors.df$Opp.Assists/warriors.df$Opp.Turnovers
warriors.df = cbind(warriors.df, AssistTurnoverRatio)
warriors.df = cbind(warriors.df, OppAssistTurnoverRatio)

colnames(warriors.df)
# 只取需要的部分
warriors.df = warriors.df[,c(8,10,13,15,19:24,44,45,47:49)]
nbadata.df = nbadata.df[,c(8,10,13,15,19:24,44,45,47:49)]
colnames(warriors.df)
colnames(nbadata.df)
# 勇士平均 vs 聯盟平均 
GSWstats = apply(warriors.df[,-13], 2, mean) 
GSWstats = as.data.frame(GSWstats)
GSWstats$stats = row.names(GSWstats)
GSWstats = data.frame(GSWstats, row.names = c(1:14))
# GSWstats = t(GSWstats) #t()可直接轉置data frame
LeagueStat = apply(nbadata.df[,-13], 2, mean)
LeagueStat = as.data.frame(LeagueStat)
LeagueStat$stats = row.names(LeagueStat)
LeagueStat = data.frame(LeagueStat, row.names = c(15:28))

colnames(GSWstats) = c("GSWvsLeague", "stats")
colnames(LeagueStat) = c("GSWvsLeague", "stats")
GSWvsLeague = rbind(GSWstats, LeagueStat)
row.names(GSWvsLeague)
nrow(GSWvsLeague)
GSWvsLeague$GSorLeague = c(rep("GSW",14), rep("League", 14))
# GSWvsLeague$col = row.names(GSWvsLeague) # ggplot吃的到row name
#畫圖，分四張圖比較(比率1、比率2、數值1、數值2)
GSWvsLeagueRatio1 = GSWvsLeague[c(4,11,12,18,25,26),]
GSWvsLeagueRatio2 = GSWvsLeague[c(13:14, 27:28),]
GSWvsLeaguenum1=GSWvsLeague[c(1:3,15:17),]
GSWvsLeaguenum2=GSWvsLeague[c(5:10,19:24),]
# GSWvsLeagueRatio1 = t(GSWvsLeagueRatio1)
# GSWvsLeagueRatio1 = as.data.frame(GSWvsLeagueRatio1)
# 先寫好theme、geom_text、scale_fill_manual每張圖都可用
my_theme = theme(text = element_text(size=30), axis.text.x = element_text(angle=45, hjust=1))
my_geom_text = geom_text(aes(label=round(GSWvsLeague, 2)), vjust=1.6, color="white",
                         position = position_dodge(0.9), size=6)
my_custom_color = scale_fill_manual(values = c("skyblue2", "orange2"))
# bar plot
ggplot(GSWvsLeagueRatio1, aes(x = stats, y = GSWvsLeague, fill = GSorLeague)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(text = element_text(size=25), axis.text.x = element_text(angle=30, hjust=0.8, vjust = 1)) +
  my_geom_text + my_custom_color + xlab('GSW vs League Ratio 1') + ylab('ratio')
ggplot(GSWvsLeagueRatio2, aes(x = stats, y = GSWvsLeague, fill = GSorLeague)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.5) +
  theme(text = element_text(size=20), axis.text.x = element_text(angle=0, hjust=0.5)) +
  geom_text(aes(label=round(GSWvsLeague, 2)), vjust=1, color="white", hjust = 0.5,
            position = position_dodge(0.5), size=6) + my_custom_color +
  xlab('GSW vs League Ratio 2') + ylab('ratio')
ggplot(GSWvsLeaguenum1, aes(x = stats, y = GSWvsLeague, fill = GSorLeague)) +
  geom_bar(stat="identity", position=position_dodge()) +
  my_theme + my_geom_text + my_custom_color + xlab('GSW vs League Num 1') + ylab('quantity')
ggplot(GSWvsLeaguenum2, aes(x = stats, y = GSWvsLeague, fill = GSorLeague)) +
  geom_bar(stat="identity", position=position_dodge()) +
  my_theme + my_geom_text + my_custom_color + xlab('GSW vs League Num 2') + ylab('quantity')


# 勇士贏 vs 勇士輸

warriors.df1 = nbadata.df[which(nbadata.df$Team == "GSW"),]

colnames(warriors.df1)
# 只取需要的部分
warriors.df1 = warriors.df1[,c(8,9,12,13,15,20,21,24,28,31,47:49)]

GSWwin = warriors.df1[which(warriors.df1$Result==1),]
GSWwinStats = apply(GSWwin[,-11], 2, mean) 
GSWwinStats = as.data.frame(GSWwinStats)
GSWwinStats$stats = row.names(GSWwinStats)
GSWwinStats = data.frame(GSWwinStats, row.names = c(1:12))

GSWloss = warriors.df1[which(warriors.df1$Result==0),]
GSWlossStats = apply(GSWloss[,-11], 2, mean)
GSWlossStats = as.data.frame(GSWlossStats)
GSWlossStats$stats = row.names(GSWlossStats)
GSWlossStats = data.frame(GSWlossStats, row.names = c(13:24))
# 合併
colnames(GSWwinStats) = c("WinVSLoss", "stats")
colnames(GSWlossStats) = c("WinVSLoss", "stats")
WinvsLoss = rbind(GSWwinStats, GSWlossStats)
WinvsLoss$result = c(rep("Win",12), rep("Loss", 12))
# 畫圖，分四張圖比較(比率1、比率2、數值1、數值2)
WinvsLossRatio1 = WinvsLoss[c(3,5,9,10,15,17,21,22),]
WinvsLossRatio2 = WinvsLoss[c(11:12, 23:24),]
WinvsLossNum1=WinvsLoss[c(1,2,6,13,14,18),]
WinvsLossNum2=WinvsLoss[c(4,7:8,16,19:20),]

# 先寫好theme、geom_text、scale_fill_manual每張圖都可用
my_theme1 = theme(text = element_text(size=30), axis.text.x = element_text(angle=45, hjust=1))
my_geom_text1 = geom_text(aes(label=round(WinVSLoss, 2)), vjust=1.6, color="black",
                          position = position_dodge(0.9), size=5)
my_custom_color1 = scale_fill_manual(values = c("magenta2", "yellow"))

ggplot(WinvsLossRatio1, aes(x = stats, y = WinVSLoss, fill = result)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8) +
  my_theme1 + my_geom_text1 + my_custom_color1 + xlab('Win vs Loss Ratio 1') + ylab('ratio')
ggplot(WinvsLossRatio2, aes(x = stats, y = WinVSLoss, fill = result)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.5) +
  theme(text = element_text(size=20), axis.text.x = element_text(angle=0, hjust=0.5)) +
  geom_text(aes(label=round(WinVSLoss, 2)), vjust=1, color="black", hjust = 0.5,
            position = position_dodge(0.5), size=6) + my_custom_color1 +
  xlab('Win vs Loss Ratio 2') + ylab('ratio')
ggplot(WinvsLossNum1, aes(x = stats, y = WinVSLoss, fill = result)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8) +
  my_theme1 + my_geom_text1 + my_custom_color1 + xlab('Win vs Loss Num 1') + ylab('quantity')
ggplot(WinvsLossNum2, aes(x = stats, y = WinVSLoss, fill = result)) +
  geom_bar(stat="identity", position=position_dodge()) +
  my_theme1 + my_geom_text1 + my_custom_color1 + xlab('Win vs Loss Num 2') + ylab('quantity')


##建模
nba.df = read.csv("D://BA data//nba.games.stats.csv")
str(nba.df)

# 把WINorLOSS轉成用1和0表示(原本是2和1)
nba.df$WINorLOSS = ifelse(nba.df$WINorLOSS == 'W', 1, 0)

# 新增「三分球佔所有出手次數比率」的欄位
nba.df$X3PointsAttemptRatio = nba.df$X3PointShotsAttempted / nba.df$FieldGoalsAttempted

# 新增「助攻失誤比」的欄位
nba.df$AssistsToTurnoversRatio = nba.df$Assists / nba.df$Turnovers

# 新增「對手兩分球命中率」的欄位
nba.df$Opp.2PointShots. = (nba.df$Opp.FieldGoals - nba.df$Opp.3PointShots) / 
  (nba.df$Opp.FieldGoalsAttempted - nba.df$Opp.3PointShotsAttempte)

# 切分訓練集、驗證集、測試集
nba.df.14151617 = nba.df[1:7380,]
Test = nba.df[7381:9840,] # 測試集

library(caTools)
set.seed(10) #我們是第十組
spl = sample.split(nba.df.14151617$WINorLOSS, SplitRatio = 0.8)
Train = subset(nba.df.14151617, spl==TRUE)
Validation = subset(nba.df.14151617, spl==FALSE)

# 羅吉斯迴歸
nba.logit = glm(WINorLOSS ~ X3PointsAttemptRatio + AssistsToTurnoversRatio + Opp.3PointShots. + 
                  Opp.2PointShots.+ Turnovers + TotalRebounds + Steals + Blocks + FreeThrows.+ TotalFouls, 
                data = Train, family = binomial(link="logit"))
summary(nba.logit)

# 羅吉斯回歸 - 共線性檢驗
library(car)
vif(nba.logit)


# 決策樹
library(rpart)
library(rpart.plot)
nba.tree = rpart(WINorLOSS ~ X3PointsAttemptRatio + AssistsToTurnoversRatio + Opp.3PointShots. + 
                   Opp.2PointShots.+ Turnovers + TotalRebounds + Steals + Blocks + FreeThrows.+ TotalFouls, 
                 data = Train, method="class", minbucket = 25, parms = list(split="information"))

prp(nba.tree)

# 隨機森林
library(randomForest)
nba.rf = randomForest(as.factor(WINorLOSS)~X3PointsAttemptRatio + AssistsToTurnoversRatio + Opp.3PointShots. + 
                        Opp.2PointShots.+ Turnovers + TotalRebounds + Steals + Blocks + FreeThrows.+ TotalFouls,
                      data = Train, ntree=500, nodesize = 25, mtry = 5, importance=TRUE)
varImpPlot(nba.rf)
importance(nba.rf)

# gbm
str(Train)
library(gbm)
nba.gbm = gbm(WINorLOSS~X3PointsAttemptRatio + AssistsToTurnoversRatio + Opp.3PointShots. + 
                Opp.2PointShots.+ Turnovers + TotalRebounds + Steals + Blocks + FreeThrows.+ TotalFouls,
              data = Train, n.trees=5000, interaction.depth=4,
              distribution="bernoulli",shrinkage=0.05, cv.folds = 5)
summary(nba.gbm)
print(nba.gbm)



# ROC curve  羅吉斯+決策樹
library(ROCR)
logit.nba.out = predict(nba.logit, newdata = Test, type="response")
tree.nba.out = predict(nba.tree, newdata = Test, type="prob")

#
predlogit = prediction(logit.nba.out, Test$WINorLOSS)
predtree = prediction(tree.nba.out[,2], Test$WINorLOSS)
#
plot(performance(predlogit, "tpr", "fpr"),col='red',lty=1,lwd=3)
plot(performance(predtree, "tpr", "fpr"),col='blue',add=T,lty=2,lwd=3)
abline(0,1,lty=2)
legend("bottomright", legend=c("logistic regression", "decision tree"),
       col=c("red", "blue"), lty = 1:2, lwd = 3, cex = 1.2)

performance(predlogit, "auc")@y.values[[1]]
performance(predtree, "auc")@y.values[[1]]

# ROC curve 隨機森林+gbm
library(ROCR)
Forest.out = predict(nba.rf, newdata = Validation, type = "prob")
Gbm.out = predict(nba.gbm, newdata = Validation, n.trees=, type= "response")
predForest = prediction(Forest.out[,2], Validation$WINorLOSS)
predGbm = prediction(Gbm.out, Validation$WINorLOSS)
plot(performance(predForest, "tpr", "fpr"),col='green',lty=3,lwd=3)  # green 是隨機森林
plot(performance(predGbm, "tpr", "fpr"),col='purple',add=T,lty=3,lwd=3) #purple 是gbm
abline(0,1,lty=2)
legend("bottomright", legend=c("random forest", "gbm"),
       col=c("green", "purple"), lty = 1:2, lwd = 3, cex = 1.2)

performance(predForest, "auc")@y.values[[1]]
performance(predGbm, "auc")@y.values[[1]]

# 測試
gbm.test.out = predict(nba.gbm, newdata = Test, n.trees=, type= "response")
predGbm.test = prediction(gbm.test.out, Test$WINorLOSS)
plot(performance(predGbm, "tpr", "fpr"),col='red',lty=3,lwd=3)
abline(0,1,lty=2)
legend("bottomright", legend= "gbm",
       col="red", lty = 1:2, lwd = 3)
performance(predGbm.test, "auc")@y.values[[1]]
