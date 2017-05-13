install.packages("RSQLite")
install.packages("ggplot2")
install.packages("lmtest")
install.packages("MASS")
install.packages("car")
install.packages("boot")

library(boot)
library(car)
library(MASS)
library(lmtest)
library("RSQLite")
library("ggplot2")

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="database.sqlite")

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}


Database<-function(x){
  lDataFrames[[x]]
}
#create dataframes
Country_ID<-Database(1)
Country_league<-Database(2)
match_results<-Database(3)
players<-Database(4)
player_attributes<-Database(5)
teams<-Database(6)
team_attributes<-Database(7)


#subset to only EPL(English) games
epl_match<-match_results[match_results$country_id==1729,]
rows<-length(epl_match$id)
#removing unnecessary cols

for (i in 12:55){
  epl_match[12]<-NULL
}
for (i in 42:71){
  epl_match[42]<-NULL
}

#reorganizing by date played
epl_match <- epl_match[order(epl_match$date),]

#renumbering ID
epl_match$id<-1:rows

#replace team_id with string name
for(i in 1:rows){
  epl_match[i,8]<-teams$team_long_name[teams$team_api_id==epl_match[i,8]]
  epl_match[i,9]<-teams$team_long_name[teams$team_api_id==epl_match[i,9]]
}

#home team win or loss/draw (1 or 0)
epl_match$home_win<-0

for(i in 1:rows){
  if (epl_match[i,10]>epl_match[i,11]){
    epl_match$home_win[i]<-1
  }else if(epl_match[i,10]==epl_match[i,11]){
    epl_match$home_win[i]<-0
  }else
    epl_match$home_win[i]<--1
}

#away win or loss/draw
epl_match$away_win<-0

for(i in 1:rows){
  if (epl_match[i,10]>epl_match[i,11]){
    epl_match$away_win[i]<--1
  }else if(epl_match[i,10]==epl_match[i,11]){
    epl_match$away_win[i]<-0
  }else
    epl_match$away_win[i]<-1
}


#average player rating
row2<-length(players$id)
players$avg_rating<-0
player_attributes<-na.omit(player_attributes)

for(i in 1:row2){
  players$avg_rating[i]<-mean(player_attributes$overall_rating[player_attributes$player_api_id==players$player_api_id[i]])
}

#adding average player rating by team
epl_match$home_avgplayer1<-0
epl_match$home_avgplayer2<-0
epl_match$home_avgplayer3<-0
epl_match$home_avgplayer4<-0
epl_match$home_avgplayer5<-0
epl_match$home_avgplayer6<-0
epl_match$home_avgplayer7<-0
epl_match$home_avgplayer8<-0
epl_match$home_avgplayer9<-0
epl_match$home_avgplayer10<-0
epl_match$home_avgplayer11<-0

epl_match$away_avgplayer1<-0
epl_match$away_avgplayer2<-0
epl_match$away_avgplayer3<-0
epl_match$away_avgplayer4<-0
epl_match$away_avgplayer5<-0
epl_match$away_avgplayer6<-0
epl_match$away_avgplayer7<-0
epl_match$away_avgplayer8<-0
epl_match$away_avgplayer9<-0
epl_match$away_avgplayer10<-0
epl_match$away_avgplayer11<-0

for(c in 44:65){
  for(r in 1:rows){
    c2<-c-32 #playercols
    if(is.na(epl_match[r,c2])==FALSE){
      epl_match[r,c]<-players$avg_rating[players$player_api_id==epl_match[r,c2]]
      
    }
  }
}

epl_match$hometeam_avg <- rowMeans(epl_match[,44:54], na.rm=TRUE)
epl_match$awayteam_avg <- rowMeans(epl_match[,55:65], na.rm=TRUE)

epl_match$homegoalie_avg <- epl_match[,44]
epl_match$homeDefense_avg <- rowMeans(epl_match[,45:48], na.rm=TRUE)
epl_match$homeMid_avg <- rowMeans(epl_match[,49:52], na.rm=TRUE)
epl_match$homeAtk_av <- rowMeans(epl_match[,53:54], na.rm=TRUE)

epl_match$awayGoalie_avg<-epl_match[,55]
epl_match$awayDefense_avg<-rowMeans(epl_match[,56:59], na.rm=TRUE)
epl_match$awayMid_avg <- rowMeans(epl_match[,60:63], na.rm=TRUE)
epl_match$awayAtk_avg <-rowMeans(epl_match[,64:65], na.rm=TRUE)

epl_match$team_difference<-0

for(i in 1:rows){
  epl_match$team_difference[i]<-epl_match$hometeam_avg[i]-epl$awayteam_avg[i]

}

#replace player id with string name(readability)

# for (c in 12:33){
#   for (r in 1:rows){
#     if(is.na(epl_match[r,c])==FALSE){
#       epl_match[r,c]<-players$player_name[players$player_api_id==epl_match[r,c]]
#     }
#   }
# }


#Calculating form

#home team form(only home games)
ma<-function(x,n){filter(x,rep(1/n,n), sides=1)}
team1<-unique(epl_match$home_team_api_id)
tot_teams<-length(unique(epl_match$home_team_api_id))
epl_match$home_form<-0

for (team in 1:tot_teams){
  index<-which(epl_match$home_team_api_id==team1[team])
  wins<-epl_match$home_win[epl_match$home_team_api_id==team1[team]]
  n<-length(wins)
  avwin<-ma(wins,5)
  for(i in 1:n){
    epl_match$home_form[index[i]]<-avwin[i]
  }
}

#away team form(only away games)

epl_match$away_form<-0

for (team in 1:tot_teams){
  index<-which(epl_match$away_team_api_id==team1[team])
  wins<-epl_match$away_win[epl_match$away_team_api_id==team1[team]]
  n<-length(wins)
  avwin<-ma(wins,5)
  for(i in 1:n){
    epl_match$away_form[index[i]]<-avwin[i]
  }
}



#Average goals scored

#home team
epl_match$home_avgGoal<-0

for (team in 1:tot_teams){
  index<-which(epl_match$home_team_api_id==team1[team])
  goals<-epl_match$home_team_goal[epl_match$home_team_api_id==team1[team]]
  n<-length(goals)
  avwin<-ma(goals,5)
  for(i in 1:n){
    epl_match$home_avgGoal[index[i]]<-avwin[i]
  }
}
#away team
epl_match$away_avgGoal<-0

for (team in 1:tot_teams){
  index<-which(epl_match$away_team_api_id==team1[team])
  goals<-epl_match$away_team_goal[epl_match$away_team_api_id==team1[team]]
  n<-length(goals)
  avwin<-ma(goals,5)
  for(i in 1:n){
    epl_match$away_avgGoal[index[i]]<-avwin[i]
  }
}

#Average goal conceded

#home team
epl_match$home_avgConcede<-0

for (team in 1:tot_teams){
  index<-which(epl_match$home_team_api_id==team1[team])
  goals<-epl_match$away_team_goal[epl_match$home_team_api_id==team1[team]]
  n<-length(goals)
  avwin<-ma(goals,5)
  for(i in 1:n){
    epl_match$home_avgConcede[index[i]]<-avwin[i]
  }
}

#away team
epl_match$away_avgConcede<-0

for (team in 1:tot_teams){
  index<-which(epl_match$home_team_api_id==team1[team])
  goals<-epl_match$home_team_goal[epl_match$away_team_api_id==team1[team]]
  n<-length(goals)
  avwin<-ma(goals,5)
  for(i in 1:n){
    epl_match$away_avgConcede[index[i]]<-avwin[i]
  }
}



#avg goal diff home
epl_match$home_goaldiffAvg<-NA
epl_match$away_goaldiffAvg<-NA

for(i in 1:rows){
  epl_match$home_goaldiffAvg[i]<-epl_match$home_avgGoal[i]-epl_match$home_avgConcede[i]
  epl_match$away_goaldiffAvg[i]<-epl_match$away_avgGoal[i]-epl_match$away_avgConcede[i]
  
}

#Possession 
epl_match$home_pos<-0

team1<-unique(epl_match$home_team_api_id)
tot_teams<-length(unique(epl_match$home_team_api_id))

#home possession
for (r in 1:rows){
  pos<-na.omit(as.numeric(unlist(strsplit(unlist(epl_match$possession[r]), "[^0-9]+"))))
  epl_match$home_pos[r]<-pos[1]
}

#away possesssion
epl_match$away_pos<-0
for (r in 1:rows){
  epl_match$away_pos[r]<-100-epl_match$home_pos[r]
}

#home possession last 15 avg
epl_match$home_posAvg<-0

for (team in 1:tot_teams){
  index<-which(epl_match$home_team_api_id==team1[team])
  pos<-epl_match$home_pos[epl_match$home_team_api_id==team1[team]]
  n<-length(pos)
  avwin<-ma(pos,15)
  for(i in 1:n){
    epl_match$home_posAvg[index[i]]<-avwin[i]
  }
}

#away posssession last 15 avg
epl_match$away_posAvg<-0

for (team in 1:tot_teams){
  index<-which(epl_match$away_team_api_id==team1[team])
  pos<-epl_match$away_pos[epl_match$away_team_api_id==team1[team]]
  n<-length(pos)
  avwin<-ma(pos,15)
  for(i in 1:n){
    epl_match$away_posAvg[index[i]]<-avwin[i]
  }
}

#possession difference
for(i in 1:rows){
  epl_match$possess_diff[i]<-epl_match$home_posAvg[i]-epl_match$away_posAvg[i]
}
#Home record last 2 games against away

epl_match$home_record<-NA

for(t1 in 1:tot_teams){
  home_team<-team1[t1]
  t1_index<-which(epl_match$home_team_api_id==team1[t1])#home team indices
  for(t2 in 1:tot_teams){
    away_team<-team1[t2]
    if(home_team!=away_team){
      t2_index<-which(epl_match$away_team_api_id==team1[t2])#away team indices
      index<-Reduce(intersect, list(t1_index,t2_index))#where home against away
      index_len<-length(index)
      record<-vector(mode="numeric", length=index_len)
      if(index_len>2){
        for(z in 1:index_len){
          record[z]<-epl_match$home_win[index[z]]#total record of home team against away
          n<-length(record)
          avwin<-ma(record,2)
          for(i in 1:n){
            epl_match$home_record[index[i]]<-avwin[i]
          }
        }
      }
    }
  }
}



#Away record last 2 games against home

epl_match$away_record<-NA

for(t1 in 1:tot_teams){
  home_team<-team1[t1]
  t1_index<-which(epl_match$away_team_api_id==team1[t1])#away team indices
  for(t2 in 1:tot_teams){
    away_team<-team1[t2]
    if(home_team!=away_team){
      t2_index<-which(epl_match$home_team_api_id==team1[t2])#home team indices
      index<-Reduce(intersect, list(t1_index,t2_index))#where away against home
      index_len<-length(index)
      record<-vector(mode="numeric", length=index_len)
      if(index_len>2){
        for(z in 1:index_len){
          record[z]<-epl_match$home_win[index[z]]#total record of away team against home
          n<-length(record)
          avwin<-ma(record,2)
          for(i in 1:n){
            epl_match$away_record[index[i]]<-avwin[i]
          }
        }
      }
    }
  }
}

#Overall home team record against away last 3 games

epl_match$home_record_tot<-NA

for(t1 in 1:tot_teams){
  home_team<-team1[t1]
  t1_index<-which(epl_match$home_team_api_id==team1[t1])
  t1_index2<-which(epl_match$away_team_api_id==team1[t1]) 
  t1_index<- sort(c(t1_index,t1_index2)) #home team indices
  for(t2 in 1:tot_teams){
    away_team<-team1[t2]
    if(home_team!=away_team){
      t2_index<-which(epl_match$away_team_api_id==team1[t2])
      t2_index2<-which(epl_match$home_team_api_id==team1[t2])
      t2_index<- sort(c(t2_index,t2_index2))#away team indices
      index<-Reduce(intersect, list(t1_index,t2_index))#where home against away
      index_len<-length(index)
      record<-vector(mode="numeric", length=index_len)
      if(index_len>3){
        for(z in 1:index_len){
          record[z]<-epl_match$home_win[index[z]]#total record of home team against away
          n<-length(record)
          avwin<-ma(record,3)
          for(i in 1:n){
            epl_match$home_record_tot[index[i]]<-avwin[i]
          }
        }
      }
    }
  }
}


#t-test
t.test(epl_match$home_team_goal, epl_match$away_team_goal, mu=0, alternative="greater", paired=TRUE)

# Paired t-test
# 
# data:  epl_match$home_win and epl_match$away_win
# t = 11.233, df = 3039, p-value < 2.2e-16
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   0.2936802       Inf
# sample estimates:
#   mean of the differences 
# 0.3440789 

#Model 1

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


epl$home_win<-factor(epl$home_win,levels=c(-1,0,1),ordered = TRUE)
model1<- polr(as.factor(home_win) ~ hometeam_avg+awayteam_avg+home_form+away_form+home_avgGoal+away_avgGoal+home_avgConcede+away_avgConcede+home_posAvg+away_posAvg+home_record_tot, data=epl_match, Hess=TRUE)
summary(model1)
ctable <- coef(summary(model1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
lrtest(model1)

# Value Std. Error      t value      p value
# hometeam_avg     0.0226678415 0.02070661   1.09471498 2.736415e-01
# awayteam_avg    -0.0577103690 0.01935317  -2.98195886 2.864105e-03
# home_form        2.3041060426 0.38648665   5.96167040 2.496724e-09
# away_form       -4.1159394672 0.33127549 -12.42452145 1.923741e-35
# home_avgGoal     0.3957247291 0.12397480   3.19197710 1.413025e-03
# away_avgGoal    -0.0038193804 0.11952918  -0.03195354 9.745091e-01
# home_avgConcede -0.6133166781 0.12919715  -4.74713779 2.063154e-06
# away_avgConcede -0.0501156628 0.08516555  -0.58844991 5.562303e-01
# home_posAvg     -0.0041677123 0.01174312  -0.35490676 7.226594e-01
# away_posAvg      0.0005472492 0.01138911   0.04805021 9.616762e-01
# home_record_tot  3.9598537041 0.21207236  18.67218198 8.337581e-78
# -1|0            -2.9384948818 1.84676649  -1.59115671 1.115743e-01
# 0|1             -0.9327702650 1.84528445  -0.50548861 6.132157e-01
# test<-predict(model1)
# length(test)


model1<- polr(as.factor(home_win) ~ awayteam_avg+home_form+away_form+home_avgGoal+home_avgConcede+home_record_tot, data=epl_match, Hess=TRUE)
summary(model1)
ctable <- coef(summary(model1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
# Value Std. Error    t value      p value
# awayteam_avg    -0.05630066 0.01559226  -3.610809 3.052438e-04
# home_form        2.31551233 0.38210015   6.059962 1.361539e-09
# away_form       -4.12914518 0.26882180 -15.360158 3.028380e-53
# home_avgGoal     0.45177677 0.11803113   3.827607 1.293952e-04
# home_avgConcede -0.66557884 0.12515406  -5.318076 1.048700e-07
# home_record_tot  3.95661951 0.21009286  18.832718 4.073093e-79
# -1|0            -4.22797007 1.16334310  -3.634328 2.787064e-04
# 0|1             -2.22629646 1.16005780  -1.919125 5.496846e-02
lrtest(model1)
# Likelihood ratio test
# 
# Model 1: as.factor(home_win) ~ awayteam_avg + home_form + away_form + 
#   home_avgGoal + home_avgConcede + home_record_tot
# Model 2: as.factor(home_win) ~ 1
# #Df  LogLik Df Chisq Pr(>Chisq)    
# 1   8 -1517.7                        
# 2   2 -2249.2 -6  1463  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

exp(model1$coefficients)

test1<-completeFun(epl_match,c("awayteam_avg","home_form","away_form","home_avgGoal","home_avgConcede","home_record_tot"))
test1$predict<-predict(model1)

#Finding Percent Correct Predictions Model 1
correct<-length(test1$predict[test1$predict==test1$home_win])
correct/length(test1$predict)


#Model 2
model2<-lm(home_team_goal ~hometeam_avg+awayteam_avg+home_form+away_form+home_avgGoal+away_avgGoal+home_avgConcede+away_avgConcede+home_posAvg+away_posAvg+home_record_tot , data=epl_match)
anova(model2)


model2<-lm(home_team_goal ~awayteam_avg+away_form+home_avgGoal+away_avgGoal+home_record_tot , data=epl_match)
summary(model2)
# lm(formula = home_team_goal ~ awayteam_avg + home_form + away_form + 
#      home_avgGoal + away_avgGoal + home_record_tot, data = epl_match)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7886 -0.7345 -0.0937  0.6090  5.8093 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      4.081144   0.541630   7.535 7.22e-14 ***
#   awayteam_avg    -0.060364   0.007489  -8.060 1.27e-15 ***
#   home_form       -0.327667   0.129542  -2.529   0.0115 *  
#   away_form       -1.253257   0.142037  -8.823  < 2e-16 ***
#   home_avgGoal     0.963847   0.045441  21.211  < 2e-16 ***
#   away_avgGoal     0.394930   0.054525   7.243 6.13e-13 ***
#   home_record_tot  1.143649   0.087375  13.089  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.038 on 2101 degrees of freedom
# (932 observations deleted due to missingness)
# Multiple R-squared:  0.4001,	Adjusted R-squared:  0.3984 
# F-statistic: 233.5 on 6 and 2101 DF,  p-value: < 2.2e-16

anova(model2)
# 
# Analysis of Variance Table
# 
# Response: home_team_goal
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# awayteam_avg       1  166.92  166.92 154.997 < 2.2e-16 ***
#   home_form          1  528.67  528.67 490.907 < 2.2e-16 ***
#   away_form          1   57.83   57.83  53.703 3.310e-13 ***
#   home_avgGoal       1  524.41  524.41 486.950 < 2.2e-16 ***
#   away_avgGoal       1   46.50   46.50  43.178 6.282e-11 ***
#   home_record_tot    1  184.50  184.50 171.322 < 2.2e-16 ***
#   Residuals       2101 2262.62    1.08                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

ggplot(model2, aes(x=.fitted, y=.resid)) + geom_point() + 
  geom_hline(yintercept=0, linetype="dashed")

## Residuals vs. explanatory variables plot
ggplot(model2, aes(x=home_form, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(model2, aes(x=away_form, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(model2, aes(x=home_avgGoal, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(model2, aes(x=away_avgGoal, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(model2, aes(x=home_record_tot, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

X <- data.frame(resid = residuals(model2))

y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm( c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   

ggplot(X, aes(sample = resid)) + stat_qq() + 
  geom_abline(intercept=int, slope=slope) 


stud.resid <- rstandard(model2)
length(stud.resid[stud.resid>3])
length(stud.resid[stud.resid<-3])

influential<-summary(influence.measures(model2))
influence.measures(model2)
Rstud <- rstudent(model2)
length(Rstud[Rstud>2])
length(Rstud[Rstud<-2])
vif(model2)
test2<-completeFun(epl_match,c("awayteam_avg","home_form","away_form","home_avgGoal","away_avgGoal","home_record_tot"))
test2$home_goal_predict<-predict(model2)


#away goal predict
model2b<-lm(away_team_goal ~hometeam_avg+awayteam_avg+home_form+away_form+home_avgGoal+away_avgGoal+home_avgConcede+away_avgConcede+home_posAvg+away_posAvg+home_record_tot , data=epl_match)
summary(model2b)
model2b<-lm(away_team_goal ~hometeam_avg+home_form+away_avgGoal+home_avgConcede+home_record_tot , data=epl_match)
summary(model2b)



ggplot(model2b, aes(x=.fitted, y=.resid)) + geom_point() + 
  geom_hline(yintercept=0, linetype="dashed")

## Residuals vs. explanatory variables plot
ggplot(model2b, aes(x=home_form, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(model2b, aes(x=hometeam_avg, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(model2b, aes(x=home_avgConcede, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(model2b, aes(x=away_avgGoal, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(model2b, aes(x=home_record_tot, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")

X <- data.frame(resid = residuals(model2b))

y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm( c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   

ggplot(X, aes(sample = resid)) + stat_qq() + 
  geom_abline(intercept=int, slope=slope) 


stud.resid <- rstandard(model2b)
length(stud.resid[stud.resid>3])
length(stud.resid[stud.resid<-3])

influential<-summary(influence.measures(model2b))
influence.measures(model2b)
Rstud <- rstudent(model2b)
length(Rstud[Rstud>2])
length(Rstud[Rstud<-2])
vif(model2b)
away_team_goal ~hometeam_avg+home_form+away_avgGoal+home_avgConcede+home_record_tot 

test2$away_goal_predict<-predict(model2b)
test2$predictions<-NA
for (i in 1:length(test2$id)){
  if(test2$home_goal_predict[i]-test2$away_goal_predict[i]>0.5){
    test2$predictions[i]<- 1
  }else if(test2$away_goal_predict[i]-test2$home_goal_predict[i]>0.5){
    test2$predictions[i]<- -1
  }else{
    test2$predictions[i]<- 0
  }
}

#Finding Percent Correct of Predictions Model 2
correct<-length(test2$predictions[test2$predictions==test2$home_win])
correct/length(test2$predictions)
