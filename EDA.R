
###########################################################################################################################
## load data
df = read.csv(file.choose())

## import package
install.packages("tidyverse",dependencies = TRUE)
library(tidyverse)


## stats summary
str(df)
summary(select(df,win,mean_age,mean_hgt,sum_tot_attacks,sum_tot_kills,sum_tot_errors,
               mean_tot_hitpct,sum_tot_aces,sum_tot_serve_errors,sum_tot_blocks,sum_tot_digs))
tournament_count = df %>% group_by(tournament) %>% summarise(count = n()) %>% arrange(desc(count))
ggplot(tournament_count, aes(x = reorder(tournament,-count), y = count)) + geom_col() 

year_count = df %>% group_by(year) %>% summarise(count = n()) %>% arrange(desc(year))
ggplot(year_count, aes(x = year, y = count)) + geom_col() + theme(rect = element_rect(fill = "transparent"))

bracket_count = df %>% group_by(bracket) %>% summarise(count = n()) %>% arrange(desc(count))
ggplot(bracket_count, aes(x = reorder(bracket,-count), y = count)) + geom_col() 

###########################################################################################################################
## plot
hist(df$mean_age,prob=T, main="Team's Average Age",breaks=5, xlab="Age")
boxplot(df$mean_age, main="Team's Average Age")

hist(df$mean_hgt,prob=T, main="Team's Average Height",breaks=5, xlab="Height")
boxplot(df$mean_hgt, main="Team's Average Height")

# attack
boxplot(df$sum_tot_attacks, main="Attack", xlab="Attacking Swings Over the Net")

# point ending
boxplot(select(df,sum_tot_kills,sum_tot_aces,sum_tot_blocks), main="At Point Ending")

# defending
boxplot(df$sum_tot_digs, main="Digs")

# error
boxplot(select(df,sum_tot_errors,sum_tot_serve_errors), main="Errors")

# hit%
boxplot(df$mean_tot_hitpct, main="Hit%")

# boxplot
library(ggplot2)
ggplot(df, aes(x=result, y=mean_age))  + geom_boxplot() + ggtitle("Age") 
ggplot(df, aes(x=result, y=mean_hgt))  + geom_boxplot() + ggtitle("Height") 
ggplot(df, aes(x=result, y=sum_tot_attacks))  + geom_boxplot() + ggtitle("Attacks") 
ggplot(df, aes(x=result, y=sum_tot_kills))  + geom_boxplot() + ggtitle("Kills") 
ggplot(df, aes(x=result, y=sum_tot_errors))  + geom_boxplot() + ggtitle("Errors") 
ggplot(df, aes(x=result, y=mean_tot_hitpct))  + geom_boxplot() + ggtitle("Hit%") 
ggplot(df, aes(x=result, y=sum_tot_aces))  + geom_boxplot() + ggtitle("Aces") 
ggplot(df, aes(x=result, y=sum_tot_serve_errors))  + geom_boxplot() + ggtitle("Serve Errors") 
ggplot(df, aes(x=result, y=sum_tot_blocks))  + geom_boxplot() + ggtitle("Blocks") 
ggplot(df, aes(x=result, y=sum_tot_digs))  + geom_boxplot() + ggtitle("Digs") 

# differences between win/loss
 df %>% group_by(result) %>% summarise(mean_size = mean(sum_tot_kills))
 df %>% group_by(result) %>% summarise(mean_size = mean(sum_tot_blocks))
 df %>% group_by(result) %>% summarise(mean_size = mean(sum_tot_errors))
 df %>% group_by(result) %>% summarise(mean_size = mean(mean_tot_hitpct))
 

###########################################################################################################################
## corr

cor(df$win, df$mean_age) # 0.0354696
cor(df$win, df$mean_hgt) # 0.1258047
cor(df$win, df$sum_tot_attacks) # -0.07855409
cor(df$win, df$sum_tot_kills) # 0.2944179
cor(df$win, df$sum_tot_errors) # -0.4017174
cor(df$win, df$mean_tot_hitpct) # 0.5723484
cor(df$win, df$sum_tot_aces) # 0.3007221
cor(df$win, df$sum_tot_serve_errors) # -0.01759939
cor(df$win, df$sum_tot_blocks) # 0.2868204
cor(df$win, df$sum_tot_digs) #0.1869515

corr_to_win = as.data.frame(cor(df$win, (select(df,mean_age,mean_hgt,sum_tot_attacks,sum_tot_kills,sum_tot_errors,
                    mean_tot_hitpct,sum_tot_aces,sum_tot_serve_errors,sum_tot_blocks,sum_tot_digs))),row.names=NULL)
corr_to_win = t(corr_to_win)
colnames(corr_to_win) = 'win_correlation'
corr_to_win

corr = cor((select(df,win,mean_age,mean_hgt,sum_tot_attacks,sum_tot_kills,sum_tot_errors,
            mean_tot_hitpct,sum_tot_aces,sum_tot_serve_errors,sum_tot_blocks,sum_tot_digs)), 
    (select(df,win,mean_age,mean_hgt,sum_tot_attacks,sum_tot_kills,sum_tot_errors,
                    mean_tot_hitpct,sum_tot_aces,sum_tot_serve_errors,sum_tot_blocks,sum_tot_digs)))

heatmap(corr, margins = c(10, 10)) 
