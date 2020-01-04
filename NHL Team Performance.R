library(tidyverse)
library(rvest)
library(ggformula)
library(ggthemes)

mydata = tibble(team = factor(30*14),
                division = factor(30*14),
                season = factor(30*14),
                ptpct = numeric(30*14))

teams = c('ANA','PHX','BOS','BUF','CGY','CAR','CHI','COL','CBJ','DAL',
          'DET','EDM','FLA','LAK','MIN','MTL','NSH','NJD','NYI','NYR','OTT',
          'PHI','PIT','SJS','STL','TBL','TOR','VAN','WSH','WPG')

divs = c('Atlantic','Metropolitan','Central','Pacific')

seasons = c('05-06','06-07','07-08','08-09','09-10',
            '10-11','11-12','12-13','13-14','14-15',
            '15-16','16-17','17-18','18-19')

mydata$team = rep(teams, 14)
mydata %>% arrange(team) -> mydata
mydata$season = rep(seasons, 30)

atl = c('BOS','BUF','TBL','DET','FLA','OTT','MON','TOR')
met = c('CAR','CBJ','NJD','NYI','NYR','PHI','PIT','WSH')
cen = c('CHI','STL','WPG','NSH','DAL','MIN','COL')
pac = c('ANA','ARI','CGY','EDM','LAK','SJS','VAN','VGK')

mydata$division = ifelse(mydata$team %in% atl, 'Atlantic',
                         ifelse(mydata$team %in% met, 'Metropolitan',
                                ifelse(mydata$team %in% cen, 'Central',
                                       'Pacific')))

tm = teams[1]
teams[2] = 'PHX'
for(tm in teams){
  url = paste0('https://www.hockey-reference.com/teams/',tm,'/')
  webpage = read_html(url)
  dat = html_nodes(webpage, ':nth-child(10)')
  dat_convert = html_text(dat)
  dat_convert[c(22:17,15:8)]
  mydata %>%
    filter(team == tm) %>%
    mutate(ptpct = ifelse(team == tm, dat_convert[c(22:17,15:8)], ptpct)) -> temp
  mydata = rbind(mydata,temp)
}
mydata = mydata[!(mydata$ptpct == 0),]
mydata$team[mydata$team == 'PHX'] <- 'ARI'

vgk = tibble(team = 'VGK',
             division = 'Pacific',
             season = factor(2),
             ptpct = numeric(2))
vgk$season = c('17-18','18-19')
vgk$ptpct = c(.665,.567)

mydata = rbind(mydata, vgk)

mydata$ptpct = as.numeric(mydata$ptpct)

mydata = as.data.frame(mydata)

# write_csv(mydata, 'nhlPtPct.csv')
mydata = read_csv("~/Dropbox/R/nhlPtPct.csv")


# lets do some plotting

group.colors = c('BOS' = 'gold', 'BUF' = 'blue4',  'DET' = 'red',
                 'FLA' = 'red3', 'OTT' = 'lightsalmon4', 'TBL' = 'navy', 'TOR' = 'steelblue1',
                 'CAR' = 'firebrick2', 'CBJ' = 'blue4', 'NJD' = 'darkgreen',
                 'NYI' = 'blue3', 'NYR' = 'blue','PHI' = 'chocolate1',
                 'PIT' = 'gold', 'WSH' = 'red1', 'CHI' = 'firebrick3','STL' = 'blue3',
                 'WPG' = 'darkblue', 'NSH' = 'darkgoldenrod1', 'DAL' = 'green4',
                 'MIN' = 'darkgreen','COL' = 'maroon4', 'ANA' = 'darkorchid',
                 'ARI' = 'firebrick4', 'CGY' = 'firebrick1', 'EDM' = 'darkorange1',
                 'LAK' = 'gray0', 'SJS' = 'darkturquoise', 'VAN' = 'blue3', 'VGK' = 'gray40')

# plot using a line chart
div = divs[1]
for(div in divs){
  ggplot(mydata %>% filter(division == div), aes(season, ptpct, color = team, group = team)) +
    geom_line() +
    scale_y_continuous(limits = c(0.25, 0.85), breaks = c(0.25, 0.5, 0.75)) +
    ggtitle(paste(div, ' Division')) +
    theme_fivethirtyeight() +
    theme(axis.title = element_text()) + xlab('Season') + ylab('Point %') +
    scale_color_manual(values = group.colors) -> p
  print(p)
}

# plot using stat_smooth
div = divs[1]
for(div in divs){
 if(div == 'Pacific'){
   ggplot(mydata %>% filter(division == div, team != 'VGK'), aes(season, ptpct, color = team, group = team)) +
   geom_smooth(method = 'loess', se = F) +
   scale_y_continuous(limits = c(0.25, 0.85), breaks = c(0.25, 0.5, 0.75)) +
   ggtitle(paste(div, ' Division')) +
   theme_fivethirtyeight() +
   theme(axis.title = element_text()) + xlab('Season') + ylab('Point %') +
   scale_color_manual(values = group.colors) -> p
 } else {
   ggplot(mydata %>% filter(division == div), aes(season, ptpct, color = team, group = team)) +
     geom_smooth(method = 'loess', se = F) +
     scale_y_continuous(limits = c(0.25, 0.85), breaks = c(0.25, 0.5, 0.75)) +
     ggtitle(paste(div, ' Division')) +
     theme_fivethirtyeight() +
     theme(axis.title = element_text()) + xlab('Season') + ylab('Point %') +
     scale_color_manual(values = group.colors) -> p
 }
 print(p)
}

# plot all teams together
ggplot(mydata %>% filter(team != 'VGK') %>% na.omit(), aes(season, ptpct, color = team, group = team)) +
  geom_smooth(method = 'loess', se = F) +
  scale_y_continuous(limits = c(0.25, 0.85), breaks = c(0.25, 0.5, 0.75)) +
  ggtitle('Entire League') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + xlab('Season') + ylab('Point %') +
  scale_color_manual(values = group.colors) +
  guides(row = guide_legend(nrow = 2))
