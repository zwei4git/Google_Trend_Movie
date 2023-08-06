library(readr)
library(usmap)
library(ggplot2)
library(gganimate)
library(tidyverse)
library(gifski)

setwd("Z:/ZW_R_DICT/Animated_plot/gtrend_movie/")
#Run python scraping twice to capture sometimes missed data
movie_gtrend_1 <- read_csv("Z:/ZW_R_DICT/Animated_plot/gtrend_movie/movie_gtrend_new.csv")
View(movie_gtrend)

movie_gtrend_2 <- read_csv("Z:/ZW_R_DICT/Animated_plot/gtrend_movie/movie_gtrend_aug.csv")
View(movie_gtrend)

#combine two datasets to fill up some of the missing records
movie_gtrend_1%>%
  select(-kwd)%>%
  rbind.data.frame(movie_gtrend_2)->movie_gtrend
  

#some states had missing info from api scraping 
movie_gtrend%>%
  distinct(state, date)%>%
  group_by(state)%>%
  count()%>%
  View()


#check to remove non-movie words
movie_gtrend%>%
  select(-...1)%>%
  filter(query_type=='top')%>%
  filter(!grepl(('movie tavern|movie theater'),query,perl = TRUE))%>%
  filter(!query %in% c('movies','zarina hashmi','new movie'))%>%
  group_by(state,date)%>%
  filter(value==max(value))%>%
  group_by(query)%>%
  count()
  
  
movie_gtrend%>%
  select(-...1)%>%
  filter(query_type=='top')%>%
  filter(!grepl(('movie tavern|movie theater'),query,perl = TRUE))%>%
  filter(!query %in% c('movies','zarina hashmi','new movie'))%>%
  group_by(state,date)%>%
  filter(value==max(value))%>%
  mutate(date=gsub(' .*','',date),
         query2=toupper(query),
         movie=gsub(' MOVIE|MOVIE ','',query2))%>%
  distinct(state,date,movie,.keep_all = TRUE)%>%
  ##QC! some states have two top queries on the same date
  ##check gtrend website (AK 7/16, 7/17: barbie; AR 7/3 Barb; HI 7/1, Super Mro, 7/9 Barb;
  # MS 7/3, 7/7 Barb; MT 7/8 7/9 Barb; ND 7/11 Sound; NE 7/1 Mario; NH 7/2 Barb;
  # NM 7/2 Mario 7/7 Barb; OK 7/1 Mario; RI 7/2 Barb; SC 7/8 Sound; SD 7/12 Sound;
  # WV 7/9 Barb; WY 7/10 7/11 Barb)
  #group_by(state,date)%>%filter(n()>1)%>%View()
  mutate(movie=case_when(state %in% c('AK') & date %in% c('2023-07-16','2023-07-17')~'BARBIE',
                         state %in% c('AR') & date=='2023-07-03'~'BARBIE',
                         state %in% c('HI') & date=='2023-07-01'~'SUPER MARIO',
                         state %in% c('HI') & date=='2023-07-09'~'BARBIE',
                         state %in% c('MS') & date %in% c('2023-07-03','2023-07-07')~'BARBIE',
                         state %in% c('MT') & date %in% c('2023-07-08','2023-07-09')~'BARBIE',
                         state %in% c('ND') & date=='2023-07-11'~'SOUND OF FREEDOM',
                         state %in% c('NE') & date=='2023-07-01'~'SUPER MARIO',
                         state %in% c('NH') & date=='2023-07-02'~'BARBIE',
                         state %in% c('NM') & date=='2023-07-02'~'SUPER MARIO',
                         state %in% c('NM') & date=='2023-07-07'~'BARBIE',
                         state %in% c('OK') & date=='2023-07-01'~'SUPER MARIO',
                         state %in% c('RI') & date=='2023-07-02'~'BARBIE',
                         state %in% c('SC') & date=='2023-07-08'~'SOUND OF FREEDOM',
                         state %in% c('SD') & date %in% c('2023-07-08','2023-07-12')~'SOUND OF FREEDOM',
                         state %in% c('WV') & date=='2023-07-09'~'BARBIE',
                         state %in% c('WY') & date %in% c('2023-07-10','2023-07-11')~'BARBIE',
                         movie=='MARIO'~'SUPER MARIO',
                         movie=='FREEDOM'~'SOUND OF FREEDOM',
                         TRUE~movie))%>%
  distinct(state,date,movie,.keep_all = TRUE)->movie_trend_top


#check date range
range(movie_trend_top$date)

#go to aug 1st to have gganimte show the last day of jul-31
date_range<-seq(as.Date("2023-07-01"), as.Date("2023-07-31"),by='day')

full_state_date<-expand.grid(state=unique(movie_trend_top$state),date=date_range)

#Checked all 7/31 missing data are barbie
movie_trend_top%>%
  mutate(date=as.Date(date))%>%
  right_join(full_state_date,by=c('state','date'))%>%
  mutate(movie=replace_na(movie,'NO DATA'),
         movie=case_when(movie == 'NO DATA' & date %in% as.Date(c('2023-07-31'))~'BARBIE',
                         TRUE~movie))->movie_trend_top2



movie_trend_top2%>%
  #filter(date %in% seq(as.Date("2023-07-20"), by = "day", length.out = 5))%>%
  mutate(fips=fips(state),
         date_format = format(date, '%b %d, %Y'),
         date_format=factor(date_format, unique(date_format)))->movie_trend_top3


View(movie_trend_top3)

#movie_trend_top3%>%group_by(state,date_format)%>%filter(n()>1)


table(movie_trend_top3$movie,useNA = 'ifany')

col_val <- c("BARBIE" = "maroon1", "65" = "darkgreen", "ELEMENTAL" = "gold",
             "SOUND OF FREEDOM"="navy",
             "SUPER MARIO"="tomato","NO DATA"="darkgrey")


plot_usmap(data = movie_trend_top3,exclude = 'DC',
           values = 'movie',labels = F)+
  labs(title = 'July 2023 Google Query Top Movie',
       subtitle = "Date: {closest_state}")+ 
  scale_fill_manual(values = col_val,name='MOVIE EVER TOPPED \nIN A STATE')+ 
  theme(panel.background=element_blank(),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        legend.position = 'bottom',
        legend.title  = element_text(size=16,family = "serif"),
        legend.text  = element_text(size=16,family = "serif"))+
  transition_states(date_format,transition_length = 0,state_length = 1,wrap = FALSE)->gtrend_movie


gtrend_movie2<-animate(gtrend_movie,duration = 40, fps = 5, end_pause = 5,detail = 5,
                       height=600,width=800, res=80,renderer = gifski_renderer())


anim_save("Z:/ZW_R_DICT/Animated_plot/gtrend_movie/gtrend_movie_updt.gif", gtrend_movie2)
  