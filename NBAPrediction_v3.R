##---------------------------------------------------\
##-------------------------------------------------| |
##                                                 | |
##          ***NBA Prediction R Code***            | |
##                                                 | |
##-------------------------------------------------| |
##---------------------------------------------------/
##----Load Libraries----
library(XLConnect)
library(XML)
library(tm)
library(Rstem)
library(Snowball)
library(RWeka)

setwd("C:/Users/Nick/Documents/RCode/GA/NBA")
##----Set Parameters----
days.predict=8
year = 2014 #This is the year of the spring part of the season of interest
N_gen <- 20
N_pop <- 125
p_stat_indices <- c(3,6:45)
t_stat_indices <- 6:35
player_char_num <- 41
team_char_num <- 31
schedule_char_num<-3
health_index <- 43
top_selection <- 0.25
mutation_p <- 3/(player_char_num+team_char_num+schedule_char_num)

# schedule = read.csv("Schedule_2013_14.csv",header=TRUE,stringsAsFactors=FALSE)
Cr_yr = as.numeric(substr(year,3,4))

##----Load Webscrapers----
team.stats.webretrieval <- function(year){
  team.site1 = paste("http://www.basketball-reference.com/play-index/tsl_finder.cgi?request=1&match=single&type=team_per_game&lg_id=&year_min=",year,"&year_max=",year,"&franch_id=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=team_name&order_by_asc=Y",sep="")
  team.site2 = paste("http://www.basketball-reference.com/play-index/tsl_finder.cgi?request=1&match=single&type=advanced&lg_id=&year_min=",year,"&year_max=",year,"&franch_id=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=team_name&order_by_asc=Y",sep="")
  
  team.doc1 = readLines(team.site1)
  team.doc2 = readLines(team.site2)
  
  names.team1 = c("Games","Wins","Losses","W.L.per","MinPlayed","FG","FGA","ThreeP","ThreePA",
                  "FT","FTA","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS")
  
  team.index = c(grep("<tr  class=\"\">",team.doc1)+1,grep("</div><!-- div.table_container.p402_hide#div_xxxx -->",team.doc1))
  team.index2 = c(grep("<tr  class=\"\">",team.doc2)+1,grep("</div><!-- div.table_container.p402_hide#div_xxxx -->",team.doc2))
  num.teams = length(team.index)-1
  
  team.frame1 = data.frame("Team"=rep(0,num.teams),"Games"=rep(0,num.teams),"Wins"=rep(0,num.teams),
                           "Losses"=rep(0,num.teams),"W.L.per"=rep(0,num.teams),"MinPlayed"=rep(0,num.teams),
                           "FG"=rep(0,num.teams),"FGA"=rep(0,num.teams),"ThreeP"=rep(0,num.teams),
                           "ThreePA"=rep(0,num.teams),"FT"=rep(0,num.teams),"FTA"=rep(0,num.teams),
                           "ORB"=rep(0,num.teams),"DRB"=rep(0,num.teams),"TRB"=rep(0,num.teams),
                           "AST"=rep(0,num.teams),"STL"=rep(0,num.teams),"BLK"=rep(0,num.teams),
                           "TOV"=rep(0,num.teams),"PF"=rep(0,num.teams),"PTS"=rep(0,num.teams),
                           "MOV"=rep(0,num.teams),"SOS"=rep(0,num.teams),"SRS"=rep(0,num.teams),
                           "Pace"=rep(0,num.teams),"ORtg"=rep(0,num.teams),"DRtg"=rep(0,num.teams),
                           "eFG"=rep(0,num.teams),"TOV.per"=rep(0,num.teams),"ORB.per"=rep(0,num.teams),
                           "FT.FGA"=rep(0,num.teams),"eFG.opp"=rep(0,num.teams),"TOV.per.opp"=rep(0,num.teams),
                           "ORB.opp"=rep(0,num.teams),"FT.FGA.opp"=rep(0,num.teams))
  
  for (t in 1:num.teams){
    team.info.raw = team.doc1[team.index[t]:(team.index[t+1]-1)]
    team.info.raw2 = team.doc2[(team.index2[t]+2):(team.index2[t+1]-1)]
    
    team.name.index = grep("\" title=\"",team.info.raw)
    team.name.temp = substr(strsplit(team.info.raw[team.name.index],"title")[[1]][1],38,40)
    stats.raw = unlist(strsplit(team.info.raw[5:24],"\"right\""))
    stats.index = seq(2,40,by=2)
    stats1 = sapply(stats.raw[stats.index],function(x) as.numeric(substr(x,3,(nchar(x)-5))))
    
    stats.raw2 = unlist(strsplit(team.info.raw2[3:20],"\"right\""))
    stats.index2 = seq(2,36,by=2)
    stats2 = sapply(stats.raw2[stats.index2],function(x) as.numeric(substr(x,3,(nchar(x)-5))))
    
    team.frame1[t,]=c(team.name.temp,stats1,stats2[5:18])
    
  }
  
  team.frame1[,2:35] = sapply(team.frame1[,2:35],as.numeric)
  return(team.frame1)
}
player.stats.webretrieval <- function(year){
  player.site1 = paste("http://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=single&type=totals&per_minute_base=36&lg_id=&is_playoffs=N&year_min=",year,"&year_max=",year,"&franch_id=&season_start=1&season_end=-1&age_min=0&age_max=99&height_min=0&height_max=99&birth_country_is=Y&birth_country=&is_active=Y&is_hof=&is_as=&as_comp=gt&as_val=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5stat=&c5comp=gt&c6mult=1.0&c6stat=&order_by=player&order_by_asc=Y",sep="")
  player.site2 = "&offset="
  player.site.ad = paste("http://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=single&per_minute_base=36&type=advanced&lg_id=&is_playoffs=N&year_min=",year,"&year_max=",year,"&franch_id=&season_start=1&season_end=-1&age_min=0&age_max=99&height_min=0&height_max=99&birth_country_is=Y&birth_country=&is_active=Y&is_hof=&is_as=&as_comp=gt&as_val=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5stat=&c5comp=gt&c6mult=1.0&c6stat=&order_by=player&order_by_asc=Y",sep="")
  add.on.seq = seq(100,400,by=100)
  
  player.frame = data.frame("Player"=rep(0,0),"Season"=rep(0,0),"Age"=rep(0,0),"Team"=rep(0,0),
                            "G"=rep(0,0),"Gs"=rep(0,0),"MP"=rep(0,0),"FG"=rep(0,0),"FGA"=rep(0,0),
                            "ThreeP"=rep(0,0),"ThreePA"=rep(0,0),"FT"=rep(0,0),"FTA"=rep(0,0),"ORB"=rep(0,0),
                            "DRB"=rep(0,0),"TRB"=rep(0,0),"AST"=rep(0,0),"STL"=rep(0,0),"BLK"=rep(0,0),"TOV"=rep(0,0),
                            "PF"=rep(0,0),"PTS"=rep(0,0),"FG.per"=rep(0,0),"ThreeP.per"=rep(0,0),"FT.per"=rep(0,0),
                            "PER"=rep(0,0),"TS.per"=rep(0,0),"eFG.per"=rep(0,0),"ORB.per"=rep(0,0),"DRB.per"=rep(0,0),"TRB.per"=rep(0,0),
                            "AST.per"=rep(0,0),"STL.per"=rep(0,0),"BLK.per"=rep(0,0),"TOV.per"=rep(0,0),"USG.per"=rep(0,0),
                            "ORtg" = rep(0,0),"DRtg"=rep(0,0),"OWS"=rep(0,0),"DWS"=rep(0,0),"WS"=rep(0,0),"WS.48"=rep(0,0),
                            "FG.per"=rep(0,0),"ThreeP.per"=rep(0,0),"FT.per"=rep(0,0))
  player.counter = 1
  player.counter.ad = 1
  for (p in 1:(length(add.on.seq)+1)){
    
    if (p==1){
      player.site = player.site1
      player.site.adv = player.site.ad
    }else{
      player.site = paste(player.site1,player.site2,add.on.seq[p-1],sep="")
      player.site.adv = paste(player.site.ad,player.site2,add.on.seq[p-1],sep="")
    }
    
    player.doc1 = readLines(player.site)
    player.doc.ad = readLines(player.site.adv)
    
    player.index = c(grep("align=\"left\"  class=\" highlight_text\" cs",player.doc1),
                     grep(" div.table_container.p402_hide#div_xxxx",player.doc1))
    
    player.index.ad = c(grep("align=\"left\"  class=\" highlight_text\" cs",player.doc.ad),
                        grep(" div.table_container.p402_hide#div_xxxx",player.doc.ad))
    
    for(n in 1:(length(player.index)-1)){
      
      raw.player.info = player.doc1[player.index[n]:(player.index[n+1]-2)]
      
      player.name.index = grep("highlight_text\" csk",raw.player.info)
      raw.player.name = strsplit(strsplit(raw.player.info[player.name.index],"csk")[[1]][2],"players")[[1]][1]
      player.name = substr(raw.player.name,3,nchar(raw.player.name)-12)
      
      player.season = year
      
      player.age = substr(raw.player.info[3],23,24)
      
      player.team.index = grep("a href=\"/teams",raw.player.info)
      
      if (length(player.team.index)==0){
        player.team="TOT"
      }else{
        team.raw = strsplit(raw.player.info[player.team.index],"teams")[[1]][2]
        player.team = substr(team.raw,2,4)
      }
      
      
      p.stats.raw2 = unlist(strsplit(raw.player.info[6:26],"\"right\""))
      p.index.raw = seq(2,42,by=2)
      p.stats = sapply(p.stats.raw2[p.index.raw],function(x) as.numeric(substr(x,3,(nchar(x)-5))))
      
      p.stats[is.na(p.stats)]=0
      
      player.frame[player.counter,1:25]=c(player.name,player.season,player.age,player.team,p.stats)
      
      player.counter = player.counter + 1
      
    }#End player loop (n)
    
    for(n in 1:(length(player.index.ad)-1)){
      
      raw.player.info.ad = player.doc.ad[player.index.ad[n]:(player.index.ad[n+1]-2)]
      
      p.stats.raw2.ad = unlist(strsplit(raw.player.info.ad[9:28],"\"right\""))
      p.index.raw.ad = seq(2,40,by=2)
      p.stats.ad = sapply(p.stats.raw2.ad[p.index.raw.ad],function(x) as.numeric(substr(x,3,(nchar(x)-5))))
      
      p.stats.ad[is.na(p.stats.ad)]=0
      
      player.frame[player.counter.ad,26:45]=p.stats.ad
      
      player.counter.ad = player.counter.ad + 1
      
    }#End advanced player loop (a)
    
    print(paste("Done with player page",p,"out of",(length(add.on.seq)+1)))
  }#End page loop (p)
  
  
  #Change the player basic info to basic info/minutes played
  
  player.frame[,c(2,3,5:45)] = sapply(player.frame[,c(2,3,5:45)],as.numeric)
  
  player.frame[,8:25] = player.frame[,8:25]/player.frame$MP
  
  return(player.frame)
}
schedule.webretrieval <- function(year){
  schedule.site = paste("http://www.basketball-reference.com/leagues/NBA_",year,"_games.html",sep="")
  schedule.doc = readLines(schedule.site)
  game.index = grep("   <td align",schedule.doc)
  
  schedule.dates = sapply(schedule.doc[game.index[seq(1,length(game.index),by=8)]],function(x) substr(strsplit(x,">")[[1]][3],1,17))
  names(schedule.dates)=1:length(schedule.dates)
  schedule.dates = as.Date(schedule.dates,format="%a, %b %d, %Y")
  
  Visitor = sapply(schedule.doc[game.index[seq(1,length(game.index),by=8)+2]],function(x) strsplit(x,"/")[[1]][3])
  names(Visitor)=1:length(Visitor)
  
  Home = sapply(schedule.doc[game.index[seq(1,length(game.index),by=8)+4]],function(x) strsplit(x,"/")[[1]][3])
  names(Home)=1:length(Home)
  
  Visitor.score = sapply(schedule.doc[game.index[seq(1,length(game.index),by=8)+3]],function(x) strsplit(strsplit(x,">")[[1]][2],"<")[[1]][1])
  names(Visitor.score)=1:length(Visitor.score)
  Visitor.score = as.numeric(Visitor.score)
  
  Home.score = sapply(schedule.doc[game.index[seq(1,length(game.index),by=8)+5]],function(x) strsplit(strsplit(x,">")[[1]][2],"<")[[1]][1])
  names(Home.score)=1:length(Home.score)
  Home.score = as.numeric(Home.score)
  
  schedule.frame = data.frame("Date"=schedule.dates,"Visitor"=Visitor,"Visitor.PTS"=Visitor.score,
                              "Home"=Home,"Home.PTS"=Home.score)
  
  return(schedule.frame)
  
}
health.webretrieval <- function(){
  
  health.site = "http://espn.go.com/nba/injuries"
  health.doc = suppressWarnings(readLines(health.site))
  player.index = grep("http://espn.go.com/nba/player/_/id",health.doc)
  
  split.list = sapply(health.doc[player.index],function(x) strsplit(x,">"))
  
  name.list = sapply(1:length(split.list),function(x){
    name.before.index = grep("http://espn.go.com/nba/player/_/id",split.list[[x]])
    substr(split.list[[x]][name.before.index+1],1,nchar(split.list[[x]][name.before.index+1])-3)
  })
  last.name = sapply(name.list,function(x) strsplit(x," ")[[1]][2])
  first.name = sapply(name.list,function(x) strsplit(x," ")[[1]][1])
  name.list.v2 = paste(last.name,first.name,sep=",")
  
  status.list = sapply(1:length(split.list),function(x){
    name.before.index = grep("http://espn.go.com/nba/player/_/id",split.list[[x]])
    substr(split.list[[x]][name.before.index+4],1,nchar(split.list[[x]][name.before.index+4])-4)
  })
  
  comment.list = sapply(1:length(split.list),function(x){
    name.before.index = grep("http://espn.go.com/nba/player/_/id",split.list[[x]])
    substr(split.list[[x]][name.before.index+12],1,nchar(split.list[[x]][name.before.index+12])-4)
  })
  
  date.list = sapply(1:length(split.list),function(x){
    name.before.index = grep("http://espn.go.com/nba/player/_/id",split.list[[x]])
    paste(substr(split.list[[x]][name.before.index+6],1,nchar(split.list[[x]][name.before.index+6])-4), format(Sys.Date(),"%Y"),sep=" ")
  })
  
  date.list = as.Date(date.list,format="%b %d %Y")
  
  player.health = data.frame("Player"=name.list.v2,"Modification"=rep(1,length(name.list.v2)),
                             "Status"=status.list,"Date"=date.list,"Comment"=comment.list)
  
  player.health = player.health[!duplicated(player.health$Player),]
  
  player.health$Modification[player.health$Status=="Out"]=0
  player.health$Modification[player.health$Status=="Day-To-Day"]=0.75
  return(player.health)
}

##----Perform Data Webscrape----
team.stats = team.stats.webretrieval(year)
player.stats = player.stats.webretrieval(year)
schedule = schedule.webretrieval(year)

schedule$Visitor = as.character(schedule$Visitor)
schedule$Home = as.character(schedule$Home)
schedule$Winner = sapply(1:(dim(schedule)[1]), function(x){
    if(!is.na(schedule$Visitor.PTS[x])){
      if(schedule$Visitor.PTS[x]>schedule$Home.PTS[x]){
        schedule$Visitor[x]
      }else{
        schedule$Home[x]
      }
    }else{
      0
    }
  })

##----Load Player Health Data-----
player.health = health.webretrieval()
# player.health = read.csv("player_health.csv",header=TRUE,stringsAsFactors=FALSE)
player.health = player.health[,c(1,2)]
player.health = rbind(player.health,data.frame(
  "Player"=player.stats$Player[!player.stats$Player%in%player.health$Player],
  "Modification"=rep(1,length(player.stats$Player[!player.stats$Player%in%player.health$Player]))))

##----Set up----
today.date = Sys.Date()
output.file = paste("Predictions_",gsub("-","_",today.date),".xls",sep="")

team.list = team.stats$Team
schedule$Date = as.Date(schedule$Date,format="%b %d,%Y")
team_colnames <- colnames(team.stats)
indices_team_used <- 2:dim(team.stats)[2]
num_games <- sum(schedule$Visitor.PTS>0,na.rm=TRUE)

##----Group Players by Team----
#Make a list of players on current teams (list of size 30).
team.list.players = lapply(team.list,function(x) player.stats[player.stats$Team==x,])
names(team.list.players)=team.list

##----Set Parameter Limits----
# Make the upper and lower bound vectors for knowning wether a stat is good (0 - 1) or bad (-1 - 0) or unknown (-1 - 1)
upper_limits <- rep(1,75)
lower_limits <- rep(-1,75)

##----Set up Game Weights----

if((today.date-42)<min(schedule$Date)){
  if((today.date-21)<min(schedule$Date)){
    game_weight<- rep(1,num_games)
  }else{
    game_weight<- rep(1,num_games)
    break_1 <- min(schedule$Date)
    break_2 <- today.date - 21
    game_weight[1:sum(schedule$Date<break_1)]<-0
    x_num1 <- as.numeric(break_1)
    x_num2 <- as.numeric(break_2)
    x_seqs <- as.numeric(schedule$Date[schedule$Date>=break_1 & schedule$Date<break_2])
    y_seqs <- (1/(x_num2 - x_num1))*(x_seqs-x_num1)
    game_weight[(sum(schedule$Date<break_1)+1):sum(schedule$Date<break_2)]<- y_seqs
    game_weight[(sum(schedule$Date<break_2)+1):sum(schedule$Date<today.date)]<-1
  }
  
}else{
  game_weight<- rep(1,num_games)
  break_1 <- today.date - 42
  break_2 <- today.date - 21
  game_weight[1:sum(schedule$Date<break_1)]<-0
  x_num1 <- as.numeric(break_1)
  x_num2 <- as.numeric(break_2)
  x_seqs <- as.numeric(schedule$Date[schedule$Date>=break_1 & schedule$Date<break_2])
  y_seqs <- (1/(x_num2 - x_num1))*(x_seqs-x_num1)
  game_weight[(sum(schedule$Date<break_1)+1):sum(schedule$Date<break_2)]<- y_seqs
  game_weight[(sum(schedule$Date<break_2)+1):sum(schedule$Date<today.date)]<-1
}
game_weight <- game_weight/sum(game_weight)

num_games <- sum(game_weight>0)
base_g <- which(game_weight>0)[1]

features <- player_char_num + team_char_num + schedule_char_num

initial_pop_players <- matrix((2*runif(player_char_num*N_pop)-1),N_pop,player_char_num)
initial_pop_team <- matrix((2*runif(team_char_num*N_pop)-1),N_pop,team_char_num)
initial_pop_schedule <- matrix((2*runif(schedule_char_num*N_pop)-1),N_pop,schedule_char_num)
population = cbind(initial_pop_players,initial_pop_team,initial_pop_schedule)

##----Create a player.healthiest-----
player.healthiest = data.frame("Player"=player.health$Player,"Modification"=rep(1,dim(player.health)[1]))

##----Declare schedule stats function-----
schedule.stats = function(schedule,game_weight,g){
  today.date = Sys.Date()
  num_games <- sum(game_weight>0)
  
  team_a <- schedule$Visitor[g]#Visitor , visitor gives a schedule value of -1
  team_b <- schedule$Home[g]#Home    , home gives a schedule value of +1
  team_a_pts <- schedule$Visitor.PTS[g]
  team_b_pts <- schedule$Home.PTS[g]
  winner <- schedule$Winner[g]
  
  #Determine the last time a team played from schedule of predicted year (if first game, this stat default to 7(1 week))
  
  team_a_dates <- schedule$Date[(regexpr(team_a,schedule$Home)>0 | regexpr(team_a,schedule$Visitor)>0)&
                                  (schedule$Date<schedule$Date[g])]
  team_b_dates <- schedule$Date[(regexpr(team_b,schedule$Home)>0 | regexpr(team_b,schedule$Visitor)>0)&
                                  (schedule$Date<schedule$Date[g])]
  
  current_date <- schedule$Date[g] # not really current date, more like a game date
  
  if (any(as.numeric(current_date-team_a_dates)>0)){
    team_a_rest<- min(as.numeric(current_date-team_a_dates)[(as.numeric(current_date-team_a_dates)>0)])
  } else{
    team_a_rest <- 7
  }
  if (any(as.numeric(current_date-team_b_dates)>0)){
    team_b_rest <- min(as.numeric(current_date-team_b_dates)[(as.numeric(current_date-team_b_dates)>0)])
  }else{
    team_b_rest <- 7
  }
  
  ## Now create luck and streak stats
  #streak
  num_prev_games_a <- sum(team_a_dates<=current_date)
  num_prev_games_b <- sum(team_b_dates<=current_date)
  winner_list_a <- rep(0,num_prev_games_a)
  winner_list_b <- rep(0,num_prev_games_b)
  if (num_prev_games_a > 0){
    for (sa in 1:num_prev_games_a){# These next two loops create a binary win (1) loss (-1) vector for all previous played games
      date_checkeda <- team_a_dates[sa]
      
      if (as.character(schedule$Winner[schedule$Date==date_checkeda &(schedule$Home==team_a |
                                                                        schedule$Visitor==team_a)])==as.character(team_a)){
        winner_list_a[sa]<-1
      }else{
        winner_list_a[sa]<- -1
      }
    }
  }else{
    winner_list_a <- 1
  }
  if (num_prev_games_b >0){
    for (sb in 1:num_prev_games_b){
      date_checkedb <- team_b_dates[sb]
      
      if (as.character(schedule$Winner[schedule$Date==date_checkedb &(schedule$Home==team_b |
                                                                        schedule$Visitor==team_b)])==as.character(team_b)){
        winner_list_b[sb]<-1
      }else{
        winner_list_b[sb]<- -1
      }
    }
  }else{
    winner_list_b <- 1
  }
  
  # Now we need to turn the above win/loss binary vector into a streak number (most recent game outcome is last in vector)
  winner_list_a<-rev(winner_list_a)
  winner_list_b<-rev(winner_list_b)
  if (abs(sum(winner_list_a))==length(winner_list_a)){
    streak_a_stat <- sum(winner_list_a)
  }else{
    if (length(winner_list_a)>1){
      streak_a <- c((seq(1:length(winner_list_a))[winner_list_a==-1][1])-1,-((seq(1:length(winner_list_a))[winner_list_a==1][1])-1))
      streak_a_stat <- streak_a[abs(streak_a)==max(abs(streak_a))]
    } else{
      streak_a_stat <- winner_list_a
    }
  }
  
  if (abs(sum(winner_list_b))==length(winner_list_b)){
    streak_b_stat <- sum(winner_list_b)
  }else{
    if (length(winner_list_b)>1){
      streak_b <- c((seq(1:length(winner_list_b))[winner_list_b==-1][1])-1,-((seq(1:length(winner_list_b))[winner_list_b==1][1])-1))
      streak_b_stat <- streak_b[abs(streak_b)==max(abs(streak_b))]
    }else{
      streak_b_stat <- winner_list_b
    }
  }
  schedule.stats = data.frame("Home.Visitor"=c(1,-1),"Rest"=c(team_b_rest,team_a_rest),"Streak"=c(streak_b_stat,streak_a_stat))
  
  return(schedule.stats)
}

##----Declare fitness function----
fitness = function(individual,game_weight,schedule,p_stat_indices,t_stat_indices,p,N_pop,n,player.health){
  num_right <- 0 # initialize correct prediction number
  num_games <- sum(game_weight>0)
  base_g <- which(game_weight>0)[1]
  player_char_num = length(p_stat_indices)
  team_char_num = length(t_stat_indices)
  schedule_char_num = 3
  
  for (g in 1:num_games){
    s.stats = schedule.stats(schedule,game_weight,g)
    
    team_a <- schedule$Visitor[(g+base_g-1)]#Visitor , visitor gives a schedule value of -1
    team_b <- schedule$Home[(g+base_g-1)]#Home    , home gives a schedule value of +1
    team_a_pts <- schedule$Visitor.PTS[(g+base_g-1)]
    team_b_pts <- schedule$Home.PTS[(g+base_g-1)]
    winner <- schedule$Winner[(g+base_g-1)]
    team_a_tstats = team.stats[team.stats$Team==team_a,t_stat_indices]
    team_a_pstats = na.omit(player.stats[player.stats$Team==team_a,])
    team_b_tstats = team.stats[team.stats$Team==team_b,t_stat_indices]
    team_b_pstats = na.omit(player.stats[player.stats$Team==team_b,])
    
    team_a_names = team_a_pstats$Player
    team_b_names = team_b_pstats$Player
    
    team_a_sstats <- as.matrix(s.stats[2,])
    team_b_sstats <- as.matrix(s.stats[1,])
    
    num_players_a <- dim(team_a_pstats)[1]
    num_players_b <- dim(team_b_pstats)[1]
    team_a_pstats<-as.matrix(team_a_pstats[,p_stat_indices])
    team_b_pstats<-as.matrix(team_b_pstats[,p_stat_indices])
    team_a_tstats <- as.matrix(team_a_tstats)
    team_b_tstats <- as.matrix(team_b_tstats)
    
    team_a_pstats = na.omit(team_a_pstats)
    team_b_pstats = na.omit(team_b_pstats)
    
    health_a_mod = player.health$Modification[player.health$Player%in%team_a_names]
    health_b_mod = player.health$Modification[player.health$Player%in%team_b_names]
    
    team_a_num <- mean((team_a_pstats%*%individual[1:player_char_num])*health_a_mod) +
      team_a_tstats%*%individual[(player_char_num+1):(player_char_num+team_char_num)] +
      team_a_sstats%*%individual[(player_char_num+team_char_num+1):(player_char_num+team_char_num+schedule_char_num)]
    team_b_num <- mean((team_b_pstats%*%individual[1:player_char_num])*health_b_mod) +
      team_b_tstats%*%individual[(player_char_num+1):(player_char_num+team_char_num)] +
      team_b_sstats%*%individual[(player_char_num+team_char_num+1):(player_char_num+team_char_num+schedule_char_num)]
    
    if (team_a_num > team_b_num){test_winner <- team_a}else test_winner <- team_b
    
    if (as.character(test_winner)==winner){num_right<-num_right+game_weight[g]}
#     cat("Done with game ",g,".\n")
    
  }# End Game Loop
  
  fitness <- num_right
  cat("Initial Fitness of individual #",as.character(p),"out of", as.character(N_pop)," Generation: ",n,".\n")
  
  return(fitness)
}

##----Detmine the initial fitness-----
fitness_initial <- sapply(1:N_pop,function(x) fitness(population[x,],game_weight,schedule,
                                                      p_stat_indices,t_stat_indices,x,N_pop,0,player.healthiest))


fitness_values <- fitness_initial
max_fitness <- max(fitness_values)

##----Declare Genetic Functions-----

#Create Children
create.children = function(parent1,parent2){
  features = length(parent1)
  crossoverpoint <- round((features-2)*runif(1)+1) # Pick a spot to cross over from
  progeny <- c(parent1[1:crossoverpoint],parent2[(crossoverpoint+1):features])
  return(progeny)
}

#Mutate Individual
mutate.individual = function(individual,mutation_p,lower_limits,upper_limits){
  features = length(individual)
  for (f in 1:features){
    new_feature <- runif(1)#rnorm(1,mean_f,sdev)
    if (new_feature < lower_limits[f]){new_feature<-lower_limits[f]}
    if (new_feature > upper_limits[f]){new_feature<-upper_limits[f]}
    if (runif(1) < mutation_p){individual[f]<-new_feature}
#     cat(f)
  }# End loop through features
  
  return(individual)
}

##----Start Genetic Algorithm----
fitness.plot = rep(0,N_gen)
for (n in 1:N_gen){
  
  # Cross over
  top_numbers <- round(N_pop*top_selection+0.0001)
  ranks <- sort(fitness_values,decreasing=TRUE,index.return=TRUE)$i
  top_population <- population[ranks[1:top_numbers],]
  num_progeny <- N_pop - top_numbers
  progeny <- t(sapply(1:num_progeny,function(x) create.children(top_population[sample(seq(1:top_numbers),1),],top_population[sample(seq(1:top_numbers),1),])))
  
  # Mutation
  mutated.progeny = t(sapply(1:num_progeny,function(x) mutate.individual(progeny[x,],mutation_p,lower_limits,upper_limits)))
  
  #combine and remove populations
  population = rbind(top_population,mutated.progeny)
  rm(top_population,progeny,mutated.progeny)
  
  # Fitness
  fitness_values <- sapply(1:N_pop,function(x) fitness(population[x,],game_weight,schedule,
                                                        p_stat_indices,t_stat_indices,x,N_pop,n,player.healthiest))
  
  fitness.plot[n] <- max(fitness_values)
  
}# END GA (n)

plot(fitness.plot,type="l",main="Max Fitness over generations",xlab="Generation",ylab="Max Fitness (%accuracy)")

max_fitness = max(fitness_values)

max.individual <- colMeans(rbind(population[fitness_values==max_fitness,],population[fitness_values==max_fitness,]))

##----Declare Prediction Function-----
predict.games = function(days.predict,player.health,max.individual,schedule,game_weight,player.stats,team.stats,p_stat_indices,t_stat_indices){
  num_games <- sum(game_weight>0)
  base_g <- which(game_weight>0)[1]
  player_char_num = length(p_stat_indices)
  team_char_num = length(t_stat_indices)
  schedule_char_num = 3
  
  num_difference <- rep(0,num_games)
  psf <- rep(0,num_games)
  num_sum <- rep(0,num_games)
  sum_pts <- rep(0,num_games)
  
  for (g in base_g:(num_games)){
    s.stats = schedule.stats(schedule,game_weight,g)
    
    team_a <- schedule$Visitor[g]#Visitor , visitor gives a schedule value of -1
    team_b <- schedule$Home[g]#Home    , home gives a schedule value of +1
    team_a_pts <- schedule$Visitor.PTS[g]
    team_b_pts <- schedule$Home.PTS[g]
    winner <- schedule$Winner[g]
    team_a_tstats = team.stats[team.stats$Team==team_a,t_stat_indices]
    team_a_pstats = na.omit(player.stats[player.stats$Team==team_a,])
    team_b_tstats = team.stats[team.stats$Team==team_b,t_stat_indices]
    team_b_pstats = na.omit(player.stats[player.stats$Team==team_b,])
    
    team_a_names = team_a_pstats$Player
    team_b_names = team_b_pstats$Player
    
    team_a_sstats <- as.matrix(s.stats[2,])
    team_b_sstats <- as.matrix(s.stats[1,])
    team_a_tstats <- as.matrix(team_a_tstats)
    team_b_tstats <- as.matrix(team_b_tstats)
    
    team_a_pstats = as.matrix(team_a_pstats[,p_stat_indices])
    team_b_pstats = as.matrix(team_b_pstats[,p_stat_indices])
    
    health_a_mod = player.health$Modification[player.health$Player%in%team_a_names]
    health_b_mod = player.health$Modification[player.health$Player%in%team_b_names]
    
    team_a_num <- mean((team_a_pstats%*%max.individual[1:player_char_num])*health_a_mod) +
      team_a_tstats%*%max.individual[(player_char_num+1):(player_char_num+team_char_num)] +
      team_a_sstats%*%max.individual[(player_char_num+team_char_num+1):(player_char_num+team_char_num+schedule_char_num)]
    team_b_num <- mean((team_b_pstats%*%max.individual[1:player_char_num])*health_b_mod) +
      team_b_tstats%*%max.individual[(player_char_num+1):(player_char_num+team_char_num)] +
      team_b_sstats%*%max.individual[(player_char_num+team_char_num+1):(player_char_num+team_char_num+schedule_char_num)]
    
    if (team_a_num > team_b_num){
      num_difference[g] <- team_a_num-team_b_num
      psf[g] <- team_a_pts - team_b_pts
      
    }else {
      num_difference[g] <- team_b_num-team_a_num
      psf[g] <- team_b_pts - team_a_pts
    } 
    num_sum[g] <- team_b_num + team_a_num
    sum_pts[g] <- team_a_pts + team_b_pts
    
  }# end game loop (g)
  
  # plot(num_difference,psf)
  pt_spread.lm <- lm(as.vector(psf) ~ as.vector(num_difference))
  m_psf <- pt_spread.lm$coefficients[2]
  b_psf <- pt_spread.lm$coefficients[1]
  
  pt_sum.lm <- lm(as.vector(sum_pts) ~ as.vector(num_sum))
  m_sum <- pt_sum.lm$coefficients[2]
  b_sum <- pt_sum.lm$coefficients[1]
  # abline(pt_spread.lm)
  
  todays_date <- schedule$Date[is.na(schedule$Home.PTS)][1]#as.Date("2012-12-02")
  
  end_date <- todays_date + days.predict
  
  num_predictions <- sum(((end_date - schedule$Date)>0) & (is.na(schedule$Home.PTS)))
  
  visitor_list <- rep(0,num_predictions)
  home_list <- rep(0,num_predictions)
  winner_list <- rep(0,num_predictions)
  psf_list <- rep(0,num_predictions)
  sum_list <- rep(0,num_predictions)
  date_list <- rep(0,num_predictions)
  
  classifier <- max.individual
  start_game_index = which(is.na(schedule$Home.PTS))[1]
  
  for (d in 1:num_predictions){#cycle through the days to predict and spit out the predictions
    
    team_a <- schedule$Visitor[d+start_game_index-1]
    team_b <- schedule$Home[d+start_game_index-1]
    
    s.stats = schedule.stats(schedule,game_weight,d+start_game_index-1)
    team_a_pts <- schedule$Visitor.PTS[(d+start_game_index-1)]
    team_b_pts <- schedule$Home.PTS[(d+start_game_index-1)]
    winner <- schedule$Winner[(d+start_game_index-1)]
    team_a_tstats = team.stats[team.stats$Team==team_a,t_stat_indices]
    team_a_pstats = na.omit(player.stats[player.stats$Team==team_a,])
    team_b_tstats = team.stats[team.stats$Team==team_b,t_stat_indices]
    team_b_pstats = na.omit(player.stats[player.stats$Team==team_b,])
    
    team_a_names = team_a_pstats$Player
    team_b_names = team_b_pstats$Player
    
    team_a_pstats = as.matrix(team_a_pstats[,p_stat_indices])
    team_b_pstats = as.matrix(team_b_pstats[,p_stat_indices])
    
    team_a_sstats <- as.matrix(s.stats[2,])
    team_b_sstats <- as.matrix(s.stats[1,])
    team_a_tstats <- as.matrix(team_a_tstats)
    team_b_tstats <- as.matrix(team_b_tstats)
    
    health_a_mod = player.health$Modification[player.health$Player%in%team_a_names]
    health_b_mod = player.health$Modification[player.health$Player%in%team_b_names]
    
    team_a_num <- mean((team_a_pstats%*%max.individual[1:player_char_num])*health_a_mod) +
      team_a_tstats%*%max.individual[(player_char_num+1):(player_char_num+team_char_num)] +
      team_a_sstats%*%max.individual[(player_char_num+team_char_num+1):(player_char_num+team_char_num+schedule_char_num)]
    team_b_num <- mean((team_b_pstats%*%max.individual[1:player_char_num])*health_b_mod) +
      team_b_tstats%*%max.individual[(player_char_num+1):(player_char_num+team_char_num)] +
      team_b_sstats%*%max.individual[(player_char_num+team_char_num+1):(player_char_num+team_char_num+schedule_char_num)]
    
    
    if (team_a_num > team_b_num){
      num_calc <- team_a_num-team_b_num
      psf_list[d] <- num_calc*m_psf+b_psf
      winner_list[d] <- as.character(team_a)
    }else {
      num_calc <- team_b_num-team_a_num
      psf_list[d] <- num_calc*m_psf+b_psf
      winner_list[d] <- as.character(team_b)
    } 
    sum_list[d] <- (team_b_num + team_a_num)*m_sum + b_sum
    date_list[d] <- as.character(schedule$Date[d+start_game_index-1])
    visitor_list[d] <- as.character(team_a)
    home_list[d]<-as.character(team_b)
  }# End prediction Loop (d)
  
  prediction.frame = data.frame("Date"=date_list,"Home"=home_list,"Visitor"=visitor_list,
                                "Winner"=winner_list,"Pt.Diff"=psf_list,"Total"=sum_list)
  
  return(prediction.frame)
}

##----Prediction-----
prediction.frame = predict.games(days.predict,player.health,max.individual,schedule,
                                 game_weight,player.stats,team.stats,p_stat_indices,t_stat_indices)

##-----Output Results to Excel File------
wb <- loadWorkbook(output.file,create=TRUE)
createSheet(wb,name="Predictions")
writeWorksheet(wb,prediction.frame,sheet="Predictions",startRow=1,startCol=1)

saveWorkbook(wb)

save.image(file="NBA_Pred.RData")

# wb <- loadWorkbook("health.test.file.xls",create=TRUE)
# createSheet(wb,name="output")
# writeWorksheet(wb,player.health,sheet="output",startRow=1,startCol=1)
# 
# saveWorkbook(wb)

