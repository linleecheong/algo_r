games <- read.csv("http://statweb.stanford.edu/~jgorham/games.csv",as.is=TRUE)
teams <- read.csv("http://statweb.stanford.edu/~jgorham/teams.csv",as.is=TRUE)

head(games)
head(teams)

all.teams<-sort(unique(c(teams$team,games$home,games$away)))
head(all.teams)

#pick columns
ii<-names(games) %in% c('home','homeScore')
games[,ii]

#find the difference in Scores
games$diff=games$homeScore-games$awayScore

total.margin <- function(team){
  #with function unwraps the game and is ready for use
  with(games,
       sum(homeScore[home==team])
       +sum(awayScore[away==team])
       -sum(awayScore[home==team])
       -sum(homeScore[away==team]))
}

number.games <- function(team){
  with(games,sum(home==team)+sum(away==team))
}

#apply second function to the first list
margins <- sapply(teams$team,total.margin)
head(margins)

number.games <- sapply(teams$team,number.games)
head(number.games)

#make sure order of names didn't get messed up
mean(names(margins)==names(number.games))

margin.per.games <- margins/number.games
rank.table <- cbind('Margin (Avg)'=margin.per.games,
                    'Margin Rank'=rank(-margin.per.games,ties='min'),
                    'AP Rank'=teams$apRank,
                    'USAT Rank'=teams$usaTodayRank)
margin.top25 <- order(margin.per.games, decreasing=TRUE)[1:25]
rank.table[margin.top25,]

#trick with sorting
ii <- with(games,order(date,home))

#predicting  margin of victory or defect for home team
# rank.difference.APrank <- function(hometeam,awayteam){
#   with(teams,
#        aprank[team==hometeam]-aprank[team==awayteam])
# }
# 
# rank.difference.usaTodayRank <- function(hometeam,awayteam){
#   with(teams,
#        usaTodayRank[team==hometeam]-usaTodayRank[team==awayteam])
# }
# 
# games$diffAP <- sapply (c(games$home,games$away),rank.difference.APrank)

y <- with(games,homeScore-awayScore)
X0 <- as.data.frame(matrix(0,nrow(games),length(all.teams)))
names(X0) <- all.teams
for(tm in all.teams){
  #[[]] gives you the column
  X0[[tm]] <- 1*(games$home==tm) -1*(games$away==tm)
}
unique(rowSums(X0))

#Identifiability Problem
X <- X0[,names(X0) != 'stanford-cardinal']
dim(X0)
dim(X)

#gives back indices
reg.season.games <- which(games$gameType=='REG')
#+0 or -1 to remove intercept
mod <- lm(y~0+.,data=X,subset=reg.season.games)
head(coef(summary(mod)))
betas <-coef(summary(mod))
betas[betas[,1]=='alabama-crimson-tide',]