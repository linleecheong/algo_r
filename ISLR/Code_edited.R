### 1. Loading the Data

games <- read.csv("C:\Users\licheong\Documents\Classes_ISLR\games.csv",as.is=TRUE)
teams <- read.csv("C:\Users\licheong\Documents\Classes_ISLR\teams.csv",as.is=TRUE)

head(games)
head(teams)

all.teams <- sort(unique(c(teams$team,games$home,games$away)))

### 2. How to Rank the Teams?

total.margin <- function(team) {
    with(games,
    sum(homeScore[home==team])
    + sum(awayScore[away==team])
    - sum(homeScore[away==team])
    - sum(awayScore[home==team]))
}

number.games <- function(team) {
    with(games, sum(home==team) + sum(away==team))
}

margins <- sapply(teams$team, total.margin)
number.games <- sapply(teams$team, number.games)
# check: names line up
mean(names(margins) == names(number.games))

margin.per.game <- margins / number.games

rank.table <- cbind("Margin (Avg)" = margin.per.game,
"Margin Rank"  = rank(-margin.per.game,ties="min"),
"AP Rank"      = teams$apRank,
"USAT Rank"    = teams$usaTodayRank)

margin.top25 <- order(margin.per.game,decreasing=TRUE)[1:25]
rank.table[margin.top25,]

### 3. A Linear Regression Model for Ranking Teams

y <- with(games, homeScore-awayScore)

X0 <- as.data.frame(matrix(0,nrow(games),length(all.teams)))
names(X0) <- all.teams

for(tm in all.teams) {
    X0[[tm]] <- 1*(games$home==tm) - 1*(games$away==tm)
}

### 4. An Identifiability Problem

X <- X0[,names(X0) != "stanford-cardinal"]
reg.season.games <- which(games$gameType=="REG")

mod <- lm(y ~ 0 + ., data=X, subset=reg.season.games)
head(coef(summary(mod)))

summary(mod)$r.squared

### 5. Interpreting the Model

coef(mod)["`alabama-crimson-tide`"] - coef(mod)["`air-force-falcons`"]

homeAdv <- 1 - games$neutralLocation
Xh <- cbind(homeAdv=homeAdv, X)
homeAdv.mod <- lm(y ~ 0 + ., data=Xh, subset=reg.season.games)
head(coef(summary(homeAdv.mod)), 1)
