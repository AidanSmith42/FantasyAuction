library(XML)
library(RCurl)
library(rlist)
library(tidyverse)


r <- getURL(paste0("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=AD&pos=ALL&sort=DA_AP"))

tables <- readHTMLTable(r)
tables <- list.clean(tables, fun = is.null, recursive=F)


tab <- as.data.frame(tables)
names(tab) <- c("Name", "Proj", "Avg", "drop")
tab <- tab %>% select(-("drop"))
tab$Proj <- as.numeric(gsub("\\$", "",as.character(tab$Proj)))
tab$Avg <- as.numeric(gsub("\\$", "",as.character(tab$Avg)))
test <- tables$draftanalysistable$Name

start <- gregexpr('\n', test) %>% unlist()
start <- start[seq(1,length(start), 5)]
stop <- gregexpr('- ', test) %>% as.numeric()

x <- strsplit(substring(test, start+2, stop+3) %>% trimws(), " ")
lastNames <- character()
position <- character()
team <- character()
for(i in 1:50){
  lastNames[i] <- x[[i]][2]
  position[i] <- x[[i]][length(x[[i]])]
  team[i] <- x[[i]][(length(x[[i]])-2)]
}
df <- data.frame("Name" = lastNames, "Pos" = position,'Team'=toupper(team), "Proj" = tab$Proj, "AvgCost"= tab$Avg)

r <- getURL(paste0("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=AD&pos=ALL&sort=DA_AP&count=50"))

tables <- readHTMLTable(r)
tables <- list.clean(tables, fun = is.null, recursive=F)


tab <- as.data.frame(tables)
names(tab) <- c("Name", "Proj", "Avg", "drop")
tab <- tab %>% select(-("drop"))
tab$Proj <- as.numeric(gsub("\\$", "",as.character(tab$Proj)))
tab$Avg <- as.numeric(gsub("\\$", "",as.character(tab$Avg)))
test <- tables$draftanalysistable$Name

start <- gregexpr('\n', test) %>% unlist()
start <- start[seq(1,length(start), 5)]
stop <- gregexpr('- ', test) %>% as.numeric()

x <- strsplit(substring(test, start+2, stop+3) %>% trimws(), " ")
lastNames <- character()
position <- character()
team <- character()
for(i in 1:50){
  lastNames[i] <- x[[i]][2]
  position[i] <- x[[i]][length(x[[i]])]
  team[i] <- x[[i]][(length(x[[i]])-2)]
}
df3 <- data.frame("Name" = lastNames, "Pos" = position,'Team'=toupper(team), "Proj" = tab$Proj, "AvgCost"= tab$Avg)

r <- getURL(paste0("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=AD&pos=ALL&sort=DA_AP&count=100"))

tables <- readHTMLTable(r)
tables <- list.clean(tables, fun = is.null, recursive=F)


tab <- as.data.frame(tables)
names(tab) <- c("Name", "Proj", "Avg", "drop")
tab <- tab %>% select(-("drop"))
tab$Proj <- as.numeric(gsub("\\$", "",as.character(tab$Proj)))
tab$Avg <- as.numeric(gsub("\\$", "",as.character(tab$Avg)))
test <- tables$draftanalysistable$Name

start <- gregexpr('\n', test) %>% unlist()
start <- start[seq(1,length(start), 5)]
stop <- gregexpr('- ', test) %>% as.numeric()

x <- strsplit(substring(test, start+2, stop+3) %>% trimws(), " ")
lastNames <- character()
position <- character()
team <- character()
for(i in 1:50){
  lastNames[i] <- x[[i]][2]
  position[i] <- x[[i]][length(x[[i]])]
  team[i] <- x[[i]][(length(x[[i]])-2)]
}
df4 <- data.frame("Name" = lastNames, "Pos" = position,'Team'=toupper(team), "Proj" = tab$Proj, "AvgCost"= tab$Avg)
#Join with ADP
df <- rbind(df,df3,df4)
df2 <- read_csv("fantasypros.csv") %>% as.data.frame() %>% select('Overall', 'Avg', 'Pos', 'Team')
X <- strsplit(df2$Overall, " ")
Name <- character()
for(i in 1:nrow(df2)){
 Name[i] <-  X[[i]][2]
}
df2$Pos <- substring(df2$Pos,1,2)
df2$Name <- Name
df$Name <- as.character(df$Name)
df$Pos <- as.character(df$Pos)
df$Team <- as.character(df$Team)
df2 <- df2 %>% mutate(Team = ifelse(Team== 'JAC', 'JAX', Team))
DF <- left_join(df2, df, by=c("Name", "Pos", "Team")) %>% arrange(Avg) %>% mutate(Pos = ifelse(Pos=='DS', 'D', Pos))

dfs <- DF %>% select(Overall, Pos, Avg, AvgCost)
names(dfs) <- c("Name", "Position", "Points", "Salary")

fwrite(dfs, "dfs2.csv")
