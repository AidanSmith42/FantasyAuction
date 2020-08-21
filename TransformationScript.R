DF <- fread("Book3.csv") %>% na.omit() %>% as.data.frame()

#DF[,1] <- gsub("\\*", "", DF[,1])
#DF[,1] <- gsub("\\+", "", DF[,1])
DF[,3] <- gsub("SUS", "", DF[,3])
DF[,3] <- gsub("PUP", "", DF[,3])

#[,4] <- gsub("NA", 0, DF[,4])
 
DF1 <- DF[,c(1:2, 5)]
DF2 <- DF[,c(3:4, 5)]

df <- left_join(DF1, DF2)

df <- df %>% mutate(ADP=ifelse(is.na(ADP), 99, ADP))

df <- df %>% group_by(Year) %>% arrange(ADP) %>% ungroup()

df <- df %>% arrange(Year) %>% filter(ADP!=99)



AAA <- df %>% filter(Year ==2010) %>%  select(VBD) 
AAA$VBD <- as.numeric(AAA$VBD)
AAa <- df %>% filter(Year ==2011) %>%  select(VBD) 
AAa$VBD <- as.numeric(AAa$VBD)
Aa <- df %>% filter(Year ==2012) %>%  select(VBD) 
Aa$VBD <- as.numeric(Aa$VBD)
AAAa <- df %>% filter(Year ==2013) %>%  select(VBD) 
AAAa$VBD <- as.numeric(AAAa$VBD)

a <- df %>% filter(Year ==2014) %>%  select(VBD) 
a$VBD <- as.numeric(a$VBD)
b <- df %>% filter(Year ==2015) %>%  select(VBD)
b$VBD <- as.numeric(b$VBD)
c <- df %>% filter(Year ==2016) %>%  select(VBD)
c$VBD <- as.numeric(c$VBD)
d <- df %>% filter(Year ==2017) %>%  select(VBD)
d$VBD <- as.numeric(d$VBD)
e <- df %>% filter(Year ==2018) %>%  select(VBD)
e$VBD <- as.numeric(e$VBD)
f <- df %>% filter(Year ==2019) %>%  select(VBD)
f$VBD <- as.numeric(f$VBD)

x <- qpcR:::cbind.na(AAA, AAa, Aa, AAAa, a,b,c,d,e,f) %>% na.omit()

Values <- numeric()
for(i in 1:33)
{
  Values[i] <- mean(as.numeric(x[i,]), trim=.30) 
}



TeamValue <- data.frame(Pick = character(12), Value = numeric(12))

Value <- c(281.3333334
,281
,261.3333333
,270
,266.1666667
,247.4166667
,240.3333333
,232.9166667
,225.3333333
,256.25
,241.1666667
,239.0833333)

#X
Value <- c(267.4166667
           ,261.0833333
           ,251.5
           ,252.6666666
           ,260.75
           ,241
           ,235.5833333
           ,255.9166667
           ,249.3333333
           ,226.25
           ,252.9166667
           ,252.8333333)


#This one for real
ValuE<- c(267.4166667
,261.0833333
,251.5
,252.6666666
,260.75
,241
,235.5833333
,226.9166667
,249.3333333
,255.25
,252.9166667
,252.8333333)

v <- c(
121.1111111
,106.1111111
,99
,113.9444444
,101.6666667
,68.33333333
,66.66666667
,91.33333333
,77.11111111
,66.61111111
,67.83333333
,64.61111111
,57
,40.55555556
,48.55555556
,53.5
,75.5
,55.61111111
,63.11111111
,55.94444444
,48.44444444
,50.05555556
,41.38888889
,42.55555556)




#TeamValue <- data.frame(Pick = character(24), Value = numeric(24))
TeamValue$Value <- v[1:12]
TeamValue$Pick <- 1:24

#TeamValue$Value[8] <- TeamValue$Value[8] - 16
#TeamValue$Value[1] <- TeamValue$Value[1] + 40
t <- data.frame(Pick = character(12), Value = numeric(12))
Values[17] <- 55
Values[28:71] <- seq(45,1,-(45)/(72-28))


t$Pick <- 1:12
t$Value[1] <- Values[1] + Values[24] + Values[25] + Values[48] + Values[49]
t$Value[2] <- Values[2] + Values[23] + Values[26] + Values[47] + Values[50]
t$Value[3] <- Values[3] + Values[22] + Values[27] + Values[46] + Values[51]
t$Value[4] <- Values[4] + Values[21] + Values[28] + Values[45] + Values[52]
t$Value[5] <- Values[5] + Values[20] + Values[29] + Values[44] + Values[53]
t$Value[6] <- Values[6] + Values[19] + Values[30] + Values[43] + Values[54]
t$Value[7] <- Values[7] + Values[18] + Values[31] + Values[42] + Values[55]
t$Value[8] <- Values[8] + Values[17] + Values[32] + Values[41] + Values[56]
t$Value[9] <- Values[9] + Values[16] + Values[33] + Values[40] + Values[57]
t$Value[10] <- Values[10] + Values[15] + Values[34] + Values[39] + Values[58]
t$Value[11] <- Values[11] + Values[14] + Values[35] + Values[38] + Values[59]
t$Value[12] <- Values[12] + Values[13] + Values[36] + Values[37] + Values[60]

#t$Value[1] <- 143
#t$Value[12] <- t$Value[8] - 35
#t$Value[8] <- 115.5
#t$Value[11] <- 113.5
#t$Value[10] <- 109
ggplot(TeamValue) + geom_bar(stat="identity", aes(Pick, Value), fill="steelblue4") + 
  geom_text(aes(x=Pick, y= Value, label=round(Value,1)), position=position_dodge(width=0.9), vjust=-0.25, size =3.5)   +   
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + theme_economist() + ggtitle("Average Team Value by Snake Draft Pick (2014-2019)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(ggplot(t) + geom_point(stat="identity", aes(Pick, Value), color="steelblue4", size = 2) + geom_line(aes(Pick, Value), color="steelblue4")+
  #geom_text(aes(x=Pick, y= Value, label=round(Value,1)), position=position_dodge(width=0.9),  size =7.5)   +   
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + theme_economist() + ggtitle("Average Snake Pick Value (2009-2019)") +
  theme(plot.title = element_text(hjust = 0.5)))

