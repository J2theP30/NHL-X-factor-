library(nhlscrapr)
library(plyr)
library(dplyr)
nhl<- grand.data
nhl1 <- filter(nhl,nhl$away.skaters == "6" & nhl$home.skaters == "6")
nhl2 <- filter(nhl1,etype == "SHOT"| etype == "MISS" |etype == "BLOCK")
games2 <-filter(games,season == "20142015")
nhl3 <- merge.data.frame(nhl2,games2,by.x="gcode", by.y="gcode")
nhl3$awaywin_awayshot_tf <- with(nhl3,nhl3$awayscore > nhl3$homescore & nhl3$ev.team == nhl3$awayteam.y)
nhl3$awaywin_homeshot_tf <- with(nhl3,nhl3$awayscore > nhl3$homescore & nhl3$ev.team == nhl3$hometeam.y)
nhl3$homewin_awayshot_tf <- with(nhl3,nhl3$awayscore < nhl3$homescore & nhl3$ev.team == nhl3$awayteam.y)
nhl3$homewin_homeshot_tf <- with(nhl3,nhl3$awayscore < nhl3$homescore & nhl3$ev.team == nhl3$hometeam.y)
nhl5 <- data.frame(nhl3$awaywin_awayshot_tf)
#away win and away shot (awayCFinWin)
nhl5$awayplayer1 <- ifelse(nhl3$awaywin_awayshot_tf == TRUE,
       nhl3$a1,
       " ")
nhl5$awayplayer2 <- ifelse(nhl3$awaywin_awayshot_tf== TRUE,
                           nhl3$a2,
                           " ")
nhl5$awayplayer3 <- ifelse(nhl3$awaywin_awayshot_tf == TRUE,
                           nhl3$a3,
                           " ")
nhl5$awayplayer4 <- ifelse(nhl3$awaywin_awayshot_tf == TRUE,
                           nhl3$a4,
                           " ")
nhl5$awayplayer5 <- ifelse(nhl3$awaywin_awayshot_tf == TRUE,
                           nhl3$a5,
                           " ")
nhl5$awayplayer5 <- ifelse(nhl3$awaywin_awayshot_tf == TRUE,
                           nhl3$a5,
                           " ")
#away win and away shot (homeCAinLoss)
nhl6 <- data.frame(nhl3$awaywin_awayshot_tf)
nhl6$homeplayer1 <- ifelse(nhl3$awaywin_awayshot_tf == TRUE,
                           nhl3$h1,
                           " ")
nhl6$homeplayer2 <- ifelse(nhl3$awaywin_awayshot_tf== TRUE,
                           nhl3$h2,
                           " ")
nhl6$homeplayer3 <- ifelse(nhl3$awaywin_awayshot_tf == TRUE,
                           nhl3$h3,
                           " ")
nhl6$homeplayer4 <- ifelse(nhl3$awaywin_awayshot_tf == TRUE,
                           nhl3$h4,
                           " ")
nhl6$homeplayer5 <- ifelse(nhl3$awaywin_awayshot_tf == TRUE,
                           nhl3$h5,
                           " ")
#away win and home shot (homeCFinloss)
nhl7 <- data.frame(nhl3$awaywin_homeshot_tf)
nhl7$homeplayer1 <- ifelse(nhl3$awaywin_homeshot_tf == TRUE,
                           nhl3$h1,
                           " ")
nhl7$homeplayer2 <- ifelse(nhl3$awaywin_homeshot_tf == TRUE,
                           nhl3$h2,
                           " ")
nhl7$homeplayer3 <- ifelse(nhl3$awaywin_homeshot_tf == TRUE,
                           nhl3$h3,
                           " ")
nhl7$homeplayer4 <- ifelse(nhl3$awaywin_homeshot_tf== TRUE,
                           nhl3$h4,
                           " ")
nhl7$homeplayer5 <- ifelse(nhl3$awaywin_homeshot_tf == TRUE,
                           nhl3$h5,
                           " ")
#away win and home shot (awayCAinwin)
nhl8 <- data.frame(nhl3$awaywin_homeshot_tf)
nhl8$awayplayer1 <- ifelse(nhl3$awaywin_homeshot_tf == TRUE,
                           nhl3$a1,
                           " ")
nhl8$awayplayer2 <- ifelse(nhl3$awaywin_homeshot_tf == TRUE,
                           nhl3$a2,
                           " ")
nhl8$awayplayer3 <- ifelse(nhl3$awaywin_homeshot_tf == TRUE,
                           nhl3$a3,
                           " ")
nhl8$awayplayer4 <- ifelse(nhl3$awaywin_homeshot_tf== TRUE,
                           nhl3$a4,
                           " ")
nhl8$awayplayer5 <- ifelse(nhl3$awaywin_homeshot_tf == TRUE,
                           nhl3$a5,
                           " ")
#home win and away shot (awayCFinloss)
nhl9 <- data.frame(nhl3$homewin_awayshot_tf)
nhl9$awayplayer1 <- ifelse(nhl3$homewin_awayshot_tf == TRUE,
                           nhl3$a1,
                           " ")
nhl9$awayplayer2 <- ifelse(nhl3$homewin_awayshot_tf == TRUE,
                           nhl3$a2,
                           " ")
nhl9$awayplayer3 <- ifelse(nhl3$homewin_awayshot_tf == TRUE,
                           nhl3$a3,
                           " ")
nhl9$awayplayer4 <- ifelse(nhl3$homewin_awayshot_tf== TRUE,
                           nhl3$a4,
                           " ")
nhl9$awayplayer5 <- ifelse(nhl3$homewin_awayshot_tf == TRUE,
                           nhl3$a5,
                           " ")
#home win and away shot (homeCAinwin)
nhl10 <- data.frame(nhl3$homewin_awayshot_tf)
nhl10$homeplayer1 <- ifelse(nhl3$homewin_awayshot_tf == TRUE,
                           nhl3$h1,
                           " ")
nhl10$homeplayer2 <- ifelse(nhl3$homewin_awayshot_tf == TRUE,
                           nhl3$h2,
                           " ")
nhl10$homeplayer3 <- ifelse(nhl3$homewin_awayshot_tf == TRUE,
                           nhl3$h3,
                           " ")
nhl10$homeplayer4 <- ifelse(nhl3$homewin_awayshot_tf== TRUE,
                           nhl3$h4,
                           " ")
nhl10$homeplayer5 <- ifelse(nhl3$homewin_awayshot_tf == TRUE,
                           nhl3$h5,
                           " ")
#home win and home shot (awayCAinloss)
nhl11 <- data.frame(nhl3$homewin_homeshot_tf)
nhl11$awayplayer1 <- ifelse(nhl3$homewin_homeshot_tf == TRUE,
                           nhl3$a1,
                           " ")
nhl11$awayplayer2 <- ifelse(nhl3$homewin_homeshot_tf == TRUE,
                           nhl3$a2,
                           " ")
nhl11$awayplayer3 <- ifelse(nhl3$homewin_homeshot_tf == TRUE,
                           nhl3$a3,
                           " ")
nhl11$awayplayer4 <- ifelse(nhl3$homewin_homeshot_tf== TRUE,
                           nhl3$a4,
                           " ")
nhl11$awayplayer5 <- ifelse(nhl3$homewin_homeshot_tf == TRUE,
                           nhl3$a5,
                           " ")
#home win and home shot (homeCFinwin)
nhl12 <- data.frame(nhl3$homewin_homeshot_tf)
nhl12$homeplayer1 <- ifelse(nhl3$homewin_homeshot_tf == TRUE,
                            nhl3$h1,
                            " ")
nhl12$homeplayer2 <- ifelse(nhl3$homewin_homeshot_tf == TRUE,
                            nhl3$h2,
                            " ")
nhl12$homeplayer3 <- ifelse(nhl3$homewin_homeshot_tf == TRUE,
                            nhl3$h3,
                            " ")
nhl12$homeplayer4 <- ifelse(nhl3$homewin_homeshot_tf== TRUE,
                            nhl3$h4,
                            " ")
nhl12$homeplayer5 <- ifelse(nhl3$homewin_homeshot_tf == TRUE,
                            nhl3$h5,
                            " ")
nhl5$nhl3.awaywin_awayshot_tf <- NULL
nhl8$nhl3.awaywin_homeshot_tf <- NULL
nhl6$nhl3.awaywin_awayshot_tf <- NULL
nhl7$nhl3.awaywin_homeshot_tf <- NULL
nhl9$nhl3.homewin_awayshot_tf <- NULL
nhl10$nhl3.homewin_awayshot_tf <- NULL
nhl11$nhl3.homewin_homeshot_tf <- NULL
nhl12$nhl3.homewin_homeshot_tf <- NULL

AWAY_WIN_CF <- data.frame(table(as.data.frame(data.frame(a=unlist(nhl5,use.names = FALSE)))))
HOME_LOSS_CA <- data.frame(table(as.data.frame(data.frame(a=unlist(nhl6,use.names = FALSE)))))
HOME_LOSS_CF <- data.frame(table(as.data.frame(data.frame(a=unlist(nhl7,use.names = FALSE)))))
AWAY_WIN_CA <- data.frame(table(as.data.frame(data.frame(a=unlist(nhl8,use.names = FALSE)))))
AWAY_LOSS_CF <- data.frame(table(as.data.frame(data.frame(a=unlist(nhl9,use.names = FALSE)))))
HOME_WIN_CA <- data.frame(table(as.data.frame(data.frame(a=unlist(nhl10,use.names = FALSE)))))
AWAY_LOSS_CA <- data.frame(table(as.data.frame(data.frame(a=unlist(nhl11,use.names = FALSE)))))
HOME_WIN_CF<- data.frame(table(as.data.frame(data.frame(a=unlist(nhl12,use.names = FALSE)))))

HOME_WIN_CORSI <-merge.data.frame(HOME_WIN_CF,HOME_WIN_CA,by.x = "Var1", by.y="Var1")
AWAY_WIN_CORSI <-merge.data.frame(AWAY_WIN_CF,AWAY_WIN_CA,by.x = "Var1", by.y="Var1")
HOME_LOSS_CORSI <-merge.data.frame(HOME_LOSS_CF,HOME_LOSS_CA,by.x = "Var1", by.y="Var1")
AWAY_LOSS_CORSI <-merge.data.frame(AWAY_LOSS_CF,AWAY_LOSS_CA,by.x = "Var1", by.y="Var1")

WIN_CORSI <- merge.data.frame(HOME_WIN_CORSI,AWAY_WIN_CORSI,by.x = "Var1", by.y="Var1",all.x = TRUE,all.y= TRUE)
WIN_CORSI$HOME_CF <- WIN_CORSI$Freq.x.x
WIN_CORSI$HOME_CA <- WIN_CORSI$Freq.y.x
WIN_CORSI$AWAY_CF <- WIN_CORSI$Freq.x.y
WIN_CORSI$AWAY_CA <- WIN_CORSI$Freq.y.y
WIN_CORSI$Freq.x.x <- NULL
WIN_CORSI$Freq.y.x <- NULL
WIN_CORSI$Freq.x.y <- NULL
WIN_CORSI$Freq.y.y <- NULL
WIN_CORSI$CF <- WIN_CORSI$HOME_CF + WIN_CORSI$AWAY_CF
WIN_CORSI$CA <- WIN_CORSI$HOME_CA + WIN_CORSI$AWAY_CA
WIN_CORSI$CPLUSMINUS <- WIN_CORSI$CF - WIN_CORSI$CA
WIN_CORSI$CFpercent <- (WIN_CORSI$CF/(WIN_CORSI$CF+WIN_CORSI$CA))*100

LOSS_CORSI <- merge.data.frame(HOME_LOSS_CORSI,AWAY_LOSS_CORSI,by.x = "Var1", by.y="Var1",all.x = TRUE,all.y= TRUE)
LOSS_CORSI$HOME_CF <- LOSS_CORSI$Freq.x.x
LOSS_CORSI$HOME_CA <- LOSS_CORSI$Freq.y.x
LOSS_CORSI$AWAY_CF <- LOSS_CORSI$Freq.x.y
LOSS_CORSI$AWAY_CA <- LOSS_CORSI$Freq.y.y
LOSS_CORSI$Freq.x.x <- NULL
LOSS_CORSI$Freq.y.x <- NULL
LOSS_CORSI$Freq.x.y <- NULL
LOSS_CORSI$Freq.y.y <- NULL
LOSS_CORSI$CF <- LOSS_CORSI$HOME_CF + LOSS_CORSI$AWAY_CF
LOSS_CORSI$CA <- LOSS_CORSI$HOME_CA + LOSS_CORSI$AWAY_CA
LOSS_CORSI$CPLUSMINUS <- LOSS_CORSI$CF - LOSS_CORSI$CA
LOSS_CORSI$CFpercent <- (LOSS_CORSI$CF/(LOSS_CORSI$CF+LOSS_CORSI$CA))*100

CORSI_XFACTOR <- merge.data.frame(WIN_CORSI,LOSS_CORSI,by.x = "Var1", by.y="Var1",all.x = TRUE,all.y= TRUE)
CORSI_XFACTOR$XFACTOR <- CORSI_XFACTOR$CFpercent.x - CORSI_XFACTOR$CFpercent.y
CORSI_ROSTERS <- merge.data.frame(roster.unique,CORSI_XFACTOR,by.x = "player.id", by.y="Var1",all.x = TRUE,all.y= TRUE)
CORSI_ROSTERS$TOTALCF <- CORSI_ROSTERS$CF.x + CORSI_ROSTERS$CF.y
CORSI_ROSTERS$TOTALCA <- CORSI_ROSTERS$CA.x + CORSI_ROSTERS$CA.y
CORSI_ROSTERS$TOTALCplusminus <- CORSI_ROSTERS$TOTALCF + CORSI_ROSTERS$TOTALCA
CORSI_ROSTERS$TOTALCFpercent <- (CORSI_ROSTERS$TOTALCF/ (CORSI_ROSTERS$TOTALCF + CORSI_ROSTERS$TOTALCA))*100
CORSI_ROSTERS$XFACTORplusminus <- CORSI_ROSTERS$CPLUSMINUS.x - CORSI_ROSTERS$CPLUSMINUS.y

#scatterplot of xfactor, in quadrants
plot(CORSI_ROSTERS$CFpercent.x,CORSI_ROSTERS$CFpercent.y,main = "2014-15 NHL Corsi 'X-Factor'", xlab = "Corsi For % in WIN", ylab = "Corsi For % in LOSS")
abline(v=50, lty=3)
abline(h=50, lty=3)

    