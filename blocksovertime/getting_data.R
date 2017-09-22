# this is a little outdated (it uses plyr, which I don't use anymore), but works fine
# i is the year you want
# for 1989 to 2016, I called this function to a for loop where i took on values from 1989 to 2016
# in this for loop, I binded rows from each resulting data.frame together to make the 1989 to 2016 file

allplayerstats <- function(i) {
  require(rvest)
  require(plyr)
  possURL <- paste("http://www.basketball-reference.com/leagues/NBA_",i,"_per_poss.html", sep="")
  advancedURL <- paste("http://www.basketball-reference.com/leagues/NBA_",i,"_advanced.html", sep="")
  per36URL <- paste("http://www.basketball-reference.com/leagues/NBA_",i,"_per_minute.html", sep="")
  totURL <- paste("http://www.basketball-reference.com/leagues/NBA_",i,"_totals.html", sep="")
  pergURL <- paste("http://www.basketball-reference.com/leagues/NBA_",i,"_per_game.html", sep="")
  
  perposs <- possURL %>% read_html() %>% html_table()
  perposs <- as.data.frame(perposs, stringsAsFactors=FALSE, header=TRUE)
  perposs <- perposs[perposs$Rk != "Rk",]
  perposs <- perposs[-which(duplicated(perposs$Player)),-c(1,30)]
  colnames(perposs) <- c("player","pos","age","team","g","gs","mptot","fgp100","fgap100","fgpct",
                         "x3pp100","x3pap100","x3ppct","x2pp100","x2pap100","x2ppct","ftp100",
                         "ftap100","ftpct","orbp100","drbp100","trbp100","astp100","stlp100",
                         "blkp100","tovp100","pfp100","ptsp100","ortg","drtg")
  
  advanced <- advancedURL %>% read_html() %>% html_table()
  advanced <- as.data.frame(advanced, stringsAsFactors=FALSE, header=TRUE)
  advanced <- advanced[advanced$Rk != "Rk",]
  advanced <- advanced[-which(duplicated(advanced$Player)),-c(1,3:7,20,25)]
  colnames(advanced) <- c("player","per","tspct","x3par","ftr","orbpct","drbpct","trbpct",
                          "astpct","stlpct","blkpct","tovpct","usgrate","ows","dws","ws",
                          "wsp48","obpm","dbpm","bpm","vorp")
  
  per36 <- per36URL %>% read_html() %>% html_table()
  per36 <- as.data.frame(per36, stringsAsFactors=FALSE, header=TRUE)
  per36 <- per36[per36$Rk != "Rk",]
  per36 <- per36[-which(duplicated(per36$Player)),-c(1,3,4,5,6,7,8,11,14,17,20)]
  colnames(per36) <- c("player","fgper36","fgaper36","x3pper36","x3paper36","x2pper36",
                       "x2paper36","ftper36","ftaper36","orbper36","drbper36",
                       "trbper36","astper36","stlper36","blkper36","tovper36",
                       "pfper36","ptsper36")
  
  tot <- totURL %>% read_html() %>% html_table()
  tot <- as.data.frame(tot, stringsAsFactors=FALSE, header=TRUE)
  tot <- tot[tot$Rk != "Rk",]
  tot <- tot[-which(duplicated(tot$Player)),-c(1,3,4,5,6,7,8,11,14,17,21)]
  colnames(tot) <- c("player","fgtot","fgatot","x3ptot","x3patot","x2ptot","x2patot",
                     "efgpct","fttot","ftatot","orbtot","drbtot","trbtot","asttot",
                     "stltot","blktot","tovtot","pftot","ptstot")
  
  perg <- pergURL %>% read_html() %>% html_table()
  perg <- as.data.frame(perg, stringsAsFactors=FALSE, header=TRUE)
  perg <- perg[perg$Rk != "Rk",]
  perg <- perg[-which(duplicated(perg$Player)),-c(1,3,4,5,6,7,11,14,17,18,21)]
  colnames(perg) <- c("player","mpperg","fgperg","fgaperg","x3pperg","x3paperg",
                      "x2pperg","x2paperg","ftperg","ftaperg","orbperg",
                      "drbperg","trbperg","astperg","stlperg","blkperg",
                      "tovperg","pfperg","ptsperg")
  
  alltables <- join_all(list(perposs, advanced, per36, tot, perg), by="player")
  alltables$year <- i
  alltables$player <- gsub("[*].*$","",alltables$player)
  alltables[,-c(1,2,4)] <- apply(alltables[,-c(1,2,4)], 2, as.numeric)
  alltables$pos <- as.factor(alltables$pos)
  alltables$team <- as.factor(alltables$team)
  alltables
}
