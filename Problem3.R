#Problem 3
getML100K <- function(needDownload=FALSE){
  if (needDownload) {
    # 5 Mb
    download.file(
      'http://files.grouplens.org/datasets/movielens/ml-100k.zip',
      'ml-100k.zip')
    unzip('ml-100k.zip')
  }
  currdir <- getwd()  # leave a trail of bread crumbs
  datadir <- 'ml-100k'  # easier to hard code
  setwd(datadir)
  on.exit(setwd(currdir))
  
  # make matrices ud, uu and ui, for the main ratings data, user info
  # an item info
  
  ud <- read.table('u.data',header=F,sep='\t')
  colnames(ud) <- c('user','item','rating','timestamp')
  ud <- as.data.frame(ud)
  
  uu <- read.table('u.user',header=F,sep='|',stringsAsFactors=TRUE)
  ur <- split(ud[,3],ud[,1])  # ratings by user
  uu <- cbind(uu,sapply(ur,mean))
  uu <- cbind(uu,sapply(ur,length))
  colnames(uu) <- c('user','age','gender','occ','zip','userMean','Nuser')
  
  # reading u.item is tricky, with some problematic records etc.;
  # fortunately, we only need the last 19 fields
  z <- readLines('u.item')
  zs <- strsplit(z,'|',fixed=TRUE)  # splits to single characters
  zgl <- lapply(zs,function(charvec) charvec[6:24])  # get the genre dummies
  zgls <- t(sapply(zgl,as.integer))  # create matrix of genres
  ui <- cbind(1:nrow(zgls),zgls)
  ir <- split(ud[,3],ud[,2])  # ratings by item
  ui <- cbind(ui,sapply(ir,mean))
  ui <- cbind(ui,sapply(ir,length))
  colnames(ui) <- c('item',paste('G',1:19,sep=''),'itemMean','Nitem')
  
  setwd(currdir) # follow the trail back 
  uduu <- merge(ud,uu)
  uduuui <- merge(uduu,ui)
  # this ends up in (item,user) order, whereas we need the opposite
  outdf <- uduuui[,c(2,1,3:ncol(uduuui))]
  attr(outdf,'useritemCovs') <- c(4,4)
  attr(outdf,'userCovs') <- c(5,10)
  attr(outdf,'itemCovs') <- c(11,31)
  outdf
}

install.packages("ggplot2")
library(ggplot2)
ml100k <- getML100K()
rawData <- ml100k[,c(2,12:19,30)]
rawDataOrder <- rawData[order(rawData$item),]
data <- unique(rawDataOrder)
genreList<- data[,c(2:9)]

for (i in 1:nrow(genreList)){
  if (is.na(genreList[i,])){
    genreList[i,] = c(rep(0,8))
  }
  if (sum(genreList[i,])>1){
    a <- sample(genreList[i,genreList[i,]==1],1)
    genreList[i,] = c(rep(0,8))
    z <- colnames(a)
    genreList[i,z] = 1
    rm(a)
    rm(z)
  }
}
rm(i)

g2 <- genreList[genreList$G2 == 1,]
v_2 = vector()
for (item in rownames(g2)){
  v_2 <- append(v_2, data[item,]$itemMean)
}
rm(item)


g3 <- genreList[genreList$G3 == 1,]
v_3 = vector()
for (item in rownames(g3)){
  v_3 <- append(v_3, data[item,]$itemMean)
}
rm(item)


g4 <- genreList[genreList$G4 == 1,]
v_4 = vector()
for (item in rownames(g4)){
  v_4 <- append(v_4, data[item,]$itemMean)
}
rm(item)


g5 <- genreList[genreList$G5 == 1,]
v_5 = vector()
for (item in rownames(g5)){
  v_5 <- append(v_5, data[item,]$itemMean)
}
rm(item)


g6 <- genreList[genreList$G6 == 1,]
v_6 = vector()
for (item in rownames(g6)){
  v_6 <- append(v_6, data[item,]$itemMean)
}
rm(item)


g7 <- genreList[genreList$G7 == 1,]
v_7 = vector()
for (item in rownames(g7)){
  v_7 <- append(v_7, data[item,]$itemMean)
}
rm(item)

g8 <- genreList[genreList$G8 == 1,]
v_8 = vector()
for (item in rownames(g8)){
  v_8 <- append(v_8, data[item,]$itemMean)
}
rm(item)


g9 <- genreList[genreList$G9 == 1,]
v_9 = vector()
for (item in rownames(g9)){
  v_9 <- append(v_9, data[item,]$itemMean)
}
rm(item)



inputDF <- list(v_2,v_3,v_4,v_5,v_6,v_7,v_8,v_9)

# max.len = max(length(v_2), length(v_4), length(v_5), length(v_6), length(v_7), length(v_8), length(v_9))
# inputDF= data.frame()
# v_2 = c(v_2, rep(NA, max.len - length(v_2)))
# v_3 = c(v_3, rep(NA, max.len - length(v_3)))
# v_4 = c(v_4, rep(NA, max.len - length(v_4)))
# v_5 = c(v_5, rep(NA, max.len - length(v_5)))
# v_6 = c(v_6, rep(NA, max.len - length(v_6)))
# v_7 = c(v_7, rep(NA, max.len - length(v_7)))
# v_8 = c(v_8, rep(NA, max.len - length(v_8)))
# v_9 = c(v_9, rep(NA, max.len - length(v_9)))
# inputDF <- rbind(inputDF, data.frame(v_2))
# inputDF <- cbind(inputDF, data.frame(v_3))
# inputDF <- cbind(inputDF, data.frame(v_4))
# inputDF <- cbind(inputDF, data.frame(v_5))
# inputDF <- cbind(inputDF, data.frame(v_6))
# inputDF <- cbind(inputDF, data.frame(v_7))
# inputDF <- cbind(inputDF, data.frame(v_8))
# inputDF <- cbind(inputDF, data.frame(v_9))
# inputDF[is.na(inputDF)] <- ""

plotDensities <- function(inputDF,xName,grpName){
  lineNumber <- length(grpName)
  temp <- inputDF[[1]]
  temp<- na.omit(temp)
  getDensity<- density(temp)
  plot(getDensity,main="density plot: mean rating for different genre", xlab = xName,ylim = c(0,1),col = 1)
  for (i in 2:lineNumber){
    temp <- inputDF[[i]]
    temp<- na.omit(temp)
    getDensity<- density(temp)
    lines(getDensity,col = i)
    
  }
  legend("topleft",grpName,fill = 1:8)
  
}

plotDensities(inputDF,"Mean Rating",c("Action","Adventure","Animation","Children's","Comedy","Crime","Documentary","Drama"))



# #dev.new(width=5, height=4)
# getDensity<- density(v_2)
# plot(getDensity)
# getDensity<- density(v_3)
# lines(getDensity)
# getDensity<- density(v_4)
# lines(getDensity)
# getDensity<- density(v_5)
# lines(getDensity)
# getDensity<- density(v_6)
# lines(getDensity)
# getDensity<- density(v_7)
# lines(getDensity)
# getDensity<- density(v_8)
# lines(getDensity)
# getDensity<- density(v_9)
# lines(getDensity)




