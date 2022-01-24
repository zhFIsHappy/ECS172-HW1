#Problem 1
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



diff <- function(input){
  input<-sort(input)
  lengthin <- length(input)
  xright <- input[2:lengthin]
  output <- xright - input[1:lengthin-1]
  return(output)
}

  
mergeEm <- function(listOfVecs){
   wi <- lapply(listOfVecs,mean)
   return(wi)
}

  
waitTimes <- function(rawData){
  rawDataOrder <- rawData[order(rawData$user),]
  listOfVecs<-tapply(rawDataOrder$timestamp, rawDataOrder$user,diff)
  wi <- mergeEm(listOfVecs)
  rawDataAll <- sort(rawData[,c(2)])
  w <- mean(diff(rawDataAll))
  output <- list(wi,w)
  return(output)
}

ml100k <- getML100K(TRUE)
rawData <- ml100k[,c(1,4)]
times <- waitTimes(rawData)
wi_10 <- times[[1]][1:10]
w <- times[[2]]
print(wi_10)
print(w)







