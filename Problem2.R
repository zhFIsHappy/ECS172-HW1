#Problem 2
# read the table properly
tableReaded<-read.table("./ml-100k/u.item", sep="|",encoding='latin1',fill=TRUE,quote="")
# extract the release year for each movie
year <-format(tableReaded$V3,format="%Y")
year <- sub('.*(\\d{4}).*', '\\1', year)
# extract the movie name
movieName<-substr(tableReaded$V2,0,nchar(tableReaded$V2)-6)
#get the movieName length for each movie name
movieNameLength <- nchar(movieName, type = "chars", allowNA = TRUE, keepNA = NA)
# add name length and release year column to the data frame
tableReaded$nameLength <-c(movieNameLength)
tableReaded$extractedYear <- c(year)
# get the mean of movie name length according to year
afterMean <-tapply(tableReaded$nameLength,tableReaded$extractedYear,mean)
# the first column is removed due to no value
afterMean<-afterMean[-1];
# construct releaseYear Variable using combine and for loop
releaseYears <- c()
releaseYears<-c(releaseYears,"1922")
releaseYears<-c(releaseYears,"1926")
for(x in 1930:1998){
  releaseYears<-c(releaseYears,x)
}
#plot
plotted<-plot(afterMean,releaseYears,xlab="Movie Name Length", ylab="ReleaseYear")
# add title
title(main="Mean Release Year against Movie Name Length",font.main=2,cex.main=1,col.main="red")
