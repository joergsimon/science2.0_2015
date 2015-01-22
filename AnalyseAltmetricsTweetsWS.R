pkgTest <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("gdata")
pkgTest("stringr")
pkgTest("gplots")
pkgTest("ggplot2")
pkgTest("scales")
pkgTest("reshape2")
#pkgTest("reshape")
#pkgTest("plyr")

processXlsRow <- function(x) {
  from <- x["from_user"]
  text <- x["text"]
  regx <- "(?<=^|(?<=[^a-zA-Z0-9-_\\.]))@([A-Za-z]+[A-Za-z0-9_]+)"
  mentions <<- str_extract_all(text, perl(regx))
  for (i in 1:length(mentions)) {
    m <- mentions[i]
    for (y in 1:length(m)) {
      word <- m[[y]]
      print(word)
      print(paste("d",length(word)))
      if (length(word) > 0) {
        for (z in 1:length(word)) {
          if (substring(word[[z]],1,1)=="@") {
            word[[z]] <- substring(word[[z]],2)
          }
        }
        m[[y]] <- word
      }
    }
    mentions[i] <- m
  }
  result <- c(from, mentions)
  return( result )
}

getSortedUniqueStrings <- function(fromTo) {
  allStr <- unlist(fromTo, recursive=TRUE, use.names=FALSE)
  uniques <- sort(unique(allStr))
  return ( uniques )
}

setAdjecancyPoints <- function(x, adjecancy, uniques) {
  # x == list with name (row index) and mentions
  mat <- adjecancy
  if (length(x) < 2) {
    return ( mat )
  }
  i <- which(uniques==x$from_user)
  setRow <- function(dataRow, adjRowNum) {
    for (counter in 1:length(dataRow)) {
      dataColum <- which(uniques==dataRow[counter])
      mat[adjRowNum,dataColum] <- mat[adjRowNum,dataColum]+1
    }
    return( mat )
  }
  mat <- setRow(x[[2]],i)
  return( mat )
}

buildMatrix <- function(fromToData, sortedUniqueLabels) {
  l <- length(sortedUniqueLabels)
  m <- matrix(rep.int(0,l*l),nrow=l,ncol=l)
  for (dataRows in 1:length(fromToData)) {
    m <- setAdjecancyPoints(fromToData[[dataRows]], m, sortedUniqueLabels)
  }
  
  return ( m )
}


plotHeatmap2 <- function(pots2) {
  mat1 <- melt(log(data.matrix(pots2)))
  ggplot(data=mat1, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(colour="gray80") +
    theme_gray(8) + 
    scale_fill_gradient2(low = muted("blue"), mid = "green", high = muted("red"),
                         midpoint = 3, space = "Lab", na.value = "white", guide = "colourbar")
}

main <- function() {
  xslPath <- "/Volumes/Data/Dropbox/uni/Science2.0/T2/"
  csvFile <- paste(xslPath, "tweets.csv",sep="")
  allTweets <<- read.csv2(csvFile)
  fromTo <<- apply(allTweets,1,processXlsRow)
  uniques <<- getSortedUniqueStrings(fromTo)
  m <<- buildMatrix(fromTo, uniques)
  df <- data.frame(m)
  colnames(df) <- uniques
  rownames(df) <- uniques
  return ( df ) 
}

reducedDataFrame <- function(m) {
  reduceArray <- rowSums(m) > 10 | colSums(m) > 10
  rm <- (m[reduceArray,])[,reduceArray]
  rdf <- data.frame(rm)
  rUniques <- uniques[reduceArray]
  colnames(rdf) <- rUniques
  rownames(rdf) <- rUniques
  return ( rdf )
}

test1 <- function() {
  from_user <- c("hadrian1", "cesar_the_great", "nero_the_wannabe", "hadrian1")
  text <- c("@nero_the_wannabe thx for ceaning up rome, but @cesar_the_great will still be grater, and next am I",
            "@hadrian1 you are so right",
            "@cesar_the_great I burned you legacy! And @hadrian1 better build a wall or I will come up from the dead :P",
            "@cesar_the_great @cesar_the_great -> yes you are @cesar_the_great!")
  d <- data.frame(from_user, text)
  fromTo <<- apply(d,1,processXlsRow)
  uniques <<- getSortedUniqueStrings(fromTo)
  m <<- buildMatrix(fromTo, uniques)
}

df <- main()
plotHeatmap2(df)
plotHeatmap2(reducedDataFrame(m))
#test1()
