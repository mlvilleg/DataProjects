fileFolder <- untar("SpamAssassinTrain")
files1 = list.files(pattern = "(Spam)")
emails = lapply(files1, list.files,full.names = TRUE)


ham1 <- emails[[1]][1]
ham2 <- emails[[1]][2]
ham3 <- emails[[1]][3]
spam1 <- emails[[1]][4]
spam2 <- emails[[1]][5]

hams <- c(ham1,ham2,ham3)
spams <- c(spam1,spam2)


allFiles <- unlist(lapply(emails,function(d)  list.files(d,full.names = TRUE)))#gets all files


hamFiles <- lapply(hams,function(d)  list.files(d,full.names = TRUE))#gets all ham files
spamFiles <- lapply(spams,function(d)  list.files(d,full.names = TRUE))#gets all spam files
#----------------I will use this later to classify spam vs ham



#Split into two files to not overwhelm r when reading lines
hamFiles <- unlist(hamFiles)#unlist ham files
length(hamFiles) #returns 4864

spamFiles <- unlist(spamFiles)
length(spamFiles) #retuns 1677
readLines(spamFiles[1])[1]


ham = lapply(hamFiles,readLines)
spam = lapply(spamFiles,readLines)

All <- lapply(allFiles,readLines)#Retuns 6541
length(All) == length(spam) + length(ham)
#spam[[1]] <- NULL
spamFiles <- spamFiles[-c(1)]

msg <- function(file){
  list1 <- list()
  for(i in 1:length(file)){
    list1 <- append(list1,list(file[[i]]))
  }
  return(list1)
}

emailHam <- msg(ham)
emailSpam <- msg(spam)
spam[[1]] <- NULL #remove non email







#-----------Gets where the first line does not have "from"
getFromFalse <- function(ham){
  list = c()
  for ( i in 1:length(ham) ){
    TrueOrFalse <-  grepl("^From",ham[[i]][1]) 
    list <- append(list,TrueOrFalse )
    whereFalse <- which(list == "FALSE")
  }
  return(whereFalse)
}




length(getFromFalse(ham))/(length(ham) + length(spam)) # 0.07353616

getFromFalse(spam)
length(getFromFalse(spam))/(length(ham) + length(spam))#0.02981651


#---------------Gets headers
getHeader <- function(ham){
  endHeaderlist = c()
  HeaderList = list()
  for(i in 1:length(ham)){
    endHeader <- which(ham[[i]] == "")[1]
    endHeaderlist <-  append(endHeaderlist,endHeader)
    headers <- list(ham[[i]][1:(endHeaderlist[i]-1)])
    HeaderList <- append(HeaderList,headers )
  }
  return(HeaderList)
}





#---------------Gets header names
getHeaderNames <- function(ham){
  endHeaderlist = c()
  namesList = list()
  for(i in 1:length(ham)){
    endHeader <- which(ham[[i]] == "")[1]
    endHeaderlist <-  append(endHeaderlist,endHeader)
    headers <- ham[[i]][1:(endHeaderlist[i]-1)]
    names <- list(names(read.dcf(textConnection(headers), all=TRUE)))
    namesList <- append(namesList,names)
    
  }
  return(namesList)
}


#---------------Gets headers
getBodies<- function(ham){
  endHeaderlist = c()
  BodyList = list()
  for(i in 1:length(ham)){
    endHeader <- which(ham[[i]] == "")[1]
    endHeaderlist <-  append(endHeaderlist,endHeader)
    Bodies <- list( ham[[i]][(endHeaderlist[i]+1):length(ham[[i]])])
    BodyList <- append(BodyList,Bodies)
  }
  return(BodyList)
}



readEmail = function(file){
  hdr = getHeader(file)
  body = getBodies(file)
  hdrNames = getHeaderNames(file)
  return(list(email = msg(file),header = hdr, headersNames = hdrNames,body = body))
}
hamReadEmail <- readEmail(ham)
spamReadEmail <- readEmail(spam)
#hamFiles[[1]]

FindnumLinesInBody <- function(file){
  numLines = list()
  countLines = c()
  Email <- readEmail(file)
  Body <- Email[[3]]
  BodyList <- list(Body = Body)
  for(i in 1:length(file)){
    append <- BodyList[[1]][i]
    numLines <-  append(numLines,append)
  }
  return(lapply(numLines,length))
  
}
numLinesInBody<- FindnumLinesInBody(ham)
numLinesInBodyMean <- do.call(mean,numLinesInBody)#26

numLinesInBodySpam<- FindnumLinesInBody(spam)
numLinesInBodyMeanSpam <- do.call(mean,numLinesInBodySpam)#12


FindisSpam <- function(file){
  fileList <- list()
  for(i in 1:length(spam)){
    if (file[[i]] == spam[[i]]){
      fileList <- append(fileList,TRUE)
    }else{
      fileList <- append(fileList,FALSE)
    }
  }
  return(fileList)
}
isSpamHam <- FindisSpam(ham)
isSpamSpam <- FindisSpam(spam)


IfisRe <- function(file){
  Email <- readEmail(file)
  WhereRe <- list()
  for (i in 1:length(file)){
    subject <- which(grepl("Subject",Email[[1]][[i]]))
    List <- list(which(grepl("Re: ",Email[[1]][[i]][subject])))
    if(length(which(grepl("Re: ",Email[[1]][[i]][subject]))) > 0 ){
      WhereRe <- append(WhereRe,TRUE)
    }else{
      WhereRe <- append(WhereRe,FALSE)
    }
    
  }
  return(WhereRe)
  
}
isReHam <- IfisRe(ham)
isReSpam <- IfisRe(spam)

do.call(sum,isReHam)/(length(isReHam) + length(isReSpam))#Proportion subject of ham with Re =0.333792

do.call(sum,isReSpam)/(length(isReHam) + length(isReSpam))#  0.008562691


IsReplyTo <- function(file){
  ListTrue <- list()
  where <- lapply(getHeader(file),function(x) which(grepl("^In-Reply-To: ",x)))
  for (i in 1:length(where)){
    if(length(where[[i]])> 0){
      ListTrue <- append(ListTrue,TRUE)
    }else{
      ListTrue <- append(ListTrue,FALSE)
    }
    
  }
  return(ListTrue)
}

isInReplyToHam <- IsReplyTo(ham)
isInReplyToSpam <- IsReplyTo(spam)

do.call(sum,isInReplyToHam)/(length(isInReplyToHam) + length(isInReplyToSpam))#Ratio of ham headers with In-Reply-To

do.call(sum,isInReplyToSpam)/(length(isInReplyToHam) + length(isInReplyToSpam))#0



#Data Analysis
library(ggplot2)
library(ggpubr)
# --------------Data Visualization

matrix(unlist(emailHam), nrow=length(emailHam), byrow=T)
matrix(unlist(isReHam), nrow=length(isReHam), byrow=T)

dataFrame <-data.frame(EmailFile = matrix(unlist(hamFiles),nrow=length(hamFiles),byrow = T),isRe = matrix(unlist(isReHam), nrow=length(isReHam), byrow=T),BodyLines = matrix(unlist(numLinesInBody), nrow=length(numLinesInBody), byrow=T), HasReplyToHam = matrix(unlist(isInReplyToHam),nrow=length(isInReplyToHam), byrow=T))

HamisReGraph <- ggplot(dataFrame, aes(isRe)) + 
  geom_bar()

HamBodyLinesGraph <- ggplot(dataFrame, aes(BodyLines)) + 
  geom_bar() 

HamHasReplyTo <- ggplot(dataFrame, aes(HasReplyToHam)) + 
  geom_bar() 




dataFrameSpam <-data.frame(SpamEmailFile = matrix(unlist(spamFiles),nrow=length(spamFiles),byrow = T),SpamisRe = matrix(unlist(isReSpam), nrow=length(isReSpam), byrow=T),BodyLines = matrix(unlist(numLinesInBodySpam), nrow=length(numLinesInBodySpam), byrow=T), HasReplyToSpam = matrix(unlist(isInReplyToSpam),nrow=length(isInReplyToSpam), byrow=T))

SpamisReGraph <- ggplot(dataFrameSpam, aes(SpamisRe)) + 
  geom_bar()


SpamBodyLinesGraph <- ggplot(dataFrameSpam, aes(BodyLines)) + 
  geom_bar() 

SpamHasReplyTo <- ggplot(dataFrameSpam, aes(HasReplyToSpam)) + 
  geom_bar()
