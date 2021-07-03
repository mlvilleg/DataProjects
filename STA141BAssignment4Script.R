library(XML)
library(RCurl)
url <-  "https://www.cybercoders.com/search/"
getConnect <- getForm(url, searchterms=c("Data Analyst"), .opts = list(verbose=TRUE))
doc = htmlParse(getConnect)
posts = getNodeSet(doc, "//div[@class = 'job-listing-item']") 

#length(posts)
p = posts[[2]]



#-----------------Geting Job title
xpathSApply(p,"//div[@class = 'job-title']",xmlValue)


job1 <- xpathSApply(p,".//div[contains(@class,'job-title')]",xmlValue)

trimws(gsub("\\\r","",gsub("\\\n", "", job1)))




#-----------------Geting Job Location
location <- xpathSApply(p,".//div[contains(@class,'location')]",xmlValue)


#-----------------Geting Job Wage
wage <- xpathSApply(p,".//div[contains(@class,'wage')]",xmlValue)

#-----------------Geting Date Posted
datePosted <- xpathSApply(p,".//div[contains(@class,'posted')]",xmlValue)

#-----------------Geting Description
description <- trimws(gsub("\\\n","",gsub("\\\r","",xpathSApply(p,".//div[contains(@class,'description')]",xmlValue))))


#-----------------Geting Skills

skills <- trimws(gsub("\\\n","",gsub("\\\r","",xpathSApply(p,".//li[contains(@class,'skill-item')]",xmlValue))))




#----------create functions

#---------Title
titleFunction = function(j){
  trimws(gsub("\\\r","",gsub("\\\n", "", xpathSApply(j,".//div[contains(@class,'job-title')]",xmlValue))))
  
  
}


#--------Location
locationFunction = function(j){
  trimws(xpathSApply(j,".//div[contains(@class,'location')]",xmlValue))
} 

#--------Wage
wageFunction <- function(j){
  xpathSApply(j,".//div[contains(@class,'wage')]",xmlValue)
}

#--------Date Posted
datePostedFunction <-function(j){
  xpathSApply(j,".//div[contains(@class,'posted')]",xmlValue)
}

#--------Description
descriptionFunction <- function(j){
  trimws(gsub("\\\n","",gsub("\\\r","",xpathSApply(j,".//div[contains(@class,'description')]",xmlValue))))
}

#---------Skills

skillsFunction <- function(j){
  trimws(gsub("\\\n","",gsub("\\\r","",xpathSApply(j,".//li[contains(@class,'skill-item')]",xmlValue))))
}





#-----------Creating a dataset


jobSearch = function(query){
  txt = getForm("https://www.cybercoders.com/search/", searchterms= query, .opts = list(verbose=TRUE))
  
  doc = htmlParse(txt)
  
  jobs =getNodeSet(doc, "//div[@class = 'job-listing-item']")
  
  titleList <-  lapply(jobs,titleFunction)
  
  locationList <-  lapply(jobs,locationFunction)
  
  wageList <- lapply(jobs,wageFunction)
  
  datePostedList <- lapply(jobs,datePostedFunction)
  
  descriptionList <- lapply(jobs,descriptionFunction)
  
  skillsList <- lapply(jobs,skillsFunction)
  
  skills <- lapply(skillsList,paste,collapse = "|")
  skillsEmpty <- which(nzchar(skills) == FALSE)
  if (length(skillsEmpty) != 0){
    skills <- skills[-skillsEmpty]
    
  }
  
  
  df <- data.frame(Title = unlist(titleList),Location = unlist(locationList),Wage = unlist(wageList),DatePosted = unlist(datePostedList),Description = unlist(descriptionList),Skills = unlist(skills))
  
  
  
  return (df)
}


DfDataAnalyst <- jobSearch(query = "Data Analyst")
DfDataScientist <- jobSearch(query = "Data Scientist")
BusinessAnalyst <- jobSearch(query = "Business Analyst")
