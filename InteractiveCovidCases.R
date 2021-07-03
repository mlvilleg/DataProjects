library(ggplot2)
library(plotly)
library(htmlwidgets)
library(dplyr)
library(XML)
library(RCurl)
# We manually found the following link via the Web browser's Developer Tools in the Network tab.
# We found the json files

u = "https://static01.nyt.com/newsgraphics/2020/03/16/coronavirus-maps/a3c88fc852744c0bdb9ff9ee092cc5db785705d6/data/timeseries/en/USA.json"

tt = getURLContent(u, verbose = TRUE, followlocation = TRUE)

library(RJSONIO)
us = fromJSON(tt)


# The URL for the JSON changes each day. So today's URL, we want to find it in the current day's page.
# We get the top-level page

ny = htmlParse(getURLContent("https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html?action=click&module=Top%20Stories&pgtype=Homepage"))

# Then we find the HTML elements that have an immediate child text node that contains the string USA.json.
js = xpathSApply(ny, "//*[contains(./text(), 'USA.json')]", xmlValue)

#These are <script> elements containing JavaScript, not JSON.
# But we can find the URL with a regular expression in these
u = gsub('.*"(https://[^"]+USA.json)".*', "\\1", js)
u = unique(u)
# There is only 1 URL repeated multiple times.

# So now we have these

tt = getURLContent(u, verbose = TRUE, followlocation = TRUE)
library(RJSONIO)
us = fromJSON(tt)
#length(us$data)
CA <- us[[2]][[6]][2]

#Find all counties in California

AllCounty <- Filter(function(x) all(x$hierarchy[3] == "USA-06"),us[[2]])

#Remove the 59th element as this is unknown
AllCounty <- AllCounty[-59]


#Get County Population
AllCountyPop <- unlist(lapply(AllCounty, "[[", 7))

#Get county Name
AllCountyName <- lapply(AllCounty, "[[", 1)

#Get All Cases
AllCases <- lapply(AllCounty, "[[", 12)

#Get All Deaths
AllDeaths <- lapply(AllCounty, "[[", 13)

#Get Range of dates and turn to sequence
from = us[[1]][1]
to = us[[1]][2]
Datevalues = seq(from = as.Date(from), to = as.Date(to), by = 'day')
#Turn to dates
Datevalues <- as.Date(Datevalues,"%Y-%m-%d")



rep <- length(unlist(AllDeaths))/58 
County <- rep(AllCountyName,each = rep)
#Create a matrix
DataMatrix <- as.matrix(cbind(unlist(AllCases),unlist(AllDeaths))) 

df <- as.data.frame(DataMatrix)
df$County <-  unlist(County) %>% as.factor()
df$Date <-  rep(Datevalues,58 )

df         
colnames(df)
names(df)[names(df)== "V1"] <- "Cases"
names(df)[names(df)== "V2"] <- "Deaths"


#Cases per capita
casesPop <- rep(AllCountyPop, each = rep)

DeathPerCapita <- (df$Deaths/casesPop)

#plot(x = df$Date , y = df$Deaths)

p1 <- df %>%
  highlight_key(~County) %>%
  ggplot(aes(y= Cases ,x = Date, colour=County,group=County,DeathPerCapita =DeathPerCapita,Deaths = Deaths ))+
  geom_line()+
  ggtitle("California Covid Cases By County")







CasesPlot <- ggplotly(p1, tooltip = c("x","group","y" ,"DeathPerCapita","Deaths")) %>%
  highlight(on = "plotly_hover", off= "plotly_doubleclick")


saveWidget(CasesPlot,"Cases.html",selfcontained = FALSE)





doc <- htmlParse("Cases.html")



library(htmltools)
h1 = HTML("<h2>California Covid Cases By County</h2><p>
This  plot shows covid cases for each county in California by date.
")

plott <- prependContent(CasesPlot,h1)


#Save modified HTML to a file
saveWidget(plott,"Cases.html",selfcontained = FALSE)


#
doc = htmlParse("Cases.html")
#Get head and add two scripts by creating script nodes
h = xmlRoot(doc)[["head"]]
sapply( c('HideLines.js'),function(f) newXMLNode("script", attrs = c(src = f), parent = h))

saveXML(doc,"CalCovid.html")
