# Group Name: WeTheDragons
# Group members:
# Zhou, Qi
# Huang, Hao
# Shao, Shiyun
# Lin, Wei
# Xue, Peng

library(XML)
library(rpart)
library(class)

#STEP2
#doc1
state=read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/stateNames.txt")
state=as.character(state[-2,])


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

getPopulation=function(xmlfile, state){
  county = xpathSApply(xmlfile,"//th[@class='results-county']",xmlValue)
  county = gsub(" 100.0% Reporting","",county)
  county = county[-1]
  
  county = paste(county, " County, ", sep="")
  county = paste(county, state, sep="")
  
  candidate = xpathSApply(xmlfile,"//th[@class='results-candidate']",xmlValue)
  candidate = candidate[-1]
  popular = xpathSApply(xmlfile,"//td[@class='results-popular']",xmlValue)
  popular = as.numeric(gsub(",","",popular))
  
  popular1=popular[candidate=="B. Obama (i)"]
  popular2=popular[candidate=="M. Romney"]
  party=character()
  
  for(i in 1:length(county)){
    if(popular1[i]>popular2[i]){
      party[i]="DEM"
    }else{
      party[i]="GOP"
    }
  }
  
  return(cbind(county,party))
}

doc1 = xmlParse("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2012/alabama.xml")
first = getPopulation(doc1,"Alabama")

updatedstate = gsub("-"," ", state)
for(i in 2:50){
  file = paste("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2012/",state[i],".xml",sep="")
  doc1 = xmlParse(file)
  first = rbind(first,getPopulation(doc1, simpleCap(updatedstate[i])))
}
first = as.data.frame(first)


#doc2
doc201 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01003.csv")[1:7857,]
doc201 = doc201[-(199:259),]
names = as.character(unique(doc201$GEO.display.label))

rowsNeeded = numeric()
for(name in names){
  rowNum = which(doc201$GEO.display.label == name)
  rows = doc201[rowNum,]
  if(length(rowNum) == 3){
    if(rows$HD01_VD01[2] >= rows$HD01_VD01[3]){
      num = rowNum[2]
    }else{
      num = rowNum[3]
    }
  }else if(length(rowNum) == 2){
    num = rowNum[2]
  }
  rowsNeeded = c(rowsNeeded, num)
}
doc201 = doc201[rowsNeeded, ]
doc201 = subset(doc201, select = c("GEO.display.label","POPGROUP.display.label"))
doc201$GEO.display.label=as.character(doc201$GEO.display.label)
doc201$GEO.display.label[1773] = "Dona Ana County, New Mexico"
doc201$GEO.display.label = gsub("Parish", "County", doc201$GEO.display.label)

doc202 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02.csv")
doc202 = doc202[-(68:96),3:ncol(doc202)] # no alaska
doc202 = doc202[,-(2:3)]
doc202$GEO.display.label = as.character(doc202$GEO.display.label)
doc202$GEO.display.label[1803-29-1] = "Dona Ana County, New Mexico"
doc202$GEO.display.label = gsub("Parish", "County", doc202$GEO.display.label)
NewNames = character()
for(name in colnames(doc202)){
  if(name == "GEO.display.label"){
    newName = name
  }else{
    newName = paste("household_", name, sep = "")
  }
  NewNames = c(NewNames, newName)
}
colnames(doc202) = NewNames

doc203 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03.csv")[1:3139,] #no puerto
doc203 = doc203[-(68:96),3:ncol(doc203)] # no alaska
doc203 = doc203[,-(2:3)]
doc203$GEO.display.label = as.character(doc202$GEO.display.label)
doc203$GEO.display.label[1803-29-1] = "Dona Ana County, New Mexico"
doc203$GEO.display.label = gsub("Parish", "County", doc203$GEO.display.label)

NewNames = character()
for(name in colnames(doc203)){
  if(name == "GEO.display.label"){
    newName = name
  }else{
    newName = paste("employ_", name, sep = "")
  }
  NewNames = c(NewNames, newName)
}

colnames(doc203) = NewNames

second = merge(doc201, doc202, by.x = "GEO.display.label", by.y = "GEO.display.label")

second = merge(second, doc203, by.x = "GEO.display.label", by.y = "GEO.display.label")

colnames(second)[1] = "county"

#doc3
doc3 = xmlParse("http://www.stat.berkeley.edu/users/nolan/data/Project2012/counties.gml")
latitude = as.numeric(xpathSApply(doc3,"//gml:X",xmlValue))
long = as.numeric(xpathSApply(doc3,"//gml:Y",xmlValue))
county = xpathSApply(doc3,"/doc/state/county/gml:name",xmlValue)
county1 = xpathSApply(doc3,"//gml:name",xmlValue)
state = xpathSApply(doc3,"/doc/state/gml:name",xmlValue)

rept = numeric()
for(i in 1:length(state)){
  if(i<length(state)){
    rept = c(rept, which(state[i+1]==county1)-which(state[i]==county1)-1)
  }else{
    rept = c(rept, length(county1) - which(state[i]==county1))
  }
}

state = gsub("\n   ","",state)
state = tolower(state)

state = sapply(state, simpleCap)
state = rep(state, rept)

county = gsub("\n    ","",county)
county = paste(county, ", ",sep = "")
county = paste(county, state, sep="")
county = gsub("Parish", "County", county)

third = data.frame("county"=county, "latitude"=latitude, "longitude"=long)


#merging 3 resources
combinedData = merge(first, second, by.x = "county", by.y = "county", all.x = T)
combinedData = merge(third, combinedData, by.x = "county", by.y = "county", all.y = T)
colnames(combinedData)[4] = "winParty12"
colnames(combinedData)[5] = "majorRace"

#STEP3
# Hao Huang worked on rpart() of step3, assisted by Shiyun Shao
# Qi Zhou leads the other two members worked on knn() of step3
# we all worked on manipulating 2004 data

data2004 = read.table("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt")
#bush == GOP kerry == DEM
colnames(data2004) = c("countyName", "GOP", "DEM")
data2004 = data2004[-1,]

#manipulate county names
for(i in 1:nrow(data2004)){
  row = data2004[i,]
  countyColumn = as.character(data2004$countyName[i])
  state = unlist(strsplit(countyColumn, "[,]"))[1]
  c = unlist(strsplit(countyColumn, "[,]"))[2]
  result = paste(simpleCap(c), " County, ", sep="")
  result = paste(result, simpleCap(state), sep="")
  data2004$NewCountyName[i] = result 
}

#add winParty column
for(i in 1:nrow(data2004)){
  row = data2004[i,]
  if ( as.numeric(as.character(data2004$GOP[i])) >= as.numeric(as.character(data2004$DEM[i]))){
    win = "GOP"
  }else{
    win = "DEM"
  }
  data2004$winParty[i] = win
}

data2004 = data2004[,4:5]
colnames(data2004) = c("county", "winParty04")

data04and12 = merge(data2004, combinedData, by.x = "county", by.y = "county", all.x = T)
data04and12 = data04and12[-2376,]
data04and12 = data04and12[-275,]

electionResult2012 = data.frame(data04and12[,5])
colnames(electionResult2012) = "electionResult2012"

matrix = data04and12[,-5]

#rpart
variables = c("county", "winParty04", "latitude", "longitude",
              "household_HC03_VC134","household_HC03_VC94",
              "household_HC03_VC168","household_HC03_VC40","household_HC03_VC18",
              "employ_HC03_VC13","employ_HC03_VC75","employ_HC03_VC156",
              "employ_HC03_VC166","employ_HC03_VC31","employ_HC03_VC59")
matrix = matrix[, variables]
colnames(matrix) = c("county", "winParty04", "latitude", "longitude",
                     "Foreign born",
                     "Education degree",
                     "Language",
                     "Divorced Males",
                     "Elders",
                     "Unemployed",
                     "Income",
                     "Poverty level all families",
                     "Poverty level all people",
                     "Commuting to Work",
                     "Job industry"
)
matrix$winParty04 = as.factor(matrix$winParty04)
matrix = matrix[,-1]
rpartmodel = rpart(winParty04~., method = "class", data = matrix)

# quartz(width = 7, height = 7)
png("rpartplot1.png", width = 1500, height = 1000)
plot(rpartmodel, uniform=TRUE, main="")
text(rpartmodel, use.n=TRUE, all=TRUE, cex=.8)
dev.off()
rpartSummary = summary(rpartmodel)
rpartpredicted = predict(rpartmodel)

#knn

matrix1 = matrix[complete.cases(matrix),]
matrix1 = matrix1[,2:3]
pred = knn(matrix1[,2:3], matrix1[,2:3], matrix1$winParty04, k=10)












