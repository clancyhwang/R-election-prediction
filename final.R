#modified county name of first
#doc1
state=read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/stateNames.txt")
state=as.character(state[-2,])
library(XML)

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

for(i in 2:50){
  file = paste("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2012/",state[i],".xml",sep="")
  doc1 = xmlParse(file)
  first = rbind(first,getPopulation(doc1, state[i]))
}
first = as.data.frame(first)

#doc2
doc201 = read.csv("B01003.csv")[1:7857,]
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
doc201$GEO.display.label[1773] = "Dona Ana County, new-mexico"

# which(doc201$POPGROUP.display.label=="Black or African American alone")

# doc202 = read.csv("DP02.csv")
# doc202 = doc202[-(68:96),4:ncol(doc202)] # no alaska
# 
# doc203 = read.csv("DP03.csv")[1:3139,] #no puerto
# doc203 = doc203[-(68:96),4:ncol(doc203)] # no alaska

doc202 = read.csv("DP02.csv")
doc202 = doc202[-(68:96),3:ncol(doc202)] # no alaska
doc202 = doc202[,-(2:3)]
doc202$GEO.display.label = as.character(doc202$GEO.display.label)
doc202$GEO.display.label[1803-29-1] = "Dona Ana County, new-mexico"

doc203 = read.csv("DP03.csv")[1:3139,] #no puerto
doc203 = doc203[-(68:96),3:ncol(doc203)] # no alaska
doc203 = doc203[,-(2:3)]
doc203$GEO.display.label = as.character(doc202$GEO.display.label)
doc203$GEO.display.label[1803-29-1] = "Dona Ana County, new-mexico"

NewNames = character()
for(name in colnames(doc203)){
  if(name == "GEO.display.label"){
    newName = name
  }else{
    newName = paste("error_", name, sep = "")
  }
  NewNames = c(NewNames, newName)
}

colnames(doc203) = NewNames

second = merge(doc201, doc202)

second = merge(second, doc203)

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

third = data.frame("county"=county, "latitude"=latitude, "longitude"=long)

