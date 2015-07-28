# Even if the functions below seem to work, there are some
# areas that are still a bit frustrating.
# Error handling needs to be improved, some improvements might be coming up.

library(XML)
library(RCurl)

# Distance between two addresses 
distanceHomeToWork <- function(adresse_maison, adresse_agence){  
  adresse_maison <- iconv(adresse_maison,to="ASCII//TRANSLIT")
  adresse_agence <- iconv(adresse_agence,to="ASCII//TRANSLIT")
  xml.url <- paste0("http://maps.googleapis.com/maps/api/directions/xml?origin=",curlEscape(adresse_maison),"&destination=",curlEscape(adresse_agence),"&mode=driving&sensor=false&departure_time=")
  print(xml.url)
  xmlfile <- xmlParse(getURL(xml.url, .encoding="UTF-8", .mapUnicode = FALSE))
  dist <- 0
  
  tryCatch(
	{
	  # try
	  for(i in 1:length(xpathApply(xmlfile,"//step/distance"))){
		
		dist <- dist + as.numeric(sub("[a-z]+","", xmlValue(xmlChildren(xpathApply(xmlfile,"//step/distance")[[i]])$value)))
		# print(dist)
	  }
	},
	error = function (cond){
	  dist <- NA
	  message("Error :")
	  message(cond)
	  message()
	},
	warning=function (cond){
	  dist <- NA
	  message("Warning :")
	  message(cond)
	  message()
	},
	finally={
	}
  )

dist
}

# Travel time between two addresses 
timeHomeToWork <- function(adresse_maison, adresse_agence){
  adresse_maison <- iconv(adresse_maison,to="ASCII//TRANSLIT")
  adresse_agence <- iconv(adresse_agence,to="ASCII//TRANSLIT")
  xml.url <- paste0("http://maps.googleapis.com/maps/api/directions/xml?origin=",curlEscape(adresse_maison),"&destination=",curlEscape(adresse_agence),"&mode=driving&sensor=false&departure_time=")
  # print(xml.url)
  xmlfile <- xmlParse(getURL(xml.url, .encoding="UTF-8", .mapUnicode = FALSE))
  trajet <- 0
  tryCatch(
    {
      # try
      for(i in 1:length(xpathApply(xmlfile,"//step/duration"))){
        trajet <- trajet + as.numeric(sub("[a-z]+","", xmlValue(xmlChildren(xpathApply(xmlfile,"//step/duration")[[i]])$text)))
        print(trajet)
      }
    },
    error = function (cond){
      message("Error :")
      message(cond)
      message()
      
    },
    warning=function (cond){
      message("Warning :")
      message(cond)
      message()
    },
    finally={
    }
  )
  trajet
}

# Travel time between two addresses using a data.frame
timeHomeToWork_df <- function(addresses){
  trajet <- c()
  for(ligne in 1:nrow(addresses)){
    print(paste0("Data.frame line to analyse : ",ligne))
    addr_home <- paste0(as.character(addresses[ligne,"QC1_HOME_ADDR_02"])," ",as.character(addresses[ligne,"QC1_HOME_ZIPCODE"]) )
    addr_work <- paste0(as.character(addresses[ligne,"QC1_ADDR_02"])," ",as.character(addresses[ligne,"QC1_ZIP_CODE"]) )
    trajet <- c(trajet,as.numeric(timeHomeToWork(addr_home,addr_work)))
  }
  trajet
}


# Travel time tests (two addresses)
timeHomeToWork('30 rue Truffaut, 75017 Paris','13 Rue Martin Bernard, 75013 Paris')
timeHomeToWork('Restaurant Le Circus 75013 Paris','13 Rue Martin Bernard, 75013 Paris')
timeHomeToWork('Gare du Nord Paris','13 Rue Martin Bernard, 75013 Paris')
timeHomeToWork('La Primavera, Quito','Colegio La Condamine, Quito')
timeHomeToWork('Colegio La Condamine, Quito','La Primavera, Quito')

# Distance tests
distanceHomeToWork('La Primavera, Quito','Colegio La Condamine, Quito')
distanceHomeToWork('Colegio La Condamine, Quito','La Primavera, Quito')

# Travel time tests (data.frame)
# You can use read.csv to import "test_dataset.csv"
# timeHomeToWork_df(test_dataset)
