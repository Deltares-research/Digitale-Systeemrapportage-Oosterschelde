
####===  install packages and dependancies ========
getPackage <- function(pkg){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
  return(TRUE)
}

getPackage("devtools")

if(!require("rwsapi", character.only = TRUE)){
  devtools::install_github("wstolte/rwsapi")
  library("rwsapi", character.only = TRUE)
}

require(rwsapi)
getPackage("tidyverse")
getPackage("httr")


# Zoek DDL stations bij watersysteem
makeLocationCodeListFromDonar <- function(gebied){
  require(tidyverse)
  # donarlocations <- read.csv2("donarlocations.csv") # comes with rwsapi package
  donarlocations %>% 
    filter(GEBOMSCH == gebied) %>% 
    select(LOCATIE) %>% unlist() %>% unname()
}

## test function
# makeLocationCodeListFromDonar({"Grevelingenmeer"})

## functie die stationsnaam, x, y selecteert voor lijstje stations uit DDL
makeStationTableDDL <- function(stations){
  options(digits=22)
  metadata <- rwsapi::rws_metadata() # haalt de catalogus op
  parsedmetadata <- jsonlite::fromJSON(content(metadata$resp, "text"), simplifyVector = T )
  return(DDLgetParametersForLocations(parsedmetadata, stations) %>%
           ungroup() %>%
           dplyr::distinct(Code, X, Y))
}




# input location codes as vector
download_waterbase <- function (locations, parameters = "overview_waterbase_netcdf_coupling.csv", startyear, endyear) {
  # rm(list = objects())
  
  require("RCurl")
  require("stringr")
  require("tcltk")
  require("downloader")
  
  subDir = "waterbaseDATA"
  subDir2 = "dump"
  
  destination_data = file.path(subDir)
  destination_dump = file.path(subDir2)
  
  #Create Cache folder + dump folder
  dir.create(file.path(subDir))
  dir.create(file.path(subDir2))
  
  # clean old file with aggregated data
  file.remove(file.path(destination_data, "collected-data.csv"))
  
  # open required locations
  WATERBASE_locations = locations
  
  # open required parameters
  WATERBASE_parameters = read.csv(file.path("data",parameters), sep = ";", stringsAsFactor = FALSE)
  
  #Parts URL
  WATERBASE_1 = "http://live.waterbase.nl/wboutput.cfm?loc="
  WATERBASE_2 = paste0("&byear=", startyear, "&bmonth=01&bday=01&eyear=", endyear, "&emonth=12&eday=31&output=Tekst&whichform=1")
  
  # Make clean log variable  
  DLlog = c("")
  
  # loop for locations
  for(i in 1:length(WATERBASE_locations)){
    # loop for substances/parameters
    for(j in 1:length(WATERBASE_parameters[,1])){
      # Clear old
      if(!(i == 1 & j == 1)){
        rm(list = c("WATERBASE_data","file","file2","REAL_WATERBASE_URL"))
      }
      # Naming for file name
      substantie_char = gsub("/","_",gsub(" ","_",WATERBASE_parameters[j,3]))
      get_id = gsub("%7C","",gsub("&wbwns=","",WATERBASE_parameters[j,4]))
      
      # define filenames for downloaded files
      file_location = file.path(destination_data,paste("id",get_id,"-",WATERBASE_locations[i],
                                                       "-", startyear, "01010000-", endyear, "12310000.txt",sep = ""))
      file_location_dump = file.path(destination_dump, paste(WATERBASE_locations[i],
                                                             "_",substantie_char,".txt", sep = ""))
      
      #example: http://live.waterbase.nl/wboutput.cfm?loc=NOORDWK20&wbwns=282|Chlorofyl-a+in+ug%2Fl+in+oppervlaktewater&byear=1970&bmonth=03&bday=02&eyear=2015&emonth=06&eday=05&output=Tekst&whichform=2 
      
      #Naming for URL
      locatie = WATERBASE_locations[i]
      substantie_code = WATERBASE_parameters[j,5]
      substantie = gsub("/","%2F",gsub(" ","+",WATERBASE_parameters[j,3]))
      
      #Download
      #Get link to the files
      WATERBASE_data <- paste(WATERBASE_1,locatie,'&wbwns=',substantie_code, "|", substantie,WATERBASE_2, sep ="") 
      
      #Connect to repos to get substances
      file = getURI(WATERBASE_data)
      file2 = unlist(str_split(file, "window.location ="))
      REAL_WATERBASE_URL = unlist(str_split(file2[3],"'"))[2]
      
      # Check if data exists
      if(is.na(REAL_WATERBASE_URL)){
        #Report to DLlog
        DLlog = c(DLlog,paste("The combination ",locatie," : ",substantie_char," does not exist!", sep = ""))
      }else{
        #Download the data
        download.file(REAL_WATERBASE_URL, destfile = file_location, mode = "w")
      }
    }
  }
  
  #check if file contains data else remove
  # setwd(destination_data)
  files_to_check = list.files(destination_data)
  
  for(k in 1:length(files_to_check)){
    file = readLines(file.path(subDir,files_to_check[k]))
    if(length(file) == 5){
      # save files without data in DLlog
      DLlog = c(DLlog,paste(files_to_check[k]," does not contain data!",sep = ""))
      # remove files without data
      file.remove(file.path(subDir,files_to_check[k]))
    }else{}
  }
  
  files_to_bind = list.files(destination_data)
  
  for(m in 1:length(files_to_bind)){  ##length(files_to_bind)
    data = read.csv(file.path(subDir,files_to_bind[m]), sep = ";", na.strings = "NA", skip = 3, stringsAsFactors = F) 
    if((m == 1)){
      collected <- data
    }
    collected = rbind(data, collected)
  }
  
  
  collected2 <- collected[!duplicated(collected),]
  
  
  # write.csv2(collected, file.path(subDir, "collected-data.csv"), row.names = F)
  
  #Evaluate script
  DLlog
  warnings()
  print("Done.")
  return(collected2)
}

#test function
# collected <- download_waterbase(locations = obsLocs$Code)


