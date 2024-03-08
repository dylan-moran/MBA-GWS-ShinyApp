#v7.2 removes dropbox fxns and only reads/writes locally




#clean sheet to help functional development
require(shiny)          # Shinyapp fxns
require(rsconnect)      # apps for Shinyapps
require(DT)             # Wrapper for JavaScript Library 'datatables'
require(shinyTime)      # time input widget for Shiny
require(shinyjs)        # improvement of UI for Shinyapps
library(leaflet)        # creates interactive maps
library(leaflet.extras) # supports leaflet
    ### library(leaflet.providers)    # do I need this for the different basemaps? couldnt get it working
library(tidyverse)      # data maniplulation
library(shinyalert)     # creates popup messages for Shiny




#Directories



Localdir <- "/Users/dylan/Documents/R Studio/Jorgensen Lab/MBA GWS ShinyApp"   # set local directory to my computer
setwd(Localdir)
    # getwd()    # remove comment to check where working directory is set



#dropbox spots...recreated on my computer within 'MBA GWS ShinyApp' folder

         # paste0(Localdir, "/", drop__)   # any time you want to set path to old dropbox dirs

dropdd <- "FinID_curator/archive"          # unused in any code? see fxns line 197

dropsc <- "FinID_curator/EncounterFiles"            # CSV save location

dropfin <- "FinID_curator/FinIDs_staging"    # a sandbox for prepping data and photos prior to an error check & 
                                             # official submission to database

droppar <- "FinID_curator/FinIDs_parent"     # folder with ALL fin photos

encounterfiles_FINAL <- "FinID_curator/EncounterFiles_FINAL"      # reviewed and submitted data gets saved here (inop)

log <- "FinID_curator/finID_SurveyLog.csv"








#list of observers available to checkbox
flds <- list(
  observers = c("PK", "SA", "SJ", "JM", "TC", "TW", "EM", "OJ", "DM"),
  users = c("Moran", "Jorgensen", "Mailander", "Chapple", "Kanive", "Anderson"),
  sites = c("PR", "FAR", "AN", "APT"),
  mandatory = c("user", "sex", "size"),
  #exists = c(tools::file_path_sans_ext(drop_dir(droppar)$name),
  coords = list(matrix(c(-123.00, 38.24), nrow=1), #Tomales station
                 matrix(c(-123.0058, 37.6979), nrow=1), #FAR
                 matrix(c(-122.34, 37.11), nrow=1), #Ano station
                 matrix(c(-121.93, 36.97), nrow=1))  # APT
)
names(flds$coords) <- flds$sites





##
#attempt to generalize
entries <- list(
  #list(id = "site.phid", type = "select", title = "Monitoring Site", choices = flds$sites),
  list(id = "date.phid", type = "date", title = "Date", value = Sys.Date(), format = "yyyy-mm-dd"),
  list(id = "sighting", type = "text", title = "Sighting #", placeholder = "0?"),
  list(id = "matchsugg", type = "text", title = "Suggested Match", placeholder = "handle, tag#?"),
  list(id = "time", type = "text", title = "Time of sighting", placeholder = "0915")
  #list(id = "sex", type = "select", title = "Sex (U if unknown", choices = c("U","M","F"), selected="U"),
  #list(id = "size", type = )
)


#add a red star next to mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}



#get data (csv)
# loadData <- function(dir) {
#   #add pattern for unique fN for survey
#   files <- list.files(file.path(dir), full.names = TRUE)
#   data <- lapply(files, read.csv, stringsAsFactors = FALSE)
#   data <- dplyr::bind_rows(data)
#   data
# }






loadData2 <- function() {          # output df is called 'EncounterData'  |  dt.filt = NULL, loc.filt = NULL
                                                                  # loc and dt filts are disabled
  dir <- list.files(dropsc)                # changed drop_dir to list.files
  #list files 
  files <- as.character(dir)
  #get metadata
  files <- bind_cols(filename = files, parse_phid(files))
  files$path <- paste0(dropsc, "/", files$filename)
  # if(!is.null(loc.filt)){
  #   #extract only files associated w/ selected survey
  #   print(loc.filt)
  #   print(dt.filt)
  #   files <- files %>% filter(loc == loc.filt)
  #   print(dim(files))
  # }
  # 
  # if(!is.null(dt.filt)){
  #   print(dim(files))
  #   files <- files %>% filter(date == dt.filt)
  #   print(dim(files))
  # }
  #read in data, all character classes
  EncounterData <- lapply(files$path, read.csv, stringsAsFactors = F,          # changed drop_read_csv to read.csv
                          as.is = T, colClasses = "character")
  EncounterData <- dplyr::bind_rows(EncounterData)
  
  # idk why these are here, maybe old QC?
  print(class(EncounterData))
  print(nrow(EncounterData))
  print(ncol(EncounterData))
  return(EncounterData)
}



### new loaddata2table fxn ###

loadData2Table <- function(){            # dt.filt = NULL, loc.filt = NULL

  loadedData<- loadData2()  # removed filts for now   | dt.filt = rev.dt(), loc.filt = rev.loc()
  loadedDT <- data.frame(loadedData)
  return(loadedDT)
  
}

##############################













#parsing data from phids
#    takes ID from naming convention and extracts dates and No.
parse_phid <- function(ids){
  require(stringr)
  if(any(nchar(ids)>11, na.rm = T)){
    #if ids are longer than standard id (ie. pathways), extract just the phid
    ids <- str_extract(ids, "([A-Z]{2,3})([0-9]){8}")
  }
  
  id.dat <- data.frame(phid = ids) %>%            #convert to df for stringr tools
    mutate(loc = as.vector(str_match(phid, "^[:alpha:]*")),
           code = str_extract(phid, "[0-9]+"),
           #8 digit #'s are stand eval; YYMMDD w/ 2 digi shark-du-jur#
           yr = ifelse(nchar(code)==8, substr(code, 1,2), NA),
           month = ifelse(nchar(code)==8, substr(code,3,4), NA),
           day = ifelse(nchar(code)==8, substr(code,5,6), NA),
           dujour = ifelse(nchar(code)==8, substr(code,7,8),NA))
  #2yr evals seem common
  id.dat$yr <- ifelse(nchar(id.dat$code)==2, id.dat$code, id.dat$yr)
  id.dat$yr4 <- ifelse(as.numeric(id.dat$yr) > 80,
                       paste0("19", id.dat$yr), paste0("20", id.dat$yr))
  #filter non standard naming
  id.dat$loc <- ifelse(id.dat$loc %in% flds$sites, id.dat$loc, NA)
  
  #make dates
  id.dat<- id.dat %>% mutate(date = as.Date(
    paste(yr4, month, day, sep = "/"), format = "%Y/%m/%d"))
  
  return(id.dat)
}









#################
##STORAGE FXNS ##
#################



#write & save data (if no file); LOCALLY
saveData <- function(dat){
  #fileName <- sprintf("here_lies_data.csv")
  # write.table(x = dat, file=file.path(dd, fN),
  #             row.names = F, quote = T, append=T, sep = ",")
  dat <- as.data.frame(dat)
  if(file.exists(file.path(dropdd, dfN))){             # added 'drop' in front of 'dd', added 'd' in front of fN for all instances here
    #append if file exists
    write.table(x = dat, file=file.path(dropdd, dfN),
                row.names = F, col.names = F,
                quote = T, append=T, sep = ",")
    # file.create(file.path(dd, 'igothere.csv'))
  }else{
    #write csv if file doesn't
    write.table(dat, file = file.path(dropdd, dfN),
                row.names = F, col.names = T,
                quote = T, append=F, sep = ",")
  }
}


#using dropbox, individual files for entries
saveData2 <- function(data) {
  data <- data.frame(data, stringsAsFactors = F)
  dropPath <- file.path(dropsc, data$dfN)
  print(paste0("dropPath is:", "(dropsc, data$dFn) which is: ", dropPath))
  tempPath <- file.path(tempdir(),
                        data$dfN)
  print(paste("storing data in this file", dropPath))
  #one entry per file
  write.csv(data, dropPath, row.names = F, quote = TRUE)     # changed from temppath to 'dropsc' folder once running
#  drop_upload(tempPath, path = dropsc, mode = "add")          # REMOVE
}


#################
## SaveLog fxn ##
#################


#save photo uploads w/ photoID naming convention

savePhoto2 <- function(photo, phid){
  
  dropPath <- file.path(paste0(dropfin,"/", phid, ".png"))     # named photo saves to 'staging' folder
  file.copy(from = photo$datapath, to = dropPath)              # copies photo from interface to folder
  print(paste0("FinPhoto is uploaded to ", dropPath))          # Prints path to photo in console (not displayed on UI)
          # drop_upload() used to be here
}










#timestamping
epochTime <- function(){
  as.integer(Sys.time())
}






#####
#RESOURCES
#####
#https://github.com/daattali/shinyforms/blob/master/R/shinyform.R
#https://github.com/JayMox/shinyforms
#Multiple user lock out: https://community.rstudio.com/t/persistent-data-storage-in-apps-with-multiple-users/1308
#reset https://stackoverflow.com/questions/49344468/resetting-fileinput-in-shiny-app
#https://github.com/karthik/rdrop2#accessing-dropbox-on-shiny-and-remote-servers
#https://deanattali.com/blog/shiny-persistent-data-storage/