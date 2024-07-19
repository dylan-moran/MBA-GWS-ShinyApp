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
library(tidyverse)      # data manipulation
library(shinyalert)     # creates popup messages for Shiny
library(shinythemes)
library(lubridate)
library(shinyFiles)
library(googledrive)




# print("Initializing Google Drive Access")
# # drive_about()
# user_email <- "sajorgensen@csumb.edu"   #as.character(input$email)
# print(paste0("user_email = ", user_email))
# drive_auth(email = user_email)
# user <- drive_user()
# print(user)
# 
# 
# # find shared drive
# shared_drives <- shared_drive_find()
# JLab_shared_drive <-
#   shared_drives %>%
#   filter(name == "Jorgensen_lab")
# 
# #print(JLab_shared_drive$name)
# 
# 
# # find shared drive folder
# GWSFinput_folder <- drive_find(pattern='GWS FINput', 
#                                type='folder', 
#                                n_max = 1, 
#                                shared_drive = JLab_shared_drive)
# 
# # list contents of folder
# GWSFinput_folder_contents <- 
#   drive_ls(path = GWSFinput_folder$id)
# 
# GD_Encs <- GWSFinput_folder_contents[GWSFinput_folder_contents$name == "FINal_Encounters",]
# GD_PhotoIDs <- GWSFinput_folder_contents[GWSFinput_folder_contents$name == "FINal_PhotoIDs",]









#shinyFilesExample()
#Directories



Localdir <- "/Users/dylan/Documents/R Studio/Jorgensen Lab/MBA GWS ShinyApp"   # set local directory to my computer
setwd(Localdir)
     #getwd()    # remove comment to check where working directory is set



#Directory Shortcuts within 'MBA GWS ShinyApp' folder

         # paste0(Localdir, "/", DIR__)   # any time you want to set path to old dropbox dirs

DIR_archive <- "FinID_curator/archive"          # unused in any code? see fxns line 197

DIR_Enc_prelim <- "FinID_curator/Encounters_prelim"            # individual encounters CSV save location
                                                               # each encounter gets its own csv. Wipes clean after final data submitted

DIR_Enc_prelim_empty <- "FinID_curator/Encounters_prelim_empty"   # placeholder folder with file if the prelim directory is empty

DIR_staging <- "FinID_curator/STAGE_FINIDS_HERE"    # a sandbox for prepping photos prior to an error check & 
                                                # official submission to database

DIR_FinID_Final <- "FinID_curator/FinIDs_FINAL"     # folder with final fin photos

DIR_Enc_final <- "FinID_curator/Encounters_FINAL"      # reviewed and submitted data gets saved here (inop)

DIR_Enc_Backup <- "FinID_curator/archive/Encounters_BACKUP"   # backup archive of all encounter submissions

DIR_ARCHIVE_LOGOS <- "FinID_curator/archive/Logos"

DIR_ARCHIVE_IMAGES <- "FinID_curator/archive/Images"

log <- "FinID_curator/finID_SurveyLog.csv"








#list of observers available to checkbox
flds <- list(
  observers = c("PK", "SA", "SJ", "JM", "TC", "TW", "EM", "OJ", "DM"),
  users = c("PK", "SA", "SJ", "JM", "TC", "TW", "EM", "OJ", "DM"),
  sites = c("PR", "FAR", "AN", "APT"),
  mandatory = c("user", "sex", "size"),
  new_mand = c("user", "site.phid", "date.phid", "sighting.phid", "time", "sex", "size"),
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






loadData2 <- function(directorypath) {          # output df is called 'EncounterData'  |  dt.filt = NULL, loc.filt = NULL
                                                                  # loc and dt filts are disabled

  dir <- list.files(directorypath)
  #list files 
  files <- as.character(dir)
  #get metadata
  files <- bind_cols(filename = files, parse_phid(files))
  files$path <- paste0(directorypath, "/", files$filename)
  #read in data, all character classes
  EncounterData <- lapply(files$path, read.csv, stringsAsFactors = F,          # changed drop_read_csv to read.csv
                          as.is = T, colClasses = "character")
  EncounterData <- dplyr::bind_rows(EncounterData)
  
  return(EncounterData)
}


check.existing.phids <- function(new.phid){      # checks for duplicate phids in total master backup
  phid.archive <- loadData2(DIR_Enc_Backup)
  phid.prelims <- loadData2(DIR_Enc_prelim) # new with OR statment below
  view(phid.prelims)
  if((new.phid %in% phid.archive$PhotoID) | (new.phid %in% phid.prelims$PhotoID) ){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

check.existing.phids2 <- function(new.phid){      # checks for duplicate phids in total master backup
  phid.archive <- loadData2(DIR_Enc_Backup)
  if(length(list.files(DIR_Enc_Backup))>0){
   if(length(list.files(DIR_Enc_prelim))>0){
    phid.prelims <- loadData2(DIR_Enc_prelim)
    if( (new.phid %in% phid.archive$PhotoID) | (new.phid %in% phid.prelims$PhotoID) ){
      return(TRUE)
    }else{
      return(FALSE)
    }
   }else{
    if((new.phid %in% phid.archive$PhotoID)){
      return(TRUE)
    }else{
      return(FALSE)
    }}
  }else{
    return(FALSE)
  }
  
  
}



  # check_files = list.files(DIR_Enc_prelim)
  # if(length(check_files)>0){
  # 
  # }
  # else{
  # 
  # }


### new loaddata2table fxn ###

loadData2Table <- function(directorypath){
    loadedData<- loadData2(directorypath)
    loadedDT <- data.frame(loadedData)
    return(loadedDT)
}

##############################


loadData3Table <- function(){
  dir <- list.files("FinID_curator/cache")
  #list files 
  files <- as.character(dir)
  #get metadata
  files <- bind_cols(filename = files, parse_phid(files))
  files$path <- paste0("FinID_curator/cache", "/", files$filename)
  #read in data, all character classes
  EncounterData <- lapply(files$path, read.csv, stringsAsFactors = F,          # changed drop_read_csv to read.csv
                          as.is = T, colClasses = "character")
  EncounterData <- dplyr::bind_rows(EncounterData)
  loadedData<- EncounterData
  loadedDT <- data.frame(loadedData)
  return(loadedDT)
}










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
  if(file.exists(file.path(DIR_archive, dfN))){             # added 'drop' in front of 'dd', added 'd' in front of fN for all instances here
    #append if file exists
    write.table(x = dat, file=file.path(DIR_archive, dfN),
                row.names = F, col.names = F,
                quote = T, append=T, sep = ",")
    # file.create(file.path(dd, 'igothere.csv'))
  }else{
    #write csv if file doesn't
    write.table(dat, file = file.path(DIR_archive, dfN),
                row.names = F, col.names = T,
                quote = T, append=F, sep = ",")
  }
}


#using dropbox, individual files for entries
saveData2 <- function(data) {
  data <- data.frame(data, stringsAsFactors = F)
  dropPath <- file.path(DIR_Enc_prelim, data$dfN)
  print(paste0("dropPath is:", "(DIR_Enc_prelim, data$dFn) which is: ", dropPath))
  tempPath <- file.path(tempdir(),
                        data$dfN)
  print(paste("storing data in this file", dropPath))
  #one entry per file
  write.csv(data, dropPath, row.names = F, quote = TRUE)     # Writes to EncPrelim
}


#################
## SaveLog fxn ##
#################


#save photo uploads w/ photoID naming convention

savePhoto2 <- function(photo, phid){
  
  dropPath <- file.path(paste0(DIR_FinID_Final,"/", phid, ".png"))     # named photo saves to 'staging' folder
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