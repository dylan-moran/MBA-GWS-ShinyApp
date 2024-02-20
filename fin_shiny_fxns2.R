#clean sheet to help functional development
require(shiny)
require(rsconnect)
require(DT)
require(shinyTime)
require(shinyjs)
require(rdrop2)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(shinyalert)


#dirs
#dropbox spot
dropdd <- "FinID_curator/archive"
dropsc <- "FinID_curator/scratch"
dropfin <- "FinID_curator/FinIDs_staging"
droppar <- "FinID_curator/FinIDs_parent"
log <- "FinID_curator/finID_SurveyLog.csv"
token <- readRDS("droptoken.rds") 

#list of observers available to checkbox
flds <- list(
  observers = c("PK", "SA", "SJ", "JM", "TC", "TW", "EM", "OJ"),
  sites = c("PR", "FAR", "AN", "APT"),
  mandatory = c("user", "sex", "size"),
  #exists = c(tools::file_path_sans_ext(drop_dir(droppar)$name),
  coords = list(matrix(c(-123.00, 38.24), nrow=1), #Tomales station
                 matrix(c(-123.00, 37.69), nrow=1), #MiroungaBay station
                 matrix(c(-122.34, 37.11), nrow=1), #Ano tation
                 matrix(c(-121.93, 36.97), nrow=1))
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


#get data
loadData <- function(dir) {
  #add pattern for unique fN for survey
  files <- list.files(file.path(dir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- dplyr::bind_rows(data)
  data
}
#dropbox load
loadData2 <- function(dt.filt = NULL, loc.filt = NULL) {
  #survey.only takes a phid & subsets files to that survey
  #get list of files
  #list the directory
  print("ooo hellooo")
  dir <- drop_dir(dropsc)
  #list files 
  files <- as.character(dir$path_display)[
    grepl(pattern="^(CCA_GWS_PHID)", dir$name)]
  #get metadata
  files <- bind_cols(path = files, parse_phid(files))
  print(dim(files))
  print("yessara")
  if(!is.null(loc.filt)){
    #extract only files associated w/ selected survey
    print("aiii not here")
    print(loc.filt)
    print(dt.filt)
    files <- files %>% filter(loc == loc.filt)
    print(dim(files))
  }
  if(!is.null(dt.filt)){
    print('not here niehter')
    print(dim(files))
    files <- files %>% filter(date == dt.filt)
    print(dim(files))
  }
  ##hard-coded for IT demo
  #data <- read.csv("/Users/jhmoxley/Dropbox (MBA)/FinID_curator/scratch/betaMaster_df.csv")
  #read in data, all character classes
  print("ohhh boy")
  data <- lapply(files$path, drop_read_csv, stringsAsFactors = F,
                  as.is = T, colClasses = "character")
  print('shit')
  data <- dplyr::bind_rows(data)
  print("oooo even worse")
  
  print("oooo wow all the way here")
  #delete column for radio button appdroach??
  # if(delete == T){
  #   sc <- data.frame(delete = as.character(1), data)
  # }
  
  print(class(data))
  print(nrow(data))
  print(ncol(data))
  return(data)
}

#parsing data from phids
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

#ban phids returns a vector of phids existing w/in the folders that cannot be duplicated
getExistingPhids <- function(all=F, paths=F, data.only = F){
  #all=T if want from both scratch & catalog
  #names = F if want the directory contets
  if(all == T){
    #get them from scratch & catalog bins
    if(paths == T){
      files <- c(drop_dir(dropsc)$path_display[
        #only get properly named
        grepl(pattern="^(CCA_GWS_PHID)", drop_dir(dropsc)$name)],
        drop_dir(dropfin)$path_display, 
        drop_dir(droppar)$path_display)
    }else{
      files <- c(drop_dir(dropsc)$name[
        #only get properly named
        grepl(pattern="^(CCA_GWS_PHID)", drop_dir(dropsc)$name)],
        drop_dir(dropfin)$name, 
        drop_dir(droppar)$name)
    }
  }else{
    if(paths == T){
      if(data.only == T){
        files <- drop_dir(dropsc)$path_display[
          #only get properly named
          grepl(pattern="^(CCA_GWS_PHID)", drop_dir(dropsc)$name)]
      }else{
        files <- c(drop_dir(dropsc)$path_display[
          #only get properly named
          grepl(pattern="^(CCA_GWS_PHID)", drop_dir(dropsc)$name)],
          drop_dir(dropfin)$path_display)
      }
    }else{
      if(data.only == T){
        files <- drop_dir(dropsc)$name[
          #only get properly named
          grepl(pattern="^(CCA_GWS_PHID)", drop_dir(dropsc)$name)]
      }else{
        files <- c(drop_dir(dropsc)$name[
          #only get properly named
          grepl(pattern="^(CCA_GWS_PHID)", drop_dir(dropsc)$name)],
          drop_dir(dropfin)$name)
      }
    }
  }
   
  return(files) 
}

############
##STORAGE FXNS
############
#write & save data (if no file); LOCALLY
saveData <- function(dat){
  #fileName <- sprintf("here_lies_data.csv")
  # write.table(x = dat, file=file.path(dd, fN),
  #             row.names = F, quote = T, append=T, sep = ",")
  dat <- as.data.frame(dat)
  if(file.exists(file.path(dd, fN))){
    #append if file exists
    write.table(x = dat, file=file.path(dd, fN),
                row.names = F, col.names = F,
                quote = T, append=T, sep = ",")
    # file.create(file.path(dd, 'igothere.csv'))
  }else{
    #write csv if file doesn't
    write.table(dat, file = file.path(dd, fN),
                row.names = F, col.names = T,
                quote = T, append=F, sep = ",")
  }
}
#using dropbox, individual files for entries
saveData2 <- function(data) {
  # get positioned
  data <- data.frame(data, stringsAsFactors = F)
  dropPath <- file.path(dropsc, data$dfN)
  tempPath <- file.path(tempdir(),
                        data$dfN)
  print(paste("storing data in this file", dropPath))
  #one entry per file
  write.csv(data, tempPath, row.names = F, quote = TRUE)
  drop_upload(tempPath, path = dropsc, mode = "add")
}

#siphon off survey data into a log of all surveys completed
saveLog <- function(data, site, date){
  #data <- loadData2(phid.only = phid$val)
  #hard code due to rdrop2 erros
  files <- list.files("/Users/jmoxley/Dropbox (MBA)/FinID_curator/scratch", 
                      full.names = T)[
    grepl(paste0(site, format(date, "%y%m%d")), #find files w/ same date/site
          list.files("/Users/jmoxley/Dropbox (MBA)/FinID_curator/scratch"))]
  
  #do i need some qc chk that data matches? 
  dat <- read.csv(files[sample(1:length(files), 1)])
  logdat <- read.csv(file = log)
  write.table(dat, file = file.path())
}

#save photo uploads w/ photoID
#dropbox style
savePhoto2 <- function(photo, phid){
  
  #set pathways
  dropPath <- file.path(dropfin)
  cat("drop here: ", dropPath, "\n")
  tempPath <- file.path(tempdir(), paste0(phid,
                        ".", tools::file_ext(photo$datapath)))
 
  #write data & upload
  cat("Copying file to:", tempPath ,"\n")
  file.copy(from = photo$datapath, to = tempPath)
  print(paste0("the local instance existence is ", file.exists(tempPath)))
  drop_upload(tempPath, dropPath, mode = "add", autorename=T)
}

#timestamping
epochTime <- function(){
  as.integer(Sys.time())
}
  
humanTime <- function(){
  as.Date(as.character(Sys.time()))
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