rm(list=ls())
library(data.table)
library(fst)
library(stringr)
library(RSQLite)
library(DBI)
library(dplyr)
```


# Call Reports


dbdir <- "C:/Users/dratnadiwakara2/Documents/OneDrive - Louisiana State University/Raw Data/Call Reports/call_reports.db"
con <- dbConnect(RSQLite::SQLite(), dbdir)


dirs <- list.dirs("C:/Users/dratnadiwakara2/Downloads/call/",recursive = T)
dirs <- dirs[-1]

for(dir in dirs) {
  
  print(dir)
  
  data_period <- substr(dir,75,82)
  print(data_period)
  files <- list.files(path=dir,full.names = F)
  if(substr(files[1],1,24)=="FFIEC CDR Call Bulk POR ") {
    
    call <- read.csv(paste0(dir,"/",files[1]),sep="\t",stringsAsFactors = F)
    call_names <- names(call)
    call_names <- str_replace_all(call_names," ","_")
    call_names <- str_replace_all(call_names,"\\.","_")
    call_names <- str_replace_all(call_names,"/","_")
    names(call) <- call_names
    call <- data.table(call)
    
    meta_data <- data.table(var_desc=NA,var_names=names(call))
    
    files <- files[2:length(files)]
    
    for (f in files) {
      
      if(f=="Readme.txt") next
      
      temp <- read.csv(paste0(dir,"/",f),skip = 1,sep="\t",stringsAsFactors = F)
      temp <- data.table(temp)
      var_desc <- names(temp)
      temp_names <- fread(paste0(dir,"/",f),nrows = 1,header = F)
      var_names <- as.character(temp_names[1,])
      names(temp) <- var_names
      temp <- temp[,!"NA"]
      temp_vars <- data.table(var_desc=var_desc,var_names=var_names)
      temp_vars <- temp_vars[var_names != "NA"]
      
      if(sum(names(temp) %in% names(call))>1) {
        dup_cols <- names(temp)[(names(temp) %in% names(call))]
        dup_cols <- dup_cols[dup_cols != "IDRSSD"]
        temp <- temp[,!dup_cols,with=FALSE] 
      }
      
      temp[,IDRSSD:=as.numeric(IDRSSD)]
      call <- merge(call,temp,by="IDRSSD",all.x=T)
      meta_data <- rbind(meta_data,temp_vars)
    }
    
    meta_data[,data_period:=data_period]
    call[,data_period:=data_period]
    
    conf_count <- data.frame(colSums(call=="CONF"))
    conf_count$variable <- row.names(conf_count)
    row.names(conf_count) <- NULL
    conf_count <- data.table(conf_count)
    names(conf_count) <- c("count","var")
    conf_count <- conf_count[count>nrow(call)/2]
    
    na_count <- data.frame(colSums(is.na(call)))
    na_count$variable <- row.names(na_count)
    row.names(na_count) <- NULL
    na_count <- data.table(na_count)
    names(na_count) <- c("count","var")
    na_count <- na_count[count>(na_count[var=="RCFD2170"]$count+25)]
    # col_names_this <- names(call)[names(call) %in% col_names]
    # call <- call[,col_names_this,with=F]
    cnames <- names(call)
    
    cnames <- cnames[which(! cnames %in% conf_count$var & ! cnames %in% na_count$var)]
    call_1_names <- cnames[which(substr(cnames,1,3)=="RCO" | cnames %in% c("IDRSSD","data_period"))]
    call_2_names <- cnames[which(substr(cnames,1,3)!="RCO" & ! cnames %in% conf_count$var)]
    call_1 <- call[,call_1_names,with=F]
    call_2 <- call[,call_2_names,with=F]
    print(ncol(call))
    dbWriteTable(con,paste0("call_1_",data_period),call_1)
    dbWriteTable(con,paste0("call_2_",data_period),call_2)
    # dbWriteTable(con,paste0("meta_data"),meta_data,append = TRUE)
  }
}

dbDisconnect(con)



# UBPR



load(file="Data Cleaning/UBPR/col_names_not_missing_ubpr.rda")

dbdir <- "C:/Users/dratnadiwakara2/Documents/OneDrive - Louisiana State University/Raw Data/Call Reports/ubpr.db"
con <- dbConnect(RSQLite::SQLite(), dbdir)

dirs <- list.dirs(path = "C:/Users/dratnadiwakara2/Downloads/extract/extract", full.names = TRUE, recursive = F)
dp <- as.Date("2023-12-31")

for(dir in dirs) {
  
  print(dir)
  
  data_period <- substr(dir,52,62)
  files <- list.files(path=dir,full.names = F)
  
  call <- NULL
  meta_data <- NULL
  
  for (f in files) {
    
    if(f=="Readme.txt") {
      next
    }
    
    temp <- read.csv(paste0(dir,"/",f),skip = 1,sep="\t",stringsAsFactors = F)
    temp <- data.table(temp)
    var_desc <- names(temp)
    temp_names <- fread(paste0(dir,"/",f),nrows = 1,header = F)
    var_names <- as.character(temp_names[1,])
    names(temp) <- var_names
    names(temp)[1:2] <- c("data_period","IDRSSD")
    temp[,data_period:=substr(data_period,1,10)]
    temp[,data_period:=as.Date(data_period, format = "%m/%d/%Y")]
    temp <- temp[data_period==dp]
    temp <- temp[,!"NA"]
    temp_vars <- data.table(var_desc=var_desc,var_names=var_names)
    temp_vars <- temp_vars[var_names != "NA"]
    
    if(sum(names(temp) %in% names(call))>1) {
      dup_cols <- names(temp)[(names(temp) %in% names(call))]
      dup_cols <- dup_cols[!dup_cols %in% c("data_period","IDRSSD")]
      temp <- temp[,!dup_cols,with=FALSE] 
    }
    
    temp[,IDRSSD:=as.numeric(IDRSSD)]
    if(is.null(call))  {
      call <- temp
    } else {
      call <- merge(call,temp,by=c("data_period","IDRSSD"),all.x=T)
    } 
    if(is.null(meta_data))  {
      meta_data <- temp_vars
    } else {
      meta_data <- rbind(meta_data,temp_vars)
    } 
  }
  
  meta_data[,data_period:=data_period]
  col_names_this <- names(call)[names(call) %in% col_names]
  call <- call[,col_names_this,with=F]
  print(ncol(call))
  dbWriteTable(con,paste0("ubpr_",data_period),call,append=TRUE)
  # dbWriteTable(con,paste0("ubpr_meta_data"),meta_data,append = TRUE)
  
  
}

dbDisconnect(con)


