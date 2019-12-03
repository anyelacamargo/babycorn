library(openxlsx)



file_names <- list.files(path = '.', pattern = '.xlsx')


raw_trial_data <- c()
for(fname in file_names){

  sname <- getSheetNames(fname)
  sname <- sname[-grep('details', sname)]
  for(tname in sname){
  
    raw_trial_data <- rbind(raw_trial_data, cbind(strsplit(fname, '\\ ')[[1]][3], 
                                                  strsplit(tname)[[1]][3], 
                                                  readWorkbook(fname, tname)))
  
  }
}

head(raw_trial_data)

#r <- gsub('Data March-June 2018 (Exp 1-5).xlsx', 
                             #'2018', raw_trial_data$fname)
