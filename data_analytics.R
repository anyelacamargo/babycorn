library(openxlsx)
library(ggplot2)
library(reshape2)
#library(dplyr)



file_names <- list.files(path = '.', pattern = '.xlsx')
fnames <- read.csv('field_names.csv')

raw_trial_data <- c()
for(fname in file_names){

  sname <- getSheetNames(fname)
  sname <- sname[-grep('details', sname)]
  for(tname in sname){
  
    raw_trial_data <- rbind(raw_trial_data, 
                            cbind(year = strsplit(fname, '\\ ')[[1]][3], 
                            exp = strsplit(tname, '\\ ')[[1]][2], 
                            readWorkbook(fname, tname)))
  
  }
}

raw_trial_data[['Var']] <- as.factor(raw_trial_data[['Var']])
raw_trial_data[['N']] <- as.factor(raw_trial_data[['N']])
raw_trial_data[['P']] <- as.factor(raw_trial_data[['P']])
raw_trial_data[['S']] <- as.factor(raw_trial_data[['S']])


raw_trial_data <- raw_trial_data[, 1:50]
break
v <- c("LFW", "LDW", "LeafN", "LNU","SPAD","LCC","PFW","PDW","PlN","PNU","NDVI")
#m <- raw_trial_data


predictor <- v[1]
i <- grep(predictor, colnames(raw_trial_data))
m <- melt(raw_trial_data, id.vars = names(raw_trial_data)[1:17],
          measure.vars = paste(predictor, 1:3, sep = ''), 
          variable.name = 'sample',
          value.name = predictor)
colnames(m)[ncol(m)] = predictor
for(predictor in v[2:length(v)]){
  print(predictor)
  #i <- grep('sample', colnames(m))
  #if(length(i) != 0){
  #  m <- m[, -i]
  #}
  i <- grep(predictor, colnames(raw_trial_data))
  mm <- melt(raw_trial_data, id.vars = names(raw_trial_data)[1:17],
            measure.vars = paste(predictor, 1:3, sep = ''), 
            variable.name = 'sample',
            value.name = predictor)
  m <- cbind(m, mm[,19])
  colnames(m)[ncol(m)] = predictor
 
  
}


i <- grep('sample', colnames(m))
m$sample <- as.numeric(m$sample)
m[['sample']] <- as.factor(m[['sample']])
m[['Rep']] <- as.factor(m[['Rep']])

#' Output 1
m1 <- subset(m, exp != '3')
#m1 <- mutate(m1, Nv = (((as.numeric(N)-1) * 30) *10)/100)
#m1 <- mutate(m1, Yv = (as.numeric(UPYLD) * 3)/10)
#m1[['Nv']] <- as.factor(m1[['Nv']])



pdf('babycorn_prelplots.pdf')
caption_n <- 'Columns on right, left-to-right, indicate exp, Phosphorus level, 
              Spacing level, Var = Seed variety'
for(tname in names(m1)[8:17]){
  i <- match(tname, fnames$tname)
  p <- ggplot(m1, aes_string(x = 'N', y = tname, color = 'Var', fill = 'Var')) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, fatten = 1,
                 notch = FALSE, color="black") +
    #geom_point(aes_string(x = 'Nv', y = 'Yv')) +
    #geom_smooth() +
    facet_grid(S + P + exp ~ year) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) +
    labs(title = "Babycorn data analytics preliminary results by year", 
         #subtitle = "Carat weight by Price", 
         caption = caption_n) +
    xlab('Nitrogen levels') + ylab(fnames[i, 2]) 
 
  
  print(p)
}


caption_n <- 'Columns on right, left-to-right, indicate exp, Phosphorus level, 
              Spacing level, Var = Seed variety'
for(tname in names(m1)[19:29]){
  i <- match(tname, fnames$tname)
  p <- ggplot(m1, aes_string(x = 'N', y = tname, color = 'Var', fill = 'Var')) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, fatten = 1,
                 notch = FALSE, color="black") +
    #geom_point(aes_string(x = 'Nv', y = 'Yv')) +
    #geom_smooth() +
    facet_grid(S + P + exp ~ sample) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) +
    labs(title = "Babycorn data analytics preliminary results by sample", 
         #subtitle = "Carat weight by Price", 
         caption = caption_n) +
    xlab('Nitrogen levels') + ylab(fnames[i, 2]) 
  
  
  print(p)
}

dev.off()