library(XML)
library(xml2)
library(pracma)
library(kulife)
library(ggplot2)
library(reshape)
library(data.table)
library(dplyr)
library(elliptic)
#library(AquaCropR)
lapply(paste('../aquacropR/R/', list.files('../aquacropr/R', pattern='*.R'), 
             sep=''), function(x) source(x))


#' Lat =  30.9860
#' Lon = 75.7635
#' Elevation 246.71 



plot_scatter <- function(observed_data, predicted_data){
  
  res <- caret::postResample(observed_data, predicted_data)
  print(res)
  plot(observed_data, type = 'points', ylim = c(0,20), cex.axis = 0.8, 
       
       xlab = 'Observations years 1983-2015',
       ylab = 'Grain Yield (t/ha)',
       pch = 17, cex = 2)
  points(predicted_data, col='red', cex.axis = 0.8, 
         xlab = 'Observations years 1983-2015',
         ylab = 'Grain Yield (t/ha)', pch = 19, ylim = c(0,18))
  legend("topleft", c("AquaCrop-OS", "AquaCropR"), col = 1:2, pch = c(17:19),
         y.intersp=1, bty='n', title = paste('R2: ', 
           round(res[[2]],2), 'RMSE: ',  round(res[[1]],2)), cex = 0.8, xjust=0)
  
}


   
get_weather_data <- function(fname){
  
  sdata <- read.csv(fname, header = TRUE)
  month = as.numeric(sapply(1:nrow(sdata), function(x) 
    strsplit(ChangeDateFormat(c('YEAR' = sdata$YEAR[x], 
                                'DOY' = sdata$DOY[x])), '-')[[1]][2]))
  day = as.numeric(sapply(1:nrow(sdata), function(x) 
    strsplit(ChangeDateFormat(c('YEAR' = sdata$YEAR[x], 
                                'DOY' = sdata$DOY[x])), '-')[[1]][3]))
  evp = as.numeric(sapply(1:nrow(sdata), function(x)
      get_Eto(list('Tmax'  = sdata$T2M_MAX[x], 
                'Tmin'  = sdata$T2M_MIN[x], 
                'rs' = sdata$ALLSKY_SFC_SW_DWN[x], 
                'Wind' = sdata$WS2M[x], 
                'Tdew' = sdata$T2MDEW[x], 
                'altitude' = sdata$altitude[x],
                'lat' = sdata[['LAT']][x],
                'hemis' = 'N',
                'DOY' = sdata[['DOY']][x]))))
  
  w <- data.frame(day, month, year = sdata$YEAR, mintp = sdata$T2M_MIN,
                  mxntp = sdata$T2M_MAX, p = sdata$PRECTOT, evp)
 
  
  return(w)
  
    
          
  
}


    #weather_raw <- 'Weather_babycorn_raw.csv'
    #write.csv(get_weather_data(weather_raw), 
     #           'input_babycorn/weather_babycorn.csv', row.names = FALSE)
 
    folder_name <- 'input_babycorn'
    FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml', 
                                           sep=''))
    #break
    InitialiseStruct <- Initialise(FileLocation)
    #break
    Outputs <- PerformSimulation(InitialiseStruct)
    Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
    Outputs <- subset(Outputs, PlantingDate != '0000-01-01')
    Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
    Outputs <- setDT(mutate(Outputs, DOY = convertDOY(Outputs$PlantingDate)))
    Outputs_month <- split(Outputs, by = 'PlantingDate')
    i = lapply(Outputs_month, function(x) x[as.numeric(which(x$Yield == 
                                                               max(x$Yield)))][1])
    u = data.frame(t(data.frame(rbind(sapply(i, function(x) x)))))
    #break
    d <- list()
    d[['RefBio']] <- 'Biomass (g m-2)'
    d[['Yield']] <- 'Grain Yield t/ha'
    d[['CC']] <- 'Canopy cover (%)'
    d[['Infl']] <- 'Infiltration (mm)'
    d[['Irr']] <- 'Irrigation (mm)'
    d[['Et0']] <- 'Et0'
    
   
 
    for(cname in names(d)){
      tiff(paste(FileLocation$Output, 'Figure_', cname, '.tiff', sep=''),  
            width = 800,height = 600, res = 145)
      p <- ggplot(Outputs, aes(x = TotGDD, y = Outputs[[cname]], 
                               col = PlantingDate)) +
        geom_line(aes(linetype=PlantingDate, color=PlantingDate), size = 0.7) + 
        theme_bw() +  labs(y = d[[cname]], x = 'cum Degree Day (cd)') +
        theme(axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 11.5),
              axis.text.y = element_text(size = 11.5),
              legend.text = element_text(size = 12),
              legend.position="bottom")
              #legend.position = "none"))
      print(p)
      dev.off() 
   
    }

    Outputs_month <- split(Outputs, by = 'PlantingDate')
    i = lapply(Outputs_month, function(x) x[as.numeric(which(x$Yield == 
                                                               max(x$Yield)))][1])
    u = data.frame(t(data.frame(rbind(sapply(i, function(x) x)))))
    
    # od <- read.csv('input_maize_usa/Sample_FinalOutput_v2.csv', header = TRUE)
    # 
    #  i <- which(is.na(od$Yield) == TRUE)
    #  
    # tiff('Fig1.tiff', width  = 800, height = 800, res=200)
    # plot_scatter(od$Yield[-i], as.numeric(u$Yield)[c(-i,-34 )])
    # dev.off()
    print(u$Yield)
    