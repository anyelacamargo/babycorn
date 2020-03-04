library(openxlsx)
library(ggplot2)
library(reshape2)
#library(dplyr)
library(caret)
library(outliers)
library(glmnet)
library(e1071)
library(xgboost)


parm_search_xgboost <- function(dtrain){
  
  
  
  searchGridSubCol <- expand.grid(subsample = c(0.5, 0.75, 1), 
                                  colsample_bytree = c(0.6, 0.8, 1),
                                  lambda = seq(0, 1, by = 0.5),
                                  lambda_bias = seq(0, 1, by = 0.5),
                                  alpha = seq(0, 1, by = 0.5),
                                  eta = seq(0, 1, by = 0.1))
  ntrees <- 100
  
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    lambda <- parameterList[["lambda"]]
    lambda_bias <- parameterList[["lambda_bias"]]
    alpha <- parameterList[["alpha"]]
    eta <- parameterList[["eta"]]
    
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = ntrees, nfold = 5, 
                             showsd = TRUE, 
                             metrics = "rmse", verbose = FALSE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", 'booster' = "gblinear",
                             "max.depth" = 15,                                
                             "subsample" = currentSubsampleRate, 
                             "colsample_bytree" = currentColsampleRate,
                             'lambda' = lambda,
                             'lambda_bias' = lambda_bias,
                             'alpha' = alpha, 'eta' = eta)
    
    xvalidationScores <- data.frame(xgboostModelCV$evaluation_log)
    #print(xvalidationScores)
    #Save rmse of the last iteration
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    
    return(c(rmse, currentSubsampleRate, currentColsampleRate, lambda, 
             lambda_bias, alpha, eta))
    
  })
  
  return(rmseErrorsHyperparameters[,which(rmseErrorsHyperparameters[1,] == 
                                     min(rmseErrorsHyperparameters[1,]))])
  
  
}



run_gboosting <- function(sdata, class_name, descriptor_list){
  
  
  par(mfrow = c(2,2))
  
  for(kw in c('SPAD', 'NDVI', 'LCC')){
    
    i <- grep(kw, descriptor_list)
    subdata <- sdata[, c(descriptor_list[c(1:4, i)], class_name)]
    index <- createDataPartition(subdata[[class_name]], p = 0.7, list = FALSE)
    train_data <- subdata[index, ]
    test_data  <- subdata[-index, ]
    
    dtrain <- xgb.DMatrix(data=as.matrix(train_data[, -match(class_name, 
                                                             colnames(train_data))]),
                          label=train_data$UPYLD, missing=NA)
    
    dtest <- xgb.DMatrix(data=as.matrix(test_data[, -match(class_name, 
                                                           colnames(test_data))]),
                         label=test_data$UPYLD, missing=NA)
    
    param <- list(booster = "gblinear", 
                  objective = "reg:linear", 
                  subsample = 0.7, 
                  max_depth = 2, 
                  colsample_bytree = 0.7, 
                  eval_metric = 'mae', 
                  base_score = 0.012, 
                  min_child_weight = c(1,2),
                  lambda = 1,
                  lambda_bias = 0.5,
                  alpha = 0,
                  eta = 0.1)
    
     xgb_cv <- xgb.cv(data=dtrain,
                      params=param,
                      nrounds=100,
                      prediction=TRUE,
                      maximize=FALSE,
                      #folds=foldsCV,
                      nfold = 5,
                      early_stopping_rounds = 30,
                      print_every_n = 5
     )
     
     # Check best results and get best nrounds
     print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
     nrounds <- xgb_cv$best_iteration
     
     xgb <- xgb.train(params = param
                     , data = dtrain
                     # , watchlist = list(train = dtrain)
                     , nrounds = nrounds
                     , verbose = 1
                     , print_every_n = 5
                     #, feval = amm_mae
    )
    
  
    xgb.pred <- predict(xgb, dtest)
    plot(test_data[[class_name]], xgb.pred , col = "blue", pch=4, main = 'GB')
    res <- caret::postResample(as.numeric(test_data[[class_name]]), xgb.pred)
    print(res)
  }
  
  
}


tune_svm <- function(sdata, f){
  
  tc <- tune.control(cross = 5)
  tuned_par <- tune.svm(f, data = sdata, 
                        gamma = 10^(-5:-1), cost = seq(1,3, by = 0.5),
                        epsilon = seq(0, 0.2, by = 0.01), tunecontrol = tc)
  return(tuned_par)
}



run_SVM <- function(sdata, class_name, descriptor_list){
  
  
  #index <- sample(1:nrow(sdata))
  par(mfrow = c(2,2))
  for(kw in c('SPAD', 'NDVI', 'LCC')){
    
    i <- grep(kw, descriptor_list)
    subdata <- sdata[, c(descriptor_list[c(1:4, i)], class_name)]
    index <- createDataPartition(subdata[[class_name]], p = 0.7, list = FALSE)
    train_data <- subdata[index, ]
    test_data  <- subdata[-index, ]
  
    set.seed(42)
    f <- as.formula(paste(class_name, '~ .'))
 
    o <- tune_svm(train_data, f)
    tuned_par <- list()
    tuned_par$gamma <- o$best.parameters$gamma 
    tuned_par$epsilon <- o$best.parameters$epsilon
    tuned_par$cost <- o$best.parameters$cost
    
    save(o, file = 'o.dat')
    svm_model <- svm(f, train_data, gamma = tuned_par$gamma, 
                     epsilon = tuned_par$epsilon, 
                     cost=tuned_par$cost,
                     type="eps-regression", kernel = 'radial')
    
    w <- t(svm_model$coefs) %*% svm_model$SV
    b <- -svm_model$rho
    # in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
    predictYsvm <- predict(svm_model, test_data[, -match(class_name, colnames(test_data))])
    res <- caret::postResample(as.numeric(test_data[[class_name]]), predictYsvm)
    plot(test_data[[class_name]], predictYsvm, col = "green", pch=4,  main = 
           paste(class_name, ' prediction using ', kw, sep = ''),
         ylab= 'predicted', xlab = 'observed')
    abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3, lwd=1)
    text(10.5, 8, paste('R2 = ', round(res[[2]],2), sep =''))
    
    print(res)
  }
  
}


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


# raw_trial_data[['Var']] <- as.factor(raw_trial_data[['Var']])
# raw_trial_data[['N']] <- as.factor(raw_trial_data[['N']])
# raw_trial_data[['P']] <- as.factor(raw_trial_data[['P']])
# raw_trial_data[['S']] <- as.factor(raw_trial_data[['S']])

raw_trial_data <- raw_trial_data[, 1:50]
descriptor_list <- c("Var", "N", "P", "S",  
                     "SPAD1", "SPAD2", "SPAD3",
                     "NDVI1", "NDVI2", "NDVI3",
                     "LCC1","LCC2","LCC3",
                     "LFW2", "LFW3",
                     "LDW2", "LDW3", 
                     "LeafN2","LeafN3", 
                     "LNU2", "LNU3", 
                     "PFW1", "PDW1", "PlN1",
                     "PNU1", "PFW2", "PDW2", "PlN2", "PNU2", 
                     "PFW3", "PDW3", "PlN3", "PNU3")



class_name <- 'UPYLD'

run_SVM(raw_trial_data, class_name, descriptor_list)

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


#Unpeeled Baby Corn Rs
UBC <- 9000
#The price of N is Rs. per ton.
PoN <- 12400

i <- grep('sample', colnames(m))
m$sample <- as.numeric(m$sample)
m[['sample']] <- as.factor(m[['sample']])
m[['Rep']] <- as.factor(m[['Rep']])

#' Output 1
m1 <- subset(m, exp != '3')
#m1 <- mutate(m1, Nv = (((as.numeric(N)-1) * 30) * PoN)/100)
#m1 <- mutate(m1, Yv = as.numeric(UPYLD) * UBC)
#m1[['Nv']] <- as.factor(m1[['Nv']])

break

pdf('babycorn_prelplots.pdf')
caption_n <- 'Col on right, left-to-right, indicate exp, Phosphorus level, 
              Spacing level, Var = Seed variety'

for(tname in names(m1)[8:17]){
  tiff(paste(tname, '.tiff', sep = ''), res = 100)
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
  dev.off()
}


caption_n <- 'Cols on right, left-to-right, indicate exp, Phosphorus level, 
              Spacing level, Var = Seed variety'
for(tname in names(m1)[19:29]){
  tiff(paste(tname, '.tiff', sep = ''), res = 100)
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
  dev.off()
}

dev.off()
