#!/usr/bin/env Rscript
# Lupu-Warner-Why-Are.R
# Conducts all statistical analysis and creates all figures and tables
# Zach Warner
# 2 January 2021

##### SET-UP #####

### NB: Please ensure that the /results/variable-importances/ folder is empty
### before attempting a replication run, otherwise the script will produce
### errors. 

### start the sink
sink("/logs/Lupu-Warner-Why-Are-log.txt")

### load packages
library(ggplot2); library(reshape); library(mice); library(randomForest)
library(boot); library(MASS); library(scoring); library(RSNNS)
library(caTools); library(caret); library(edarf)

### set the random seed
set.seed(2365256)

# get a function for rescaling which will be useful in plotting
rescalr <- function(x, theoryMin, theoryMax, newMin, newMax){
  (x - theoryMin)/(theoryMax - theoryMin)*(newMax - newMin) + newMin
}

##### DATA PREPARATION #####

### read in data
df <- read.csv("/data/Lupu-Warner-Why-Are.csv", stringsAsFactors = F)

### fix a few variables for ease of interpretation
# flip clientelism
df$vdem_clientelism <- (df$vdem_clientelism*-1) + 
  (max(df$vdem_clientelism - abs(min(df$vdem_clientelism, na.rm = T)), 
       na.rm = T))
# log net FDI
df$fdi_net_log <- log(df$fdi_net)
# fix NaNs that are created from logging
df[is.na(df)] <- NA

##### DATA DESCRIPTION (IN TEXT) #####

# See Lupu and Warner (forthcoming) for 92,000 legislator-years and 3.2m 
# citizen-years

# number of country-years
nrow(df)
# number of countries
length(unique(df$country))
# number of years
length(unique(df$year))
# unique countries (footnote 6)
sort(unique(df$country))
# unique yars (footnote 6)
sort(unique(df$year))
# observations with at least 30 citizens and 30 legisaltors
nrow(df[which(df$nobs_elite >= 30 & df$nobs_mass_all >= 30),])

##### FIGURE 1 ######

### get data together for plotting
dd <- df[,c("country", "nobs_elite", "emd_lessaffluent_full", 
            "emd_moreaffluent_full")]
dd$emd_diff <- dd$emd_lessaffluent - dd$emd_moreaffluent
dd$n30 <- ifelse(dd$nobs_elite < 30 | is.na(dd$nobs_elite), 0, 1)
dd$emd_lessaffluent <- dd$emd_moreaffluent <- dd$nobs_elite <- NULL

### make a vertical location variable
dd$loc <- rep(NA, nrow(dd))
cn <- sort(unique(dd$country))
for(i in 1:length(cn)){
  dd$loc[which(dd$country == cn[i])] <- i
}
# flip it so it's alphabetical
dd$loc <- (dd$loc - max(dd$loc))*-1

### plot
fig <- ggplot(dd, aes(x = emd_diff, y = loc)) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = NA, size = 1, linetype = "solid"),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color = "black")) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "darkgray") +
  geom_point(aes(shape = factor(n30)), alpha = .5, show.legend = F) +
  scale_shape_manual(values = c(4, 19)) +
  scale_y_continuous(name = NULL, breaks = unique(dd$loc), 
                     labels = factor(unique(dd$country)),
                     expand = c(.02,.02)) +
  scale_x_continuous(limits = c(-.4, .4)) +
  labs(x = "Difference in EMD, least and most affluent", title = NULL)
ggsave("/results/figure_1.pdf", fig, device = "pdf", width = 5.5, 
       height = 7.5, units = "in")

### clean up
rm(cn, dd, fig, i)

##### FOOTNOTE 8: BREAKDOWN OF HOW WE MEASURE CLASS #####

### number of observations by source of affluence data
summary(as.factor(df$mclass_var))

##### TABLE 1 #####
df_nobs <- df[which(df$nobs_elite >= 30),]
dd <- data.frame(matrix(nrow = 19, ncol = 5))
names(dd) <- c("variable", "min", "mean", "max", "missing")
# substitute variable names for what we call them in the paper
vars <- c("emd_diff", "vdem_corr_index", "fdi_net_log", "gdp_log", "undp_hdi",
          "gini", "trade_pc_gdp", "trade_union_density", "democracy_duration", 
          "vdem_compulsory",  "disproportionality", "mean_party_age", 
          "vdem_donate", "vdem_cs_partic", "vdem_clientelism", 
          "xcut_race_income", "left_gov","pc_women_elite_real", 
          "vdem_turnout_vap")
nms <- c("Change in EMD", "Corruption", "Foreign cap. depend.", "GDP (logged)",
         "HDI", "Income inequality", "Trade openness", "Trade union density",
         "Age of democracy", "Compulsory voting", "Disproportionality", 
         "Party institutionalization", "Pol. donation restrictions", 
         "Civil society", "Clientelism", "Cross-cuttingness", 
         "Government ideology", "% female legislators", "Turnout")
# organize summary statistics
emd_diff <- df_nobs$emd_diff <- 
  df_nobs$emd_lessaffluent - df_nobs$emd_moreaffluent
for(i in 1:nrow(dd)){
  dd$variable[i] <- nms[i]
  if(i == 1){
    dd[i, c(2:4)] <- unname(summary(emd_diff)[c(1, 4, 6)])
    dd$missing[i] <- length(emd_diff[which(is.na(emd_diff))])/length(emd_diff)
  } else {
    dd[i, c(2:4)] <- eval(parse(text = paste("unname(summary(df_nobs$", vars[i], 
                                             ")[c(1, 4, 6)])", sep = "")))
    dd$missing[i] <- eval(parse(text = paste("nrow(df[which(is.na(df_nobs$", 
                                             vars[i], ")),])/nrow(df)", 
                                             sep = "")))
  }
}
# clean up the digits
dd[,c(2:4)] <- round(dd[,c(2:4)], 2)
dd$missing <- round(dd$missing*100, 0)
# reorder so it follows the order in the paper
dd$cat <- rep(1, nrow(dd))
dd$cat[which(dd$variable %in% c("Foreign cap. depend.", "GDP (logged)", "HDI", 
                                "Income inequality", "Trade openness"))] <- 2
dd$cat[which(dd$variable %in% c("Age of democracy", "Disproportionality", 
                                "Party institutionalization"))] <- 3
dd$cat[which(dd$variable %in% c("Clientelism", "Corruption", 
                                "Government ideology", 
                                "% female legislators"))] <- 4
dd$cat[which(dd$variable %in% c("Civil society", "Pol. donation restrictions", 
                                "Trade union density"))] <- 5
dd$cat[which(dd$variable %in% c("Compulsory voting", "Cross-cuttingness", 
                                "Turnout"))] <- 6
# final cleaning
dd$variable <- gsub("%", "Percent", dd$variable)
dd <- dd[order(dd$cat, dd$variable),]
dd$cat <- NULL
names(dd) <- c("Variable", "Min.", "Mean", "Max.", "% Miss.")
# print
dd
# save
write.csv(dd, "./results/table_1.csv", row.names = F)
rm(emd_diff, dd, df_nobs, i, vars, nms)

##### IMPUTE DATA #####

### subset to cases with more than 30 elite obs, subset to imputation variables
df <- df[which(df$nobs_elite >= 30),
         c("country", "region", "year","escale","mscale", "gdp", "gdp_log",
           "gini", "trade_pc_gdp", "fdi_net_log",
           "ext_debt_interest", "remit_pc_gdp", "pov_pc_pop",
           "pov_rate", "vdem_donate", "vdem_pubfin", "vdem_polyarchy",
           "vdem_libdem", "vdem_delibdem", "vdem_egaldem", "vdem_partipdem",
           "vdem_compulsory", "vdem_turnout_vap", "vdem_turnout_vap_leg",
           "vdem_turnout_reg", "vdem_turnout_reg_leg", "vdem_left_gov",
           "vdem_party_inst", "vdem_cs_partic", "vdem_ccso_index",
           "vdem_corr_index", "vdem_corr_leg", "vdem_corr_ti_cpi",
           "vdem_clientelism", "vdem_gdp_maddison", "vdem_democracy_factored",
           "trade_globalization", "financial_globalization",
           "personal_globalization", "information_globalization",
           "cultural_globalization", "political_globalization",
           "xcut_race_religion", "xcut_race_geog", "xcut_race_income",
           "xcut_lang_religion", "xcut_lang_geog",
           "xcut_lang_income", "xcut_religion_geog", "xcut_religion_income",
           "xcut_income_geog", "coll_bargaining_pc", "trade_union_density",
           "dreher_corruption", "democracy_duration", "dalp_clientelism",
           "dalp_clientelism_econ", "gini_disp", "undp_hdi", "vi_wcoord",
           "ictwss_cent", "ictwss_conc", "ban_corp_don_parties",
           "contrib_limit_parties", "limit_party_spending", "camp_fin",
           "camp_fin_add", "ts_ban_corp_don_parties",
           "ts_contrib_limit_parties", "ts_limit_party_spending", "compulsory",
           "disproportionality", "pr", "presidential", "fptp", "dist_mag",
           "mean_party_age", "pc_women_elite", "pc_women_elite_real",
           "clea_turnout", "left_gov", "emd_lessaffluent", "emd_midloaffluent",
           "emd_midaffluent", "emd_midhiaffluent", "emd_moreaffluent",
           "emd_lessaffluent_loinfo", "emd_midloaffluent_loinfo",
           "emd_midaffluent_loinfo", "emd_midhiaffluent_loinfo",
           "emd_moreaffluent_loinfo","emd_lessaffluent_hiinfo",
           "emd_midloaffluent_hiinfo" ,"emd_midaffluent_hiinfo",
           "emd_midhiaffluent_hiinfo", "emd_moreaffluent_hiinfo")]

### impute data
n_imp <- ceiling(mean(is.na(df))*100)
max_it <- 10
imp <- mice(df, m = n_imp, maxit = max_it, seed = 8245713)

### create samples and resampling indices for each imputation
testXs <- testYs <- trainXs <- trainYs <- indexPreds <- vector("list", n_imp)
for(i in 1:n_imp){
  
  ### get the imputed data
  tmp <- complete(imp, i)
  
  # create the DV - do this here so the imputation doesn't fail
  tmp$emd_diff <- tmp$emd_lessaffluent - tmp$emd_moreaffluent
  
  ### subset to the variables we need
  tmp <- tmp[,c("emd_diff", "vdem_turnout_vap", "vdem_compulsory", "left_gov",
                "trade_pc_gdp", "fdi_net_log", "xcut_race_income", 
                "disproportionality", "democracy_duration", "mean_party_age",
                "vdem_cs_partic", "vdem_donate", "vdem_corr_index", 
                "vdem_clientelism", "gini", "gdp_log", "undp_hdi",
                "trade_union_density", "pc_women_elite_real")]
  
  ### make sure classes are all numeric
  for(j in 1:ncol(tmp)){
    tmp[,j] <- as.numeric(as.character(tmp[,j]))
  }
  
  ### partition data into train and test sets
  index <- createDataPartition(tmp$emd_diff, p = 0.75, list = FALSE)
  trainDF <- tmp[index,]
  testDF <- tmp[-index,]
  
  ### get resampling profiles
  indexes <- createMultiFolds(trainDF$emd_diff, 5, 5)
  
  ### split the covariates from the response
  trainX <- trainDF[,-which(names(trainDF) == "emd_diff")]
  trainY <- trainDF$emd_diff
  testX <- testDF[,-which(names(testDF) == "emd_diff")]
  testY <- testDF$emd_diff
  
  ### store everything for looping
  trainXs[[i]] <- trainX
  trainYs[[i]] <- trainY
  testXs[[i]] <- testX
  testYs[[i]] <- testY
  indexPreds[[i]] <- indexes
  
  ### delete so we don't accidentally duplicate stuff (unnecessary but safer)
  rm(tmp, index, indexes, trainDF, testDF, trainX, trainY, testX, testY)
}

### get the models we're going to use
models <- sort(c("avNNet", "cforest", "dnn", "glm", "glmboost",
                 "glmnet", "knn", "mlp", "nnet", "pcaNNet", "ppr", "rf", 
                 "treebag"))

### set aside the storage for all the model fits, metrics, and predictions
for(i in 1:length(models)){
  eval(parse(text=paste(models[i],'Mods <- vector("list",n_imp)',sep="")))
  eval(parse(text=paste(models[i],'Imps <- vector("list",n_imp)',sep="")))
  eval(parse(text=paste(models[i],'Preds <- vector("list",n_imp)',sep="")))
}

##### ESTIMATE THE MODELS #####
for(j in 1:length(models)){
  
  ### grab the model
  meth <- models[j]
  
  ### loop over the imputations
  for(i in 1:n_imp){
    
    ### force random forest to get the importance metric
    if(meth %in% c("rf")){
      # set the control object
      objControl <- trainControl(method = 'repeatedcv', number = 5, 
                                 repeats = 5, index = indexPreds[[i]], 
                                 returnResamp='final', search = "random", 
                                 verboseIter = F, savePredictions="final", 
                                 allowParallel = F)
      # print a message updating progress
      message(paste("ESTIMATING MODEL: ", meth, " (", j, " of ", length(models),
                    "), replicate ", i, " of ", n_imp, " at ", Sys.time(), 
                    sep=""))
      # estimate the model
      tmp <- train(x = trainXs[[i]], y = trainYs[[i]], metric = "RMSE",
                   trControl = objControl, tuneLength=10, method = meth,
                   importance = TRUE)
      # store the results
      eval(parse(text=paste(models[j],'Mods[[',i,']] <- tmp',sep="")))
      eval(parse(text=paste(models[j],'Imps[[',i,
                            ']] <- varImp(tmp)$importance[1]',sep="")))
      # advance the loop
      next
    }
    
    ### run the rest of the models
    # set the control object
    objControl <- trainControl(method = 'repeatedcv', number = 5, 
                               repeats = 5, index = indexPreds[[i]], 
                               returnResamp='final', search = "random", 
                               verboseIter = F, savePredictions="final", 
                               allowParallel = F)
    # print a message updating progress
    message(paste("ESTIMATING MODEL: ", meth, " (", j, " of ", length(models),
                  "), replicate ", i, " of ", n_imp, " at ", Sys.time(), 
                  sep=""))
    # estimate the model
    tmp <- train(x = trainXs[[i]], y = trainYs[[i]], metric = "RMSE",
                 trControl = objControl, tuneLength=10, method = meth)
    # store the results
    eval(parse(text=paste(models[j],'Mods[[',i,']] <- tmp',sep="")))
    eval(parse(text=paste(models[j],'Imps[[',i,
                          ']] <- varImp(tmp)$importance[1]',sep="")))
  }
}

### variable importance -- reshape then spit out csvs for temporary storage
outs <- ls(pattern = "Imps")
for(i in 1:length(outs)){
  eval(parse(text=paste("tmp <- ", outs[i], sep="")))
  tmp <- do.call(cbind, tmp)
  names(tmp) <- paste0("imp", c(1:n_imp), sep="")
  tmp$av <- apply(tmp, 1, mean)
  tmp$var <- rownames(tmp); row.names(tmp) <- NULL
  for(j in 1:n_imp){
    eval(parse(text=paste("tmp$rank",j,
                          "<- rank(-tmp$imp",
                          j,", ties.method = 'average')", # -1 so they descend
                          sep=""))) 
  }
  tmp$rav <- apply(tmp[, which(grepl("rank", names(tmp)) == T)], 1, mean)
  tmp <- tmp[order(tmp$rav, tmp$var),
             c((n_imp + 2), ncol(tmp), (n_imp + 1),
               1:n_imp, (n_imp+3):(ncol(tmp)-1))]
  nout <- paste("/results/variable-importances/", models[i], ".csv", sep="")
  write.csv(tmp, nout, row.names = F)
  eval(parse(text=paste(outs[i],"<- tmp",sep="")))
}

### how do the learners compare predictively?
preds <- ls(pattern = "Preds"); preds <- preds[-which(preds == "indexPreds")]
mods <- ls(pattern = "Mods")
for(j in 1:length(preds)){
  message(paste("PREDICTING OUT OF: ", models[j], " (", j, " of ", 
                length(models), ")",sep=""))
  for(i in 1:n_imp){
    eval(parse(text=paste(preds[j], '[[', i, ']] <- predict(',
                          mods[j], '[[', i, ']], newdata=testXs[[', i,
                          ']])', sep="")))
  }
}
for(i in 1:length(preds)){
  eval(parse(text=paste("tmp <- ", preds[i], sep="")))
  tmp <- lapply(tmp, function(x) as.data.frame(x))
  tmp <- mapply(cbind, tmp, "n_imp"=c(1:n_imp), SIMPLIFY=F)
  tmp <- data.frame(do.call(rbind, tmp))
  names(tmp)[1] <- "yhat"
  eval(parse(text=paste(preds[i],"<- tmp",sep="")))
}
# store the Y
Y <- testYs
Y <- mapply(cbind, Y, "n_imp"=c(1:n_imp), SIMPLIFY=F)
Y <- data.frame(do.call(rbind,Y))
names(Y)[1] <- "y"

### AUCs
rmse <- data.frame(matrix(nrow=n_imp, ncol=length(models)))
names(rmse) <- models
for(j in 1:length(preds)){
  eval(parse(text=paste("tmp <- ", preds[j], sep="")))
  for(i in 1:nrow(rmse)){
    rmse[i,j] <- sqrt(mean((tmp$yhat[which(tmp$n_imp == i)] - 
                         Y$y[which(Y$n_imp == i)])^2))
  }
}
# clean up and reshape for output
tab <- data.frame(model = names(rmse),
                  rmse = unname(apply(rmse, 2, mean)),
                  sd = unname((apply(rmse, 2, sd))))
tab <- rbind(tab, tab)
tab <- tab[order(tab$model),]
for(i in 1:nrow(tab)){
  if(i%%2 == 0){
    tab$model[i] <- ""
    tab$rmse[i] <- tab$sd[i]
  }
}
tab$sd <- NULL
tab$rmse <- round(tab$rmse, 3)
tab$rmse <- as.character(tab$rmse)
tab$rmse <- ifelse(tab$model == "", paste("(", tab$rmse, ")", sep = ""),
                   tab$rmse)
# save it
names(tab) <- c("Model", "RMSE")
write.csv(tab, "/results/table_A2.csv", row.names = F)
# clean up
rm(tab, i)

# which learners produce the best AUC? which are the most stable?
sort(apply(rmse, 2, mean))
sort(apply(rmse, 2, sd))

### all basically the same predictively, so we are going to study rf
bestmods <- "rf"

##### FIGURE 2 #####

### get variable importance
imps <- list.files(path = "/results/variable-importances/")
imps <- lapply(imps, function(x){
  x <- read.csv(paste("/results/variable-importances/", x, sep = ""))
})
imps <- imps[which(models %in% bestmods)]
# add a column for which model it is
imps <- mapply(cbind, imps, "model"=models[which(models %in% bestmods)], 
               SIMPLIFY=F)
# merge
imps <- do.call(rbind, imps)

### compute global means and interquartile ranges
vari <- sort(unique(as.character(imps$var)))
plt <- data.frame(matrix(nrow = length(vari), ncol = 5))
names(plt) <- c("lo", "mean", "hi", "var", "loc")
for(i in 1:length(vari)){
  x <- as.matrix(imps[which(imps$var == vari[i]), 
                      which(grepl("imp", names(imps)))])
  plt$var[i] <- vari[i]
  plt$mean[i] <- median(x)
  plt$lo[i] <- unname(quantile(x, .25))
  plt$hi[i] <- unname(quantile(x, .75))
}
plt <- plt[order(plt$mean, plt$var),]
plt$loc <- 1:nrow(plt)
# fix names for plotting
plt$nm <- rep(NA, nrow(plt))
plt$nm[which(plt$var == "disproportionality")] <- "Disproportionality"
plt$nm[which(plt$var == "vdem_compulsory")] <- "Compulsory voting"
plt$nm[which(plt$var == "fdi_net_log")] <- "Foreign cap. depend."
plt$nm[which(plt$var == "xcut_race_income")] <- "Cross-cuttingness"
plt$nm[which(plt$var == "vdem_turnout_vap")] <- "Turnout"
plt$nm[which(plt$var == "trade_pc_gdp")] <- "Trade openness"
plt$nm[which(plt$var == "vdem_cs_partic")] <- "Civil society"
plt$nm[which(plt$var == "trade_union_density")] <- "Trade union density"
plt$nm[which(plt$var == "undp_hdi")] <- "HDI"
plt$nm[which(plt$var == "democracy_duration")] <- "Age of democracy"
plt$nm[which(plt$var == "vdem_corr_index")] <- "Corruption"
plt$nm[which(plt$var == "mean_party_age")] <- "Party institutionalization"
plt$nm[which(plt$var == "gini")] <- "Income inequality"
plt$nm[which(plt$var == "vdem_donate")] <- "Pol. donation restrictions"
plt$nm[which(plt$var == "left_gov")] <- "Government ideology"
plt$nm[which(plt$var == "pc_women_elite_real")] <- "% female legislators"
plt$nm[which(plt$var == "gdp_log")] <- "GDP (logged)"
plt$nm[which(plt$var == "vdem_clientelism")] <- "Clientelism"

### plot
fig2 <- ggplot(plt, aes(x = mean,y = loc)) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color = "black")) +
  geom_vline(xintercept = mean(plt$mean), 
             linetype = "longdash", color = "darkgray") +
  geom_point(color = "black") +
  geom_segment(aes(x = lo, xend = hi, y = loc, yend = loc), color = "black") +
  scale_y_continuous(name = "Covariate", breaks = plt$loc, 
                     labels = factor(plt$nm)) +
  labs(x = "Variable importance", title = NULL)
ggsave("/results/figure_2.pdf", fig2, device = "pdf", width = 7.5, height = 5.5, 
       units = "in")

##### FIGURE 3 #####

### compute partial dependence from rf
pdlist <- vector("list", n_imp)
# keep track of names and, for EJPR, plotting order
vars <- c("disproportionality", "vdem_compulsory", "mean_party_age", 
          "vdem_donate", "xcut_race_income", "trade_pc_gdp", 
          "trade_union_density", "vdem_turnout_vap", "vdem_cs_partic", 
          "fdi_net_log", "pc_women_elite_real", "democracy_duration", 
          "vdem_corr_index", "gdp_log", "undp_hdi", "left_gov", "gini", 
          "vdem_clientelism")
nms <- c("Disproportionality", "Compulsory voting", 
         "Party institutionalization", "Pol. donation restrictions", 
         "Cross-cuttingness", "Trade openness", "Trade union density", 
         "Turnout", "Civil society", "Foreign cap. depend.", 
         "% female legislators", "Age of democracy", "Corruption", 
         "GDP (logged)", "HDI", "Government ideology", "Income inequality", 
         "Clientelism")
ords <- c("11", "9", "14", "15", "10", "16", "17", "18", "8", "12", "4", "7", 
          "5", "2", "3", "13", "6", "1")
pb <- txtProgressBar(min = 1, max = length(rfMods), style = 3)
for(m in 1:n_imp){
  mod <- rfMods[[m]]$finalModel
  dat <- rfMods[[m]]$trainingData
  pd <- partial_dependence(mod, vars, n = c(nrow(dat), nrow(dat)), data = dat)
  pd$m <- rep(m, nrow(pd))
  pdlist[[m]] <- pd
  setTxtProgressBar(pb, m)
}
pd_all <- do.call(rbind, pdlist)

### create a table to store effects of moving across interquartile ranges
tab <- data.frame(var = vars, name = nms, pred_lo = NA, pred_hi = NA, 
                  quant_lo = NA, quant_hi = NA, subst_effect = NA)

### create the sub-plots
for(i in 1:length(vars)){
  eval(parse(text=paste("tmp <- pd_all[,c('",vars[i],"','prediction')]",
                        sep="")))
  names(tmp) <- c("x","y")
  # group by iteration
  tmp <- tmp[which(!is.na(tmp$x)),] # should be nrow(dat)*n_imp
  tmp$iter <- rep(c(1:nrow(dat)),n_imp)
  # different plotting approach for the only binary variable
  if(vars[i] == "vdem_compulsory"){
    tmplot <- ggplot(tmp) +
      geom_smooth(aes(x = x, y = y), method = "loess", color = "black") 
    tmp <- as.data.frame(ggplot_build(tmplot)$data)
    # find nearest values
    lo <- min(dat$vdem_compulsory)
    hi <- max(dat$vdem_compulsory)
    lo <- which.min(abs(lo - tmp$x))
    hi <- which.min(abs(hi - tmp$x))
    # grab the predictions closest
    tmp <- tmp[c(lo, hi),]
    tiks <- tmp$x
    # plot
    tmplot <- ggplot(tmp) +
      geom_point(aes(x = x, y = y)) +
      geom_segment(aes(x = x, xend = x, y = ymin, yend = ymax)) +
      scale_x_continuous(breaks = tiks, labels = c("No", "Yes"), 
                         limits = c(-1.2339901, 1.8710608))
  } else {
    # recover x axis
    eval(parse(text=paste("z1 <- dat$", vars[i], sep = "")))
    eval(parse(text=paste("z2 <- df$", vars[i], sep = "")))
    tmp$x <- rescalr(tmp$x, min(z1, na.rm = T), max(z1, na.rm = T), 
                     min(z2, na.rm = T), max(z2, na.rm = T))
    # get interquartile range of variable
    cuts <- unname(quantile(na.omit(z2), c(0.25, 0.75)))
    tiks <- unname(quantile(na.omit(z2), c(.26, .42, .58, .74)))
    tmp <- tmp[which(tmp$x >= cuts[1] & tmp$x <= cuts[2]),]
    # plot
    tmplot <- ggplot(tmp) +
      geom_smooth(aes(x = x, y = y), method = "loess", color = "black") +
      geom_rug(aes(x = x), size = .05, alpha= .5)
  }
  # fix HDI, since the label gets cut off
  if(vars[i] == "undp_hdi"){
    tmplot <- tmplot +
      scale_x_continuous(breaks = c(0.75, 0.80, 0.85), 
        labels = c("0.75", "0.80", "0.85"))
  }
  # finish the plot
  tmplot <- tmplot +
    labs(x=nms[i], y=NULL, title=NULL) +
    theme(axis.line=element_blank(),
          axis.text = element_text(color="black", size = rel(1.5)),
          axis.title.y=element_blank(),
          axis.title.x=element_text(size = rel(1.5)),
          legend.position="none", panel.background=element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), 
          plot.background=element_blank())
  eval(parse(text=paste("pl_", vars[i]," <- tmplot", sep = "")))
  # save the range of predictions to the table
  tp <- ggplot_build(tmplot)$data[[1]]
  tab$pred_lo[i] <- mean(tp$y[which(tp$x ==  min(tp$x))])
  tab$pred_hi[i] <- mean(tp$y[which(tp$x ==  max(tp$x))])
}

# save the figures
for(i in 1:length(vars)){
  # this if-else nonsense is just to appropriately name the figures for EJPR
  if(as.numeric(ords[i]) < 7){
    eval(parse(text=paste('ggsave("/results/figure_3_', 
                          ords[i], '.pdf", pl_', vars[i], 
                          ', device = "pdf", width = 4, height = 4, units = "in")', 
                          sep=""))) 
  } else {
    eval(parse(text=paste('ggsave("/results/figure_a1_', 
                          as.character(as.numeric(ords[i])-6), 
                          '.pdf", pl_', vars[i], 
                          ', device = "pdf", width = 4, height = 4, units = "in")', 
                          sep="")))
  }
}

##### TABLE 2 #####

### substantive magnitude of the effects
# recreate the main DV in the overall sample 
df$emd_diff <- df$emd_lessaffluent - df$emd_moreaffluent
# get the quantiles among the observed values of the predictions
perc <- ecdf(df$emd_diff)
tab$quant_lo <- perc(tab$pred_lo)
tab$quant_hi <- perc(tab$pred_hi)
# compute real and absolute substantive effects
tab$subst_effect <- tab$quant_hi-tab$quant_lo
tab$abs_effect <- abs(tab$subst_effect)
# subset to just the variables we highlight
ss <- tab[which(tab$var %in% c("vdem_clientelism", "pc_women_elite_real",
                               "gdp_log", "vdem_corr_index", "undp_hdi",
                               "gini")),]
ss <- ss[nrow(ss):1,c("name", "subst_effect")]
ss$subst_effect <- round(ss$subst_effect, 2)
# put them in the order of the paper
ss$order <- rep(NA, nrow(ss))
ss$order[which(ss$name == "Clientelism")] <- 1
ss$order[which(ss$name == "GDP (logged)")] <- 2
ss$order[which(ss$name == "HDI")] <- 3
ss$order[which(ss$name == "% female legislators")] <- 4
ss$order[which(ss$name == "Corruption")] <- 5
ss$order[which(ss$name == "Income inequality")] <- 6
ss <- ss[order(ss$order),]
ss$order <- NULL
# save it
names(ss) <- c("Variable", "Effect")
write.csv(ss, "/results/table_2.csv", row.names = F)

### case examples 1
# representation quantiles for clientelism at low and high values
tab$quant_lo[which(tab$var == "vdem_clientelism")]
tab$quant_hi[which(tab$var == "vdem_clientelism")]
# find the "low clientelism" 25% percentile
quantile(df$vdem_clientelism, .25)
# this is Finland 2014
df[which(df$country == "Finland" & df$year == "2014"), 
   c("country", "year", "vdem_clientelism", "emd_diff")]
# find the "high clientelism" 75% percentile
quantile(df$vdem_clientelism, .75)
# this is Mexico 1997
df[which(df$country == "Mexico" & df$year == "1997"), 
   c("country", "year", "vdem_clientelism", "emd_diff")]

### case example 2
# representation quantiles for clientelism at low and high values
tab$quant_lo[which(tab$var == "undp_hdi")]
tab$quant_hi[which(tab$var == "undp_hdi")]
# find the "low HDI" 25% percentile
quantile(df$undp_hdi, .25, na.rm = T)
# this is DR 2014
df[which(df$country == "Dominican Republic" & df$year == "2014"), 
   c("country", "year", "undp_hdi", "emd_diff")]
# find the "high HDI" 75% percentile
quantile(df$undp_hdi, .75, na.rm = T)
# this is Ireland 2011
df[which(df$country == "Ireland" & df$year == "2011"), 
   c("country", "year", "undp_hdi", "emd_diff")]

##### FIGURE 4 #####

### clean up
rm(list=ls()[!(ls() %in% c("df", "imp", "n_imp"))])

### create samples and resampling indices for each imputation
testXs <- testYs <- trainXs <- trainYs <- indexPreds <- vector("list", n_imp)
for(i in 1:n_imp){
  
  ### get the imputed data
  tmp <- complete(imp, i)
  
  # create the DV - do this here so the imputation doesn't fail
  tmp$emd_diff <- tmp$emd_lessaffluent - tmp$emd_moreaffluent
  
  ### subset to cases in Europe
  tmp <- tmp[which(tmp$region == "Europe"),]
  tmp <- tmp[-which(tmp$country %in% c("Bulgaria", "Croatia", "Cyprus", 
                                       "Czech Republic", "Estonia", "Hungary", 
                                       "Latvia", "Lithuania", "Poland", 
                                       "Romania", "Slovakia", "Slovenia", 
                                       "Ukraine")),]


  ### subset to the variables we need
  tmp <- tmp[,c("emd_diff", "vdem_turnout_vap", "vdem_compulsory", "left_gov",
                "trade_pc_gdp", "fdi_net_log", "xcut_race_income", 
                "disproportionality", "democracy_duration", "mean_party_age",
                "vdem_cs_partic", "vdem_donate", "vdem_corr_index", 
                "vdem_clientelism", "gini", "gdp_log", "undp_hdi",
                "trade_union_density", "pc_women_elite_real")]
  
  ### make sure classes are all numeric
  for(j in 1:ncol(tmp)){
    tmp[,j] <- as.numeric(as.character(tmp[,j]))
  }
  
  ### partition data into train and test sets
  index <- createDataPartition(tmp$emd_diff, p = 0.75, list = FALSE)
  trainDF <- tmp[index,]
  testDF <- tmp[-index,]
  
  ### get resampling profiles
  indexes <- createMultiFolds(trainDF$emd_diff, 5, 5)
  
  ### split the covariates from the response
  trainX <- trainDF[,-which(names(trainDF) == "emd_diff")]
  trainY <- trainDF$emd_diff
  testX <- testDF[,-which(names(testDF) == "emd_diff")]
  testY <- testDF$emd_diff
  
  ### store everything for looping
  trainXs[[i]] <- trainX
  trainYs[[i]] <- trainY
  testXs[[i]] <- testX
  testYs[[i]] <- testY
  indexPreds[[i]] <- indexes
  
  ### delete so we don't accidentally duplicate stuff (inefficient but safer)
  rm(tmp, index, indexes, trainDF, testDF, trainX, trainY, testX, testY)
}

### get the models we're going to use
models <- sort(c("avNNet", "cforest", "dnn", "glm", "glmboost",
                 "glmnet", "knn", "mlp", "nnet", "pcaNNet", "ppr", "rf", 
                 "treebag"))

### set aside the storage for all the model fits, metrics, and predictions
for(i in 1:length(models)){
  eval(parse(text=paste(models[i],'Mods <- vector("list",n_imp)',sep="")))
  eval(parse(text=paste(models[i],'Imps <- vector("list",n_imp)',sep="")))
  eval(parse(text=paste(models[i],'Preds <- vector("list",n_imp)',sep="")))
}

### estimate the models
for(j in 1:length(models)){
  
  ### grab the model
  meth <- models[j]
  
  ### loop over the imputations
  for(i in 1:n_imp){
    
    ### force random forest to get the importance metric
    if(meth %in% c("rf")){
      # set the control object
      objControl <- trainControl(method = 'repeatedcv', number = 5, 
                                 repeats = 5, index = indexPreds[[i]], 
                                 returnResamp='final', search = "random", 
                                 verboseIter = F, savePredictions="final", 
                                 allowParallel = F)
      # print a message updating progress
      message(paste("ESTIMATING MODEL: ", meth, " (", j, " of ", length(models),
                    "), replicate ", i, " of ", n_imp, " at ", Sys.time(), 
                    sep=""))
      # estimate the model
      tmp <- train(x = trainXs[[i]], y = trainYs[[i]], metric = "RMSE",
                   trControl = objControl, tuneLength=10, method = meth,
                   importance = TRUE)
      # store the results
      eval(parse(text=paste(models[j],'Mods[[',i,']] <- tmp',sep="")))
      eval(parse(text=paste(models[j],'Imps[[',i,
                            ']] <- varImp(tmp)$importance[1]',sep="")))
      # advance the loop
      next
    }
    
    ### run the rest of the models
    # set the control object
    objControl <- trainControl(method = 'repeatedcv', number = 5, 
                               repeats = 5, index = indexPreds[[i]], 
                               returnResamp='final', search = "random", 
                               verboseIter = F, savePredictions="final", 
                               allowParallel = F)
    # print a message updating progress
    message(paste("ESTIMATING MODEL: ", meth, " (", j, " of ", length(models),
                  "), replicate ", i, " of ", n_imp, " at ", Sys.time(), 
                  sep=""))
    # estimate the model
    tmp <- train(x = trainXs[[i]], y = trainYs[[i]], metric = "RMSE",
                 trControl = objControl, tuneLength=10, method = meth)
    # store the results
    eval(parse(text=paste(models[j],'Mods[[',i,']] <- tmp',sep="")))
    eval(parse(text=paste(models[j],'Imps[[',i,
                          ']] <- varImp(tmp)$importance[1]',sep="")))
  }
}

### variable importance -- reshape, spit out csvs for temporary storage
outs <- ls(pattern = "Imps")
for(i in 1:length(outs)){
  eval(parse(text=paste("tmp <- ", outs[i], sep="")))
  tmp <- do.call(cbind, tmp)
  names(tmp) <- paste0("imp", c(1:n_imp), sep="")
  tmp$av <- apply(tmp, 1, mean)
  tmp$var <- rownames(tmp); row.names(tmp) <- NULL
  for(j in 1:n_imp){
    eval(parse(text=paste("tmp$rank",j,
                          "<- rank(-tmp$imp",
                          j,", ties.method = 'average')", # -1 so they descend
                          sep=""))) 
  }
  tmp$rav <- apply(tmp[, which(grepl("rank", names(tmp)) == T)], 1, mean)
  tmp <- tmp[order(tmp$rav, tmp$var),
             c((n_imp + 2), ncol(tmp), (n_imp + 1),
               1:n_imp, (n_imp+3):(ncol(tmp)-1))]
  nout <- paste("/results/variable-importances/europe_", models[i], ".csv", 
                sep="")
  write.csv(tmp, nout, row.names = F)
  eval(parse(text=paste(outs[i],"<- tmp",sep="")))
}

### how do the learners compare predictively?
preds <- ls(pattern = "Preds"); preds <- preds[-which(preds == "indexPreds")]
mods <- ls(pattern = "Mods")
for(j in 1:length(preds)){
  message(paste("PREDICTING OUT OF: ", models[j], " (", j, " of ", 
                length(models), ")",sep=""))
  for(i in 1:n_imp){
    eval(parse(text=paste(preds[j], '[[', i, ']] <- predict(',
                          mods[j], '[[', i, ']], newdata=testXs[[', i,
                          ']])', sep="")))
  }
}
for(i in 1:length(preds)){
  eval(parse(text=paste("tmp <- ", preds[i], sep="")))
  tmp <- lapply(tmp, function(x) as.data.frame(x))
  tmp <- mapply(cbind, tmp, "n_imp"=c(1:n_imp), SIMPLIFY=F)
  tmp <- data.frame(do.call(rbind, tmp))
  names(tmp)[1] <- "yhat"
  eval(parse(text=paste(preds[i],"<- tmp",sep="")))
}
# store the Y
Y <- testYs
Y <- mapply(cbind, Y, "n_imp"=c(1:n_imp), SIMPLIFY=F)
Y <- data.frame(do.call(rbind,Y))
names(Y)[1] <- "y"

### AUCs
rmse <- data.frame(matrix(nrow=n_imp, ncol=length(models)))
names(rmse) <- models
for(j in 1:length(preds)){
  eval(parse(text=paste("tmp <- ", preds[j], sep="")))
  for(i in 1:nrow(rmse)){
    rmse[i,j] <- sqrt(mean((tmp$yhat[which(tmp$n_imp == i)] - 
                         Y$y[which(Y$n_imp == i)])^2))
  }
}

# which learners produce the best AUC? which are the most stable?
sort(apply(rmse,2,mean))
apply(rmse, 2, sd)

### all basically the same predictively, but take the best
bestmods <- names(sort(apply(rmse,2,mean)))[1:1]

### get variable importance
imps <- list.files(path = "/results/variable-importances/", pattern = "europe_")
imps <- lapply(imps, function(x){
  x <- read.csv(paste("/results/variable-importances/", x, sep = ""))
})
imps <- imps[which(models %in% bestmods)]
# add a column for which model it is
imps <- mapply(cbind, imps, "model"=models[which(models %in% bestmods)], 
               SIMPLIFY=F)
# merge
imps <- do.call(rbind, imps)

### compute global means and interquartile ranges
vari <- sort(unique(as.character(imps$var)))
plt <- data.frame(matrix(nrow = length(vari), ncol = 5))
names(plt) <- c("lo", "mean", "hi", "var", "loc")
for(i in 1:length(vari)){
  x <- as.matrix(imps[which(imps$var == vari[i]), 
                      which(grepl("imp", names(imps)))])
  plt$var[i] <- vari[i]
  plt$mean[i] <- median(x)
  plt$lo[i] <- unname(quantile(x, .25))
  plt$hi[i] <- unname(quantile(x, .75))
}
plt <- plt[order(plt$mean, plt$var),]
plt$loc <- 1:nrow(plt)

### fix names for plotting
plt$nm <- rep(NA, nrow(plt))
plt$nm[which(plt$var == "disproportionality")] <- "Disproportionality"
plt$nm[which(plt$var == "vdem_compulsory")] <- "Compulsory voting"
plt$nm[which(plt$var == "fdi_net_log")] <- "Foreign cap. depend."
plt$nm[which(plt$var == "xcut_race_income")] <- "Cross-cuttingness"
plt$nm[which(plt$var == "vdem_turnout_vap")] <- "Turnout"
plt$nm[which(plt$var == "trade_pc_gdp")] <- "Trade openness"
plt$nm[which(plt$var == "vdem_cs_partic")] <- "Civil society"
plt$nm[which(plt$var == "trade_union_density")] <- "Trade union density"
plt$nm[which(plt$var == "undp_hdi")] <- "HDI"
plt$nm[which(plt$var == "democracy_duration")] <- "Age of democracy"
plt$nm[which(plt$var == "vdem_corr_index")] <- "Corruption"
plt$nm[which(plt$var == "mean_party_age")] <- "Party institutionalization"
plt$nm[which(plt$var == "gini")] <- "Income inequality"
plt$nm[which(plt$var == "vdem_donate")] <- "Pol. donation restrictions"
plt$nm[which(plt$var == "left_gov")] <- "Government ideology"
plt$nm[which(plt$var == "pc_women_elite_real")] <- "% female legislators"
plt$nm[which(plt$var == "gdp_log")] <- "GDP (logged)"
plt$nm[which(plt$var == "vdem_clientelism")] <- "Clientelism"

### plot
fig4 <- ggplot(plt, aes(x = mean,y = loc)) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color = "black")) +
  geom_vline(xintercept = mean(plt$mean), 
             linetype = "longdash", color = "darkgray") +
  geom_point(color = "black") +
  geom_segment(aes(x = lo, xend = hi, y = loc, yend = loc), color = "black") +
  scale_y_continuous(name = "Covariate", breaks = plt$loc, 
                     labels = factor(plt$nm)) +
  labs(x = "Variable importance, Western Europe only", title = NULL)
ggsave("/results/figure_4.pdf", fig4, device = "pdf", 
       width = 7.5, height = 5.5, units = "in")

##### FOOTNOTE 10: ROBUSTNESS TO NO IMPUTATION #####

### create the dependent variable
df$emd_diff <- df$emd_lessaffluent - df$emd_moreaffluent

### subset to the variables we need
df <- df[,c("emd_diff", "vdem_turnout_vap", "vdem_compulsory", "left_gov",
            "trade_pc_gdp", "fdi_net_log", "xcut_race_income", 
            "democracy_duration", "mean_party_age",
            "vdem_cs_partic", "vdem_donate", "vdem_corr_index", 
            "vdem_clientelism", "gini", "gdp_log", "undp_hdi",
            "trade_union_density", "pc_women_elite_real")]

### drop NA
df <- na.omit(df)

### make sure classes are all numeric
for(j in 1:ncol(df)){
  df[,j] <- as.numeric(as.character(df[,j]))
}

### partition data into train and test sets
index <- createDataPartition(df$emd_diff, p = 0.75, list = FALSE)
trainDF <- df[index,]
testDF <- df[-index,]

### get resampling profiles
indexes <- createMultiFolds(trainDF$emd_diff, 5, 5)

### split the covariates from the response
trainX <- trainDF[,-which(names(trainDF) == "emd_diff")]
trainY <- trainDF$emd_diff
testX <- testDF[,-which(names(testDF) == "emd_diff")]
testY <- testDF$emd_diff

### run the random forest on the data
# set the control object
objControl <- trainControl(method = 'repeatedcv', number = 5, 
                           repeats = 5, index = indexes, 
                           returnResamp='final', search = "random", 
                           verboseIter = F, savePredictions="final", 
                           allowParallel = F)
# estimate the model
mod <- train(x = trainX, y = trainY, metric = "RMSE",
             trControl = objControl, tuneLength=10, method = "rf",
             importance = TRUE)
# store the variable importance results
imp <- varImp(mod)$importance[1]
imp$var <- row.names(imp); row.names(imp) <- NULL
imp <- imp[order(imp$Overall, decreasing = T),]

### export as a table
vars <- c("disproportionality", "vdem_compulsory", "mean_party_age", 
          "vdem_donate", "xcut_race_income", "trade_pc_gdp", 
          "trade_union_density", "vdem_turnout_vap", "vdem_cs_partic", 
          "fdi_net_log", "pc_women_elite_real", "democracy_duration", 
          "vdem_corr_index", "gdp_log", "undp_hdi", "left_gov", "gini", 
          "vdem_clientelism")
nms <- c("Disproportionality", "Compulsory voting", 
         "Party institutionalization", "Pol. donation restrictions", 
         "Cross-cuttingness", "Trade openness", "Trade union density", 
         "Turnout", "Civil society", "Foreign cap. depend.", 
         "% female legislators", "Age of democracy", "Corruption", 
         "GDP (logged)", "HDI", "Government ideology", "Income inequality", 
         "Clientelism")
imp$nm <- rep(NA, nrow(imp))
for(i in 1:length(vars)){
  imp$nm[which(imp$var == vars[i])] <- nms[i]
}
imp <- imp[,c("nm", "Overall")]
imp$Overall <- round(imp$Overall, 2)
names(imp) <- c("Variable", "Importance")
write.csv(imp, "/results/table_A3.csv", row.names = F)

#### VERSION CONTROL #####

### print session info
sessionInfo()

### turn off the sink
sink()