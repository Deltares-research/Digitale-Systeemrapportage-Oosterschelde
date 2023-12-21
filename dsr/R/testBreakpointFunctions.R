

# source("runThisFirst.R")
# WQwaterbase <- readr::read_csv(file.path(datadir, "chemie/eutrofiering/base/waterbase_collected2.csv")#,
#                                # col_types = "ciiiTcniiic", 
#                                # delim = ";"
# ) %>% 
#   mutate(locatie = as.factor(locatie), parameter = as.factor(parameter)) %>% 
#   filter(!is.na(waarde))


plotBreakPointTimeSeries <- function(df, mylocation, mysubstance, h = NULL, breaks = NULL, minjaar, maxjaar, jaarnaam = "winterjaar"){
  require(tidyverse)
  require(xts)
  require(zoo)
  require(strucchange)
  
  # df$winterjaar <- unlist(df[,jaarnaam])
  
  subset <- df[
    df[,"locatie"] == mylocation & 
      df[,"parameter"] == mysubstance &
      df[,jaarnaam] <= maxjaar & df[,jaarnaam] >= minjaar,] %>%
    dplyr::mutate(parameter = as.character(parameter), locatie = as.character(locatie)) %>%
    dplyr::filter(!is.na(waarde)) %>%
    dplyr::group_by(winterjaar, parameter, locatie) %>% 
    dplyr::summarize(jaargemiddelde = mean(waarde)) %>%
    dplyr::ungroup() %>%
    # mutate(winterjaar = as.Date(as.character(winterjaar), "%Y")) %>%
    as.data.frame() 
  # %>%  ## necessary to convert tibble to dataframe for time series 
  #   base::split(interaction(.$parameter, .$locatie))
  
  if(length(subset[,1])<1) return(NA)
  g <- data.frame(winterjaar = seq(min(subset[,jaarnaam]), max(subset[,jaarnaam]), by = 1)) %>% 
    dplyr::left_join(subset) %>% 
    dplyr::mutate(approx = na.approx(.[,"jaargemiddelde"]))
  number_of_na <- length(which(is.na(g[,"jaargemiddelde"])))
  print(paste("number of NA =", number_of_na))
  tsInterpolated <- ts(g[,"approx"],  start = min(g[,jaarnaam]), end = max(g[,jaarnaam]))
  #   return(g)
  # }
  # 
  # makeInterpolatedTimeseries(subset[[20]], "jaargemiddelde", "winterjaar")
  
  # timeserieslist <- lapply(subset, makeInterpolatedTimeseries, "jaargemiddelde", "winterjaar")
  
  # plotBreakPointTimeSeries <- function(timeserieslist, location, substance){
  
  # timeseries <- timeserieslist[[paste(substance, location, sep = ".")]]
  name = paste(mylocation, mysubstance)
  bp <- strucchange::breakpoints(tsInterpolated ~ 1, h = h, breaks = breaks)
  fm0 <- lm(tsInterpolated ~ 1)
  fm1 <- lm(tsInterpolated ~ breakfactor(bp))
  plot(tsInterpolated, xlab = "jaar", ylab = name, type = "p")
  lines(ts(fitted(fm0), start = min(time(tsInterpolated))), col = 3)
  lines(ts(fitted(fm1), start = min(time(tsInterpolated))), col = 4)
  lines(bp)
  ci_subset_ts <- confint(bp)
  ci_subset_ts
  lines(ci_subset_ts)
}



plotBreakPointTimeSeries(WQwaterbase, mylocation = "Dreischor", mysubstance = "PO4_mg/l", minjaar = 2000, maxjaar = 2017)



plotChangepointTimeSeries <- function(df, mylocation, mysubstance, h = NULL, breaks = NULL, minjaar, maxjaar, jaarnaam = "winterjaar"){
  require(tidyverse)
  require(ggfortify)
  require(changepoint)
  # require(xts)
  # require(zoo)
  # require(strucchange)
  
  # df$winterjaar <- unlist(df[,jaarnaam])
  
  subset <- df[
    df[,"locatie"] == mylocation & 
      df[,"parameter"] == mysubstance &
      df[,jaarnaam] <= maxjaar & df[,jaarnaam] >= minjaar,] %>%
    dplyr::mutate(parameter = as.character(parameter), locatie = as.character(locatie)) %>%
    dplyr::filter(!is.na(waarde)) %>%
    dplyr::group_by(winterjaar, parameter, locatie) %>% 
    dplyr::summarize(jaargemiddelde = mean(waarde)) %>%
    dplyr::ungroup() %>%
    # mutate(winterjaar = as.Date(as.character(winterjaar), "%Y")) %>%
    as.data.frame() 
  # %>%  ## necessary to convert tibble to dataframe for time series 
  #   base::split(interaction(.$parameter, .$locatie))
  
  if(length(subset[,1])<1) return(NA)
  g <- data.frame(winterjaar = seq(min(subset[,jaarnaam]), max(subset[,jaarnaam]), by = 1)) %>% 
    dplyr::left_join(subset) %>% 
    dplyr::mutate(approx = na.approx(.[,"jaargemiddelde"]))
  number_of_na <- length(which(is.na(g[,"jaargemiddelde"])))
  print(paste("number of NA =", number_of_na))
  tsInterpolated <- ts(g[,"approx"],  start = min(g[,jaarnaam]), end = max(g[,jaarnaam]))
  #   return(g)
  # }
  # 
  # makeInterpolatedTimeseries(subset[[20]], "jaargemiddelde", "winterjaar")
  
  # timeserieslist <- lapply(subset, makeInterpolatedTimeseries, "jaargemiddelde", "winterjaar")
  
  # plotBreakPointTimeSeries <- function(timeserieslist, location, substance){
  
  # timeseries <- timeserieslist[[paste(substance, location, sep = ".")]]
  name = paste(mylocation, mysubstance)
  
  # tsInterpolated %>%
  #   changepoint::cpt.var() %>%
  #   autoplot()
  
  bp <- strucchange::breakpoints(tsInterpolated ~ 1, h = h, breaks = breaks)
  autoplot(bp, ylab = mysubstance, axis.text.x = FALSE, ts.geom = 'bar')
}

plotChangepointTimeSeries(WQwaterbase, mylocation = "Dreischor", mysubstance = "NH4_mg/l", minjaar = 1990, maxjaar = 2017)
