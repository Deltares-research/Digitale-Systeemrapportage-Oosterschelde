
require(tidyverse)
require(mblm)
require(scales)

# common plot style
mystyle <- list(theme_bw(), 
                theme(plot.title = element_text(size = rel(0.8), color = "purple")))

legendbottomstyle <- list(theme_bw(), 
                          theme(legend.position = "bottom", legend.direction = "horizontal"),
                          theme(plot.title = element_text(size = rel(0.8), color = "purple")))


# otherFillStyle <- list(theme_fivethirtyeight(),
#                    scale_fill_hc())


checkWQdata <- function(data, maxPlotwaarde = 100000, season = "winter", loc, param, mystyle, beginjaar, eindjaar = 2020){

WQ %>%
  filter(locatie %in% loc) %>%
  filter(seizoen %in% season) %>%
  # filter(if (season == "summer") maand %in% seq(3,10) else if (season == "winter") maand %in% c(1,2,11,12) else maand %in% seq(1,12)) %>%
  filter(parameter %in% param) %>%
  # filter(waarde < maxPlotwaarde) %>%
  filter(datumtijd > as.POSIXct(paste0(beginjaar, "-01-01"))) %>%
  filter(datumtijd < as.POSIXct(paste0(eindjaar, "-12-31"))) %>%
  group_by(locatie, parameter, jaar, maand) %>% 
  summarize(maandgemiddelde = mean(waarde, na.rm = T)) %>% ungroup() %>%
  group_by(locatie, parameter, jaar) %>% 
  summarize(jaargemiddelde = mean(maandgemiddelde, na.rm = T), n = n()) %>% ungroup() %>%
  filter(!is.na(jaargemiddelde)) %>%
  spread(key = locatie, value = jaargemiddelde) %>%
  gather(key = locatie, value = jaargemiddelde, -jaar, -n, -parameter) %>%
  ggplot(aes(jaar, jaargemiddelde)) +
  geom_line(aes(color = locatie)) +
  geom_point(aes(size = n, color = locatie)) +
  scale_size_continuous(range = c(0,4)) +
  mystyle
}


# hoeft geen maandgemiddelde te zijn, kan ook 
plotWQseason <- function(data, maxPlotwaarde = 100000, loc, param, mystyle, beginjaar, eindjaar = 2020, pointsplot = F, lineplot = F, boxplotting = F, smoothing = F, logscale = F){
  plotTitel = paste(loc, collapse = ', ')
  neededNames <- c("parameter", "locatie", "jaar", "maand")
  failingNames <- !(neededNames %in% colnames(data))
  if(any(failingNames)){
    return(cat("error: column ", neededNames[failingNames], " is needed and not present in the dataframe"))
  }
  
  filldata <- data.frame()
  
  for(ii in c(1:length(loc))){
    for(jj in c(1:length(param))){
      filldata <- filldata %>% bind_rows(
        data.frame(
          parameter = param[[jj]],
          locatie = loc[[ii]],
          jaar = seq(beginjaar+0.5, eindjaar-0.5), 
          dag = 0.5, #dag = 0.5,
          waarde = NA)
      )
    }
  }
    
  data <- data %>%
    filter(locatie %in% loc) %>%
    filter(parameter %in% param) %>%
    filter(waarde < maxPlotwaarde) %>%
    filter(datumtijd > as.POSIXct(paste0(beginjaar, "-01-01"))) %>%
    filter(datumtijd < as.POSIXct(paste0(eindjaar, "-12-31"))) %>%
    mutate(week = lubridate::week(datumtijd)) %>%
    
    mutate(dag = lubridate::yday(datumtijd)) %>%
    # group_by(locatie, week, jaar) %>% mutate(waarde = mean(waarde), n = n()) %>% ungroup() %>%
    bind_rows(filldata
      # data.frame(jaar = seq(beginjaar+0.5, eindjaar-0.5), 
      #                    dag = 0.5, #dag = 0.5,
      #                    waarde = NA)
      ) %>%
    arrange(locatie, parameter, jaar, dag)
  
  if(logscale) yposition <- quantile(data$waarde, 0.008, na.rm = T)
  if(!logscale) yposition <- -quantile(data$waarde, 0.2, na.rm = T)
  
  maanddagen = data %>% group_by(maand) %>%
    summarize(maanddag = mean(dag))
  
  pl <- data %>%  ggplot(aes(dag, waarde))
  if(lineplot) pl <- pl +     geom_path(aes(color = jaar), size = 1)
  if(pointsplot) pl <- pl + geom_point(aes(color = jaar), shape = 21, fill = "white")
  if(smoothing) pl <- pl +   geom_smooth(span = 0.2)
  if(boxplotting) pl <- pl +   geom_boxplot(aes(group = week))
  if(logscale) pl <- pl +  scale_y_log10()
  
  pl + theme(axis.title.x = element_text(hjust=5)) +  # werkt nog niet goed
    geom_text(data = maanddagen, aes(x = maanddag, y = yposition, label = month.abb[maand])) +
    # geom_text(data = maanddagen, aes(x = dag, y = 1, group = maand)) +
    ggtitle(paste(param, plotTitel)) + mystyle +
    scale_x_continuous(breaks = cumsum(lubridate::day(seq(as.Date("2017-02-01"),length=12,by="months")-1)))
}


plotWQyears <- function(data, maxPlotwaarde = 100000, loc, param, 
                        mystyle, beginjaar, eindjaar, pointsplot = T, lineplot = T, trend = T,
                        smoothing = F, logscale = F, season = c("zomer", "winter")){
  plotTitel = paste(loc, collapse = ', ')
  neededNames <- c("parameter", "locatie", "jaar", "maand")
  failingNames <- !(neededNames %in% colnames(data))
  if(any(failingNames)){
    return(cat("error: column ", neededNames[failingNames], " is needed and not present in the dataframe"))
  }
  
  data1 <- data %>%
    filter(locatie %in% loc) %>%
    filter(seizoen %in% season) %>%
    # filter(if (season == "summer") maand %in% seq(3,10) else if (season == "winter") maand %in% c(1,2,11,12) else maand %in% seq(1,12)) %>%
    filter(parameter %in% param) %>%
    filter(waarde < maxPlotwaarde) %>%
    filter(datumtijd > as.POSIXct(paste0(beginjaar, "-01-01"))) %>%
    filter(datumtijd < as.POSIXct(paste0(eindjaar, "-12-31"))) %>%
    group_by(locatie, parameter, jaar, maand) %>% 
    summarize(maandgemiddelde = mean(waarde, na.rm = T)) %>% ungroup() %>%
    group_by(locatie, parameter, jaar) %>% 
    summarize(jaargemiddelde = mean(maandgemiddelde, na.rm = T)) %>% ungroup() %>%
    filter(!is.na(jaargemiddelde)) %>%
    spread(key = locatie, value = jaargemiddelde) %>%
    gather(key = locatie, value = jaargemiddelde, -jaar, -parameter)
  
  p.vals = sapply(unique(data1$locatie), function(i) {
    coef(summary(lm(jaargemiddelde ~ jaar, data=data1[data1$locatie==i, ])))[2,4]
  })
  names(p.vals) = unique(data1$locatie)
                         
  data1$parameter <- factor(as.character(data1$parameter), levels = param)
  data1$locatie <- factor(as.character(data1$locatie), levels = loc)
  

  if(logscale) yposition <- quantile(data1$jaargemiddelde, 0.008, na.rm = T)
  if(!logscale) yposition <- -quantile(data1$jaargemiddelde, 0.2, na.rm = T)
  
  
  pl <- data1 %>%  ggplot(aes(jaar, jaargemiddelde, color = locatie))
  if(lineplot) pl <- pl +     geom_line(size = 1, alpha = 0.5)
  if(pointsplot) pl <- pl + geom_point(fill = "white", size = 2)
  if(smoothing) pl <- pl +   geom_smooth(span = 0.2)
  if(trend) pl <- pl + geom_smooth(data=data1[data1$locatie %in% names(p.vals)[p.vals < 0.05],], aes(jaar, jaargemiddelde), method='lm')
  # if(boxplotting) pl <- pl +   geom_boxplot(aes(group = week))
  if(logscale) pl <- pl +  scale_y_log10()
  
  unit <- str_split(param, "_", simplify = T)[1,2]
  
  yLabel = ifelse(all(season == "zomer"), "groeiseizoen gemiddeld",
                  ifelse(all(season == "winter"), "winter gemiddeld", "jaargemiddeld"))
  
  pl + theme(axis.title.x = element_text(hjust=5)) +  # werkt nog niet goed
    # geom_text(data = maanddagen, aes(x = maanddag, y = yposition, label = month.abb[maand])) +
    # geom_text(data = maanddagen, aes(x = dag, y = 1, group = maand)) +
    ggtitle(paste(param, plotTitel)) + mystyle +
    scale_x_continuous(breaks = pretty_breaks()) +
    ylab(paste(yLabel, unit, sep = " \n"))
    # scale_x_continuous(breaks = cumsum(lubridate::day(seq(as.Date("2017-02-01"),length=12,by="months")-1)))

  
}



plotWQdata <- function(data, maxPlotwaarde = 100000, loc, param, line = F, 
                       loess = F, span = 0.7, perc90line = F, perc10line = F, 
                       trend = F, sen = F, boxplot = F, mystyle, beginjaar, eindjaar = 2020, 
                       events = NULL, logscale = F, points = T, pointalpha = 0.6) {
  
  plotTitel = paste(loc, collapse = ', ')
  ## check if necessary column names are present
  neededNames <- c("parameter", "locatie", "datumtijd", "jaar", "maand", "seizoen")
  failingNames <- !(neededNames %in% colnames(data))
  if(any(failingNames)){
    return(cat("error: column ", neededNames[failingNames], " is needed and not present in the dataframe"))
  }
  
  multiParam <- ifelse(length(param)==1, F, T)
  multiLocatie <- ifelse(length(loc)==1, F, T)
  
  data <- data %>%
    filter(locatie %in% loc) %>%
    filter(parameter %in% param) %>%
    filter(waarde < maxPlotwaarde) %>%
    filter(datumtijd > as.POSIXct(paste0(beginjaar, "-01-01"))) %>%
    filter(datumtijd < as.POSIXct(paste0(eindjaar, "-12-31")))
  # browser()
  if(length(data$datumtijd) == 0) {stop("No data available for this combination of arguments")}
  
  data$parameter <- factor(data$parameter, levels = param)
  
  x = as.numeric(data$datumtijd); y = data$waarde
  minscale   <- min(data$datumtijd, na.rm = T)
  maxscale   <- max(data$datumtijd, na.rm = T)
  
  minscale <- min(data$datumtijd, na.rm = T)
  maxscale <- max(data$datumtijd, na.rm = T)
  yposition <- quantile(data$waarde, 0.99, na.rm = T)
  
  pl <- data %>%  ggplot(aes(datumtijd, waarde))
  
  if(points){
    
  if(multiLocatie) pl <- pl + geom_point(aes(fill = locatie), color = "white", shape = 21, size = 2, alpha = pointalpha) else

    pl <- pl + geom_point(aes(), fill = "seagreen4", shape = 21, size = 2, color = "white", alpha = pointalpha)
}  
  if(loess) {pl <- pl + geom_smooth(aes(), method='loess', span = span, size = 1)}
  
  if(perc90line){
    
    # calculate quantile statistics
    data_jaar <- plyr::ddply(data, ~ jaar + seizoen + locatie, summarize, perc90 = quantile(waarde, probs = 0.9, na.rm = T))
    data_jaar$jaar <- as.POSIXct(paste(as.character(data_jaar$jaar), "-07-01 00:00:00", sep = ""))
    
    # add quantiles as crossbars to plot
    pl <- pl + geom_crossbar(data = data_jaar, aes(jaar, perc90, color = interaction(seizoen, locatie)), size = 0.4, linetype = 1, ymin =F, ymax = F) +
      scale_colour_discrete(guide = "legend")
  }
  
  if(perc10line){
    
    # calculate quantile statistics
    data_jaar2 <- plyr::ddply(data, ~ jaar + seizoen, summarize, perc10 = quantile(waarde, probs = 0.1, na.rm = T))
    data_jaar2$jaar <- as.POSIXct(paste(as.character(data_jaar2$jaar), "-07-01 00:00:00", sep = ""))
    
    # add quantiles as crossbars to plot
    pl <- pl + geom_crossbar(data = data_jaar2, aes(jaar, perc10, color = seizoen), size = 0.3, linetype = 5, ymin =F, ymax = F) +
      scale_colour_discrete(guide = "legend")
  }
  
  if(sen){

    ## experimentele code om coefficienten en p te berekenen, nog aanpassen
    # data <- WQwaterbase
    #  a <- unname(unlist(data[,"waarde"]))[1:500]
    #  b <- unname(unlist(data[,"datumtijd"]))[1:500]
    # nonparamtrend <- mblm::mblm(a ~ b)
    # summary(nonparamtrend)
    # str(nonparamtrend)
    # coef(nonparamtrend)
    # hoe komt p er uit???

    sen <- function(..., weights = NULL) {
      mblm::mblm(...)
    }
    pl <- pl + geom_smooth(method = sen)
  }
  
  
  if(trend){
    
    # create statistics summary values for linear regression
    regression <- summary(glm(y ~ x + I(cos(2 * pi * x / 31556995)) + I(sin(2 * pi * x / 31556995))))
    slope      <- regression$coefficients[2,1]  #slope of regression including season effect
    pvalue     <- format(regression$coefficients[2,4], digits = 2)
    intercept  <- regression$coefficients[[1]]
    
    # create y position for text in graph
    yposition  <- quantile(data$waarde, 0.99, na.rm = T)
    
    # add trendline with seasonal variation
    pl <- pl + geom_smooth(aes(), method='lm', formula = y ~ x+I(cos(2*pi*x/31622400))+I(sin(2*pi*x/31622400)), size = 0.7, alpha = 0.3, n = 1000)
    
    # add linear trendline without seasonal variation
    pl <- pl + geom_abline(intercept = intercept, slope = slope , color = "darkblue", size = 1)
    
    # add text on linear trend statistics
    pl <- pl + annotate("text", label = paste("linear trend =", format(slope*31622400, digits = 2), param, "per year; ", "p=", pvalue), x = maxscale - 0.5 * (maxscale - minscale), y = yposition, size = 3)
  }
  
  if(line) {pl <- pl + geom_line(aes(), color = "darkgrey")}
  if(!is.null(events)){pl <- pl + geom_vline(xintercept = events, color = "orange", size = 1)}
  
  if(boxplot) {
    
    # add box and whiskers to plot
    pl <- pl + geom_boxplot(aes(x = as.POSIXct(paste0(jaar, '-07-01 00:00:00')),
                                y = waarde,
                                color = seizoen, group = interaction(seizoen, jaar)), 
                            alpha = 0.6,
                            outlier.size = 0)
  }
  
  # add vertical lines for model period
  # pl <- pl + geom_vline(xintercept = as.numeric(as.POSIXct("2014-01-01")), size = 1, linetype = 2, color = "black", alpha = 0.5)
  # pl <- pl + geom_vline(xintercept = as.numeric(as.POSIXct("2015-01-01")), size = 1, linetype = 2, color = "black", alpha = 0.5)
  
  pl <- pl + xlab("jaren") + ylab(paste(param)) +
    scale_x_datetime(minor_breaks = date_breaks("1 year"))
  pl <- pl + ggtitle(paste(param, plotTitel)) + mystyle
  if(logscale) { pl <- pl + scale_y_log10()}
  if(multiParam) {pl <- pl + facet_grid(parameter ~ ., scales = "free")}
  return(pl)
}


plotHeatMap <- function(data, loc, param, minPlotwaarde = 0, maxPlotwaarde = NA, beginjaar = 1970, eindjaar = 2020, mystyle = mystyle){
  plotTitel = paste(loc, collapse = ', ')
  data <- data %>% 
    filter(locatie %in% loc) %>%
    filter(parameter %in% param) %>%
    # filter(waarde < maxPlotwaarde) %>%
    filter(datumtijd > as.POSIXct(paste0(beginjaar, "-01-01"))) %>%
    filter(datumtijd < as.POSIXct(paste0(eindjaar, "-12-31"))) %>%
    group_by(jaar, maand, parameter, locatie) %>% summarize(maandgemiddelde = mean(waarde, na.rm = T)) %>%
    ggplot(aes(jaar, maand)) +
    geom_tile(aes(fill = maandgemiddelde)) +
    scale_fill_gradientn(colours=rev(rainbow(4)), limits = c(minPlotwaarde,maxPlotwaarde)) +
    scale_y_continuous(breaks = c(2,4,6,8,10, 12), labels = month.abb[c(2,4,6,8,10,12)]) +
    ggtitle(paste(param, plotTitel)) +
    scale_x_continuous(breaks = pretty_breaks()) +
    mystyle
  return(data)
}


plotWQfractions <- function(data, maxPlotwaarde = 100000, loc, params, sumparam, mystyle, beginjaar, eindjaar = 2020, 
                            events = NULL, logscale = F) {
  
  plotTitel = paste(sumparam, paste(params, collapse = ','), paste(loc, collapse = ','))
  ## check if necessary column names are present
  neededNames <- c("parameter", "locatie", "datumtijd", "jaar", "maand", "seizoen")
  failingNames <- !(neededNames %in% colnames(data))
  if(any(failingNames)){
    return(cat("error: column ", neededNames[failingNames], " is needed and not present in the dataframe"))
  }
  
  areadata <- data %>%
    filter(locatie %in% loc) %>%
    filter(parameter %in% params) %>%
    filter(waarde < maxPlotwaarde) %>%
    filter(datumtijd > as.POSIXct(paste0(beginjaar, "-01-01"))) %>%
    filter(datumtijd < as.POSIXct(paste0(eindjaar, "-12-31"))) %>%
    group_by(locatie, parameter, jaar, maand) %>% summarize(maandgemiddelde = mean(waarde, na.rm = T)) %>% ungroup() %>%
    group_by(locatie, parameter, jaar) %>% summarize(jaargemiddelde = mean(maandgemiddelde, na.rm = T)) %>% ungroup() %>%
    filter(!is.na(jaargemiddelde)) %>%
    spread(key = locatie, value = jaargemiddelde) %>%
    gather(key = locatie, value = jaargemiddelde, -jaar, -parameter)
  
  
  if(!is.null(sumparam)){
    sumdata <- data %>%
      filter(locatie %in% loc) %>%
      filter(parameter %in% sumparam) %>%
      filter(waarde < maxPlotwaarde) %>%
      filter(datumtijd > as.POSIXct(paste0(beginjaar, "-01-01"))) %>%
      filter(datumtijd < as.POSIXct(paste0(eindjaar, "-12-31"))) %>%
      group_by(locatie, parameter, jaar, maand) %>% summarize(maandgemiddelde = mean(waarde, na.rm = T)) %>% ungroup() %>%
      group_by(locatie, parameter, jaar) %>% summarize(jaargemiddelde = mean(maandgemiddelde, na.rm = T)) %>% ungroup() %>%
      filter(!is.na(jaargemiddelde)) %>%
      spread(key = locatie, value = jaargemiddelde) %>%
      gather(key = locatie, value = jaargemiddelde, -jaar, -parameter)
    
  }
  
  # yposition <- quantile(data$waarde, 0.99, na.rm = T)
  
  pl <- areadata %>%  ggplot(aes(jaar, jaargemiddelde))
  
  pl <- pl + geom_area(aes(fill = parameter))
  
  if(!is.null(sumparam))  {
    # pl <- pl + geom_point(data = sumdata, aes(), fill = "black", shape = 21, size = 2, color = "white", alpha = 0.8)
    pl <- pl + geom_line(data = sumdata, aes(), color = "black")
  }  
  # add vertical lines for model period
  # pl <- pl + geom_vline(xintercept = as.numeric(as.POSIXct("2014-01-01")), size = 1, linetype = 2, color = "black", alpha = 0.5)
  # pl <- pl + geom_vline(xintercept = as.numeric(as.POSIXct("2015-01-01")), size = 1, linetype = 2, color = "black", alpha = 0.5)
  
  pl <- pl + xlab("jaren") + ylab(paste(sumparam)) #+
    # scale_x_datetime(minor_breaks = date_breaks("1 year"))
  pl <- pl + ggtitle(paste(plotTitel)) + mystyle
  if(logscale) { pl <- pl + scale_y_log10()}
  pl
}

plot.meetpaal.data <- function (file, xx = "datum", yy = "value", pointcolor = "positie", ylabel, xlabel, facets = F) {
  plot <- ggplot(file ,aes_string(xx, yy))
  plot <- plot + 
    geom_line(aes_string(color = pointcolor), na.rm = T) +
    # geom_vline(xintercept = as.numeric(as.Date("2004-06-01")), size = 1, linetype = 2, color = "black", alpha = 0.5) +
    ylab(ylabel) + xlab(xlabel) +theme(legend.position = "top", legend.direction = "horizontal")
  if(facets){plt <- plot + facet_grid(locatie ~ .)}
  plot + scale_x_date(minor_breaks = date_breaks("year"))
}



plot.windrose <- function(data = NULL,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0,
                          name_fill="Wind m/s"){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    # cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = name_fill, 
                      values = spd.colors,
                      drop = FALSE) +
    #theme_bw() +
    theme(axis.title.x = element_blank(),
          #panel.border = element_rect(colour = "blank"),
          panel.grid.major = element_line(colour="grey65"))
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose + theme_minimal())  
  
  # return the handle to the wind rose
  # return(p.windrose)
}
