require(ggplot2)

## Utility functions
onlyNumeric <- function(x) {
  suppressWarnings(any(is.na(as.numeric(x[!is.na(x)]))))
}

deFactorise <- function(data, ignore.var = NULL) {
  facVar <- names(sapply(data, class)[which(sapply(data, class) == "factor")])
  for(var in facVar[!facVar %in% ignore.var]) {
    data[, var] <- as.character(data[, var])
  }
  return(data)
}

autoNumeric <- function(data, ignore.var = NULL) {
  charVar <- names(sapply(data, class)[which(sapply(data, class) == "character")])
  for(var in charVar[!charVar %in% ignore.var]) {
    if(!onlyNumeric(data[, var])) data[, var] <- as.numeric(data[, var])
  }
  return(data)
}

## Takes a map object of class 'mpcross' and returns marker locations and chromosome.
## The immediate purpose is to get the chromosome lengths from this information


ggGenMap <- function(object, printPlot = F){
  require(plyr)
  require(ggplot2)
  groups <- object$map
  map.df <- ldply(groups, function(x) {
    datOut <- as.data.frame(x)
    datOut$Marker <- row.names(datOut)
    names(datOut)[1] <- c("distance")
    return(datOut)
  })
  names(map.df)[1] <- "id"
  if(printPlot) print(ggplot(map.df, aes(id, distance)) + geom_line(aes(group = id)) + scale_y_reverse())
  return(map.df)
}


## Creates a simple plot of the linkage groups

ggGenMap.df <- function(GenMap.df, id = "id", distance = "distance") {
  require(ggplot2)
  print(ggplot(GenMap.df, aes_string(x = id, y = distance)) + geom_line(aes_string(group = id)) +
          scale_y_reverse())
}

## Takes a map from ggGenMap, returns linkage
mapParams <- function(map) {
  if(class(map) == "mpcross") map <- ggGenMap(map)
  map <- map[grepl("^[1-7][A-D]$", map$id),]
  map$Chr <- sub("([1-7])[A-D]", "\\1", map$id)
  map.length <- map[match(unique(map$id), map$id),]
  map.length$group <- factor(sub("[0-7]", "", map.length$id))
  map.length$length <- tapply(map$distance, map$id, max)
  map.length <- map.length[match(unique(map.length$id), map.length$id),]
  #map.length$background <- factor(c(rep(1:2, 10), 1))
  map.length$offset <- c(0, cumsum(map.length$length[-nrow(map.length)]))
  map.length$max <- with(map.length, length + offset)
  return(map.length)
}

baseParams <- function(map) {
  if(class(map) == "mpcross") map <- ggGenMap(map)
  map.length <- mapParams(map)
  map.layout <- setNames(map.length[,c("id", "offset", "max")], c("id", "min", "max"))
  map.layout <- melt(map.layout, id.vars="id", variable.name="join", value.name="location")
}

## Converts interval QTL data into marker
interval2Marker <- function(data) {
  dataMarker <- within(data, {
    Marker <- Left.Marker
    dist..cM. <- (Left.dist + Right.dist)/2
    #if(!"X..var" %in% names(data)) X..var <- var
    if(any(grepl("LOD", names(data)))){
      LOGP <- LOD ## THIS IS WRONG!! JUST A PLACEHOLDER
      Founder.LOGP <- Founder.LOD ## SAME GOES FOR THIS
    }
  })
}

qtlParams <- function(data, map, yVariable, returnMain = FALSE, nameMain = NULL, returnFounder = FALSE, nameFounder = NULL) {
  if(any(grepl("Left", names(data)))) data <- interval2Marker(data)
  if(class(map) == "mpcross") map <- ggGenMap(map)
  require(zoo)
  require(plyr)
  #browser()
  founders <- levels(as.factor(data$Founder))
  mapDat <- mapParams(map)
  temp <- data
  temp <- data.frame(apply(data, 2, function(x) {
    x[x == ""] <- NA
    return(x)
  }), stringsAsFactors = FALSE)
  temp <- data.frame(apply(temp, 2, na.locf), stringsAsFactors = FALSE)
  temp <- autoNumeric(deFactorise(temp))
  #   temp <- ddply(temp, .(yVariable, Chromosome, Marker), mutate, 
  #                QTLid = paste(yVariable[1], Marker[1], sep = "_"))
  if(is.factor(data[, yVariable])){
    temp[, yVariable] <- factor(temp[, yVariable], levels = levels(data[, yVariable]))
    temp$yVars <- as.numeric(temp[, yVariable])
  } 
  else temp$yVars <- as.numeric(factor(temp[, yVariable]))
  #mainQTL <- temp[temp$Founder == founders[1],c("Chromosome", "yVars", "Marker", "dist..cM.", "Prob", "X..var", "LOGP", yVariable)]
  mainQTL <- merge(temp, mapDat[,c("offset", "id")], by.x = "Chromosome", by.y = "id")
  mainQTL$offsetDist <- with(mainQTL, dist..cM. + offset)
  founderQTL <- temp[,c("Chromosome", "dist..cM.", "yVars", "Founder", "Size", "Founder.Prob", "Founder.LOGP", yVariable)]
  founderQTL <- merge(founderQTL, mapDat[,c("offset", "id")], by.x = "Chromosome", by.y = "id")
  founderQTL$offsetDist <- with(founderQTL, dist..cM. + offset)
  founderQTL <- ddply(founderQTL, yVariable, transform,
                      scaledSize = Size/max(abs(Size)))
  if(returnMain) assign(if(is.null(nameMain)) {
    paste(deparse(substitute(data)), "MainQTL", sep = "_")
  } else {
    nameMain
  }, mainQTL, envir=sys.frame(-1))
  
  if(returnFounder) assign(if(is.null(nameFounder)) {
    paste(deparse(substitute(data)), "FounderQTL", sep = "_")
  } else {
    nameFounder
  }, founderQTL, envir=sys.frame(-1))
  #assign(mapDat, "mapBase", envir=sys.frame(-1))
}

mapStructure <- function(data, map, yVariable, type) {
  if(any(grepl("Left", names(data)))) data <- interval2Marker(data)
  if(class(map) == "mpcross") map <- ggGenMap(map)
  founders <- levels(as.factor(data$Founder))
  mapDat <- mapParams(map)
  if(is.factor(data[, yVariable])) {
    yVars <- levels(data[, yVariable]) 
  } else {
    yVars <- as.character(unique(unlist(data[, yVariable])))[order(as.character(unique(unlist(data[, yVariable]))))] # Variable will be trait/site
  }
  temp <- mapDat[mapDat$Founder == founders[1],]
  xBreaks <- unique(with(mapDat, max - (length/2)))
  xLabels <- unique(mapDat$id)
  baseLayout <- merge(expand.grid(id = unique(mapDat$id), founderNum = 1:length(founders), siteNum = 1:length(yVars)),
                      mapDat, by = "id")
  baseLayout <- within(baseLayout, {
    Founder <- factor(founderNum)
    levels(Founder) <- founders
    yVar <- factor(siteNum)
    levels(yVar) <- yVars
  })
  if(type == "founder") {
    return(list(data = baseLayout, xBreaks = xBreaks, xLabels = xLabels, yVars = yVars, type = "founder"))
  }
  if(type == "main") {
    temp <- baseLayout[baseLayout$Founder == founders[1],]
    temp$Founder <- "QTL"
    return(list(data = temp, yVars = yVars, xBreaks = xBreaks, xLabels = xLabels, type = "main"))
  }
}

basePlot <- function(mapParams) {
  
  base <- ggplot(mapParams$data, aes(offset, siteNum)) + 
    geom_rect(aes(xmin = offset, xmax = max, ymin = siteNum - 0.5, ymax = siteNum + 0.5, fill = group)) +
    geom_hline(aes(yintercept = siteNum)) +
    scale_x_continuous(expand = c(0,0), breaks = mapParams$xBreaks, labels = mapParams$xLabels) +
    scale_fill_manual(values = c("#323f4a", "#566d7f", "#80a3bf"), guide = guide_legend(title = "Genome")) +
    scale_y_reverse(expand = c(0,0), breaks = 1:length(mapParams$yVars), labels = mapParams$yVars)
  if(mapParams$type == "main") return(base)
  if(mapParams$type == "founder") return(base)# + facet_wrap(~Founder, ncol = 1))
}

beadPlotFounder <- function(data, map, yVariable, founderPointRange, founderLabel = NULL, axisTextScale, useTheme = FALSE, multiScale = FALSE) {
  ##browser()
  require(ggplot2)
  sizeBreaks <- axisTicks(c(0, max(data$LOGP, na.rm = TRUE)), TRUE)
  if(any(grepl("Left", names(data)))) data <- interval2Marker(data)
  if(class(map) == "mpcross") map <- ggGenMap(map)
  baseStr <- mapStructure(data, map, yVariable, type = "founder")
  base <- basePlot(baseStr)
  qtlParams(data, map, returnFounder = TRUE, nameFounder = "dataFounderQTL", yVariable=yVariable)
  if(multiScale) baseP <-  base + geom_point(data = dataFounderQTL, aes(y = yVars, x = offsetDist, size = 1 - Founder.Prob, colour = scaledSize))
  else baseP <- base + geom_point(data = dataFounderQTL, aes(y = yVars, x = offsetDist, size = 1 - Founder.Prob, colour = Size))
  comp <- baseP + scale_size_continuous(range=founderPointRange, name = "Founder Probability", breaks = c(0.9, 0.75, 0.6)) + #, labels = c(0.75, 0.50, 0.25, 0)) + 
    guides(size = guide_legend(order = 1, reverse = TRUE)) + facet_wrap(~Founder, ncol = 1)
    theme(plot.title = element_text(size = rel(2)), axis.text = element_text(size = rel(axisTextScale), colour = "black"))
  if(!is.null(founderLabel)) comp <- comp + labs(title = founderLabel) + theme(plot.title = element_text(size = rel(3), hjust = -0.075))
  if(multiScale) comp <- comp + scale_color_gradient2(low=143, high = "darkgreen", mid="grey", name = "Allelic effect", breaks = c(-1, 1), labels = c("Min", "Max"))
    else comp <- comp + scale_color_gradient2(low=143, high = "darkgreen", mid="grey", name = "Allelic effect", guide = guide_colourbar(draw.ulim = TRUE))
  if(!useTheme) return(comp)
  else return(comp + qtlVizThemeSingle)
}

beadPlotMain <- function(data, map, yVariable, colourBarName = colourBarName, qtlPointRange, mainLabel = NULL, axisTextScale, useTheme = FALSE) {
  if(is.null(colourBarName)) colourBarName = expression(-log[10](p))
  #browser()
  colourBreaks <- axisTicks(c(0, log10(max(data$LOGP, na.rm = TRUE))), TRUE)
  require(ggplot2)
  if(any(grepl("Left", names(data)))) data <- interval2Marker(data)
  if(class(map) == "mpcross") map <- ggGenMap(map)
  baseStr <- mapStructure(data, map, yVariable, type = "main")
  base <- basePlot(baseStr)
  qtlParams(data, map, returnMain = TRUE, nameMain = "dataMainQTL", yVariable=yVariable)
  comp <- base + geom_point(data = dataMainQTL, aes(y = yVars, x = offsetDist, size = X..var, colour = LOGP)) +
    scale_size_continuous(trans = "log10", range=qtlPointRange, name = "% Variance explained", breaks = c(1, 5, 10, 20, 40, 80)) + 
    scale_color_continuous(trans = "log10", low="white", high = "red", breaks = colourBreaks, name = colourBarName) +
    guides(size = guide_legend(order = 1)) + guides(size = guide_legend(order = 1)) +
    theme(plot.title = element_text(size = rel(2)), axis.text = element_text(size = rel(axisTextScale), colour = "black"),
          strip.text = element_blank(), strip.background = element_blank(), legend.position = "bottom") + qtlVizThemeSingle
  if(!is.null(mainLabel)) comp <- comp + labs(title = mainLabel) + theme(plot.title = element_text(size = rel(3), hjust = -0.075))
  if(!useTheme) return(comp)
  else return(comp + qtlVizThemeSingle)
}

#Put it in error messages if arguments are blank
beadPlotCombo <- function(data, map, yVariable, colourBarName, qtlPointRange, founderPointRange, mainLabel = NULL, founderLabel = NULL, axisTextScale,
                          multiScale = FALSE) {
  require(gridExtra)
  require(scales)
  if(any(grepl("Left", names(data)))) data <- interval2Marker(data)
  if(class(map) == "mpcross") map <- ggGenMap(map)
  founderPlot <- beadPlotFounder(data, map, yVariable, founderPointRange, founderLabel, axisTextScale, multiScale = multiScale) + qtlVizThemeCombo
  mainPlot <- beadPlotMain(data, map, yVariable, colourBarName, qtlPointRange, mainLabel, axisTextScale) + qtlVizThemeCombo
  comboPlot <- arrangeGrob(mainPlot + theme(legend.position = "bottom"), founderPlot + theme(legend.position = "bottom"), ncol = 1, heights = c(1.7, 4))
}

beadPlot <- function(data, map, yVariable, type, colourBarName, qtlPointRange, founderPointRange, mainLabel = NULL, founderLabel = NULL, axisTextScale = 1,
                     multiScale = FALSE) {
  if(type == "founder") {
    return(beadPlotFounder(data, map, yVariable, founderPointRange, founderLabel, axisTextScale, useTheme = TRUE, multiScale = multiScale))
  }
  else if(type == "main") {
    return(beadPlotMain(data, map, yVariable, colourBarName, qtlPointRange, mainLabel, axisTextScale, useTheme = TRUE))
  }
  else return(beadPlotCombo(data, map, yVariable, colourBarName, qtlPointRange, founderPointRange, mainLabel, founderLabel, axisTextScale, multiScale))
}



qtlVizThemeSingle <- theme(legend.key = element_blank(),
                          panel.background = element_blank(),
                          axis.ticks = element_blank(),
                          axis.title = element_blank(),
                          legend.direction = "horizontal",
                          legend.box = "horizontal",
                          legend.position = "bottom")
qtlVizThemeCombo <- theme(legend.direction = "horizontal",
                     legend.box = "horizontal",
                     legend.key = element_blank(),
                     panel.background = element_blank(),
                     axis.ticks = element_blank(),
                     axis.title = element_blank())