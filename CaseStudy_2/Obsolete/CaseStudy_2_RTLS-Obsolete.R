
options(digits = 2)
# Setting working directory to folder where source
# code is located
library(lattice)
library(fields)
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

# Data Exploration
# Loading Offline Data text file
txt = readLines("Data/offline.final.trace.txt")
length(txt)    # Loaded 151392 lines/strings
head(txt,4)

# Locating and counting the lines/strings that 
# begin with '#' Comment Lines
sum(substr(txt, 1, 1) == "#") # Total of 5312

# Parsing 'txt' character strings; Eliminating the comment lines
# beginning with '#'
lines = txt[ substr(txt, 1, 1) != "#" ] # 146080 strings


# From documnentation, we expect there to be 146,080 lines
# (166 locations x 8 angles x 110 recodings) = 146,080
# This leaves 5312 comment lines (151392 - 146080)

        #------------------ sample Parsing
        # We split the string on several
        # different characters ( ; = , ). Using ( txt[4] )
        # as sample. Saving strings to new variable ' tokens'
        tokens = strsplit(txt[4], "[;=,]")[[1]]
        
        # The first 10 parts are
        tokens[1:10]
        tokens[c(2, 4, 6:8, 10)]
        
        # Displaying tokens variables minus the identification components located
        # in the first 10 variables.  MACID; Signal Strength, Freq., Mode
        tokens[ - ( 1:10 ) ]

        # convert tokens to martrix w/o ID components. Matrix 4 columns X # of rows
        tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
        # combines tokens' tmp matrix, adding on the relavent ID components from tokens
        mat = cbind(matrix(tokens[c(2, 4, 6:8, 10)], 
                           nrow = nrow(tmp),
                           ncol = 6, byrow = TRUE), tmp)
        
        # Creates a 11 X 10 matrix of the results of txt[4].
        dim(mat)

            #--------------------- processingLine()  Parsing function
            # Now that we know the parsing works properly, we can create a function to 
            # to preform the same parsing activity to all observations
            
            processLine = function(x)
            {
              tokens = strsplit(x, "[;=,]")[[1]]
              
              # Adds error statement if tokens length is == 10 (no sensor data)
              # Will return a null value instead or warning message
              if (length(tokens) == 10) 
                return(NULL)
              
              tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
              cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6, 
                           byrow = TRUE), tmp)
            }
            #-------------------------
        
        # Trying the Parsing function on the offline dataset (txt)
        options(error = recover, warn = 1)
        tmp = lapply(lines, processLine)
        typeof(tmp) # Creates a list
        tmp[1]

        # Listing the number of rows in for each  of the first 17 'tmp' list
        sapply(tmp[c(1:17)], nrow) # 11 10 10 11  9 10  9  9 10 11 11  9  9  9  8 10 14
        
        # Create a dataframe, called offline' with the lists from tmp
        offline = as.data.frame(do.call("rbind", tmp),stringsAsFactors = FALSE)
        dim(offline) #1181628 rows 10 columns
        
        #####offlineTest = offline
        ####identical(offline,offlineTest)
        
        # Changing names of columns in offline dataFrame to more
        # descriptive names
        names(offline) = c("time", "scanMac", "posX", "posY", "posZ", 
                           "orientation", "mac", "signal", 
                           "channel", "type")
        
        # Creating list numVars: of 'numerical' variables columns.
        #   Time, Position, Orientation, Signal Strength
        numVars = c("time", "posX", "posY", "posZ", 
                    "orientation", "signal")
        
        
        # Converting Offline dataFrame variables to numeric variables from character
        # using NumVars list
        offline[ numVars ] =  lapply(offline[ numVars ], as.numeric)
        summary(offline)
        #dim(offline) #1181628 rows x 10 columns
        
        # Keeping only 'Access point'  type = 3 (adhoc type=1 are eliminated)
        offline = offline[ offline$type == "3", ] # 203,185 observations eliminated
        offline = offline[ , "type" != names(offline) ] # Removing 'type' from Dataframe
        #dim(offline) #978443 rows x 9 columns

        # Creating a new variable 'rawTime'
        offline$rawTime = offline$time
        offline$time = offline$time/1000
        # Creating list of classes for offline$time
        class(offline$time) = c("POSIXt", "POSIXct")
        # Converts time variable into 2 variable types time1-POSIXt and time2-POSIXct
        unlist(lapply(offline, class))
        offline$time[1]
  
        # Displaying summary of offline dataframe (variable columns only)
        summary(offline[, numVars])
        
        #Displaying summary of offline dataframe (character variables)
        # Also converting 'mac','channel',and 'scanMac' columns to factors
        summary(sapply(offline[ , c("mac", "channel", "scanMac")],
                       as.factor))
        
        # Eliminating the scanMac and z position 'posZ' variables/columns from dataFrame
        offline = offline[ , !(names(offline) %in% c("scanMac", "posZ"))]
        #dim(offline) #978443 rows x 8 columns
        
        # Determining the number of unique orientations within
        # the dataset
        length(unique(offline$orientation)) #203 unique orientations
        
        ####testing = offline
        ####identical(offline, testing)
        #------------- FIG 1.2-Test  Plotting Empirical Cumulative Distribution
        #?ecdf()
        plot(ecdf(offline$orientation))
        # Creating PDF of ECDF graph
        pdf(file = "Fig 1.2-Test Geo_ECDFOrientation.pdf", width = 10, height = 7)
        oldPar = par(mar = c(4, 4, 1, 1))
        plot(ecdf(offline$orientation), pch = 19, cex = 0.3,
             xlim = c(-5, 365), axes = FALSE,
             xlab = "orientation", ylab = "Empirical CDF", main = "")
        box()
        axis(2)
        axis(side = 1, at = seq(0, 360, by = 45))
        par(oldPar)
        dev.off()
        
        #------------- Plotting Geo Density Orientation
        # Creating PDF of Geo Density graph
        pdf(file = "Fig 1.2b-Test Geo_DensityOrientation.pdf", width = 10, height = 5)
        oldPar = par(mar = c(4, 4, 1, 1))
        plot(density(offline$orientation, bw = 2), 
             xlab = "orientation", main = "")
        par(oldPar)
        dev.off()
        
        #---------- Orientation Angle (round to 8 values 0,45,90...270,315)
        # Currently 203 unique orientations. ROund to nearest value.
        roundOrientation = function(angles) {
          refs = seq(0, by = 45, length  = 9)
          q = sapply(angles, function(z) which.min(abs(z - refs)))
          c(refs[1:8], 0)[q]
        }
        
        offline$angle = roundOrientation(offline$orientation)
        
        #------------- FIG. 1.3  Plotting BoxPlots Rounded Orientation Angle
        # Creating PDF of BoxPlot Angle graph
        pdf(file = "Fig 1.3-Test Geo_BoxplotAngle.pdf", width = 10)
        oldPar = par(mar = c(4, 4, 1, 1))
        boxplot(offline$orientation ~ offline$angle,
                xlab = 'nearest 45 degree angle',
                ylab = 'orientation')
        par(oldPar)
        dev.off()
        
        
        # 12 mac addresses; 8 channels
        # For additional info about access points from
        # mac IDs http://coffer.com/mac_find/
        c(length(unique(offline$mac)), length(unique(offline$channel)))
        
        # Create a table showing the mac ID and the number of occurrances
        # Create a list of the top 7 mac IDs(access Points) in
        # decreasing order.
        # Removing the mac IDs with the lowest number of 
        # readings (to eliminate access points that are not
        # included in the tests)
        table(offline$mac)
        subMacs = names(sort(table(offline$mac), decreasing = TRUE))[1:7]
        offline = offline[ offline$mac %in% subMacs, ]
        
        # Each mac-ID is associated with a certain 'channel'
        # thus, we can eliminate the 'channel' variable from
        # the dataset
        macChannel = with(offline, table(mac, channel))
        apply(macChannel, 1, function(x) sum(x > 0))
        
        offline = offline[ , "channel" != names(offline)]
        summary(offline)
        offlineExplore = offline

        #---------------- Exploring position of Hand Held
        
        # Determining the number of locations we have. Create
        # a list of dataframes containing all unique 
        # posx (34),posy (14) combinations [476 total]
        
        locDF = with(offline, 
                     by(offline, list(posX, posY), function(x) x))
        typeof(locDF)
        locDF[3]
        length(locDF) # 476 locations
        sum(sapply(locDF, is.null)) # 310 are empty
        
        # This leaves 166 locations that were observed. Matches
        # the literature.  We eliminate the non-observed locatoins
        # from location dataframe list (locDF)
        locDF = locDF[ !sapply(locDF, is.null) ]
        length(locDF) # List of 166 unique location dataframes
        

        # Determine the observations for each observed
        # unique location
        # Creates matrix with location and count of number
        # of observations
        locCounts = sapply(locDF, 
                           function(df) 
                             c(df[1, c("posX", "posY")], count = nrow(df)))
        class(locCounts) # matrix
        dim(locCounts) # 3 166
        
        locCounts[ , 1:8] # approx 5500+ observations for each
        # observed locations (8 orient X 7 acc
        # pts x 110 replications = 6160 possible
        # observations)
        
        #-------------FIG 1.5 Plotting X,Y Dot Plot with # of Observations
        # 
        # Creating PDF of Dot plot displaying the number of observations
        # at each x,y  location
        pdf(file = "Fig 1.5-Test Geo_XYByCount.pdf", width = 10)
        oldPar = par(mar = c(3.1, 3.1, 1, 1))
        # locCounts matrix transposed
        locCounts = t(locCounts)
        plot(locCounts, type = "n", xlab = "", ylab = "")
        text(locCounts, labels = locCounts[,3], cex = .8, srt = 45)
        
        par(oldPar)
        dev.off()
        
        #-------------FIG 1.6 Creating a grid of boxplots of signal strength by 
        # orientation/angle, subdivided by macID
        pdf(file = "Fig 1.6-Test Geo_BoxplotSignalByMacAngle.pdf", width = 7)
        oldPar = par(mar = c(3.1, 3, 1, 1))
        
        bwplot(signal ~ factor(angle) | mac, data = offline, 
               subset = posX == 2 & posY == 12 #Chose to look at pos 2,12 for
               # reference. May choose to look at
               # other locations if desired
               & mac != "00:0f:a3:39:dd:cd",  # eliminated mac id'ed as extra address 
               layout = c(2,3))
        
        par(oldPar)
        dev.off()

        # Signal strenght ranges from min -98 to max -25. More negative the signal
        # the weaker the signal
        summary(offline$signal)
        
        
        #-------------FIG 1.7 Creating signal strength density curves matrix
        # for each macID and orientation
        pdf(file = "Fig 1.7-Test Geo_DensitySignalByMacAngle.pdf", width = 8, height = 12)
        oldPar = par(mar = c(3.1, 3, 1, 1))
        
        densityplot( ~ signal | mac + factor(angle), data = offline,
                     subset = posX == 24 & posY == 4 &  #Chose to look at pos 24,4 for
                       # reference. May choose to look at
                       # other locations if desired
                       mac != "00:0f:a3:39:dd:cd",
                     bw = 0.5, plot.points = FALSE)
        
        par(oldPar)
        dev.off()
        
                

#-------------- Read Data Function ----------
#--------------------------------------------
# Function reads data and parses it according to
# our requirements.  Duplicates all functions previously executed on dataset
# Puts it in a convieneient function

readData = 
  function(filename = 'Data/offline.final.trace.txt', 
           subMacs = c("00:0f:a3:39:e1:c0", "00:0f:a3:39:dd:cd", "00:14:bf:b1:97:8a",
                       "00:14:bf:3b:c7:c6", "00:14:bf:b1:97:90", "00:14:bf:b1:97:8d",
                       "00:14:bf:b1:97:81"))
  {
    # Loading Data Set text file
    txt = readLines(filename)
    lines = txt[ substr(txt, 1, 1) != "#" ]

    processLine = function(x)
    {
      tokens = strsplit(x, "[;=,]")[[1]]
      
      if (length(tokens) == 10) 
        return(NULL)
      
      tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
      cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6, 
                   byrow = TRUE), tmp)
    }

    tmp = lapply(lines, processLine)
    DataFunc = as.data.frame(do.call("rbind", tmp), 
                                stringsAsFactors= FALSE) 
    
    names(DataFunc) = c("time", "scanMac", 
                           "posX", "posY", "posZ", "orientation", 
                           "mac", "signal", "channel", "type")
    
    # keep only signals from access points
    DataFunc = DataFunc[ DataFunc$type == "3", ]
    
    # drop scanMac, posZ, channel, and type - no info in them
    dropVars = c("scanMac", "posZ", "channel", "type")
    DataFunc = DataFunc[ , !( names(DataFunc) %in% dropVars ) ]
    
    # drop more unwanted access points
    DataFunc = DataFunc[ DataFunc$mac %in% subMacs, ]
    
    # convert numeric values
    numVars = c("time", "posX", "posY", "orientation", "signal")
    DataFunc[ numVars ] = lapply(DataFunc[ numVars ], as.numeric)
    
    # convert time to POSIX
    DataFunc$rawTime = DataFunc$time
    DataFunc$time = DataFunc$time/1000
    class(DataFunc$time) = c("POSIXt", "POSIXct")
    
    # round orientations to nearest 45
    roundOrientation = function(angles) {
      refs = seq(0, by = 45, length  = 9)
      q = sapply(angles, function(o) which.min(abs(o - refs)))
      c(refs[1:8], 0)[q]
    }

    DataFunc$angle = roundOrientation(DataFunc$orientation)
    
    return(DataFunc)
  }
#----------- readData() Function End ------

        
#Read original dataset, using readData function
offline = readData()

        # Test to see if the original 'stepwise' data parsing
        # and function data parsing dataset are identical
        identical(offline, offlineExplore) # TRUE

        
# Creation of new variable posXY which combines both x and y IDs
offline$posXY = paste(offline$posX, offline$posY, sep = "-")
        
# Creating a list of dataframes for each unique location id (166), 
# angle(8), and macIDs(7) = Total 9296 list of dataframes
byLocAngleAP = with(offline, 
              by(offline, list(posXY, angle, mac), 
               function(x) x))
        
# ------------------- signal summary function ---
# Signal summary function: Calculates summary statistics on each
# of the dataframes
signalSummary = 
    lapply(byLocAngleAP,            
           function(oneLoc) {
             ans = oneLoc[1, ]
             ans$medSignal = median(oneLoc$signal)
             ans$avgSignal = mean(oneLoc$signal)
             ans$num = length(oneLoc$signal)
             ans$sdSignal = sd(oneLoc$signal)
             ans$iqrSignal = IQR(oneLoc$signal)
             ans
           })
        
# Creates matrix from unique values from LocAngleAP (9 variables) and 
# append signal summary data (median,mean,length,sd,IQR)
offlineSummary = do.call("rbind", signalSummary)    # 9296 x 14 
        
        
#-------------FIG 1.8 Creating boxplots of standard deviation of
# signal strength for a given range of average signal strengths
pdf(file = "Fig 1.8-Test Geo_BoxplotSignalSDByAvg.pdf", width = 10)
oldPar = par(mar = c(3.1, 3, 1, 1))
        
    breaks = seq(-90, -30, by = 5)
    bwplot(sdSignal ~ cut(avgSignal, breaks = breaks),
           data = offlineSummary, 
           subset = mac != "00:0f:a3:39:dd:cd",
           xlab = "Mean Signal", ylab = "SD Signal")
        
par(oldPar)
dev.off()

#-------------FIG 1.9 Skewness of signal strength - Scatter plot (smooth)
# looking at mean-median vs # of observations
# No apparent evidence of skewing since mean and median differ by less than
# 1 - 2 dBm. Green trend line confirms this
pdf(file = "Fig 1.9-Test Geo_ScatterMean-Median.pdf", width = 10)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

    with(offlineSummary,
         smoothScatter((avgSignal - medSignal) ~ num,
                       xlab = "Number of Observations", 
                       ylab = "mean - median"))
    abline(h = 0, col = "#984ea3", lwd = 2)
    lo.obj = 
      with(offlineSummary,
           loess(diff ~ num, 
                 data = data.frame(diff = (avgSignal - medSignal),
                                   num = num)))
    # Predicted difference (mean-median) trend line from # observation = 70-120 
    lo.obj.pr = predict(lo.obj, newdata = data.frame(num = (70:120)))
    lines(x = 70:120, y = lo.obj.pr, col = "#4daf4a", lwd = 2)

par(oldPar)
dev.off()


#------------------ surfaceSS() Signal strength heat/contour Function
surfaceSS = function(data, mac, angle = 45) {
    require(fields)
    oneAPAngle = data[ data$mac == mac & data$angle == angle, ]
    smoothSS = Tps(oneAPAngle[, c("posX","posY")], 
                   oneAPAngle$avgSignal)
    vizSmooth = predictSurface(smoothSS)
    plot.surface(vizSmooth, type = "C", main = paste("MacID ",mac," Angle ",angle,"°"),
                 xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    points(oneAPAngle$posX, oneAPAngle$posY, pch=19, cex = 0.5) 
}
#--------------------

#-------------FIG 1.10 Signal Strength Heat Map for various Access Points and Angles
pdf(file="Fig 1.10-Test MedSignal_2AccessPts_2Angles.pdf", width = 9, height = 6)
parCur = par(mfrow = c(2,2), mar = rep(1, 4)) # 2 x 2 plot
    # arbitrarily choosing macID's "00:14:bf:b1:97:90" & "00:0f:a3:39:e1:c0"
    # Angles: 0 & 135
    # using mapply to make 4 separate calls to the surfaceSS function
    mapply(surfaceSS, mac = subMacs[ rep(c(5, 1), each = 2) ], 
           angle = rep(c(0, 135), 2),
           data = list(data = offlineSummary))

par(parCur)
dev.off()

# We can identify the access point from the 'dark-red' region on the map
# The affect of orientation can also be visualized
# Corridor affect can also be noted (signal is stronger relative to the
# distance where signals are not blocked by walls)
#-------------------------
#-------------------------

# We find 2 heat maps that have similar signals, corresponding to an access
# point at roughly x=7.5 & y=6.3. [i.e subMac[2] "00:0f:a3:39:dd:cd"
# and subMac[1] "00:0f:a3:39:e1:c0"]


# removing one of the observations (subMac[2]) out of the analysis; "00:0f:a3:39:dd:cd". H
# offlineSummaryTest = subset(offlineSummary, mac != subMacs[2])
macRemoved = 1
offlineSummaryMacRemoved = subset(offlineSummary, mac != subMacs[as.integer(macRemoved)])
#identical(offlineSummaryTest, offlineSummaryMacRemoved)  

# Create a matrix with the relative locations of th 6 access points
# using the heat maps to determine which MacID goes with a given location.
AP = matrix( c( 7.5, 6.3, 2.5, -.8, 12.8, -2.8,  
                1, 14, 33.5, 9.3,  33.5, 2.8),
             ncol = 2, byrow = TRUE,
             dimnames = list(subMacs[ -(as.integer(macRemoved)) ], c("x", "y") ))
# Dispaly of Access Pt location and ID
AP

# Look at signal strength vs distance from access pt. Compute distances
# from taget emitting signal to access point.
diffs = offlineSummaryMacRemoved[ , c("posX", "posY")] - 
  AP[ offlineSummaryMacRemoved$mac, ]


# Using Eclidean distances for distance between handheld and access pts
offlineSummaryMacRemoved$dist = sqrt(diffs[ , 1]^2 + diffs[ , 2]^2)

#-------------FIG 1.11 Scatter Plot Matrix signal strength vs distance
# for each angle (8) and access point (6) = 48. 
# Printed landscape for clarity
pdf(file=paste("Fig 1.11 Geo_ScatterSignalDist_MacRemoved_",macRemoved,".pdf"), width = 7, height = 10)
oldPar = par(mar = c(3.1, 3.1, 1, 1))
library(lattice)
xyplot(signal ~ dist | factor(mac) + factor(angle), 
       data = offlineSummaryMacRemoved, pch = 19, cex = 0.3,
       par.strip.text = list(cex = 0.7),
       xlab =paste("distance   Mac ID Removed ",subMacs[macRemoved]))
par(oldPar)
dev.off()


#----------------------------------------------------
#--------------
#--------------
#--------------  Working with Online dataset
#--------------
#--------------
#----------------------------------------------------

# using readData function to read in online data and parse the data 
# accordingly.  Only using SubMac ID identified from offline results
# Creating location ID posXY variable, same as offline
macs = unique(offlineSummaryMacRemoved$mac)
online = readData("Data/online.final.trace.txt", subMacs = macs)
# 34778 observations x 8 variables
online$posXY = paste(online$posX, online$posY, sep = "-")

length(unique(online$posXY)) # 60 unique test positions [Note: 60 posX; 53 posY]

# Create a table with # of online readings at each unique location and angle 
tabonlineXYA = table(online$posXY, online$angle)
tabonlineXYA[1:6, ]

# Rearranging the dataset: 
# Creating dataset with single entry for each unique position and orientatoin
# Each obseravation will keep: Position variables orientation / angle
# Have separate variables for each MacID
# and create an average Signal Strength variable from all observations 
# in online dataset (avg roughly ±110 observations each loc/angle/macID)
keepVars = c("posXY", "posX","posY", "orientation", "angle")
byLoc = with(online, 
             by(online, list(posXY), 
                function(x) {
                  ans = x[1, keepVars]
                  avgSS = tapply(x$signal, x$mac, mean)
                  y = matrix(avgSS, nrow = 1, ncol = 6,
                             dimnames = list(ans$posXY, names(avgSS)))
                  cbind(ans, y)
                }))

onlineSummary = do.call("rbind", byLoc)  

dim(onlineSummary) # Matrix OnlineSummary 60 X 11
names(onlineSummary)

#------------ Nearest Neighbor (1.5.2)
#----------------------------------------------

# Selecting the number of different angle observations that will be used
# during the NearestNeighbor calculations. Example:  Say we have an 
# observation at 120°.  We can choose to include only training dataset
# with 135° orientation[closest angle multiple] (m=1), or we can choose
# flanking angles 90°, 135° m=2, or for 3 we choose the closest angle
# and the flanking angles: 90°,135°,180° (m=3).
# The signal strength for each location is averaged across all included
# angles 

#------------------ ReShape Dataset Function-Signal Strength
# Similar to previous code that reshaped the Online dataset.
# but creating a function to do it.
# Rearranging the dataset: 
# Creating dataset with single entry for each unique position and orientatoin
# Each obseravation will keep: Position variables orientation / angle
# Have separate variables for each MacID
# and create an average Signal Strength variable from all observations
# added the option to select one angle at random for each location.
# Using Boolean operation to make selection

reshapeSS = function(data, varSignal = "signal", 
                     keepVars = c("posXY", "posX","posY"),
                     sampleAngle = FALSE, 
                     refs = seq(0, 315, by = 45)) {
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    if (sampleAngle) {
                      x = x[x$angle == sample(refs, size = 1), ]}
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 6,
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}
#------------


#------------------ Selection of Traning Dataset Function --
# Similar to previous code that selected the number of different
# angle observations that will be used for the training dataset
# but creating a function to do it
selectTrain = function(angleNewObs, signals = NULL, m = 1){
  # m is the number of angles to keep between 1 and 5
  refs = seq(0, by = 45, length  = 8)
  nearestAngle = roundOrientation(angleNewObs)
  
  if (m %% 2 == 1) 
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {
    m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angleNewObs - nearestAngle) > -1) 
      angles = angles[ -1 ]
    else 
      angles = angles[ -m ]
  }
  angles = angles + nearestAngle
  angles[angles < 0] = angles[ angles < 0 ] + 360
  angles[angles > 360] = angles[ angles > 360 ] - 360
  angles = sort(angles) 
  
  offlineSubset = signals[ signals$angle %in% angles, ]
  reshapeSS(offlineSubset, varSignal = "avgSignal")
}
#--------

        # Testing function with an observed angle of 130°; including
        # 3 (flanking) observation angles. Signal strengths are averaged
        # for the included angle observations. 
          #train130 = selectTrain(130, offlineSummaryMacRemoved, m = 3)
          #dim(train130) #166 x 9
          #head(train130)

#------------------ Nearest Neighbor Function --
# Finding the nearest neighbor for newly observed signal,
# given the training data subset (angles included in training dataset).
# Calculates distance and returns an ordered listing of distances from
# the training observations in order of closeness to newly observed signal

findNN = function(newSignal, trainSubset) {
  diffs = apply(trainSubset[ , 4:9], 1, 
                function(x) x - newSignal)
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) )
  closest = order(dists)
  return(trainSubset[closest, 1:3 ])
}
#----------

#------------------ KNN Prediction Function --
# Predicting the XY corrdinates for newly observed signals,
# given the subset of training dataset(# of angles, 
# and # of nearest neighbors to use.
predXY = function(newSignals, newAngles, trainData, 
                  numAngles = 1, k = 3,weighted = FALSE){
  
  closeXY = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] = 
      findNN(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }
  
  # Note: Using average distance for location estimation
  # Could use distances that are weighted by the inversely proportional
  # to the distance pg 35.
  estXY = lapply(closeXY, 
                 function(x) sapply(x[ , 2:3], 
                                    function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}
#-----
        # ---------- Test Code ----
        # Using training Dataset (offlineSummaryMacRemoved) to predict the
        # location of online dataset (onlineSummary)
        # Using 3 angles nearest and flanking above and below
        # and non-weighted distance, using 3 nearest neighbors
        estXYk3 = predXY(newSignals = onlineSummary[ , 6:11], 
                         newAngles = onlineSummary[ , 4], 
                         offlineSummaryMacRemoved, numAngles = 3, k = 3)
        
        # Same code as above with the exception of
        # using 1 nearest neighbors
        estXYk1 = predXY(newSignals = onlineSummary[ , 6:11], 
                         newAngles = onlineSummary[ , 4], 
                         offlineSummaryMacRemoved, numAngles = 3, k = 1)
        
        estXYk5 = predXY(newSignals = onlineSummary[ , 6:11], 
                         newAngles = onlineSummary[ , 4], 
                         offlineSummaryMacRemoved, numAngles = 3, k = 5)


#------------------ Floor Map Function --
# Creating a floor map with to compare actual online signal XY location
# and comparing it to the predicted XY location
floorErrorMap = function(estXY, actualXY, trainPoints = NULL, AP = NULL){
        
  plot(0, 0, xlim = c(0, 35), ylim = c(-3, 15), type = "n",
    xlab = "", ylab = "", axes = FALSE)
    box()
    # Drawing of Access Points
    if ( !is.null(AP) ) points(AP, pch = 15, col = 'DarkGreen')
      if ( !is.null(trainPoints) )
        # Drawing of 'training points' locations
        points(trainPoints, pch = 19, col="grey", cex = 0.6)
          
        #Drawing of Actual point location
        points(x = actualXY[, 1], y = actualXY[, 2], 
               pch = 19, cex = 0.8, col = 'blue' )
        #Drawing of Estimated point location
        points(x = estXY[, 1], y = estXY[, 2], 
               pch = 8, cex = 0.8 )
        #Drawing line linking the est and actual points
        segments(x0 = estXY[, 1], y0 = estXY[, 2],
               x1 = actualXY[, 1], y1 = actualXY[ , 2],
               lwd = 2, col = "red")
}
#---
        
# creating variable to locate all the training Points
trainPoints = offlineSummaryMacRemoved[ offlineSummaryMacRemoved$angle == 0 & 
                                          offlineSummaryMacRemoved$mac == "00:14:bf:3b:c7:c6" ,
                              c("posX", "posY")]
        
#-------------FIG 1.12A Geo Floor Map (actual vs predicted locations)
# Using KNN = 3, Number of traning dataset angles = 3 
pdf(file=paste("Fig 1.12A GEO_FloorPlan_K3_Errors_MacRemoved_",macRemoved,".pdf"), width = 10, height = 7)
oldPar = par(mar = c(1, 1, 1, 1))
      floorErrorMap(estXYk3, onlineSummary[ , c("posX","posY")], 
                    trainPoints = trainPoints, AP = AP)
par(oldPar)
dev.off()
        
#-------------FIG 1.12B Geo Floor Map (actual vs predicted locations)
# Using KNN = 1, Number of traning dataset angles = 3 
pdf(file=paste("Fig 1.12B GEO_FloorPlan_K1_Errors_MacRemoved_",macRemoved,".pdf"), width = 10, height = 7)
oldPar = par(mar = c(1, 1, 1, 1))
floorErrorMap(estXYk1, onlineSummary[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP)
par(oldPar)
dev.off()

#-------------FIG 1.12C Geo Floor Map (actual vs predicted locations)
# Using KNN = 5, Number of traning dataset angles = 3 
pdf(file=paste("Fig 1.12C GEO_FloorPlan_K5_Errors_MacRemoved_",macRemoved,".pdf"), width = 10, height = 7)
oldPar = par(mar = c(1, 1, 1, 1))
floorErrorMap(estXYk1, onlineSummary[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP)
par(oldPar)
dev.off()


#------------------ Sum Square Error Function --
# creating function to calculate the SS error for KNN prediction models
calcError = 
  function(estXY, actualXY) 
    sum( rowSums( (estXY - actualXY)^2) )
#-----------

# Calculating Sum of squared Error for model
actualXY = onlineSummary[ ,c('posX','posY')]
SSError <- sapply(list(estXYk1,estXYk3,estXYk5),calcError,actualXY)
# SS Error for K=1 659; SS Error for K=3 307, SS Error for K=5 276,


# --------------- Cross Validation ----------------
# -------------------------------------------------

# Number of folds for cross validation
v = 11
set.seed(123) # setting seed value, so that results are the same
# for multiple runs. If not, results would vary every time
# code is re-run

          # -------------- Test Code --------
          # Taking a random sample, without replacement, of the PosXY locations
          permuteLocs = sample(unique(offlineSummaryMacRemoved$posXY))
          # Creating a matrix with v columns and unique(posXY values) / V rows
          permuteLocs = matrix(permuteLocs, ncol = v, 
                               nrow = floor(length(permuteLocs)/v))
          
          # Created a subset of offlineSummary matrix according to the split
          # created in permueLocs.
          # Picked PermuteLocs[ ,1] arbitrarity for reference
          onlineFold = subset(offlineSummaryMacRemoved, posXY %in% permuteLocs[ , 1]) # approx 7900 observations
          #---

#list(unique(offline$mac))
# Confirming that subMac[2] "00:0f:a3:39:dd:cd" is removed from offline dataset
offlineMacRemoved = offline[ offline$mac != subMacs[as.integer(macRemoved)], ] #reduced to 769332 obs
          
keepVars = c("posXY", "posX","posY", "orientation", "angle")
          
# Create a new dataframe using random sample angles for observations,
# using the reshapeSS function: 166 observations x 11 variables
onlineCVSummary = reshapeSS(offlineMacRemoved, keepVars = keepVars, 
                            sampleAngle = TRUE)

          # ----- Test Code ---
          # Create onlineFold, using XY locaitons chose by permuteLocs.
          #  Chose the 1st fold for reference: 15 obs x 11 variables
          onlineFold = subset(onlineCVSummary, 
                              posXY %in% permuteLocs[ , 1])
          
          # Create offlineFold, using XY locaitons chose by permuteLocs.
          #  Chose the last fold for reference: 7200 obs x 15 variables
          offlineFold = subset(offlineSummaryMacRemoved,
                               posXY %in% permuteLocs[ , -1])
          
          # Using predXY function to predict xy locations, using
          # cross validation folds of datasets
          estFold = predXY(newSignals = onlineFold[ , 6:11], 
                           newAngles = onlineFold[ , 4], 
                           offlineFold, numAngles = 3, k = 3)
          
          # Calculating SS Error for the folded subset (permuteLocs[ , 1])
          # of the dataset
          actualFold = onlineFold[ , c("posX", "posY")]
          calcError(estFold, actualFold) #Calculated SS Error 133 (will vary)
          
v = 11 #repeated from earlier, for clarity: # of Folds        
K = 20 # Number of nearest neighbors
err = rep(0, K)
          
# loop through all the cross validation folds, using K values from
# 1 to 20, to find and calculate the SS Errors for each
# Same as before: using numAngles = 3.
# Attempting to find the 'optimal' KNN value for the given dataset
# and parameters
# Note:  Takes Several minutes to run this loop************
for (j in 1:v) {
  onlineFold = subset(onlineCVSummary, 
                      posXY %in% permuteLocs[ , j])
  offlineFold = subset(offlineSummaryMacRemoved,
                       posXY %in% permuteLocs[ , -j])
  actualFold = onlineFold[ , c("posX", "posY")]
            
  for (k in 1:K) {
    estFold = predXY(newSignals = onlineFold[ , 6:11],
                     newAngles = onlineFold[ , 4], 
                     offlineFold, numAngles = 3, k = k)
    err[k] = err[k] + calcError(estFold, actualFold)
  }
}
          
#-------------FIG 1.13 RMSE vs K neighbors-Line graph
# Number of traning dataset angles = 3 
pdf(file = paste("Fig 1.13-Test Geo_CVChoiceOfK_MacRemoved_",macRemoved,".pdf"), width = 10, height = 6)
oldPar = par(mar = c(4, 3, 1, 1))
plot(y = err, x = (1:K),  type = "l", lwd= 2,
     ylim = c((ymin=900), 2100),
     xlab = "Number of Neighbors",
     ylab = "Sum of Square Errors")

rmseMin = min(err)
kMin = which(err == rmseMin)[1]
segments(x0 = 0, x1 = kMin, y0 = rmseMin, col = gray(0.4), 
         lty = 2, lwd = 2)
segments(x0 = kMin, x1 = kMin, y0 = ymin-100,  y1 = rmseMin, 
         col = grey(0.4), lty = 2, lwd = 2)

mtext(kMin, side = 1, line = 1, at = kMin, col = grey(0.4))
text(x = kMin - 2, y = rmseMin + 40, 
     label = as.character(round(rmseMin)), col = grey(0.4))
par(oldPar)
dev.off()

#-------
val <- match(min(err),err)        
# Finding RMSE for optimal values of KNN (k=5)
# Optimal KNN might vary if seed value changes
estXYkBest = predXY(newSignals = onlineSummary[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummaryMacRemoved, numAngles = 3, k = val)

#calcError(estXYkBest, actualXY)# Error - 276 (may vary depending on K)
paste("Minimum Calc Error: ",calcError(estXYkBest, actualXY))
paste("K value: ",val)