# Script to analyze if two userids have met in a building.
# 
# Instructions:
# Save this file in the same directory as the datafile 'reduced.csv'.
# Ensure xts and quantmod packages are installed. 
# If not, install them with:  install.packages(c('readr', 'xts', 'quantmod'))
#
# Run from R/RStudio: 
# source("analyzeMeetings.R")
# findMeetings("3c3649fb", "5e7b40e1", data)
# findMeetings("3c3649fb", "5e7b40e1", data, plotDistance=TRUE)
#
# Run from command-line:
# Rscript analyzeMeetings.R 3c3649fb 5e7b40e1
#
# CHANGELOG:
# V2: 
# -Changed to using package readr and its function read_csv for reading in the file. Speed improvement was from 20s to ca.1s
# -Added check for uid's as arguments and subsetting the file to avoid reading the whole 135MB file, unless script is run without arguments from R/RStudio environment
# -Added print of total number of timestamps satisying criteria as a probability measure of meetings.
# 


suppressPackageStartupMessages(require(readr))
suppressPackageStartupMessages(require(zoo))
suppressPackageStartupMessages(require(xts))
suppressPackageStartupMessages(require(quantmod)) # We use quantmod package for optional plotting of the timeseries




# Function to find potential meetings for 2 user ids given as argument
findMeetings <- function(uid1, uid2, data, plotDistance=FALSE, returnMet=FALSE) {
  maxTimeBetweenObservations <- 120 # In seconds, the time for the other persons last timestamp to still be counted as reliable
  maxDistance <- 2 # In metres, the max distance to be counted as a meeting
  
  if(is.na(uid1) || is.na(uid2) || nrow(data[data$uid == uid2,]) < 1 || nrow(data[data$uid == uid1,]) < 1) {
    cat("Data for at least one of the given users was not found in the data-set. \n")
    
  } else {
    
    #subsetting the data to only contain the data concerning our two users in question and adding columns for later use.
    df <- data[(data$uid == uid2 | data$uid == uid1),]
    
    df$floorOtherLastSeen <- NA
    df$xOtherLastSeen <- NA
    df$yOtherLastSeen <- NA
    df$timeOtherLastSeen <- NA
    
    # Forming an  xts of the data to be sure it is in chronological order.
    xdf <- xts(df, order.by=as.POSIXct(strptime(df$timestamp, format="%Y-%m-%dT%H:%M:%OS")))
    
    # Adding a column for the other uid so we know where the data switches to the other user.
    xdf$uidPrev <- lag(xdf$uid) #, k=-1)
    
    # Adding data-points for the last known position and last sighted timestamp for the other user to every row.
    xdf[xdf$uidPrev != xdf$uid,]$xOtherLastSeen <-   xdf[lag(xdf$uidPrev != xdf$uid, k=-1),]$x
    xdf[xdf$uidPrev != xdf$uid,]$yOtherLastSeen <-   xdf[lag(xdf$uidPrev != xdf$uid, k=-1),]$y
    xdf[xdf$uidPrev != xdf$uid,]$floorOtherLastSeen <-  xdf[lag(xdf$uidPrev != xdf$uid, k=-1),]$floor
    xdf[xdf$uidPrev != xdf$uid,]$timeOtherLastSeen <-   xdf[lag(xdf$uidPrev != xdf$uid, k=-1),]$timestamp
    # Filling out from the previously known location and time.
    xdf <- na.locf(as.data.frame(xdf))
    
    # Subsetting to when the users have been seen on the same floor
    xdf <- xdf[xdf$floor == xdf$floorOtherLastSeen,]
    
    
    # Modifying timestamps to POSIXct format and calculating the time since the counterpartys position was last known.
    xdf$timeOtherLastSeen <- as.POSIXct(strptime(xdf$timeOtherLastSeen, format="%Y-%m-%dT%H:%M:%OS"))
    xdf$timestamp <- as.POSIXct(strptime(xdf$timestamp, format="%Y-%m-%dT%H:%M:%OS"))
    xdf$timeSinceLastSeenOther <- difftime(xdf$timestamp, xdf$timeOtherLastSeen, units="secs")
    
    # Removing potential NA rows, eg. first row.
    xdf <- na.exclude(xdf)
    
    # Calculating the distance between a user and the last known position for the counterpart
    xdf$distance <- sqrt( (as.numeric(xdf$x)-as.numeric(xdf$xOtherLastSeen))^2+ (as.numeric(xdf$y)-as.numeric(xdf$yOtherLastSeen))^2 )
    
    
    # Subsetting for max time between the users observations.
    xdf <- xdf[xdf$timeSinceLastSeenOther < maxTimeBetweenObservations,]
    
    # Plot the distance as a function of time if desired
    if(plotDistance) {
      plot(as.xts(xdf$distance, order.by=xdf$timestamp), type="p", main="Distance in meters between the users")
      chartSeries(as.xts(xdf$distance, order.by=xdf$timestamp), type="p", name="Distance in meters between the users")
    }
    
    # Subsetting for max distance
    xdf <- xdf[xdf$distance < maxDistance,]
    
    # If no rows of the data conform to our criteria
    if(nrow(xdf) < 1) {
      cat("Based on our data the two users have likely not met. \n")
      if(returnMet) {
        return(FALSE)
      }
    }
    # If rows satisfy the meeting criteria we choose the first time of the meeting.
    if(nrow(xdf) > 0) {
      meeting <- xdf[1,]
      cat(" Based on the data the two users can have met the first time at", as.character(meeting$timestamp), "\n",
      "Location: Floor: ", meeting$floor, " X:", meeting$x, " Y:", meeting$y, " \n",
      "Distance between the users: ", round(meeting$distance, digits=3), " Seconds between sightings:", round(meeting$timeSinceLastSeenOther, digits=3), "\n",
      "In total there are", nrow(xdf), "timestamps with the users close to each other. \n"
      )
      if(returnMet) {
        return(TRUE)
      }
    }
  } # End found data for both users.
    
} # End function



args <- commandArgs(TRUE)

#system.time( data <- read.table("reduced.csv", header=T, stringsAsFactors=F, strip.white = T, sep=",") )
#system.time(data <- read_table("reduced_mod.csv") )#, header=T, stringsAsFactors=F, strip.white = T, sep=",")

if(length(args) == 0) {
  data <- as.data.frame(read_csv("reduced.csv", col_types="cdddc"))
}

# Can use a RData file as storage to speed up further, if using this comment the csv reading lines above.
# save(data, file="reduced.RData")
# load(file="reduced.RData")

if(length(args) > 0) {
  uid1 <- args[1]
  uid2 <- args[2]
  system("echo 'timestamp,x,y,floor,uid' > userdata.csv")
  cmdstring <- paste("grep", uid1, "reduced.csv >> userdata.csv")
  system(cmdstring)
  cmdstring <- paste("grep", uid2, "reduced.csv >> userdata.csv")
  system(cmdstring)
  data <- as.data.frame(read_csv("userdata.csv", col_types="cdddc"))
  findMeetings(uid1, uid2, data)  
}



if(F) {
  # This section contains some code-snippets
  
  #UID's for testing and as examples
  uid1 <- "3c3649fb"
  uid2 <- "5e7b40e1"
  system.time(findMeetings(uid1, uid2, data))

  findMeetings(uid1, uid2, data, plotDistance=TRUE)
  
  uid1 <- "3c3649fb"
  uid2 <- "1"
  findMeetings(uid1, uid2, data)
  
  uid1 <- "3c3649fb"
  uid2 <- NA
  findMeetings(uid1, uid2, data)
  
  # Code for building the precalculated file for fast access
  # Generating this takes time, didnt parallelize it for this excerice.
  # This proved too slow, end the end didnt proceed down this path.
  users <- unique(data$uid)
  meetings <- c()
  for(i in 1:length(users)) {
  cat("i: ", i)
  uid1 <- users[i]
  #sapply(users[-i], findMeetings, uid1, data)
   for(j in 1:length(users)) {
    uid2 <- users[j]
    if(uid1 != uid2) {
      meetings <- rbind(meetings, c(uid1, uid2, findMeetings(uid1, uid2, data, returnMet=TRUE)))
    }
   }
  }
  
  
  
  #Code for plotting the movements on the floors within the building
  #
  library(lattice)
  unique(data$floor) # 3 floors
  d <- data[data$floor==1,]
  #with(d, xyplot(y ~ x, group=uid))
  with(d, plot(y ~ x))
  
  d <- data[data$floor==2,]
  d <- data[data$floor==3,]
  
}  
