#' A cape gannet track with behavioural information
#'
#' A data frame of 5 variables reporting a gannet track with
#' information regarding its behaviour.
#' Observation rates : 5s (GPS data), 24 frame.s-1 (video).
#' Tracking points attributed with code numbers (described below) based on 
#' the observations in a four seconds interval time (2s before and 2s after).
#' Video data recorded using a micro camera deployed on the seabird concomitantly to the GPS logger. 
#' Behaviours of the equipped bird observed: taking off, landing, diving. -> inference of 3 phases = flying VS sitting on water VS diving
#' Camera devices with a short autonomy (few hours) -> observations on the first
#' few hours of the track (then no behaviour data = -1). Device: GPS I-gotU
#' (GT-600).Species: Cape Gannet breeding on Bird Island, Nelson Mandela Bay, South Africa
#'
#' @format Tracking data
#'
#' Columns:
#'
#' x = longitude in decimal degrees
#'
#' y = latitude in decimal degrees
#'
#' t = time in POSIXct
#'
#' b = behaviour observed on video data (3:flying 100\%, 2:sitting on water 100\%,
#' 1:diving 100\%,-1:no data)
#'
#' @source Andréa Thiebault
#' 
"track_CAGA_005"
