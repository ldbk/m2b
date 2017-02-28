#' 'Vincenty' (ellipsoid) great circle distance 
#'
#' The shortest distance between two points (i.e., the 'great-circle-distance'
#' or 'as the crow flies'), according to the 'Vincenty (ellipsoid)' method. 
#' This method uses an ellipsoid and the results are very accurate. 
#' The method is computationally more intensive than the other great-circled methods in this package.
#'
#' @section Source:
#' Function borrowed to the 'geosphere' package to attenuate packages' dependency.
#' See this package and the function help for more details.
#' @author Chris Veness and Robert Hijmans
#' @param p1 longitude/latitude of point(s), in degrees 1; can be a vector of
#' two numbers, a matrix of 2 columns (first one is longitude, second is
#' latitude) or a SpatialPoints* object
#' @param p2 as above
#' @param a Equatorial axis of ellipsoid
#' @param b Polar axis of ellipsoid
#' @param f Inverse flattening of ellipsoid
#' @return Distance value in the same units as the ellipsoid (default is meters)
#'
#' @name distVincentyEllipsoid
distVincentyEllipsoid <- function(p1, p2, a=6378137, b=6356752.3142, f=1/298.257223563) {
#/*  Vincenty Inverse Solution of Geodesics on the Ellipsoid (c) Chris Veness 2002-2009           #*/
#* Calculate geodesic distance (in m) between two points specified by latitude/longitude 
#* (in numeric degrees) using Vincenty inverse formula for ellipsoids
# source http://www.movable-type.co.uk/scripts/latlong-vincenty.html
# (c) 2002-2009 Chris Veness

	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
	
	p = cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(a), as.vector(b), as.vector(f))
	p1 = p[,1:2,drop=FALSE] 
	p2 = p[,3:4,drop=FALSE] 
	  
	res <- vector(length=nrow(p1))
    for (i in 1:dim(p1)[1]) {

		if ( any( is.na( c(p1[i,], p2[i,])))) {  #improvement by George Wang and Sebastian P. Luque
			res[i] <- NA
		} else if (isTRUE(all.equal(p1[i,], p2[i,]))) {
			res[i] <- 0
		} else {
			lon1 <- p1[i,1]
			lat1 <- p1[i,2]
			lon2 <- p2[i,1]
			lat2 <- p2[i,2]
			a = p[i,5]
			b = p[i,6]
			f = p[i,7]
		
			L <- (lon2-lon1)
			U1 <- atan((1-f) * tan(lat1))
			U2 <- atan((1-f) * tan(lat2))
			sinU1 <- sin(U1)
			cosU1 <- cos(U1)
			sinU2 <- sin(U2)
			cosU2 <- cos(U2)
			lambda <- L
			iterLimit <- 100
			continue <- TRUE
			while (continue) {
				sinLambda <- sin(lambda)
				cosLambda <- cos(lambda)
				sinSigma <- sqrt((cosU2*sinLambda) * (cosU2*sinLambda) + (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda))
				
				cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
				sigma <- atan2(sinSigma, cosSigma)
				sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
				cosSqAlpha <- 1 - sinAlpha*sinAlpha
				cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
				
				if (is.nan(cos2SigmaM)) cos2SigmaM <- 0  # equatorial line: cosSqAlpha=0 (par. 6)
				
				C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
				lambdaP <- lambda
				lambda <- L + (1-C) * f * sinAlpha * (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
				iterLimit <- iterLimit - 1
				continue <- (abs(lambda-lambdaP) > 1e-12 && iterLimit > 0)
			} 
			if (iterLimit==0) {
				res[i]  <- NA  # failed to converge
			} else {
				uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
				A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
				B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
				deltaSigma <- B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)- B/6*cos2SigmaM*(-3+4*sinSigma*sinSigma)*(-3+4*cos2SigmaM*cos2SigmaM)))
				res[i]  <- b*A*(sigma-deltaSigma)
			}
		}
	}
  
	return(as.vector(res))
}

.pointsToMatrix <- function(p, checkLonLat=TRUE, poly=FALSE) {
	if (inherits(p, 'SpatialPoints')) {
		test <- !is.projected(p)
		if (! isTRUE (test) ) {
			if (is.na(test)) {
				warning('Coordinate reference system of SpatialPoints object is not set. Assuming it is degrees (longitude/latitude)!')  			
			} else {
				stop('Points are projected. They should be in degrees (longitude/latitude)')  
			}
			# or rather transform them ....?
		}
		p <- coordinates(p)
	} else if (is.data.frame(p)) {
		p <- as.matrix(p)
	} else 
	
	if (is.vector(p)){
		if (length(p) != 2) {
			stop('Wrong length for a vector, should be 2')
		} else {
			p <- matrix(p, ncol=2) 
		}
	} else if (is.matrix(p)) {
		if (length(p[1,]) != 2) {
			stop( 'A points matrix should have 2 columns')
		}
		cn <- colnames(p)
		if (length(cn) == 2) {
			if (toupper(cn[1]) == 'Y' | toupper(cn[2]) == 'X')  {
				warning('Suspect column names (x and y reversed?)')
			}
			if (toupper(substr(cn[1],1,3) == 'LAT' | toupper(substr(cn[2],1,3)) == 'LON'))  {
				warning('Suspect column names (longitude and latitude reversed?)')
			}
		}		
	} else {
		stop('points should be vectors of length 2, matrices with 2 columns, or inheriting from a SpatialPoints* object')
	}

	if (! is.numeric(p) ) { p[] <- as.numeric(p) }
	
	if (checkLonLat) {
		if (length(stats::na.omit(p[,1])) > 0) {
			if (min(p[,1], na.rm=TRUE) < -360) { stop('longitude < -360') }
			if (max(p[,1], na.rm=TRUE) > 360) {  stop('longitude > 360')  }
			if (min(p[,1], na.rm=TRUE) < -180) { warning('longitude < -180') }
			if (max(p[,1], na.rm=TRUE) > 180) {  warning('longitude > 180')  }
		}
		if (length(stats::na.omit(p[,2])) > 0) {
			if (min(p[,2], na.rm=TRUE) < -90) {  stop('latitude < -90')  }
			if (max(p[,2], na.rm=TRUE) > 90) {  stop('latitude > 90')  }
		}
	}
	
	
	if (poly) {
		if (! isTRUE(all.equal(p[1,], p[nrow(p),]))) {
			p <- rbind(p, p[1,])
		} 

		i <- p[-nrow(p),1] == p[-1,1] &  p[-nrow(p),2] == p[-1,2]
		i <- which(isTRUE(i))
		if (length(i) > 0) {
			p <- p[-i, ,drop=FALSE]
		}
	
		.isPolygon(p)
	}
	
	return(p)
}

