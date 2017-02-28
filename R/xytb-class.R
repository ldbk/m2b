#' xytb class definition 
#'
#' xytb is an trajectory object with observed behaviour
#'
#' @slot desc
#'
#' @slot xyt
#'
#' @slot b 
#'
#' @slot dxyt 
#'
#' @slot befdxyt 
#'
#' @slot model 
#'
#' @slot predb 
#'
#'
#'
#' @name xytb-class
#' @exportClass xytb
setClass(
	 Class='xytb',
	 slots=list(
		    desc='character',
		    xyt='data.frame',
		    b='data.frame',
		    dxyt='data.frame',
		    befdxyt='data.frame',
		    model='list',
		    predb='data.frame'
		    ),
	 prototype=list(
			desc='unknown track',
			xyt=data.frame(),
			b=data.frame(),
			dxyt=data.frame(),
			befdxyt=data.frame(),
		    	model=list(),
		    	predb=data.frame()
			)
	 )

#' xytb class constructor
#'
#' @name xytb
#' @export
setGeneric("xytb",
#	  #function(desc,xyt,b,dxyt,befdxyt,model,predb,...){
	  function(object,desc,winsize,idquant,move,...){
		standardGeneric("xytb")
	  } 
	  )

setMethod("xytb",
	  #signature("missing", "missing", "missing", "missing", 
	  #	    "missing","missing","missing"), 
	  signature(object="missing",desc="missing"),
	  #function(desc="null xybt object",xyt,b,dxyt,befdxyt,model,predb){
	  function(desc="null xybt object"){
		  return(new("xytb"))
	  }
	  )

setMethod("xytb",
	  signature(object="data.frame",desc="character"),
	  function(object,desc="unknow track",...){
		  #check trajectory and behavioural info
		  if(!all(names(object)%in%c("id","x","y","t","b"))){
			  stop("wrong variables in the xyt data.frame: 
			       id, x, y and/or t is missing")
		  }
		  if(!is.numeric(object$x)){
			  stop("x is not numeric")
		  }
		  if(!is.numeric(object$y)){
			  stop("y is not numeric")
		  }
		  if(!is.character(object$b)){
			  stop("b is not character")
		  }
		  if(!is.character(object$id)){
			  stop("id is not character")
		  }
		  if(!any(class(object$t)%in%"POSIXct")){
			  stop("t is not POSIXct")
		  }
		  #order data according to id and time
		  object<-arrange(arrange(object,t),id)
		  xyt0<-data.frame(id=object$id,t=object$t,x=object$x,y=object$y)
		  b0<-data.frame(id=object$id,t=object$t,b=object$b)
		  dxyt0<-dxyt(xyt0)
		  new("xytb",xyt=dxyt0,b=b0,desc=desc)
	  }
	  )

setMethod("xytb",
	  signature(object="data.frame",desc="character",winsize="vector",idquant="vector"),
	  function(object,desc="unknow track",winsize=seq(3,13,2),idquant=seq(0,1,.25),...){
		  #check trajectory and behavioural info
		  if(!all(names(object)%in%c("id","x","y","t","b"))){
			  stop("wrong variables in the xyt data.frame: 
			       id, x, y or/and t is missing")
		  }
		  if(!is.numeric(object$x)){
			  stop("x is not numeric")
		  }
		  if(!is.numeric(object$y)){
			  stop("y is not numeric")
		  }
		  if(!is.character(object$b)){
			  stop("b is not character")
		  }
		  if(!is.character(object$b)){
			  stop("id is not character")
		  }
		  if(!any(class(object$t)%in%"POSIXct")){
			  stop("t is not POSIXct")
		  }
		  if(min(winsize,na.rm=T)<=2 | 
		     any(is.na(winsize))|
		     any(winsize%%1!=0)|
		     any(winsize%%2L==0L)
			){
			  stop("invalid values in winsize (has to be >2, integer and odd)")
		  }
		  if(min(idquant,na.rm=T)<0 | 
		     max(idquant,na.rm=T)>1 |
		     any(is.na(idquant))
			){
			  stop("invalid values in idquant (values in [0,1])")
		  }
		  #order data according to id and time
		  object<-arrange(arrange(object,t),id)
		  xyt0<-data.frame(id=object$id,t=object$t,x=object$x,y=object$y)
		  b0<-data.frame(id=object$id,t=object$t,b=object$b)
		  dxyt0<-dxyt(xyt0)
		  dxyt1<-dxyt2(dxyt0,winsize,idquant)
		  new("xytb",xyt=dxyt0,b=b0,desc=desc,dxyt=dxyt1)
	  }
	  )

setMethod("xytb",
	  signature(object="data.frame",desc="character",winsize="vector",idquant="vector",move="vector"),
	  function(object,desc="unknow track",winsize=seq(3,13,2),idquant=seq(0,1,.25),move=c(5,10),...){
		  #check trajectory and behavioural info
		  if(!all(names(object)%in%c("id","x","y","t","b"))){
			  stop("wrong variables in the xyt data.frame: 
			       id, x, y or/and t is missing")
		  }
		  if(!is.numeric(object$x)){
			  stop("x is not numeric")
		  }
		  if(!is.numeric(object$y)){
			  stop("y is not numeric")
		  }
		  if(!is.character(object$b)){
			  stop("b is not character")
		  }
		  if(!is.character(object$id)){
			  stop("id is not character")
		  }
		  if(!any(class(object$t)%in%"POSIXct")){
			  stop("t is not POSIXct")
		  }
		  if(min(winsize,na.rm=T)<=2 | 
		     any(is.na(winsize))|
		     any(winsize%%1!=0)|
		     any(winsize%%2L==0L)
			){
			  stop("invalid values in winsize (has to be >2, integer and odd)")
		  }
		  if(min(idquant,na.rm=T)<0 | 
		     max(idquant,na.rm=T)>1 |
		     any(is.na(idquant))
			){
			  stop("invalid values in idquant (values in [0,1])")
		  }
		  if(min(move,na.rm=T)<=0 | 
		     any(move%%1!=0)|
		     max(move)>nrow(object)|
		     any(is.na(move))
			){
			  stop("invalid values in move (has to be >0, integer and < nb of track points)")
		  }
		  #order data according to id and time
		  object<-arrange(arrange(object,t),id)
		  xyt0<-data.frame(id=object$id,t=object$t,x=object$x,y=object$y)
		  b0<-data.frame(id=object$id,t=object$t,b=object$b)
		  dxyt0<-dxyt(xyt0)
		  dxyt1<-dxyt2(dxyt0,winsize,idquant)
		  dxyt2<-shiftvalue(dxyt1,move)
		  new("xytb",xyt=dxyt0,b=b0,desc=desc,dxyt=dxyt1,befdxyt=dxyt2)
	  }
	  )

#' internal function
#'
#' @name dxyt
#' @export
dxyt<-function(xyt){
  xy<-xyt
  xy$dt<-as.numeric(c(difftime(xy$t[-1],xy$t[-nrow(xy)],units="s"),"NA"))
  idx<-which(names(xy)=="x")
  idy<-which(names(xy)=="y")
  xy$dist<- c(distVincentyEllipsoid(as.matrix(xy[1:(nrow(xy)-1),c(idx,idy)]),as.matrix(xy[2:nrow(xy),c(idx,idy)])),NA)
  xy$v<-xy$dist/xy$dt
  xy$dx<-c(diff(xy$x),NA)
  xy$dy<-c(diff(xy$y),NA)
  xy$theta<-atan2(xy$dy,xy$dx) 
  #relative angle for theta
  xy$thetarel[2:nrow(xy)]<-diff(xy$theta)
  xy$thetarel <- ifelse(xy$thetarel <= (-pi), 2 * pi + xy$thetarel, xy$thetarel)
  xy$thetarel <- ifelse(xy$thetarel > pi, xy$thetarel - 2 * pi, xy$thetarel)
  return(xy)
}

#' internal function
#'
#' @name dxyt2
#' @export
dxyt2<-function(dxyt,winsize=seq(3,13,2),idquant=seq(0,1,.25)){
 #init size window (odd number please) and quantile range
  nbw<-length(winsize)
  #dataframe variable names generation
  vnomquant<-expand.grid(paste("vquant",winsize,sep="_w"),idquant)
  thetanomquant<-expand.grid(paste("thetarelquant",winsize,sep="_w"),idquant)
  distnomquant<-expand.grid(paste("distquant",winsize,sep="_w"),idquant)
  vnomquant<-paste(vnomquant[,1],vnomquant[,2],sep="_")
  thetanomquant<-paste(thetanomquant[,1],thetanomquant[,2],sep="_")
  distnomquant<-paste(distnomquant[,1],distnomquant[,2],sep="_")
  nomparam<-c(names(dxyt),
  	apply(
		expand.grid(c("v","thetarel","dist"),
			    c("mean","sd","mad"),
			    paste0("_w",winsize),
		stringsAsFactors=F),1,paste0,collapse=""),
	vnomquant,thetanomquant,distnomquant)
  dat0<-data.frame(matrix(NA,ncol=length(nomparam),nrow=nrow(dxyt)))
  names(dat0)<-nomparam
  #calculate variables
  dat0[,1:ncol(dxyt)]<-dxyt
  ######################################################
  #absolute value of thetarel (bug correction 18/12/2014...)
  ######################################################
  dat0$theta<-abs(dxyt$thetarel)
  #####################################################
  dat0$v[is.na(dat0$v)]<-0
  dat0$theta[is.na(dat0$theta)]<-0
  dat0$dist[is.na(dat0$dist)]<-0
  print(paste("Compute",ncol(dat0)-10,"indicators on",length(winsize),"moving windows"))
  for(i in winsize){
    print(paste("Compute indicators on",i,"points"))
    #runmean
    idcol<-which(paste("vmean_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmean(dat0$v,i)
    idcol<-which(paste("thetarelmean_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmean(dat0$theta,i)
    idcol<-which(paste("distmean_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmean(dat0$dist,i)
    #runsd
    idcol<-which(paste("vsd_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runsd(dat0$v,i)
    idcol<-which(paste("thetarelsd_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runsd(dat0$theta,i)
    idcol<-which(paste("distsd_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runsd(dat0$dist,i)
    #runmad
    idcol<-which(paste("vmad_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmad(dat0$v,i)
    idcol<-which(paste("thetarelmad_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmad(dat0$theta,i)
    idcol<-which(paste("distmad_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmad(dat0$dist,i)
    #runquantile
    idcol<-grep(paste("vquant_w",i,sep=""),names(dat0))
    dat0[,idcol]<-caTools::runquantile(dat0$v,i,idquant)
    idcol<-grep(paste("thetarelquant_w",i,sep=""),names(dat0))
    dat0[,idcol]<-caTools::runquantile(dat0$theta,i,idquant)
    idcol<-grep(paste("distquant_w",i,sep=""),names(dat0))
    dat0[,idcol]<-caTools::runquantile(dat0$dist,i,idquant)
    print(paste("Done"))
  }
  dat0<-dat0[,names(dat0)%in%c("v","dist","thetarel") | !names(dat0)%in%names(dxyt)]
 return(dat0)
}
			 
#' internal function
#'
#' @name shiftvalue 
#' @export
shiftvalue<-function(dat0,mov=seq(5,250,5)){
  print("shift value backward")
  #nbcoldat<-length(8:ncol(dat0))
  nbcoldat<-ncol(dat0)
  datadd<-data.frame(matrix(NA,ncol=nbcoldat*length(mov),nrow=nrow(dat0)))
  #varn<-expand.grid(colnames(dat0)[c(-1:-7)],mov)
  varn<-expand.grid(colnames(dat0),mov)
  varn<-paste(varn[,1],sprintf("%02d",varn[,2]),sep="_mov_")
  names(datadd)<-varn
  for (i in 1:length(mov)){
    print(paste("shift backward",mov[i]))
    idcol<-grep(paste("_mov_",sprintf("%02d",mov[i]),sep=""),names(datadd))
    datadd[(mov[i]+1):nrow(datadd),idcol]<-dat0[1:(nrow(datadd)-mov[i]),1:ncol(dat0)]
  }
  print("Done")
  datadd<-cbind(dat0,datadd)
  datadd<-datadd[,!names(datadd)%in%names(dat0)]
  return(datadd)
}

#' xytb plot function
#'
#' @name plot 
#' @export
setMethod("plot",
	  signature(x="xytb"),
	  function(x){
		  pipo<-data.frame(id=x@xyt$id,x=x@xyt$x,y=x@xyt$y,b=x@b$b)	
		  ggplot(pipo,aes(x=x,y=y,color=b,group=id))+geom_path()+facet_wrap(~id)+ggtitle(x@desc)

	  }
	  )
		
#' xytb randomForest function
#'
#' Build a random forest model on an xytb object, predicting behaviour using
#' regular observation (type actual) or shifted one (type shifted). Parameters
#' are passed to the randomForest function if needed.
#'
#' @author Laurent Dubroca and Andréa Thiébault
#'
#' @param xytb: an xytb object
#' @param type: character -actual or shifted- use actual data or shifted one to
#' build the model
#' @param nob: character. Define the unobserved value of the behaviour (and
#' where prediction are done)
#' @param colin: boolean - remove colinearity among predictors (see the caret
#' package for more details)
#' @param varkeep: character vector - the variables names in this vector are
#' keeped in the model even if colinearity is found (usefull to keep 'classical'
#' parameters and to help interpretation)
#' @param zerovar: boolean - remove near zero variance predictor (see the caret
#' package for more details)
#' @param ntree: number of trees in the random Forest (see the randomForest
#' package for more details)
#' @param importance: boolean (see the randomForest package for more details)
#'
#' @name modelRF 
#' @export
modelRF<-function(xytb,type=c("actual","shifted"),nob="-1",
		  colin=T,varkeep=c("v","dist","thetarel"),
		  zerovar=T,ntree=501,importance=T,...){
	if(class(xytb)!="xytb"){
	 stop("invalid xytb object")
	}
	if(!type%in%c("actual","shifted") | length(type)!=1){
	 stop("invalid type")
	}
	if(type=="actual"){ dat0<-cbind(xytb@b,xytb@dxyt) }
	if(type=="shifted"){ dat0<-cbind(xytb@b,xytb@befdxyt) }
	if(!nob%in%dat0$b){
		stop("wrong no behavioural observation code")
	}else{
		idna<-as.numeric(attr(na.omit(dat0),"na.action"))
		dat0$b[dat0$b==nob]<-NA
		initdat0<-dat0
	}
	#remove NA value
	print("removing lines with NA values")
	dat0<-na.omit(dat0)
	b0<-dat0$b
	dat0<-dat0[,-1:-3]
	#remove colinearity
	if(colin){
		print("removing colinearity among predictors")
		idcolin<-caret::findCorrelation(cor(dat0),.9,verbose=F)
		if(length(idcolin)>0){
			print(paste(paste(varkeep,collapse=","),"keeped",collapse=" "))
			idcolin<-idcolin[which(!names(dat0)[idcolin]%in%varkeep)]
			if(length(idcolin)>0){
				print(paste(paste(names(dat0)[idcolin],collapse=","),
					    "removed",collapse=" "))
				dat0<-dat0[,-idcolin]
			}else{
				print("no colinearity found")
			}
		}
	}
	#remove zerovar 
	if(zerovar){
		print("removing near zero variance predictors")
		idzerovar<-caret::nearZeroVar(dat0)
		if(length(idzerovar)>0){
			print(paste(paste(varkeep,collapse=","),"keeped",collapse=" "))
			idzerovar<-idzerovar[which(!names(dat0)[idzerovar]%in%varkeep)]
			if(length(idzerovar)>0){
				print(paste(paste(names(dat0)[idzerovar],collapse=","),
					    "removed",collapse=" "))
				dat0<-dat0[,-idcolin]
			}else{
				print("no zero variance predictors found")
			}
		}
	}

	#build and compute model
	tabb<-table(b0)
	rfFit<-randomForest(dat0,as.factor(b0),ntree=ntree,sampsize=rep(min(tabb),length(tabb)),
		     importance=importance,...)
	rfFit<-randomForest(dat0,as.factor(b0),ntree=101,sampsize=rep(min(tabb),length(tabb)),
		     importance=T)
	#prediction
	initdat0[is.na(initdat0)]<-0
	newb<-as.character(predict(rfFit,new=initdat0))
	newb[idna]<-"no data"
	class(rfFit)<-c("list")
	xytb@model<-rfFit
	xytb@predb<-data.frame(id=initdat0$id,t=initdat0$t,b=newb)
	return(xytb)
	



}


run<-function(){
library(m2b)
library(dplyr)
dxyt<-track_CAGA_005%>%select(x,y,t,b)%>%mutate(b=as.character(b),id="pipo")
xytb<-xytb(dxyt,"pipo",3,.5)

dxyt<-dxyt(dxyt)
dxyt<-dxyt2(dxyt,3,.4)
pipo<-shiftvalue(dxyt,mov=c(5,10))
x<-pipo
}
