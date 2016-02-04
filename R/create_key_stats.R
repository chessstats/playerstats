#' Create key stats
#'
#' Creates statistics for collected keys
#' @param none none
#' @keywords Fide players list statistics
#' @export
#' @examples
#' create_key_stats()

create_key_stats <- function() {
require(xtable)
require(plyr)
key_stats<-function(ckey) {
	path=paste("current/stats",ckey,sep="/")
	files<-sub("\\.txt$","",list.files(path=path,pattern="\\.txt$"))
	s<-data.frame(files)
	s<-rename(s, c("files"=ckey))
	
	all<-integer(length(files))
	numf<-integer(length(files))
	numm<-integer(length(files))
	parf<-integer(length(files))
	numt<-integer(length(files))
	numgm<-integer(length(files))
	numgm2600<-integer(length(files))
	numgm2650<-integer(length(files))
	numgm2700<-integer(length(files))
	numfgm<-integer(length(files))
	avgr<-integer(length(files))
	avgrf<-integer(length(files))
	avgrm<-integer(length(files))
	i<-1
	tnumt<-0
	for(file in files) {
		print(file)
		f<-read.table(paste(path,"/",file,".txt",sep=""),header=TRUE)
		all[i]=length(f[,1])
		numf[i]=sum(f$sex=="F",na.rm=TRUE)
		numm[i]=sum(f$sex=="M",na.rm=TRUE)
		numt[i]=sum(f$title!="",na.rm=TRUE)
		tnumt<-tnumt+numt[i]
		numgm[i]=sum(f$title=="GM",na.rm=TRUE)
		numfgm[i]=sum((f$title=="GM")&(f$sex=="F"),na.rm=TRUE)
		numgm2600[i]=sum((f$title=="GM")&(f$rating>=2600),na.rm=TRUE)
		numgm2650[i]=sum((f$title=="GM")&(f$rating>=2650),na.rm=TRUE)
		numgm2700[i]=sum((f$title=="GM")&(f$rating>=2700),na.rm=TRUE)
		avgr[i]=sum(f$rating)/all[i]		
		fm<-f[f$sex=="M",]
		ff<-f[f$sex=="F",]
		avgrm[i]=sum(fm$rating)/numm[i]
		avgrf[i]=sum(ff$rating)/numf[i]
		parf[i]=numf[i]/(numm[i]+numf[i])
		i<-i+1
	}
	s[["all"]]=all
	s[["numm"]]=numm
	s[["numf"]]=numf
	s[["parf"]]=parf
	s[["numt"]]=numt
	s[["numgm"]]=numgm
	s[["numfgm"]]=numfgm
	s[["avgr"]]=avgr
	s[["avgrm"]]=avgrm
	s[["avgrf"]]=avgrf
	s[["numgm2600"]]=numgm2600
	s[["numgm2650"]]=numgm2650
	s[["numgm2700"]]=numgm2700
	pathtxt=paste(path,".txt",sep="")
	pathhtml=paste(path,".html",sep="")
	write.table(s,pathtxt,row.names=FALSE)
	print(xtable(s,row.names=FALSE), type="html", file=pathhtml)	
}

key_stats("country")
key_stats("birthday")
key_stats("title")
key_stats("rcluster")
}