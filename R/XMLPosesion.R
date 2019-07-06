#' Esta funcion parsea el xml de posesion para la parte territorial cada 15'
#'
#' @param x Un string con la ruta del fichero
#' @return un df de \code{x}
#' @examples
#' OptaXMLposesion(path)
#' @export
#'
#'
#'
OptaXMLposesion <- function(x,...){
  pbpParse <- xmlInternalTreeParse(x)
  gameInfo <- grabAll(pbpParse, "Possession")
  # En eventInfo guardamos la informacion de cada evento
  time<-c('1-15','16-30','31-45','46-60','61-75','76-90')
  time1<-as.data.frame(unlist(time))
  names(time1)<-c('time')
  away <- cbind(
    "names" = lapply(getNodeSet(pbpParse, "//PossessionWave/Intervals/IntervalLength/Interval/Away"), function(x) xmlValue(x))
  )

  away1<-as.data.frame(unlist(away))
  away2<-as.data.frame(away1[71:76,])
  names(away2)<-c('away')


  home <- cbind(
    "names" = lapply(getNodeSet(pbpParse, "//PossessionWave/Intervals/IntervalLength/Interval/Home"), function(x) xmlValue(x))
  )

  home1<-as.data.frame(unlist(home))
  home2<-as.data.frame(home1[71:76,])
  names(home2)<-c('home')

  middle <- cbind(
    "names" = lapply(getNodeSet(pbpParse, "//PossessionWave/Intervals/IntervalLength/Interval/Middle"), function(x) xmlValue(x))
  )

  middle1<-as.data.frame(unlist(middle))
  middle2<-as.data.frame(middle1[19:24,])
  names(middle2)<-c('middle')

    EventsExpanded <- as.data.frame(lapply(gameInfo[,1:8], function(x) rep(x, 6)), stringsAsFactors=F)
  # unimos esta informaciC3n con la de Qinfo
  QInfo <- cbind(EventsExpanded,time1,home2,middle2,away2)
  QInfo2<- reshape::melt(QInfo,id=c('away_team_id','away_team_name','competition_id','competition_name','date','game_id','home_team_id','home_team_name','time'))

}

#utility function
grabAll <- function(XML.parsed, field){
  parse.field <- xpathSApply(XML.parsed, paste("//", field, "[@*]", sep=""))

  results <- t(sapply(parse.field, function(x) xmlAttrs(x)))
  if(typeof(results)=="list"){
    do.call(plyr::rbind.fill, lapply(lapply(results, t), data.frame, stringsAsFactors=F))
  } else {
    as.data.frame(results, stringsAsFactors=F)
  }
}




