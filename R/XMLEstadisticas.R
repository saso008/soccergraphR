#' Esta funcion parsea el xml de estadisticas
#'
#' @param evento Un string con la ruta del fichero
#' @return un df de \code{evento}
#' @examples
#' OptaXMLstats(path)
#' @export
#'
#'
#'

OptaXMLstats <- function(evento,...){

pbpParse <- xmlInternalTreeParse(evento)
gameInfo <- grabAll(pbpParse, "Team")
eventInfo <- grabAll(pbpParse, "Player")
eventParse <- xpathSApply(pbpParse, "//Player/Stat")
eventParsep <- xpathSApply(pbpParse, "//Player")
pases_num <- cbind(
  "names" = lapply(getNodeSet(pbpParse, "//Player/Stat"), function(x) xmlValue(x))
)
NInfo <- sapply(eventParsep, function(x) sum(names(xmlChildren(x)) == "Stat"))
QInfo <- grabAll1(pbpParse, "//Player/Stat")
EventsExpanded <- as.data.frame(lapply(eventInfo[,1:6], function(x) rep(x, NInfo)), stringsAsFactors=F)
QInfo <- cbind(EventsExpanded, QInfo,(pases_num))
names(QInfo)[c(7,8)] <- c("Stats","value")
QInfo$value <- as.double(QInfo$value)
QInfo$nombre <- paste0(QInfo$first_name," ",QInfo$last_name)
eventos <- QInfo
}


grabAll <- function(XML.parsed, field){
  parse.field <- xpathSApply(XML.parsed, paste("//", field, "[@*]", sep=""))
  results <- t(sapply(parse.field, function(x) xmlAttrs(x)))
  if(typeof(results)=="list"){
    do.call(plyr::rbind.fill, lapply(lapply(results, t), data.frame, stringsAsFactors=F))
  } else {
    as.data.frame(results, stringsAsFactors=F)
  }
}

grabAll1 <- function(XML.parsed, field){
  parse.field <- xpathSApply(XML.parsed, paste("//", field, "[@*]", sep=""))
  results <- t(sapply(parse.field, function(x) xmlAttrs(x)))
    do.call(plyr::rbind.fill, lapply(lapply(results, t), data.frame, stringsAsFactors=F))
}
