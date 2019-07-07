#' Esta funcion parsea el xml de eventos de opta generando un data frame preparado para usarse
#'
#' @param evento Un string con la ruta del fichero
#' @return un df de \code{evento}
#' @examples
#' OptaXMLeventos(path)
#' @export
#'
#' @import XML
#' @import ggplot2
#' @import readr
#' @import ggforce
#' @import dplyr
#' @importFrom magrittr %>%

OptaXMLeventos <- function(evento,...){

  pbpParse <- xmlInternalTreeParse(evento)
  gameInfo <- grabAll(pbpParse, "Game")
  eventInfo <- grabAll(pbpParse, "Event")
  eventParse <- xpathSApply(pbpParse, "//Event")
  NInfo <- sapply(eventParse, function(x) sum(names(xmlChildren(x)) == "Q"))
  QInfo <- grabAll(pbpParse, "Q")
  EventsExpanded <- as.data.frame(lapply(eventInfo[,1:2], function(x) rep(x, NInfo)), stringsAsFactors=F)
  QInfo <- cbind(EventsExpanded, QInfo)
  names(QInfo)[c(1,3)] <- c("Eid", "Qid")
  QInfo$value <- ifelse(is.na(QInfo$value), -1, QInfo$value)
  Qual <- reshape::cast(QInfo, Eid ~ qualifier_id)
  events <- merge(eventInfo, Qual, by.x="id", by.y="Eid", all.x=T, suffixes=c("", "Q"))
  events$TimeStamp <- as.POSIXct(events$timestamp, format="%Y-%m-%dT%H:%M:%S")
  events$x <- as.double(events$x)
  events$y <- as.double(events$y)
  events$game_id <- gameInfo$id
  events$home_team_id <- gameInfo$home_team_id
  events$away_team_id <- gameInfo$away_team_id
  eventos <- events
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


