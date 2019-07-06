#' Esta funcion parsea el xml de pases de opta generando un data frame para ser leido con los
#' datos de los jugadores y sus estadisticas de pase y a que jugadores les pasan.
#'
#' @param x Un string con la ruta del fichero
#' @return un df de \code{x}
#' @examples
#' OptaXMLpassmatrix(path)
#' @export
#'
#'
OptaXMLpassmatrix <- function(x,...){
  pbpParse <- xmlInternalTreeParse(x)
  gameInfo <- grabAll(pbpParse, "SoccerFeed")
  # En eventInfo guardamos la informacion de cada evento
  eventInfo <- grabAll(pbpParse, "Player")
  # En eventParse parseamos la info de cada evento, cada evento puede tener varios elementos asociados
  # a distintos qualifier ids
  eventParse <- xpathSApply(pbpParse, "//Player/Player")
  eventParse2 <- xpathSApply(pbpParse, "//Player")
  pases_num <- cbind(
    "names" = lapply(getNodeSet(pbpParse, "//Player/Player"), function(x) xmlValue(x))
  )
  # en Ninfo calculamos el numero de elementos "Q" tiene cada evento
  NInfo <- sapply(eventParse2, function(x) sum(names(xmlChildren(x)) == "Player"))
  # En Qinfo guardamos la informacion de cadas Q, viene asociado con su Eid y Qid
  QInfo <- grabAll(pbpParse, "Player")
  QInfo <- filter(QInfo,is.na(cross_lost))
  QInfo <- select(QInfo, player_id, player_name)
  names(QInfo)[c(1,2)] <- c("player_id2", "player_name2")
  # En EventsExpanded repetimos la info de cada evento (solo su id y su eventid) tantas veces como hijos "Q" tenga
  print(x)
  EventsExpanded <- as.data.frame(lapply(eventInfo[,1:13], function(x) rep(x, NInfo)), stringsAsFactors=F)
  # unimos esta informaciC3n con la de Qinfo
  QInfo <- cbind(EventsExpanded, QInfo)
  QInfo$x <- as.double(QInfo$x)
  QInfo$y <- as.double(QInfo$y)
  QInfo$game_id <- gameInfo$game_id
  QInfo$team_id <- gameInfo$team_id
  QInfo$team_name <- gameInfo$team_name
  QInfo$home_team_id <- gameInfo$home_team_id
  QInfo$home_team_name <- gameInfo$home_team_name
  QInfo$away_team_id <- gameInfo$away_team_id
  QInfo$away_team_name <- gameInfo$away_team_name
  QInfo2 <- cbind(QInfo, pases_num)

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

