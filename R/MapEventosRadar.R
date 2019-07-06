#' Esta funcion crea graficos segun algunas estadisticas de los eventos
#' @param df Un df
#' @param player 1/0 según queramos un radar plot por jugador o por posicion
#' #' @return El mapa de eventos de \code{df} teniendo en cuenta jugador o posicion si \code{player}
#' @examples
#' OptaMAPradarevent(df,0)
#' OptaMAPradarevent(df,1)
#' @export
#'
#'
#'
#'
OptaMAPradarevent <- function(df,player){

  prueba_posesion2<-as.matrix(df)
  prueba_posesion3<-as.data.frame(prueba_posesion2)
  prueba_posesion3 <- data.frame(lapply(prueba_posesion3, as.character), stringsAsFactors=FALSE)
  prueba_posesion4<- reshape::cast(prueba_posesion3, nombre ~ Stats)
  prueba_posesion4 <- data.frame(lapply(prueba_posesion4, as.character), stringsAsFactors=FALSE)
  prueba3<-select(prueba_posesion4,nombre,Appearances,Blocks,Duels,Goals,Interceptions,Recoveries)
  prueba3[is.na(prueba3)] <- 0
  prueba3$Appearances<-as.integer(prueba3$Appearances)
  prueba3$Blocks<-as.integer(prueba3$Blocks)
  prueba3$Duels<-as.integer(prueba3$Duels)
  prueba3$Goals<-as.integer(prueba3$Goals)
  prueba3$Interceptions<-as.integer(prueba3$Interceptions)
  prueba3$Recoveries<-as.integer(prueba3$Recoveries)


  if(player==1){
  radarBoxplot::radarBoxplot(nombre ~ ., prueba3)
  }else if(player==0){
  prueba5<-dplyr::select(df,position,nombre)
  prueba6<-dplyr::distinct(df,position,nombre)
  prueba7<-dplyr::left_join(prueba3,prueba6,by=c('nombre'))
  prueba8<-select(prueba7,position,Appearances,Blocks,Duels,Goals,Interceptions,Recoveries)

  orange = "#FFA500CC"
  green = rgb(0, .7, 0, 0.6)

  radarBoxplot::radarBoxplot(position ~ ., prueba8,
               use.ggplot2=FALSE, medianLine=list(col="white"),
               innerPolygon=list(col=orange),
               outerPolygon=list(col=green))
  }

}
