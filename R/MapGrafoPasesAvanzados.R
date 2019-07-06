#' Esta funcion crea el grafo de pases para un equipo durante un partido cuando
#' los pases son avanzados
#'
#' @param df Un df
#' @param home Un numero 1/0 para representar al equipo local o visitante
#' @param metros Un integer que representa metros
#' @param pasesmin Un numero que indica el numero de pases minimo que tiene en cuenta el grafo
#'
#'
#' @return El grafo de \code{df} cuando el equipo local es \code{home} con más de \code{metros} metros adelante
#' y un numero de pases \code{pases} minimo
#' @examples
#' OptaMAPdfrontpass(df,1,3,3)
#' OptaMAPdfrontpass(df,0,3,3)
#' @export
#'
#'
OptaMAPdfrontpass <- function(df,home,metros,pases){
  #Nos quedamos con las columnas que nos interesan
  test_2<-filter(df,type_id==1 & outcome==1)

  polar<-dplyr::select(test_2,event_id,type_id,team_id,outcome,x,y,player_id,"140","141","home_team_id","away_team_id")
  #Cambiamos los nombres que los numeros no gustan
  names(polar)<-c("event_id","type_id","team_id","outcome","x","y","player_id","x1","y1","h","a")

  #convertimos a numérico las columnas Factor
  polar$x1 <- as.numeric(as.character(polar$x1))
  polar$y1 <- as.numeric(as.character(polar$y1))

  polar1<-filter(polar,x1>(x) & x1-x>metros)

  #Filtramos para el Home
  if(home==1){
    polar1 <- dplyr::filter(polar1,team_id==h)}
  else{
    polar1 <- dplyr::filter(polar1,team_id!=h)
  }

  polar1$event_id <- as.numeric(polar1$event_id)
  polar1$Evento_lig<-(polar1$event_id+1)
  polar1$Evento_lig<-as.numeric(polar1$Evento_lig)
  test_cr<-filter(df,team_id==polar1$team_id[1])
  test_cr$event_id <- as.numeric(test_cr$event_id )

  polar3<-polar1 %>%
    dplyr::left_join(select(test_cr,event_id,player_id),by=c('Evento_lig'='event_id'))

  polar4<-polar3 %>%
    dplyr::group_by(player_id.x,player_id.y) %>%
    dplyr::summarise(conteo=n())

  coor_m<-polar1 %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(xm=mean(x),ym=mean(y))

  polar4<-polar4 %>%
    dplyr::left_join(coor_m,by=c('player_id.x'='player_id'))

  nodos<-polar1 %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(conteo=n())

  arcos<-polar4

  arcos <- arcos %>%
    dplyr::left_join(coor_m, by = c('player_id.y' = 'player_id'))

  arcos<-dplyr::filter(arcos,!is.na(xm.x) , !is.na(xm.y))

  arcos<-dplyr::filter(arcos,conteo>pases-1)
  arcos<-dplyr::filter(arcos,player_id.x!=player_id.y)

  nodos <- nodos %>%
    dplyr::left_join(coor_m, by = c('player_id' = 'player_id'))

  h <- OptaMAPcampofutbol()
  p <- h +
    ggtitle(paste("\nGrafo de Pases adelantados del ",polar1$team_id[1])) +
    #Dibujamos los arcos entre jugadores
    geom_curve(data=arcos,aes(x=xm.x*100-50, y=ym.x*70-50, xend = xm.y*100-50, yend = ym.y*70-50,size=conteo,color=conteo),curvature = -0.2,arrow = arrow(length = unit(0.03, "npc")))+
    scale_color_gradient(low = "blue", high = "red")+
    #Dibujamos los nodos
    geom_point(data = nodos,aes(x = xm*100,y = ym*70,size = conteo*2,fill=conteo),color='black',shape=21,stroke = 1) +
    theme(legend.position="none")
  return(p)
}
