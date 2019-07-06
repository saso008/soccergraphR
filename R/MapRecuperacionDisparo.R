#' Esta funcion analiza los eventos defensivos de recuperacion de balon que
#' acaban en disparo en menos de 3 eventos mostrando el tiempo que pasa entre el evento
#' de tiro y el de recuperacion en segundos
#'
#' @param df Un df
#' @param home Un integer 1/0 para saber si es el equipo Local
#' @return El analisis de contras de \code{df} si es el local \code{home}
#' @examples
#' OptaMAPcontras <- function(df,1)
#' @export
#'
#'
#'
#'
OptaMAPcontras <- function(df,home){

  #Gestionamos la selección de Local o Visitante
  if(home==1){
    df <- dplyr::filter(df,team_id== home_team_id)}
  else{
    df <- dplyr::filter(df,team_id!= home_team_id)
  }


shots <- dplyr::filter(df, type_id %in% c('13','14','15','16'))
shots$event_id<-as.integer(shots$event_id)
recup <- dplyr::filter(df, type_id %in% c('7','8','49','74'))
recup <- dplyr::select(recup,type_id,event_id,min,sec,team_id,x,y,player_id, TimeStamp)


recup$event_id<-as.integer(recup$event_id)
recup$event_id2<-recup$event_id+1
recup$event_id3<-recup$event_id+2
recup$event_id4<-recup$event_id+3

recup1<-recup %>%
  dplyr::left_join(select(shots,event_id,type_id,x,y,TimeStamp),by=c('event_id2'='event_id')) %>%
  dplyr::left_join(select(shots,event_id,type_id,x,y,TimeStamp),by=c('event_id3'='event_id')) %>%
  dplyr::left_join(select(shots,event_id,type_id,x,y,TimeStamp),by=c('event_id4'='event_id'))

recup2<-recup1 %>%
  dplyr::mutate(type_idS = case_when(
    !is.na(type_id.y) ~ type_id.y,
    !is.na(type_id.x.x) ~ type_id.x.x,
    !is.na(type_id.y.y) ~ type_id.y.y)) %>%
  dplyr::mutate(xS = case_when(
    !is.na(x.y) ~ x.y,
    !is.na(x.x.x) ~ x.x.x,
    !is.na(x.y.y) ~ x.y.y))  %>%
  dplyr::mutate(yS = case_when(
    !is.na(y.y) ~ y.y,
    !is.na(y.x.x) ~ y.x.x,
    !is.na(y.y.y) ~ y.y.y))%>%
  dplyr::mutate(TS = case_when(
    !is.na(TimeStamp.y) ~ TimeStamp.y,
    !is.na(TimeStamp.x.x) ~ TimeStamp.x.x,
    !is.na(TimeStamp.y.y) ~ TimeStamp.y.y))



recup2 <- dplyr::filter(recup2, !is.na(type_idS))

recup2<-recup2 %>%
  dplyr::mutate(Tipo_Tiro = case_when(
    type_idS==13 ~ "Fuera",
    type_idS==14 ~ "Al Palo",
    type_idS==15 ~ "A puerta",
    type_idS==16 ~ "Gol"))

recup2<-recup2 %>%
  dplyr::mutate(Tipo_Recup = case_when(
    type_id.x==7 ~ "Tackle",
    type_id.x==8 ~ "Intercepcion",
    type_id.x==49 ~ "Balon Suelto",
    type_id.x==74 ~ "Intercepcion Cercana"))


recup2$tiempo <- recup2$TS - recup2$TimeStamp.x
recup2$tiempo<-as.integer(recup2$tiempo)
recup2<-dplyr::select(recup2,Tipo_Recup,team_id,x.x,y.x,player_id,TimeStamp.x,Tipo_Tiro,xS,yS,TS,tiempo)

recup3<-recup2 %>%
  dplyr::group_by(Tipo_Tiro,xS,yS,TS) %>%
  dplyr::summarise(conteo=n(),TimeStamp.x=min(TimeStamp.x),tiempo=min(tiempo))

recup4<-recup3 %>%
  dplyr::left_join(select(recup2,Tipo_Recup,team_id,x.x,y.x,player_id,TimeStamp.x),by=c('TimeStamp.x'='TimeStamp.x'))

h <- OptaMAPcampofutbol()
p <- h +
  ggtitle(("\nMapa de recuperacion y tiro asociado")) +
  geom_point(data = recup4,aes(x = x.x*106,y = y.x*70.4,colour=Tipo_Recup,fill=Tipo_Recup),size=3,stroke = 1) +
  geom_point(data = recup4,aes(x = xS*106,y = yS*70.4,colour=Tipo_Tiro,fill=Tipo_Tiro,size=-(10600-xS*106)),stroke = 1) +
  geom_segment(data=recup4,aes(x = x.x*106, y = y.x*70.4, xend = xS*106, yend = yS*70.4),colour = "#000000", size = 1) +
  geom_text(data = recup4,aes(x = xS*106,y = (yS+10)*70.4,label=tiempo), colour="white") +
  theme(legend.position="bottom")

return(p)
}

