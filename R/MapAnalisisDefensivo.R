#' Esta funcion analiza los eventos defensivos y en que zonas se realizan
#'
#' @param df Un df
#' @return El analisis defensivo a partir de \code{df}
#' @examples
#' OptaMAPanalisisdefensivo(df)
#' @export
#'
#'
#'
OptaMAPanalisisdefensivo <- function(df){

  coor<-dplyr::filter(df,x>0)
  coor2<-dplyr::select(coor,player_id,type_id,min,team_id,x,y,home_team_id,away_team_id)
  coor2$min<-as.integer(coor2$min)
  coor3<-dplyr::filter(coor2,type_id %in% c('4','7','8','12','44','45','49','56','74','16'))

  coor3<-coor3 %>%
    mutate(Local = case_when(
      team_id==home_team_id ~ 1,
      team_id!=home_team_id ~ 0
    ))


  coor3$x2<-(coor3$x-100)*-1

  coor3<-coor3 %>%
    dplyr::mutate(xf = case_when(
      team_id==home_team_id ~ x,
      team_id==away_team_id ~ x2
    ))

  coor3<-coor3 %>%
    dplyr::mutate(Tiempo = case_when(
      (min)<=15 ~ 15,
      (min)>15 & min<=30 ~ 30,
      (min)>30 & min<=45 ~ 45,
      (min)>45 & min<=60 ~ 60,
      (min)>60 & min<=75 ~ 75,
      (min)>75 & min<105 ~ 90
    )
    )

  #transformamos para calcular el número de pases por sector
  coor4 <- coor3 %>%
    dplyr::group_by(player_id,team_id) %>%
    dplyr::summarise(x=mean(xf),y=mean(y),conteo=n())

  Equipo_casa<-dplyr::distinct(coor2, home_team_id)
  Equipo_fuera<-dplyr::distinct(coor2, away_team_id)

  hull_coor <- coor4 %>%
    dplyr::group_by(team_id) %>%
    slice(chull(x, y)) %>%
    mutate(hull1= 1)

  centro_pol<-coor4 %>%
    dplyr::group_by(team_id) %>%
    dplyr::summarise(xm=mean(x),ym=mean(y))

  centro_pol<-centro_pol %>%
    dplyr::mutate(Local = case_when(
      team_id==Equipo_casa$home_team_id ~ 1,
      team_id==Equipo_fuera$away_team_id ~ 0
    )
    )

  segm_H<-dplyr::filter(centro_pol,Local==1)
  segm_A<-dplyr::filter(centro_pol,Local==0)

  h <- OptaMAPcampofutbol()
  p <- h + ggtitle(("\nMapa de acciones defensivas")) +
    geom_point(data = coor4,aes(x = x*106,y = y*70.4,colour=team_id,fill=team_id,size=conteo),stroke = 1) +
    scale_size(range = c(2,12))+
    geom_polygon(hull_coor, mapping=aes(x= x*106, y=y*70.4,color = team_id, fill = team_id), alpha = 0.2,linetype = "dashed",size = 1,show.legend = FALSE) +
    geom_point(data = centro_pol,aes(x = xm*106,y = ym*70.4,fill=team_id),shape = 21, colour = "black", size = 5, stroke = 5) +
    geom_segment(data=segm_H,aes(x = xm*106, y = ym*70.4, xend = 0, yend = 7040/2),colour = "#000000", size = 1) +
    geom_segment(data=segm_A,aes(x = xm*106, y = ym*70.4, xend = 10600, yend = 7040/2),colour = "#000000", size = 1) +
    geom_text(data = segm_H,aes(x = xm*106,y = (ym+10)*70.4,label=format(sqrt((xm-0)^2+(ym-35.2)),digits=2,nsmall=2)), colour="white") +
    geom_text(data = segm_A,aes(x = xm*106,y = (ym+10)*70.4,label=format(sqrt((xm-106)^2+(ym-35.2)),digits=2,nsmall=2)), colour="white") +
    annotate("text", x = 2300, y = 7000, label = (paste0("\nZona del ",Equipo_casa)),colour="blue") +
    annotate("text", x = 5300, y = 7000, label = (paste0("\nZona Media")),colour="green") +
    annotate("text", x = 7800, y = 7000, label = (paste0("\nZona del ",Equipo_fuera)),colour="red") +
    annotate("segment", x = 1650, xend = 1650, y = 0, yend = 7040,colour = "white",linetype="dashed") +
    annotate("segment", x = 3604, xend = 3604, y = 0, yend = 7040,colour = "white",linetype="dashed") +
    annotate("segment", x = 6996, xend = 6996, y = 0, yend = 7040,colour = "white",linetype="dashed") +
    annotate("segment", x = 8950, xend = 8950, y = 0, yend = 7040,colour = "white",linetype="dashed") +
    annotate("segment", x = 0, xend = 10600, y = 1485, yend = 1485,colour = "white",linetype="dashed") +
    annotate("segment", x = 0, xend = 10600, y = 5554, yend = 5554,colour = "white",linetype="dashed") +
    theme(legend.position="none")

  return(p)

}
