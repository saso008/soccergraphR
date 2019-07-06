#' Esta función crea un gráfico con el porcentaje de eventos por zonas (18) y poe minutos
#'
#' @param df Un df
#' @param minutos Un df
#' @return El mapa de posesiones de \code{x} teniendo en cuenta minutos si \code{y}
#' @examples
#' OptaMAPposesionzonal(df,0)
#' OptaMAPposesionzonal(df,1)
#' @export
#'
#'
#'
OptaMAPposesionzonal <- function(df,minutos){

  coor<-dplyr::filter(df,x>0)
  coor2<-dplyr::select(coor,type_id,min,team_id,x,y,home_team_id,away_team_id)

  coor2$x2<-(coor2$x-100)*-1

  coor2<-coor2 %>%
    dplyr::mutate(xf = case_when(
      team_id==home_team_id ~ x,
      team_id==away_team_id ~ x2
    ))

  coor2<-coor2 %>%
    dplyr::mutate(Sector = case_when(
      xf<=17 & y>=78.9 ~ 1,
      xf<=17 & y<78.9 & y>=21.1 ~ 7,
      xf<=17 & y<21.1 ~ 13,
      xf<=34 & xf>17 & y>=78.9 ~ 2,
      xf<=34 & xf>17 & y<78.9 & y>=21.1 ~ 8,
      xf<=34 & xf>17 & y<21.1 ~ 14,
      xf<=50 & xf>34 & y>=78.9 ~ 3,
      xf<=50 & xf>34 & y<78.9 & y>=21.1 ~ 9,
      xf<=50 & xf>34 & y<21.1 ~ 15,
      xf<=64 & xf>50 & y>=78.9 ~ 4,
      xf<=64 & xf>50 & y<78.9 & y>=21.1 ~ 10,
      xf<=64 & xf>50 & y<21.1 ~ 16,
      xf<=83 & xf>64 & y>=78.9 ~ 5,
      xf<=83 & xf>64 & y<78.9 & y>=21.1 ~ 11,
      xf<=83 & xf>64 & y<21.1 ~ 17,
      xf<=105 & xf>83 & y>=78.9 ~ 6,
      xf<=105 & xf>83 & y<78.9 & y>=21.1 ~ 12,
      xf<=105 & xf>83 & y<21.1 ~ 18
    )
    )

  coor2$min<-as.integer(coor2$min)

  coor2<-coor2 %>%
    dplyr::mutate(Tiempo = case_when(
      (min)<=15 ~ 15,
      (min)>15 & min<=30 ~ 30,
      (min)>30 & min<=45 ~ 45,
      (min)>45 & min<=60 ~ 60,
      (min)>60 & min<=75 ~ 75,
      (min)>75 & min<105 ~ 90
    )
    )

  zonas_total<-coor2 %>% dplyr::group_by(Sector) %>%
    dplyr::summarize(conteo=n())

  zonas_total_tiempo<-coor2 %>% dplyr::group_by(Sector,Tiempo) %>%
    dplyr::summarize(conteo=n())


  maestro_zonas <- data.frame("Sector" = 1:18, "xs" = c(8.5,25.5,42,58,75.5,91.5,8.5,25.5,42,58,75.5,91.5,8.5,25.5,42,58,75.5,91.5
  ), "ys" = c(89.45,89.45,89.45,89.45,89.45,89.45,50,50,50,50,50,50,10.55,10.55,10.55,10.55,10.55,10.55
  ), stringsAsFactors = FALSE)

  zonas_total_final<-dplyr::left_join(zonas_total,maestro_zonas,by=("Sector"))

  zonas_total_tiempo_final<-dplyr::left_join(zonas_total_tiempo,maestro_zonas,by=("Sector"))


  Equipo_casa<-dplyr::distinct(coor2, home_team_id)
  Equipo_fuera<-dplyr::distinct(coor2, away_team_id)

  if(minutos==1){
    h <- OptaMAPcampofutbol()
    p <- h +
    ggtitle(("\nZonas de posesión por zonas")) +
   #Dibujamos los nodos
    geom_point(data = zonas_total_final,aes(x = xs*106,y = ys*70.4,fill=conteo,size=conteo*10),shape=21,stroke = 1) +
    scale_size(range = c(2,12))+
    geom_text(data = zonas_total_final,aes(x = xs*106,y = ys*70.4,label=conteo),vjust=0.5, colour="white") +
    #Ponemos las etiquetas a los nodos
    annotate("text", x = 2300, y = 7000, label = (paste0("\nZona del ",Equipo_casa)),colour="red") +
    annotate("text", x = 5300, y = 7000, label = (paste0("\nZona Media")),colour="green") +
    annotate("text", x = 7800, y = 7000, label = (paste0("\nZona del ",Equipo_fuera)),colour="blue") +
    annotate("segment", x = 1650, xend = 1650, y = 0, yend = 7040,colour = "white",linetype="dashed") +
    annotate("segment", x = 3604, xend = 3604, y = 0, yend = 7040,colour = "white",linetype="dashed") +
    annotate("segment", x = 6996, xend = 6996, y = 0, yend = 7040,colour = "white",linetype="dashed") +
    annotate("segment", x = 8950, xend = 8950, y = 0, yend = 7040,colour = "white",linetype="dashed") +
    annotate("segment", x = 0, xend = 10600, y = 1485, yend = 1485,colour = "white",linetype="dashed") +
    annotate("segment", x = 0, xend = 10600, y = 5554, yend = 5554,colour = "white",linetype="dashed") +
    theme(legend.position="none")

  return(p)

  }else{

    j=0
    zona_plot <- list()
    for(i in seq(15, 90, by = 15)){
      j=j+1
      zona_data<-dplyr::filter(zonas_total_tiempo_final,Tiempo==i)
      h <- OptaMAPcampofutbol()
      p <- h +
      ggtitle(paste0("\nZonas de posesión por zonas. Minuto ",i)) +
      #Dibujamos los nodos
      geom_point(data = zona_data,aes(x = xs*106,y = ys*70.4,fill=conteo,size=conteo*10),shape=21,stroke = 1) +
      scale_size(range = c(2,12))+
      geom_text(data = zona_data,aes(x = xs*106,y = ys*70.4,label=conteo),vjust=0.5, colour="white") +
      #Ponemos las etiquetas a los nodos
      annotate("text", x = 2300, y = 7000, label = (paste0("\nZona del ",Equipo_casa)),colour="red") +
      annotate("text", x = 5300, y = 7000, label = (paste0("\nZona Media")),colour="green") +
      annotate("text", x = 7800, y = 7000, label = (paste0("\nZona del ",Equipo_fuera)),colour="blue") +
      annotate("segment", x = 1650, xend = 1650, y = 0, yend = 7040,colour = "white",linetype="dashed") +
      annotate("segment", x = 3604, xend = 3604, y = 0, yend = 7040,colour = "white",linetype="dashed") +
      annotate("segment", x = 6996, xend = 6996, y = 0, yend = 7040,colour = "white",linetype="dashed") +
      annotate("segment", x = 8950, xend = 8950, y = 0, yend = 7040,colour = "white",linetype="dashed") +
      annotate("segment", x = 0, xend = 10600, y = 1485, yend = 1485,colour = "white",linetype="dashed") +
      annotate("segment", x = 0, xend = 10600, y = 5554, yend = 5554,colour = "white",linetype="dashed") +
      theme(legend.position="none")

    zona_plot[[j]] <- p
    }
    do.call(pdp::grid.arrange, zona_plot)
  }

}
