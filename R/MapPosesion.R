#' Esta funcion crea un mapa con las posesiones cada 15'
#'
#' @param df Un df
#' @return El valor de posesiones de \code{df}
#' @examples
#' OptaMAPposesion(df)
#' @export
#'
#'
OptaMAPposesion <- function(df){

  #Creamos un campo nuevo con el sector de cada pase
  polar<-df %>%
    dplyr::mutate(Sector = case_when(
      time=='1-15' ~ 11,
      time=='16-30' ~ 22,
      time=='31-45' ~ 33,
      time=='46-60' ~ 44,
      time=='61-75' ~ 55,
      time=='76-90' ~ 66
    )
    )

  Equipo_casa<-dplyr::distinct(polar, home_team_name)
  Equipo_fuera<-dplyr::distinct(polar, away_team_name)



  plot_box <- ggplot(data=polar,aes(x=Sector*100,y=as.numeric(as.character(value))*106,fill=variable)) +
    geom_bar(stat="identity", alpha=0.5,position = position_stack(reverse = TRUE)) +
    annotate("text", x = 1100, y = 1000, label = "1-15min") +
    annotate("text", x = 2200, y = 1000, label = "16-30min") +
    annotate("text", x = 3300, y = 1000, label = "31-45min") +
    annotate("text", x = 4400, y = 1000, label = "46-60min") +
    annotate("text", x = 5500, y = 1000, label = "61-75min") +
    annotate("text", x = 6600, y = 1000, label = "76-90min") +

    annotate("text", x = 7700, y = 1500, label = (paste0("\nZona del ",Equipo_casa)),colour="red") +
    annotate("text", x = 7700, y = 5000, label = (paste0("\nZona Media")),colour="green") +
    annotate("text", x = 7700, y = 9000, label = (paste0("\nZona del ",Equipo_fuera)),colour="blue") +


    coord_flip() +
    theme_bw() +
    theme(line=element_blank(),text=element_blank(),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          axis.line = element_blank(),
          title=element_blank(), panel.border=element_blank(),legend.position = "none")

  PB = ggplotGrob(plot_box)

  h <- OptaMAPcampofutbol()
  p <- h +
    ggtitle(paste("\nMapa de posesiones")) +
    theme(legend.position = "bottom")

  #aquí pintamos el campo de futbol y la lista de gráficos de cada jugador
  plot1 <- p + annotation_custom(grob=PB, xmin=0, xmax=10600, ymin=0,ymax=7040)
  return(plot1)
}
