#' Esta funcion crea un mapa de radar para cada jugador con las direcciones de pase,
#' color y numero a partir de la coordenada promedio desde donde pasan
#'
#' @param df Un df
#' @param home Un numero que indica 1 si es local o 0 si es visitante
#' @return El radar de \code{df} y si es local \code{home}
#' @examples
#' OptaMAPdirectpass(df,1)
#' OptaMAPdirectpass(df,0)
#' @export
#'
#'
OptaMAPdirectpass <- function(df,home){
  #Nos quedamos con las columnas que nos interesan
  polar<-dplyr::select(df,type_id,team_id,outcome,x,y,player_id,"2","107","123","213","home_team_id","away_team_id")
  #Cambiamos los nombres que los numeros no gustan
  names(polar)<-c("type_id","team_id","outcome","x","y","player_id","a","b","c","d","e","f")
  #convertimos a numérico las columnas Factor
  polar$a <- as.numeric(as.character(polar$a))
  polar$b <- as.numeric(as.character(polar$b))
  polar$c <- as.numeric(as.character(polar$c))
  polar$d <- as.numeric(as.character(polar$d))

  #Filtramos para el Home
  if(home==1){
    polar <- dplyr::filter(polar,team_id==e)}
  else{
    polar <- dplyr::filter(polar,team_id!= e)
  }

  #Nos quedamos los eventos de pase
  polar <- dplyr::filter(polar,type_id==1 & is.na(a) & is.na(b) & is.na(c) )

  #Creamos un campo nuevo con el sector de cada pase
  polar<-polar %>%
    dplyr::mutate(Sector = case_when(
      d>=0 & d<=0.3925 ~ 4,
      d>0.3925 & d<=0.785 ~ 5,
      d>0.785 & d<=1.1775 ~ 6,
      d>1.1775 & d<=1.57 ~ 7,
      d>1.57 & d<=1.9625 ~ 8,
      d>1.9625 & d<=2.355 ~ 9,
      d>2.355 & d<=2.7475 ~ 10,
      d>2.7475 & d<=3.14 ~ 11,
      d>3.14 & d<=3.5325 ~ 12,
      d>3.5325 & d<=3.925 ~ 13,
      d>3.925 & d<=4.3175 ~ 14,
      d>4.3175 & d<=4.71 ~ 15,
      d>4.71 & d<=5.1025 ~ 16,
      d>5.1025 & d<=5.495 ~ 1,
      d>5.495 & d<=5.8875 ~ 2,
      d>5.8875 & d<=6.28 ~ 3
    )
    )

  #transformamos para calcular el número de pases por sector
  polar1 <- polar %>%
    dplyr::group_by(player_id,Sector) %>%
    dplyr::summarise(conteo=n())

  #Sacamos la coordenada promedio de cada jugador
  polar_coords <- polar %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(xcoord=mean(x)*100,ycoord=mean(y)*70)

  #Obtenemos el maximo de pases en un sector
  polar_player <- polar1 %>%
    dplyr::left_join(select(polar_coords, player_id,xcoord,ycoord), by = 'player_id') %>%
    dplyr::mutate(max.Radius = max(conteo))


  #Realmente vamos a pintar 14 gráficos sobre otro, por lo que tenemos que generar una lista de objetos gráficos (1 por jugador)
  df.grobs <- polar_player %>%
    dplyr::group_by(player_id, xcoord, ycoord, max.Radius) %>%
    do(subplots = ggplot(.,
                         aes(x = Sector, y = conteo, fill = conteo)) +
         geom_col(position = "identity", width = 1, color = "black",
                  alpha = 0.5,
                  show.legend = FALSE) +
         scale_y_continuous(limits = c(0, unique(.$max.Radius))) +
         scale_fill_gradient(low = "red", high = "green") +
         coord_polar() +
         theme_void()) %>%
    dplyr::mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                             x = xcoord -1000,
                                             y = ycoord -1000,
                                             xmax = xcoord + 1000,
                                             ymax = ycoord +1000)))


  h <- OptaMAPcampofutbol()
  p <- h +
    ggtitle(paste("\nMapa radar de pases")) +
    theme(legend.position="none")

  #aquí pintamos el campo de futbol y la lista de gráficos de cada jugador
  return(p + df.grobs$subgrobs)
}
