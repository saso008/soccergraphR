#' Esta funcion crea un mapa de calor para cada jugador con sus zonas adonde pasan
#'
#' @param df Un df
#' @param home Un número que indica 1 si es local o 0 si es visitante
#' @return El mapa de calor de \code{df} y si es local \code{home}
#' @examples
#' OptaMAPheatMapTo(df,1)
#' OptaMAPheatMapTo(df,0)
#' @export
#'
#'
OptaMAPheatMapTo <- function(df,home){

  #Seleccionamos las columnas que nos interesan
  heat<-dplyr::select(df,type_id,team_id,outcome,x,y,player_id,"2","107","123","213","home_team_id","away_team_id","140","141")
  #Cambiamos los nombres de las columnas
  names(heat)<-c("type_id","team_id","outcome","x","y","player_id","a","b","c","d","e","f","g","h")

  #Convertimos a numerico los campos Factor
  heat$a <- as.numeric(as.character(heat$a))
  heat$b <- as.numeric(as.character(heat$b))
  heat$c <- as.numeric(as.character(heat$c))
  heat$d <- as.numeric(as.character(heat$d))
  heat$g <- as.numeric(as.character(heat$g))
  heat$h <- as.numeric(as.character(heat$h))

  #Gestionamos la selección del local o visitante
  if(home==1){
    heat <- dplyr::filter(heat,team_id==e)}
  else{
    heat <- dplyr::filter(heat,team_id!= e)
  }

  #Filtramos los eventos pase
  heat <- dplyr::filter(heat,type_id==1 & is.na(a) & is.na(b) & is.na(c) )

  #Nos quedamos en un df los jugadores únicos
  players<-dplyr::distinct(heat, player_id, .keep_all = FALSE)



  #PAra cada jugador creamos un mapa de calor
  j=0
  player_plot <- list()
  for(i in players$player_id){
    j=j+1
    player_data<-dplyr::filter(heat,player_id==i)
    h <- OptaMAPcampofutbol()
    p <- h +
      ggtitle(paste("\nMapa de calor de inicio de pase de ",i)) +
      stat_density2d(data=player_data,aes(x=g*100,y=h*70,fill = ..level..,alpha=..level..), geom="polygon",show.legend = FALSE) +
      scale_fill_gradient(low="blue", high="red",aesthetics = "fill") +
      theme(legend.position="none")

    #añadimos a la lista cada gráfico
    player_plot[[j]] <- p

  }

  #llamamos a la función de pintar varios gráficos usando la lista como parámetro
  do.call(pdp::grid.arrange, player_plot)

}
