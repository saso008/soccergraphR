#' Esta funcion crea un mapa de calor de adonde se lanzan
#' los corners si son rematados y con que parte del cuerpo
#'
#' @param df Un df
#' @param home Un numero que puede ser 1 o 0 en funcion si es local(1) o visitante (0)
#' @return El mapa de \code{df}  y si es local \code{home}
#' @examples
#' OptaMAPcorner(df,1)
#' OptaMAPcorner(df,0)
#' @export

OptaMAPcorner <- function(df,home){

  #Sacamos los corners
  corner3 <- dplyr::filter(df,as.numeric(as.character(df$"6"))==-1)

  #Seleccionamos las columnas que nos interesan
  polar<-dplyr::select(corner3,event_id,type_id,min,team_id,outcome,x,y,player_id,"6","2","210","212","213","140","141","home_team_id","away_team_id")

  #Cambiamos los nombres de las columnas porque los números dan problemas
  names(polar)<-c("event_id","type_id","min","team_id","outcome","x","y","player_id","a","b","c","d","e","f","g","h","i")

  #Sacamos los tiros y la parte del cuerpo con la que fueron realizados
  shots<-dplyr::select(df,type_id,"55","15","20","72")
  #Cambiamos los nombres
  names(shots)<-c("type_id","event_id","head","der","izq")

  #Filtramos los eventos de tiro
  shots <- dplyr::filter(shots, type_id %in% c("13","14","15","16"))

  #Los campos factor que nos interesan los convertimos a numeric o string
  polar$a <- as.numeric(as.character(polar$a))
  polar$b <- as.numeric(as.character(polar$b))
  polar$c <- as.numeric(as.character(polar$c))
  polar$d <- as.numeric(as.character(polar$d))
  polar$e <- as.numeric(as.character(polar$e))
  polar$f <- as.numeric(as.character(polar$f))
  polar$g <- as.numeric(as.character(polar$g))
  shots$event_id <- (as.character(shots$event_id))
  shots$head <- (as.character(shots$head))
  shots$der <- (as.character(shots$der))
  shots$izq <- (as.character(shots$izq))



  #juntamos ambos ficheros. Ya tenemos cada corner y el tiro asociado si lo tuvo
  polar2<-dplyr::left_join(polar,shots,by=("event_id"))




  #pasamos el segundo parámetro de la función para quedarnos con los datos del equipo local o visitante
  if(home==1){
    polar2 <- dplyr::filter(polar2,team_id==h)}
  else{
    polar2 <- dplyr::filter(polar2,team_id!=h)
  }

  #creamos un campo único con la parte del cuerpo del tiro
  polar2<-polar2 %>%
    dplyr::mutate(Remate = case_when(
      der==-1 ~ "Pie",
      izq==-1 ~ "Pie",
      head==-1 ~ "Cabeza",
      is.na(type_id.y) ~ "No Remate"
    )
    )

  #Creamos un campo único con el resultado del tiro
  polar2<-polar2 %>%
    dplyr::mutate(tipo_Remate = case_when(
      type_id.y==13 ~ "Fuera",
      type_id.y==14  ~ "Poste",
      type_id.y==15  ~ "A Puerta",
      type_id.y==16  ~ "Gol",
      is.na(type_id.y) ~ "No Remate"
    )
    )



  #Creamos el campo y los objetos a dibujar:
  h <- OptaMAPcampofutbol()
  p <- h +

    ggtitle(paste("\nMapa de corners")) +
    # Aquí dibujamos el mapa de calor de los corners
    stat_density2d(data=polar2,aes(x=f*106,y=g*70,fill = ..level..,alpha=..level..), geom="polygon",show.legend = FALSE) +
    scale_fill_gradient(low="yellow", high="red",aesthetics = "fill") +
    # dejo comentado la linea siguiente para el futuro, dado que con ella dibujamos la flecha del lanzamiento de corner
    #geom_curve(data=polar2,aes(x=x*106, y=y*70, xend = f*106, yend = g*70),curvature = -0.1,arrow = arrow(length = unit(0.01, "npc")))+
    #Dibujamos los remates
    geom_point(data=polar2,aes(x = f*106, y=g*70,shape=tipo_Remate,colour=Remate),size=3,stroke = 1)+
    #metemos la leyenda abajo
    theme(legend.position="bottom")

  return(p)
}
