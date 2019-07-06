#' Esta funcion crea un mapa con las zonas de tiro indicando si era gol y el
#' tamaño según la distancia sobre una porteria y las zonas propuestas por OPTA
#'
#' @param df Un df
#' @param home Un numero que puede ser 1 o 0 en funcion si es local(1) o visitante (0)
#' @return El mapa de tiros de \code{df}  y si es local \code{home}
#' @examples
#' OptaMAPshoot(df,1)
#' OptaMAPshoot(df,0)
#' @export
#'
OptaMAPshoot <- function(df,home){

  #Nos quedamos con los campos que nos interesan
  shots<-dplyr::select(df,type_id,team_id,outcome,x,y,player_id,"102","103","home_team_id","away_team_id")
  #cambiamos los nombres de las columnas
  names(shots)<-c("type_id","team_id","outcome","x","y","player_id","a","b","c","d")

  #Convertimos a numerico los Factor
  shots$a <- as.numeric(as.character(shots$a))
  shots$b <- as.numeric(as.character(shots$b))
  shots$c <- as.numeric(as.character(shots$c))
  shots$d <- as.numeric(as.character(shots$d))

  #Gestionamos la selección de Local o Visitante
  if(home==1){
    shots <- dplyr::filter(shots,team_id== c)}
  else{
    shots <- dplyr::filter(shots,team_id!= c)
  }

  #Filtramos los tiros
  shots <- dplyr::filter(shots, match(type_id,c('13','14','15','16'))  & (!is.na(a) & !is.na(b)))

  #Calculamos la distancia a portería como un campo nuevo
  shots$distancia<-((100-shots$x)^2+(35-(shots$y*0.7))^2)^0.5


  #hacemos un BIN de la distancia
  shots<-shots %>%
    dplyr::mutate(Dist_Shoot = case_when(
      distancia>=0 & distancia<=2 ~ 2,
      distancia>2 & distancia<=5 ~ 5,
      distancia>5 & distancia<=11 ~ 11,
      distancia>11 & distancia<=20 ~ 20,
      distancia>20  ~ 50
    )
    )

  #Categorizamos los tiros en una columna
  shots<-shots %>%
    dplyr::mutate(Tipo_tiro = case_when(
      type_id==13 ~ "Fuera",
      type_id==14 ~ "Al poste",
      type_id==15 ~ "A puerta",
      type_id==16 ~ "Gol"

    )
    )

  #Creamos la plantilla del tema del gráfico (esto es de soccermatics con algunos cambios)
    theme_blankPitch = function(size=12) {
    theme(
      #axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      #axis.ticks.margin=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="#ffffff", colour=NA),
      legend.key=element_rect(colour="#ffffff",fill="#ffffff"),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = "#ffffff", fill = "#ffffff", size = .5),
      panel.background=element_rect(fill="#ffffff",colour="#ffffff"),
      #       panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour="#ffffff",size=size,angle=270),
      strip.text.x=element_text(size=size*1))}


  p <- ggplot() +
    ggtitle(paste("\nZonas de tiro")) +
    xlim(c(65,35)) + ylim(c(0,8)) +
    theme_blankPitch() +
    geom_segment(aes(x = 44.2, y = 0, xend = 44.2, yend = 3.4064),colour = "#000000",size=1) +
    geom_segment(aes(x = 44.41, y = 0, xend = 44.41, yend = 3.1964),colour = "#000000",size=1) +
    geom_segment(aes(x = 55.59, y = 0, xend = 55.59, yend = 3.1964),colour = "#000000",size=1) +
    geom_segment(aes(x = 55.8, y = 0, xend = 55.8, yend = 3.4064),colour = "#000000",size=1) +
    geom_segment(aes(x = 44.41, y = 3.1964, xend = 55.59, yend = 3.1964),colour = "#000000",size=1) +
    geom_segment(aes(x = 44.2, y = 3.4064, xend = 55.8, yend = 3.4064),colour = "#000000",size=1) +
    geom_segment(aes(x = 44.41, y = 1.5982, xend = 55.59, yend = 1.5982),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 59.3, y = 0, xend = 59.3, yend = 5.05),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 40.7, y = 0, xend = 40.7, yend = 5.05),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 35, y = 3.3, xend = 44.2, yend = 3.3),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 55.8, y = 3.3, xend = 65, yend = 3.3),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 40.7, y = 5.05, xend = 59.3, yend = 5.05),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 44.2, y = 0, xend = 44.2, yend = 8),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 55.8, y = 0, xend = 55.8, yend =8),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 51.8, y = 0, xend = 51.8, yend = 3.1964),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 48.2, y = 0, xend = 48.2, yend = 3.1964),colour = "#000000",size=0.5,linetype='dashed') +
    geom_segment(aes(x = 65, y = 0, xend = 35, yend = 0),colour = "#000000",size=0.5,linetype='solid') +
    #Pintamos los puntos ajustando a las escalas del dibujo
    geom_point(data = shots,aes(x = ((a)),y = ((b)/15.57)*1.31,color=Tipo_tiro,size=-Dist_Shoot,stroke = 1)) +
    guides(colour = guide_legend(override.aes = list(size=5))) +
    scale_radius("Distancia",breaks=c(-5,-11,-20,-50),labels=c('Menos de 5m','Menos de 11m','Menos de 20m','Mas de 20m')) +
    theme(legend.position="bottom") +
    labs(shape="Distancia", colour="Tiro")+
    coord_fixed()

  return(p)
}
