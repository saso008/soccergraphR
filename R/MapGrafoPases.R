#' Esta funcion crea el grafo de pases para un equipo durante un partido y
#' calcula ademas la centralidad de cada jugador y la densidad del grafo
#'
#' @param df Un df
#' @param pasesmin Un numero que indica el numero de pases minimo que tiene en cuenta el grafo
#' @return El grafo de \code{x}  con mas pases de \code{pasesmin}
#' @examples
#' OptaMAPmatrixpass(df,1)
#' OptaMAPmatrixpass(df,10)
#' @export
#'
#'
#'
OptaMAPmatrixpass <- function(df,pasesmin){

  kpases1 <- df

  #Nos quedamos con el nombre del equipo
  Equipo<-dplyr::distinct(kpases1, team_name)

  #Agrupamos cada jugador y obtenemos el numero de pases totales (serán los nodos del grafo)
  knodes9 <- kpases1 %>%
    dplyr::group_by(player_name,team_name,x,y) %>%
    dplyr::summarize(pass_success_t= sum(strtoi(names), na.rm = TRUE))

  #Agrupamos los jugadores y a quienes pasan en una única linea por jugador (serán los arcos de unión del grafo)
  kedges2 <- kpases1 %>%
    dplyr::group_by(player_name,player_name2,team_name) %>%
    dplyr::summarise(value= sum(strtoi(names), na.rm = TRUE))

  #unimos las tablas para conseguir las coordenadas del pase inicial y pase final entre jugadores
  kedges3 <- kedges2 %>%
    dplyr::left_join(select(knodes9, player_name,team_name,x,y), by = 'player_name')

  kedges4 <- dplyr::left_join(kedges3,knodes9,team_name, by = c("player_name2"="player_name"))



  #creamos el grafo que permite sacar metricas
  Colleague_Graph_kedges=igraph::graph.data.frame(kedges2, directed=FALSE,vertices=knodes9)

  #simplificamos los looping (es raro que haya pero alguno he visto) y las multiplicidades
  Colleague_Graph_kedges_d<-igraph::simplify(Colleague_Graph_kedges, remove.multiple=TRUE, remove.loops=TRUE)

  #Calculamos la densidad del grafo
  densidad<-igraph::graph.density(Colleague_Graph_kedges_d,loop=FALSE)

  #Calculamos la centralidad de cada jugador
  bet<-igraph::betweenness(Colleague_Graph_kedges_d,directed = FALSE)
  bet2 <- as.data.frame(bet)
  bet3 <- cbind(Label = rownames(bet2), bet2)
  bet3[,'Label'] <- as.character(bet3[,'Label'])

  #unimos a los nodos el valor de la centralidad
  knodes10 <- dplyr::left_join(knodes9,bet3, by = c("player_name"="Label"))

  #Nos quitamos los arcos de menos pases según el parámetro
  kedges5 <- dplyr::filter(kedges4,value>pasesmin)

  h <- OptaMAPcampofutbol()
  p <- h +
    ggtitle(paste("\nGrafo de Pases del ",Equipo[1])) +
    #Dibujamos los arcos entre jugadores
    geom_curve(data=kedges5,aes(x=x.x*100-50, y=y.x*70-50, xend = x.y*100-50, yend = y.y*70-50,size=value,color=value),curvature = -0.2,arrow = arrow(length = unit(0.03, "npc")))+
    scale_color_gradient(low = "blue", high = "red")+
    #Dibujamos los nodos
    geom_point(data = knodes10,aes(x = x*100,y = y*70,size = pass_success_t*2,fill=bet),color='black',shape=21,stroke = 1) +
    #Ponemos las etiquetas a los nodos
    geom_text(data = knodes10,aes(x = x*100-250,y = y*70,label=player_name),hjust=0, vjust=0,nudge_y = 100,size = 4,check_overlap = TRUE) +
    #Ponemos el valor de la densidad
    annotate("text", label = paste("Densidad del grafo: ",format(densidad, digits=2, nsmall=2)), x = 1000, y = 200, size = 5, colour = "red")+
    theme(legend.position="none")
  return(p)
}
