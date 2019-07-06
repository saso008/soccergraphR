#' Esta funcion crea el grafo de pases para un equipo durante un partido a partir de una
#' segmentacion kmeans de posiciones. Es decir, si un pase lo hace en la misma zona 2 jugadores
#' computan como 1, porque lo que se dibuja es el cluster en esa zona de pase independiente quien
#' lo haya hecho
#'
#' @param df Un df
#' @param home Un número de 1 y 0 que indica si es home o away
#' @param cl El número de clusters que queremos calcular en la red
#' @return El grafo de \code{df}  del equipo local si \code{home} con un total de \code{cl} clusters
#' @examples
#' OptaMAPmatrixpasscluster(df,1,20)
#' OptaMAPmatrixpasscluster(df,1,10)
#' @export
#'
#'
#'
OptaMAPmatrixpasscluster <- function(df,home,cl){

polar<-dplyr::select(df,type_id,team_id,outcome,x,y,player_id,"2","107","123","213","home_team_id","away_team_id","140","141")
#Cambiamos los nombres que los numeros no gustan
names(polar)<-c("type_id","team_id","outcome","x","y","player_id","a","b","c","d","e","f","g","h")
#convertimos a numérico las columnas Factor
polar$a <- as.numeric(as.character(polar$a))
polar$b <- as.numeric(as.character(polar$b))
polar$c <- as.numeric(as.character(polar$c))
polar$d <- as.numeric(as.character(polar$d))
polar$e <- as.numeric(as.character(polar$e))
polar$f <- as.numeric(as.character(polar$f))
polar$g <- as.numeric(as.character(polar$g))
polar$h <- as.numeric(as.character(polar$h))


if(home==1){
  polar <- dplyr::filter(polar,team_id==e)}
else{
  polar <- dplyr::filter(polar,team_id!=e)
}

#Nos quedamos los eventos de pase
polar <- dplyr::filter(polar,type_id==1 & is.na(a) & is.na(b) & is.na(c))

polar$yd<-polar$y*0.7
polar$hd<-polar$h*0.7

df1<-dplyr::select(polar,x,yd)
df2<-dplyr::select(polar,g,hd)
names(df2)<-c('x','yd')
matriz <- rbind(df1,df2)

set.seed(76964057) #Set the seed for reproducibility
k <-kmeans(matriz, centers=cl) #Create 5 clusters, Remove columns 1 and 2
k$centers #Display&nbsp;cluster centers
table(k$cluster) #Give a count of data points in each cluster
matriz_clusters<-as.data.frame(unlist(k$cluster))

polar_clusters<-merge(matriz_clusters,matriz,by="row.names",all.x=TRUE)
names(polar_clusters)<-c('VALUE',"cluster","x","yd")

matriz_centers<-as.data.frame(unlist(k$centers))
matriz_centers <- tibble::rownames_to_column(matriz_centers, "VALUE")

#hacer los joins.
polar1<-polar %>%
  dplyr::left_join(polar_clusters,by=c('x'='x', 'yd'='yd')) %>%
  dplyr::left_join(polar_clusters,by=c('g'='x', 'hd'='yd'))

polar1<-dplyr::select(polar1,type_id,team_id,x,y,player_id,g,yd,hd,cluster.x,cluster.y)
polar2<-distinct(polar1)

Equipo<-dplyr::distinct(polar2, team_id)

polar3<-polar2 %>%
  dplyr::group_by(cluster.x,cluster.y) %>%
  dplyr::summarise(conteo=n())

matriz_centers$VALUE<-as.integer(matriz_centers$VALUE)

polar4 <- polar3 %>%
  dplyr::inner_join(matriz_centers, by=c('cluster.x'='VALUE') )%>%
  dplyr::inner_join(matriz_centers, by=c('cluster.y'='VALUE') )

polar4<- dplyr::filter(polar4,cluster.x != cluster.y)

polar5<-polar3 %>%
  dplyr::group_by(cluster.x) %>%
  dplyr::summarise(suma=sum(conteo))

matriz_centers <- matriz_centers %>%
  dplyr::left_join(polar5, by=c('VALUE'='cluster.x') )

#Con este código podemos ver el número de clusters que podemos calcular
# rng<-2:30 #K from 2 to 20
# tries <-100 #Run the K Means algorithm 100 times
# avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
# for(v in rng){ # For each value of the range variable
#   v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
#   for(i in 1:tries){
#     k.temp <-kmeans(matriz,centers=v) #Run kmeans
#     v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
#   }
#   avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
# }
# plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
#      ylab="Average Total Within Sum of Squares",
#      xlab="Value of K")


h <- OptaMAPcampofutbol()
p <- h +
  ggtitle(paste("\nGrafo de Pases del kmeans de las posiciones de pase del ",Equipo[1])) +
  #Dibujamos los arcos entre jugadores
  geom_curve(data=polar4,aes(x=x.x*100, y=yd.x*100, xend = x.y*100, yend = yd.y*100,size=conteo,color=conteo),curvature = -0.2,arrow = arrow(length = unit(0.03, "npc")))+
  scale_color_gradient(low = "blue", high = "red")+
  #Dibujamos los nodos
  geom_point(data = matriz_centers,aes(x = x*100,y = yd*100,size = suma*2,fill=suma),color='black',shape=21,stroke = 1) +
  theme(legend.position="none")
return(p)
}
