# procesamiento base de datos TES  ####

# librerias ------
library(tidyr)
library(ggplot2)
library(lubridate)
library(visdat)
library(stringr)

# library(googledrive)
# library(googlesheets)
library(igraph)
library(ggraph)
library(dplyr)
library(wesanderson)

# leer el archivo -------

expos_44<- read.csv("data/eventos_unicos.csv")

los44<-c("Miguel Ángel Rojas",
    "Pablo Van Wong",
    "Ever Astudillo",
    "Cristina Llano",
    "Ana Claudia Múnera",
    "Ana María Rueda",
    "Óscar Muñoz",
    "Luis Fernando Roldán",
    "Álvaro Barrios",
    "Leonardo Herrera",
    "Jorge Montealegre",
    "Jorge Acero Laschevsky",
    "José Horacio Martínez",
    "José Antonio Suárez",
    "Alicia Barney",
    "Luz Ángela Lizarazo",
    "María Fernanda Cardoso",
    "Carlos Arturo Salas Silva",
    "Elías Heim",
    "Rafael Ortiz",
    "Carlos Arturo Salazar Silva",
    "Vivian Monsalve",
    "Wilson Díaz",
    "Ana María Millán",
    "Carlos Andrade",
    "Delcy Morelos",
    "Rodrigo Facundo",
    "Antonio José Caro",
    "César Alfaro Mosquera",
    "María Teresa Hincapié",
    "Rosemberg Sandoval",
    "Juan Fernando Mejía",
    "Bernardo Ortiz",
    "Eduardo Mondragón",
    "Johanna Calle",
    "Viviana Moncayo",
    "Alejandra Gutiérrez",
    "Carlos Duque",
    "Ernesto Ordoñez",
    "Juan David Medina",
    "Lucas Ospina",
    "María Evelia Marmolejo",
    "Oswaldo Maciá",
    "Rosario López")

participacion_expo<-function(art){
    if(is.na(art) )
        return(NA)
    sp<-strsplit(as.character(art), ",")
    
    if(lengths(sp)==1 & trimws(sp[1])!="Varios")
        return("individual")
    
    if(lengths(sp)==1 & trimws(sp[1])=="Varios")
        return("colectiva")
    
    if(lengths(sp) >1 )
        return("colectiva")
    
}

contar_artsitas<-function(art){
    if(is.na(art) )
        return(NA)
    sp<-strsplit(as.character(art), ",")
    
    if(lengths(sp)==1 & trimws(sp[1])=="Varios")
        return(NA)
    
    return(lengths(sp))
    
}


expos_44.std<- expos_44 %>% rowwise() %>%
    # mutate(tipo_participacion = participacion_expo(artistas_evento_std) ) %>% 
    mutate(nombre_evento_std_ano= paste(nombre_evento_std,año_inicio, sep = "-"))%>%
    select(id_evento_std,nombre_evento_std_ano,nombre_evento_std,tipo_evento_participacion, artistas_evento_std)



expos_44.std%>%
    select(nombre_evento_std_ano,artistas_evento_std, tipo_evento_participacion) %>%
    separate_rows(artistas_evento_std,sep = ";") %>% 
    mutate_if( is.character, funs(trimws(.))) %>% 
    distinct()%>%
    filter(tipo_evento_participacion=="colectiva") %>%
    filter(artistas_evento_std != "")-> tmp_enlaces

tmp_enlaces %>%
    select(nodo=artistas_evento_std) %>%
    mutate(rol=if_else(nodo %in% los44,"los 44","artistas"))%>%
    distinct()->nodos_artistas
tmp_enlaces %>%
    select(nodo=nombre_evento_std_ano) %>%
    mutate(rol="expos")%>%
    distinct() ->nodos_expos

nodos<-bind_rows(nodos_expos,nodos_artistas)
enlaces<-tmp_enlaces %>% 
    select(source=artistas_evento_std,target=nombre_evento_std_ano,tipo=tipo_evento_participacion) 


enlaces %>%
    rename(artista=source)%>%
    group_by(artista) %>%
    summarise(participaciones=n()) %>%
    mutate(grupo=if_else(artista %in% los44,"los 44","otros artistas"))%>%
    arrange(desc(participaciones))->actividad.artistas


# actividad.artistas[1:10,] %>% View()

actividad.artistas[1:40,] %>%
    group_by(grupo) %>%
    summarise(cantidad=n())

grafoEA<-graph_from_data_frame(enlaces,
                                          directed = T, 
                                          vertices = nodos)


tipo<-if_else(V(grafoEA)$name %in% actividad.artistas[1:50,]$artista &
                  !(V(grafoEA)$name %in% los44),
              "relevante",
              V(grafoEA)$rol)
label_nodes <-if_else(V(grafoEA)$name %in% actividad.artistas[1:50,]$artista &
                          !(V(grafoEA)$name %in% los44),
                      V(grafoEA)$name,"")

# tam_nodo<-if_else(V(grafoEA)$rol =="expos",5,degree(grafoEA))
tam_nodo<-degree(grafoEA)
p <- ggraph(grafoEA, layout = "fr", niter = 1500) + 
    geom_edge_link(
        # aes(width=peso_enlace),
                   # arrow = arrow(length = unit(1, 'mm')), 
                   # end_cap = circle(1, 'mm'),
                   # start_cap=circle(1, 'mm'),
                   edge_colour= "grey80") + 
    geom_node_point(aes(color = factor(tipo), 
                        # size = degree(grafoEA)
                        size = tam_nodo
                        )) +
    geom_node_text(aes(label = label_nodes),
                   repel = T 
                   # nudge_x = nchar(label_nodes)/10
    )+
    # coord_fixed()+
    scale_edge_width(range = c(0.1,1), guide = "none")+
    scale_color_manual(name ="Tipo nodo",values = wes_palette("Darjeeling1", n = length(tipo%>%unique())))+
    scale_size(name = "grado",
                range = c(0.1,7)
               )+
    ggtitle('Artistas más activos en el circuito de los 44',
            subtitle = "Los nodos con nombre están el top 50 pero no pertenecen a los 44")    

p+theme_graph() 

ggsave("g-TES-mas-activos-Circuito44.svg",device = "svg", units = "cm", width = 40, height = 30 )
ggsave("g-TES-mas-activos-Circuito44.png",device = "png", units = "cm", width = 40, height = 30 )
