##Trabajo practico de Probabilidad y Estadistica(c)
##Enunciado:
## http://cms.dm.uba.ar/academico/materias/1ercuat2015/probabilidades_y_estadistica_C/TP.txt

set.seed(385)

##A)
cantidadDePaquetes <- 0
for (i in 1:1000) {
    albumCompleto <- (1:639)
    while(length(albumCompleto) != 0) { 
         paquete <- sample(c(1:639),5,T)
         albumCompleto <- albumCompleto[!is.element(albumCompleto,paquete)]  
         cantidadDePaquetes <- cantidadDePaquetes +1 
    }
}
CantidadDePaquetesPromedioComprados=cantidadDePaquetes/1000
CantidadDePaquetesPromedioComprados


##B)
##Se realiza el mismo proceso que en A) con la diferencia que dentro del sampleo de figuritas que compramos no hay repetidas.
cantidadDePaquetes <- 0
for (i in 1:1000) {
    albumCompleto <- (1:639)
    while(length(albumCompleto) != 0) { 
         paquete <- sample(c(1:639),5,F)
         albumCompleto <- albumCompleto[!is.element(albumCompleto,paquete)]  
         cantidadDePaquetes <- cantidadDePaquetes +1 
    }
}
CantidadDePaquetesPromedioComprados=cantidadDePaquetes/1000
CantidadDePaquetesPromedioComprados


##C)
cantidadDePaquetes <- 0
for (i in 1:1000) {
    albumCompleto <- (1:639)
##mientras este no esté completo ya, 
##inicializo un paquete con 5 figuritas, sin repetidas, con las siguientes probabilidades (10 tienen una probabilidad de salir de 1/2000, y las demás, de (1-10/2000)/629).
    while(length(albumCompleto) != 0) { 
         paquete <- sample(c(1:639),5,F,prob=c(rep(1/2000,10),rep((1-10/2000)/629,629)))
         albumCompleto <- albumCompleto[!is.element(albumCompleto,paquete)]  
         cantidadDePaquetes <- cantidadDePaquetes +1 
    }
}
##Finalmente, quiero la cantidad de paquetes que se necesitaron para sacarlas a todas, o lo que es lo mismo, llenarlo, con 10 figuritas "difíciles".
CantidadDePaquetesPromedioComprados=cantidadDePaquetes/1000
CantidadDePaquetesPromedioComprados


##D)
##inicializamos un álbum "primero" y uno "segundo" en cero.
primerAlbum=NULL
segundoAlbum=NULL
##inicializamos una variable "albumVacio", que va a ser usada para comparar con un album que estemos llenando.
albumVacio=c(1:639)
for (i in 1:1000) {
##Decimos que los paquetes que compraron tanto Pedro como Gastón son 0 por ahora,que la cantidad de paquetes que compraron todavía es 0.
    paquetePedro = 0
    paqueteGaston= 0
    albumCompletopedro <- (1:639)
    albumCompletogaston<- (1:639)    
    cantidadDePaquetesP <- 0
    cantidadDePaquetesG <- 0    
    while((length(albumCompletopedro) != 0) | (length(albumCompletogaston) !=0))  { 
        if (length(albumCompletopedro) != 0) (( paqueteDePedro <- sample((1:639),5,F)) &&  (cantidadDePaquetesP=cantidadDePaquetesP +1))   
            repetidasP=c(albumVacio[!is.element(albumVacio,albumCompletopedro)],paqueteDePedro) 
            repeP=repetidasP[duplicated(repetidasP)]
            albumCompletopedro <- albumCompletopedro[!is.element(albumCompletopedro,paqueteDePedro)]
        if (length(albumCompletogaston) != 0) (( paqueteDeGaston <- sample((1:639),5,F)) &&  (cantidadDePaquetesG=cantidadDePaquetesG +1))        
            repetidasG=c(albumVacio[!is.element(albumVacio,albumCompletogaston)],paqueteDeGaston) 
            repeG=repetidasG[duplicated(repetidasG)]
            albumCompletogaston <- albumCompletogaston[!is.element(albumCompletogaston,paqueteDeGaston)]  
        albumCompletopedro= albumCompletopedro[!is.element(albumCompletopedro,repeG)]
        albumCompletogaston= albumCompletogaston[!is.element(albumCompletogaston,repeP)]
        }
     primerAlbum=c(primerAlbum,min(cantidadDePaquetesP,cantidadDePaquetesG))
     segundoAlbum=c(segundoAlbum,max(cantidadDePaquetesP,cantidadDePaquetesG))
 }    
promedioPrimerAlbum=sum(primerAlbum)/1000
promedioSegundoAlbum=sum(segundoAlbum)/1000                     
                      
promedioPrimerAlbum
promedioSegundoAlbum
