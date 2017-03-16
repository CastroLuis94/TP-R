set.seed(385)

##A)
cantidadDePaquetes <- 0
for (i in 1:1000) {
albumCompleto <- (1:639)
    while(length(albumCompleto) != 0) { 
         paquete <- sample(c(1:639),5,T)
##A un álbum completo se le van sacando las que compré nuevas, en vez de ir agregándolas.
##Luego, la cantidad de paquetes va aumentando hasta que el album se complete, gracias al ciclo while.
        albumCompleto <- 
   albumCompleto[!is.element(albumCompleto,paquete)]  
        cantidadDePaquetes <- cantidadDePaquetes +1 
        }
    }
##finalmente, quiero la cantidad de paquetes que compré, dividido los 1000 experimentos, es decir, el promedio.
CantidadDePaquetesPromedioComprados=cantidadDePaquetes/1000
CantidadDePaquetesPromedioComprados


##B)
##Se realiza el mismo proceso que en A) con la diferencia que dentro del sampleo de figuritas que compramos no hay repetidas.
cantidadDePaquetes <- 0
for (i in 1:1000) {
albumCompleto <- (1:639)
    while(length(albumCompleto) != 0) { 
         paquete <- sample(c(1:639),5,F)
albumCompleto <- 
   albumCompleto[!is.element(albumCompleto,paquete)]  
        cantidadDePaquetes <- cantidadDePaquetes +1 
        }
    }
CantidadDePaquetesPromedioComprados=cantidadDePaquetes/1000
CantidadDePaquetesPromedioComprados


##C)
##inicializamos la cantidad de paquetes comprados en cero.
##Realizamos 1000 experimentos con un for.
cantidadDePaquetes <- 0
for (i in 1:1000) {
##El álbum está compuesto por 639 figuritas.
albumCompleto <- (1:639)
##mientras este no esté completo ya, 
##inicializo un paquete con 5 figuritas, sin repetidas, con las siguientes probabilidades (10 tienen una probabilidad de salir de 1/2000, y las demás, de (1-10/2000)/629).
    while(length(albumCompleto) != 0) { 
         paquete <- sample(c(1:639),5,F,prob=c(rep(1/2000,10),rep((1-10/2000)/629,629)))
##se las quita del album completo.
albumCompleto <- 
   albumCompleto[!is.element(albumCompleto,paquete)]  
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
##mientras que el álbum de Pedro o el de Gastón no esten completos
    while((length(albumCompletopedro) != 0) | (length(albumCompletogaston) !=0))  { 
##Si el álbum de Pedro no está completo, inicializamos los paquetes de Pedro con 5 figuritas, sin repetidas, y aumentamos su cantidad, así el while lo va llenando.
if (length(albumCompletopedro) != 0) (( paqueteDePedro <- sample((1:639),5,F)) &&  (cantidadDePaquetesP=cantidadDePaquetesP +1))   
##Las repetidas de Pedro están construídas de la siguiente forma: comparo el AlbumCompleto de Pedro, que todavía no está completo, con "albumVacio" , y me dan las figuritas que ya estan pegadas en albumCompletoPedro, y las concateno al nuevo paquete.
        repetidasP=c(albumVacio[!is.element(albumVacio,albumCompletopedro)],paqueteDePedro) 
##repeP se encarga de darme las duplicadas de las que tengo de Pedro, por lo tanto, me da las repetidas. 
        repeP=repetidasP[duplicated(repetidasP)]
##Mismo sistema, se sacan las figuritas del álbum para completarlo.
        albumCompletopedro <- albumCompletopedro[!is.element(albumCompletopedro,paqueteDePedro)]
        
##Mismo procedimiento con Gastón.
 if (length(albumCompletogaston) != 0) (( paqueteDeGaston <- sample((1:639),5,F)) &&  (cantidadDePaquetesG=cantidadDePaquetesG +1))        
repetidasG=c(albumVacio[!is.element(albumVacio,albumCompletogaston)],paqueteDeGaston) 
repeG=repetidasG[duplicated(repetidasG)]
            albumCompletogaston <- albumCompletogaston[!is.element(albumCompletogaston,paqueteDeGaston)]  
##se sacan las repetidas de Pedro del albumCompleto de Gastón, y viceversa.
        albumCompletopedro= albumCompletopedro[!is.element(albumCompletopedro,repeG)]
        albumCompletogaston= albumCompletogaston[!is.element(albumCompletogaston,repeP)]
    }
##El primer álbum (que se completa) es el que posee el mínimo entre la cantidad de Paquetes Comprados por Pedro y por Gastón.
##El segundo álbum que se completó es el que tuvo el máximo entre estos últimos.
      primerAlbum=c(primerAlbum,min(cantidadDePaquetesP,cantidadDePaquetesG))
      segundoAlbum=c(segundoAlbum,max(cantidadDePaquetesP,cantidadDePaquetesG))
        }    
##Queremos el promedio de la cantidad de paquetes que se compraron (dependiendo si fueron de Pedro o de Gastón) dividido las 1000 repeticiones.
##Y lo mismo para el segundo álbum.
promedioPrimerAlbum=sum(primerAlbum)/1000
promedioSegundoAlbum=sum(segundoAlbum)/1000                     
                      
promedioPrimerAlbum
promedioSegundoAlbum
