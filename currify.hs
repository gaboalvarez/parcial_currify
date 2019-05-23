import Text.Show.Functions
data Cancion = Cancion{titulo::String,genero::String,duracion::Int}deriving(Show)
data Artista = Artista{nombre::String,canciones::[Cancion],efectoPreferido::Efecto}deriving(Show)
type Efecto = Cancion->Cancion

cancion1 = Cancion{titulo="cafe para dos",genero="rock melancolico",duracion=146}
cancion2 = Cancion{titulo="rocket raccoon",genero="rock",duracion=130}
cancion3 = Cancion{titulo="mientras mi bateria festeja",genero="cumbia",duracion=200}
cancion4 = Cancion{titulo="tomate de madera",genero="rock",duracion=140}
artista1 = Artista{nombre="los escarabajos",canciones=[cancion2,cancion3,cancion4],efectoPreferido=acortar}
artista2 = Artista{nombre="adela",canciones=[cancion5,cancion6,cancion7],efectoPreferido=remixar}
cancion5 = Cancion{titulo="te acordas?",genero="jazz",duracion=180}
cancion6 = Cancion{titulo="un pibe como vos",genero="cumbia",duracion=146}
cancion7 = Cancion{titulo="dale mecha a la lluvia",genero="pop",duracion=100}
artista3 = Artista{nombre="david guetta",canciones=[cancion1,cancion2,cancion3,cancion4,cancion5,cancion6,cancion7],efectoPreferido=acortar}

--acortar::Efecto
acortar unaCancion = unaCancion{duracion = max 0 ((duracion unaCancion)-60)}
--remixar::Efecto
remixar unaCancion = unaCancion{titulo=(titulo unaCancion)++" remix",duracion= 2*(duracion unaCancion),genero="remixado"}
--acustizar::Int->Efecto
acustizar duracionNueva unaCancion | (genero unaCancion)=="acustico" = unaCancion
 | otherwise = unaCancion{duracion=duracionNueva,genero="acustico"}
--metaEfecto::[Efecto]->Efecto
metaEfecto efectos unaCancion = (combinarEfectos efectos) unaCancion

combinarEfectos efectos = foldl1 (.) efectos

--corta::Cancion->Bool
corta unaCancion = (duracion unaCancion) < 150
--vistazo::Artista->[Cancion]
vistazo unArtista = take 3 (cancionesCortas (canciones unArtista))
--cancionesCortas::[Cancion]->[Cancion]
cancionesCortas canciones = filter corta canciones

--playlist::String->[Artista]->[Cancion]
playlist generoFiltro [] = []
playlist generoFiltro artistas = (filtrarGeneros generoFiltro ((canciones.head)artistas))++(playlist generoFiltro (tail artistas))
--filtrarGeneros::String->[Cancion]->[Cancion]
filtrarGeneros generoFiltro canciones = filter (esGenero generoFiltro) (canciones)
--esGenero::String->Cancion->Bool
esGenero generoFiltro cancion = generoFiltro == (genero cancion)

--hacerseDJ::Artista->Artista
hacerseDJ unArtista = unArtista{canciones = map (efectoPreferido unArtista) (canciones unArtista)}

--tieneGustoHomogeneo::Artista->Bool
tieneGustoHomogeneo unArtista = (length(canciones unArtista))==(length(filtrarGeneros ((genero.head.canciones) unArtista) (canciones unArtista)))

--formarBanda::String->[Artista]->Artista
formarBanda nombreBanda artistas = Artista{nombre=nombreBanda,canciones= foldl1 (++) (map canciones artistas),efectoPreferido= combinarEfectos (map efectoPreferido artistas)}

--obraMaestraProgresiva devuelve la mejor cancion (teniendo en cuenta los 3 criterios de la ultima pagina)
--obraMaestraProgresiva::Artista->Cancion
obraMaestraProgresiva unArtista = revisarCanciones (canciones unArtista)
--revisarCanciones::[Cancion]->Cancion
revisarCanciones [x] = x
revisarCanciones (x:y:xs) = revisarCanciones ((mejor x y):xs)
--mejor::Cancion->Cancion->Cancion
mejor cancion1 cancion2 | ((genero cancion1)=="rock")&&((genero cancion2)/="rock") = cancion1
 | ((genero cancion1)/="rock")&&((genero cancion2)=="rock") = cancion2
 | ((genero cancion1)=="reggaeton")&&((genero cancion2)/="reggaeton") = cancion2
 | ((genero cancion1)/="reggaeton")&&((genero cancion2)=="reggaeton") = cancion1
 | (length (genero cancion1))>(length(genero cancion2)) = cancion1
 | otherwise = cancion2