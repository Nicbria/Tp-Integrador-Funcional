module Backend exposing(..)
import Models exposing(Movie, Preferences)
import List exposing (all,any,length,map, reverse)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

buscarPorNombre : String -> List Movie ->List Movie
buscarPorNombre nombrePelicula = List.filter (dePalabraAListaDePalabrasUpper nombrePelicula)

dePalabraAListaDePalabrasUpper : String-> List String
dePalabraAListaDePalabrasUpper titulo = split " " (toUpper titulo) 

-- la idea es que el usuario le pasa "lIOn King" y nosotros pasamos eso a ["LION", "KING"]
-- luego buscamos esa lista en los titulos de las peliculas 

--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--
peliculaTienePalabrasClave palabras pelicula = String.contains palabras pelicula.title



filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (mismoGenero genero)

mismoGenero : String -> Movie -> Bool
mismoGenero genero pelicula = List.member genero pelicula.genre

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores peliculas = if mostrarSoloMenores then aptoParaMenores peliculas else peliculas

aptoParaMenores : List Movie -> List Movie
aptoParaMenores = List.filter .forKids

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating listaDePeliculas = reverse ((List.sortBy .rating) listaDePeliculas)

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = map (incrementarUnLike id)

incrementarUnLike : Int -> Movie -> Movie
incrementarUnLike id pelicula = if (id == pelicula.id) then {pelicula | likes = pelicula.likes + 1} else pelicula

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias peliculas= preferenciasPorActor + preferenciasPorGenero + preferenciasPorPalabraClave


preferenciasPorGenero : Preferences -> List Movie -> Int
preferenciasPorGenero preferencias peliculas =  preferencias.genre peliculas

preferenciasPorActriz : Preferences -> List Movie -> Int
preferenciasPorActor preferencias peliculas = preferencias.favoriteActor

preferenciasPorPalabraClave : Preferences -> List Movie -> Int
preferenciasPorPalabraClave preferencias peliculas = preferencias.keywords
