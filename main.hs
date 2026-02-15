import Data.List (intersect, sortBy, find)
import System.IO

--Deficion de datos como turistas, ciudades y conexiones

data Lugar = Lugar {
    nombreLugar :: String,
    intereses :: [String],
    estaAbierto :: Bool
} deriving (Show, Eq)

data Turista = Turista {
    nombreTurista :: String,
    nacionalidad :: String,
    preferencias :: [String]
} deriving (Show)

type Conexion = (String, String)
type Ruta = [String]

--Demostraciones de funcionamiento

lugares :: [Lugar]
lugares = [
    Lugar "Medellin" ["innovacion", "cultura", "gastronomia"] True,
    Lugar "Guatape" ["naturaleza", "paisaje", "pueblo"] True,
    Lugar "Jardin" ["naturaleza", "cafe", "pueblo"] True,
    Lugar "Santa Fe de Antioquia" ["historia", "pueblo", "calor"] False, --el false hace referencia a que el destino esta cerrado
    Lugar "Cartagena" ["playa", "historia", "gastronomia"] True,
    Lugar "Bogota" ["museo", "cultura", "gastronomia"] True,
    Lugar "Salento" ["naturaleza", "cafe", "paisaje"] True
    ]

conexiones :: [Conexion]
conexiones = [
    ("Medellin", "Guatape"),
    ("Medellin", "Jardin"),
    ("Medellin", "Bogota"),
    ("Bogota", "Cartagena"),
    ("Jardin", "Salento"),
    ("Guatape", "Santa Fe de Antioquia")
    ]

--Conejillos de indias

turistas :: [Turista]
turistas = [
    Turista "Jeffrey E" "Americano" ["naturaleza", "gastronomia", "innovacion"],
    Turista "Pulisic" "Americano" ["playa", "historia", "cultura"],
    Turista "Tony" "Aleman" ["historia", "naturaleza", "paisaje"],
    Turista "Karim" "Frances" ["gastronomia", "cultura", "cafe"],
    Turista "Seishiro Nagi" "Japones" ["cultura", "innovacion", "naturaleza", "paisaje"]
    ]

-- Uso de las funciones que pide la practica

estanConectados :: String -> String -> [Conexion] -> Bool
estanConectados ciudadA ciudadB listaConexiones =
    (ciudadA, ciudadB) `elem` listaConexiones || (ciudadB, ciudadA) `elem` listaConexiones

filtrarDestinosAbiertos :: [Lugar] -> [Lugar]
filtrarDestinosAbiertos = filter estaAbierto

calcularAfinidad :: Turista -> Lugar -> Int
calcularAfinidad turista lugar = length (intersect (preferencias turista) (intereses lugar))

recomendarDestinos :: Turista -> [Lugar] -> [Lugar]
recomendarDestinos turista listaLugares =
    let
        lugaresAfines = filter (\lugar -> calcularAfinidad turista lugar > 0) listaLugares
        ordenarPorAfinidad l1 l2 = compare (calcularAfinidad turista l2) (calcularAfinidad turista l1)
    in
        sortBy ordenarPorAfinidad lugaresAfines

buscarRutas :: String -> String -> [Conexion] -> [Ruta]
buscarRutas origen destino listaConexiones = buscarRutasAux origen []
    where
        buscarRutasAux actual visitados
            | actual == destino = [[actual]]
            | actual `elem` visitados = []
            | otherwise =
                let
                    nuevosVisitados = actual : visitados
                    vecinos = [c | (c1, c2) <- listaConexiones, c <- [c1, c2], (c1 == actual && c2 /= actual) || (c2 == actual && c1 /= actual), c `notElem` nuevosVisitados]
                    rutasDesdeVecinos = concatMap (`buscarRutasAux` nuevosVisitados) vecinos
                in
                    map (actual :) rutasDesdeVecinos

buscarRutasSoloAbiertas :: String -> String -> [Conexion] -> [Lugar] -> [Ruta]
buscarRutasSoloAbiertas origen destino listaConexiones listaLugares =
    let
        lugaresAbiertos = map nombreLugar (filtrarDestinosAbiertos listaLugares)
        conexionesAbiertas = filter (\(c1, c2) -> c1 `elem` lugaresAbiertos && c2 `elem` lugaresAbiertos) listaConexiones
    in
        buscarRutas origen destino conexionesAbiertas



guardarRutasEnArchivo :: FilePath -> [Ruta] -> IO ()
guardarRutasEnArchivo archivo rutas = writeFile archivo (unlines (map show rutas))

leerRutasDesdeArchivo :: FilePath -> IO [Ruta]
leerRutasDesdeArchivo archivo = do
    contenido <- readFile archivo
    return (map read (lines contenido))


main :: IO ()
main = do
    putStrLn "======================================================"
    putStrLn "= SISTEMA DE PLANIFICACIÓN DE RUTAS TURÍSTICAS EAFIT ="
    putStrLn "======================================================"
    
    
    putStrLn "\n-- Lugares abiertos al publico --"
    let abiertos = filtrarDestinosAbiertos lugares
    print (map nombreLugar abiertos)
    
    
    putStrLn "\n-- Recomendaciones basadas en los gustos de los turistas --"
    
    mapM_ (\turista -> do
        putStrLn ("\nRecomendaciones para " ++ nombreTurista turista ++ " (" ++ nacionalidad turista ++ "):")
        let recomendaciones = recomendarDestinos turista lugares
        
        print (map (\l -> (nombreLugar l, calcularAfinidad turista l)) recomendaciones)
        ) turistas
        
    
    putStrLn "\n-- Busqueda de posibles rutas --"
    putStrLn "\nRutas de Medellin a Cartagena (incluyendo lugares cerrados en el camino):"
    let rutasPosibles = buscarRutas "Medellin" "Cartagena" conexiones
    print rutasPosibles
    
    
    putStrLn "\n-- Busqueda de rutas de lugares abiertos para el publico --"
    putStrLn "\nRutas de Medellin a Salento (solo destinos abiertos):"
    let rutasAbiertas = buscarRutasSoloAbiertas "Medellin" "Salento" conexiones lugares
    print rutasAbiertas
    
   
    putStrLn "\n-- Guardando --"
    let archivoRutas = "rutas_recomendadas_practica_1_angel_quimbayo_david_ruiz.txt"
    if not (null rutasAbiertas) then do
        putStrLn $ "Guardando la primera ruta recomendada en " ++ archivoRutas ++ "..."
        guardarRutasEnArchivo archivoRutas [head rutasAbiertas] 
        putStrLn "Se guardo"
        
        putStrLn $ "\nLeyendo rutas desde " ++ archivoRutas ++ "..."
        rutasLeidas <- leerRutasDesdeArchivo archivoRutas
        putStrLn "Rutas leídas del archivo:"
        print rutasLeidas
    else
        putStrLn "No se encontraron rutas."