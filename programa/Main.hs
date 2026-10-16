import Data.Time (Day, UTCTime, getCurrentTime, utctDay, addDays, diffDays, toGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime)
import Data.List (find, sortBy, sort)
import Data.Char (isSpace)
import Control.Exception (IOException, try, evaluate)
import Text.Read (readMaybe)


-- ==========================================
-- ESTRUCTURAS DEL SISTEMA
-- ==========================================


-- Aqui defino las estructuras donde guardo los datos del hotel y sus reservas.
data Tipo = Tipo String String Int deriving (Show)

data Habitacion = Habitacion Int String deriving (Show)

data Ocupacion = Ocupacion Int String Int Int deriving (Show)

data Reserva = Reserva Int String UTCTime Day Day [Ocupacion] Double String deriving (Show)

data Factura = Factura Int Int Double Double Double deriving (Show)

data Hotel = Hotel String String String String String String deriving (Show)

data Sistema = Sistema Hotel [Tipo] [Habitacion] [Double] [Reserva] [Factura] deriving (Show)


-- Aqui preparo un sistema de ejemplo por si no se encuentran los datos del hotel.
sistemaInicial :: Sistema
sistemaInicial = Sistema
    (Hotel "Hotel de Ejemplo" "0000000000" "www.ejemplo.com"
           "0000-0000" "Costa Rica" "Limon")
    [] [] [] [] []


-- ==========================================
-- LECTURA Y VALIDACION DE DATOS
-- ==========================================


-- Quito los espacios al principio y al final de un texto antes de usarlo.
limpiar :: String -> String
limpiar = reverse . dropWhile isSpace . reverse . dropWhile isSpace


-- Separo una linea del CSV por comas y devuelvo cada dato en una lista.
separar :: String -> [String]
separar [] = [""]
separar (',':xs) = "" : separar xs
separar (x:xs) =
    let (a:as) = separar xs
    in (x:a):as


-- Intento convertir un texto a numero entero; si no se puede, recibo Nothing.
entero :: String -> Maybe Int
entero = readMaybe . limpiar


-- Hago la misma conversion anterior, pero esta vez para numeros decimales.
decimal :: String -> Maybe Double
decimal = readMaybe . limpiar


-- Muestro una pregunta y leo lo que la persona escribe en la consola.
preguntar :: String -> IO String
preguntar pregunta = do
    putStrLn pregunta
    getLine


-- Pido un entero mayor o igual a cero y lo vuelvo a pedir si es incorrecto.
leerNatural :: String -> IO Int
leerNatural mensaje = do
    valor <- preguntar mensaje
    case entero valor of
        Just n | n >= 0 -> return n
        _ -> do
            putStrLn "Ingrese un numero entero mayor o igual a cero."
            leerNatural mensaje


-- Convierto un texto con formato AAAA-MM-DD en una fecha valida.
fecha :: String -> Maybe Day
fecha = parseTimeM True defaultTimeLocale "%Y-%m-%d" . limpiar


-- Pido una fecha y sigo preguntando hasta que tenga el formato correcto.
leerFecha :: String -> IO Day
leerFecha mensaje = do
    texto <- preguntar mensaje
    case fecha texto of
        Just d -> return d
        Nothing -> do
            putStrLn "Fecha invalida. Use AAAA-MM-DD."
            leerFecha mensaje


-- Leo el archivo indicado y controlo el error si no existe o no puedo abrirlo.
leerArchivo :: FilePath -> IO (Either String String)
leerArchivo ruta = do
    resultado <- try (do
        contenido <- readFile ruta
        _ <- evaluate (length contenido)
        return contenido) :: IO (Either IOException String)

    case resultado of
        Left errorArchivo -> return (Left (show errorArchivo))
        Right contenido -> return (Right contenido)


-- Reviso los tres datos de una linea del CSV y creo un tipo de habitacion valido.
convertirTipo :: String -> Maybe Tipo
convertirTipo linea =
    case map limpiar (separar linea) of
        [nombre, descripcion, maximo] ->
            case entero maximo of
                Just n | n > 0 && nombre /= "" && descripcion /= "" ->
                    Just (Tipo nombre descripcion n)

                _ -> Nothing

        _ -> Nothing


-- Obtengo solo el nombre de un tipo de habitacion.
nombreTipo :: Tipo -> String
nombreTipo (Tipo n _ _) = n


-- Obtengo la capacidad maxima permitida para un tipo de habitacion.
limiteTipo :: Tipo -> Int
limiteTipo (Tipo _ _ n) = n


-- Obtengo el tipo al que pertenece una habitacion.
nombreHabitacion :: Habitacion -> String
nombreHabitacion (Habitacion _ n) = n


-- Obtengo el numero que identifica una habitacion.
idHabitacion :: Habitacion -> Int
idHabitacion (Habitacion i _) = i


-- Obtengo el numero de habitacion guardado en una ocupacion.
idOcupacion :: Ocupacion -> Int
idOcupacion (Ocupacion i _ _ _) = i


-- Obtengo la cantidad de adultos de una ocupacion.
adultosOcupacion :: Ocupacion -> Int
adultosOcupacion (Ocupacion _ _ a _) = a


-- Obtengo la cantidad de ninos de una ocupacion.
ninosOcupacion :: Ocupacion -> Int
ninosOcupacion (Ocupacion _ _ _ n) = n


-- ==========================================
-- CARGA DE TIPOS Y HABITACIONES
-- ==========================================


-- Leo el CSV de tipos, ignoro las lineas incorrectas y no agrego nombres repetidos.
cargarTipos :: Sistema -> IO Sistema
cargarTipos sis@(Sistema hotel tipos hab tarifas reservas facturas) = do
    ruta <- preguntar "Ruta del archivo de tipos (ej: datos/tipos.csv):"
    archivo <- leerArchivo ruta
    case archivo of
        Left errorTexto -> do
            putStrLn ("No se pudo leer: " ++ errorTexto)
            return sis

        Right contenido -> do
            let nuevos =
                    [tipo | linea <- lines contenido,
                            Just tipo <- [convertirTipo linea]]

            let agregar lista tipo =
                    if any ((== nombreTipo tipo) . nombreTipo) lista then
                        lista
                    else
                        lista ++ [tipo]

            let finales = foldl agregar tipos nuevos

            putStrLn ("Tipos nuevos agregados: " ++ show (length finales - length tipos))
            return (Sistema hotel finales hab tarifas reservas facturas)


-- Pido cuantas habitaciones hay por tipo y les asigno numeros desde uno.
asignarHabitaciones :: Sistema -> IO Sistema
asignarHabitaciones sis@(Sistema hotel tipos hab tarifas reservas facturas)
    | not (null hab) = do
        putStrLn "Las habitaciones ya fueron generadas."
        return sis

    | null tipos = do
        putStrLn "Primero debe cargar tipos."
        return sis

    | otherwise = do
        putStrLn "Indique la cantidad para cada tipo."
        cantidades <- mapM (\t -> leerNatural (nombreTipo t ++ ":")) tipos

        let nombres = concat
                [replicate cantidad (nombreTipo t)
                | (t, cantidad) <- zip tipos cantidades]

        if null nombres then do
            putStrLn "Debe generar al menos una habitacion."
            return sis
        else do
            let nuevas = zipWith Habitacion [1..] nombres
            putStrLn ("Habitaciones generadas: " ++ show (length nuevas))
            return (Sistema hotel tipos nuevas tarifas reservas facturas)


-- Muestro en pantalla cada habitacion con su numero y su tipo.
verHabitaciones :: Sistema -> IO ()
verHabitaciones (Sistema _ _ habitaciones _ _ _) = do
    if null habitaciones then
        putStrLn "No hay habitaciones."
    else
        mapM_ (\(Habitacion i n) ->
            putStrLn (show i ++ " - " ++ n)) habitaciones


-- ==========================================
-- TARIFAS Y PRECIOS
-- ==========================================


-- Convierto una linea del CSV en codigo y precio, validando los valores.
convertirTarifa :: String -> Maybe (Int, Double)
convertirTarifa linea =
    case map limpiar (separar linea) of
        [codigo, precio] ->
            case (entero codigo, decimal precio) of
                (Just i, Just p) | i >= 1 && i <= 8 && p >= 0 ->
                    Just (i, p)

                _ -> Nothing

        _ -> Nothing


-- Cargo las ocho tarifas solo si estan los codigos del 1 al 8 sin repetirse.
cargarTarifas :: Sistema -> IO Sistema
cargarTarifas sis@(Sistema hotel tipos hab _ reservas facturas) = do
    ruta <- preguntar "Ruta del archivo de tarifas (ej: datos/tarifas.csv):"
    archivo <- leerArchivo ruta
    case archivo of
        Left errorTexto -> do
            putStrLn ("No se pudo leer: " ++ errorTexto)
            return sis

        Right contenido -> do
            let lineas = filter (not . null . limpiar) (lines contenido)
            let pares =
                    [par | linea <- lineas,
                           Just par <- [convertirTarifa linea]]

            if length pares /= 8 || sort (map fst pares) /= [1..8] then do
                putStrLn "Archivo invalido: debe contener exactamente los codigos 1 al 8, sin repetidos."
                return sis
            else do
                let valores =
                        [snd (head (filter ((== i) . fst) pares))
                        | i <- [1..8]]

                putStrLn "Tarifas cargadas y reemplazadas."
                return (Sistema hotel tipos hab valores reservas facturas)


-- Identifico la temporada y el dia para elegir la tarifa de adultos correspondiente.
-- Uso codigos 1-4 para temporada verde y 5-8 para temporada alta.
indiceTarifa :: Day -> Int
indiceTarifa dia =
    let
        (_, mes, _) = toGregorian dia
        semana = read (formatTime defaultTimeLocale "%u" dia) :: Int
        alta = mes >= 11 || mes <= 3
        finSemana = semana == 5 || semana == 6
    in
        1 + (if alta then 4 else 0) + (if finSemana then 2 else 0)


-- Sumo lo que pagan adultos y ninos por cada noche, sin incluir el dia de salida.
-- La tarifa par que sigue a cada codigo de adulto corresponde a los ninos.
calcularSubtotal :: [Double] -> Day -> Day -> [Ocupacion] -> Double
calcularSubtotal tarifas entrada salida ocupaciones =
    sum
        [ fromIntegral (adultosOcupacion o) * (tarifas !! (codigo - 1))
          + fromIntegral (ninosOcupacion o) * (tarifas !! codigo)
        | dia <- take (fromIntegral (diffDays salida entrada))
                      (iterate (addDays 1) entrada)
        , o <- ocupaciones
        , let codigo = indiceTarifa dia
        ]


-- ==========================================
-- DISPONIBILIDAD DE HABITACIONES
-- ==========================================


-- Comparo dos rangos de fechas para saber si comparten alguna noche.
cruzan :: Day -> Day -> Day -> Day -> Bool
cruzan inicio1 fin1 inicio2 fin2 = inicio1 < fin2 && inicio2 < fin1


-- Reviso si una reserva sigue bloqueando la habitacion; las canceladas no bloquean.
bloquea :: Reserva -> Bool
bloquea (Reserva _ _ _ _ _ _ _ estado) = estado /= "Cancelada"


-- Busco si alguna reserva ocupa esa habitacion dentro de las fechas solicitadas.
estaDisponible :: [Reserva] -> Day -> Day -> Int -> Bool
estaDisponible reservas entrada salida codigo =
    not (any ocupada reservas)
  where
    ocupada (Reserva _ _ _ inicio fin ocupaciones _ estado) =
        estado /= "Cancelada" &&
        cruzan entrada salida inicio fin &&
        any ((== codigo) . idOcupacion) ocupaciones


-- Reviso cada habitacion en una fecha y busco el codigo de reserva que la ocupa.
estadoDia :: Sistema -> Day -> [(Habitacion, Maybe Int)]
estadoDia (Sistema _ _ habitaciones _ reservas _) dia =
    [(h, reservaDia (idHabitacion h)) | h <- habitaciones]
  where
    reservaDia codigo =
        case [i | Reserva i _ _ ini fin os _ est <- reservas,
                  est /= "Cancelada",
                  dia >= ini,
                  dia < fin,
                  any ((== codigo) . idOcupacion) os] of
            (x:_) -> Just x
            [] -> Nothing


-- Pido un dia y enseño solamente las habitaciones que estan libres.
mostrarDisponibilidad :: Sistema -> IO ()
mostrarDisponibilidad sis = do
    dia <- leerFecha "Dia a consultar (AAAA-MM-DD):"
    let disponibles =
            [(h, codigo) | (h, codigo) <- estadoDia sis dia,
                           codigo == Nothing]

    if null disponibles then
        putStrLn "No hay habitaciones disponibles."
    else
        mapM_ (\(Habitacion i nombre, _) ->
            putStrLn (show i ++ " - " ++ nombre)) disponibles


-- Muestro si cada habitacion esta libre u ocupada y la reserva correspondiente.
consultarOcupacion :: Sistema -> IO ()
consultarOcupacion sis = do
    dia <- leerFecha "Dia a consultar (AAAA-MM-DD):"
    mapM_ imprimir (estadoDia sis dia)
  where
    imprimir (Habitacion i nombre, Nothing) =
        putStrLn (show i ++ " - " ++ nombre ++ " - Disponible")

    imprimir (Habitacion i nombre, Just codigo) =
        putStrLn (show i ++ " - " ++ nombre ++
                  " - Ocupada / reserva " ++ show codigo)


-- Muestro los datos de una habitacion junto con sus adultos y ninos.
imprimirOcupacion :: Ocupacion -> IO ()
imprimirOcupacion (Ocupacion i tipo a n) =
    putStrLn ("  Habitacion " ++ show i ++ " (" ++ tipo ++
              ") - adultos: " ++ show a ++ ", ninos: " ++ show n)


-- Imprimo los datos completos de la reserva como comprobante de la solicitud.
imprimirReserva :: Reserva -> IO ()
imprimirReserva (Reserva codigo nombre momento ingreso salida ocupaciones subtotal estado) = do
    putStrLn "-------------------------------------------"
    putStrLn ("Reserva: " ++ show codigo ++ " | " ++ estado)
    putStrLn ("Cliente: " ++ nombre)
    putStrLn ("Realizada: " ++ show momento)
    putStrLn ("Ingreso: " ++ show ingreso ++ " | salida: " ++ show salida)
    putStrLn ("Adultos: " ++ show (sum (map adultosOcupacion ocupaciones)))
    putStrLn ("Ninos: " ++ show (sum (map ninosOcupacion ocupaciones)))
    mapM_ imprimirOcupacion ocupaciones
    putStrLn ("Subtotal sin IVA: " ++ show subtotal)


-- ==========================================
-- RESERVACIONES Y FACTURACION
-- ==========================================


-- Pido los datos de la reserva, reviso las condiciones y guardo la reserva activa.
reservar :: Sistema -> IO Sistema
reservar sis@(Sistema hotel tipos habitaciones tarifas reservas facturas)
    | null habitaciones = do
        putStrLn "Primero genere habitaciones."
        return sis

    | length tarifas /= 8 = do
        putStrLn "Primero cargue las ocho tarifas."
        return sis

    | otherwise = do
        nombre <- preguntar "Nombre de quien reserva:"
        ingreso <- leerFecha "Fecha de ingreso (AAAA-MM-DD):"
        salida <- leerFecha "Fecha de salida (AAAA-MM-DD):"

        if limpiar nombre == "" || ingreso >= salida then do
            putStrLn "Nombre y fechas invalidas: salida debe ser posterior a ingreso."
            return sis
        else do
            putStrLn "Tipos disponibles:"
            mapM_ (\t ->
                putStrLn (nombreTipo t ++ " (max " ++ show (limiteTipo t) ++ ")")) tipos

            cantidad <- leerNatural "Cantidad de habitaciones que desea reservar:"

            if cantidad == 0 then do
                putStrLn "Debe solicitar al menos una habitacion."
                return sis
            else do
                pedido <- pedirOcupaciones
                    cantidad tipos habitaciones reservas ingreso salida []

                case pedido of
                    Nothing -> do
                        putStrLn "Reservacion cancelada por datos invalidos o falta de espacio."
                        return sis

                    Just ocupaciones -> do
                        let total = calcularSubtotal tarifas ingreso salida ocupaciones
                        momento <- getCurrentTime
                        let codigo = 1 + maximum
                                (0 : [i | Reserva i _ _ _ _ _ _ _ <- reservas])
                        let nueva = Reserva codigo (limpiar nombre) momento
                                            ingreso salida ocupaciones total "Activa"

                        imprimirReserva nueva
                        return (Sistema hotel tipos habitaciones tarifas
                                        (reservas ++ [nueva]) facturas)


-- Elijo habitaciones libres sin repetirlas y reviso que no excedan su capacidad.
pedirOcupaciones :: Int -> [Tipo] -> [Habitacion] -> [Reserva] -> Day -> Day -> [Ocupacion] -> IO (Maybe [Ocupacion])
pedirOcupaciones 0 _ _ _ _ _ elegidas = return (Just elegidas)
pedirOcupaciones faltan tipos habitaciones reservas ingreso salida elegidas = do
    tipoNombre <- preguntar ("Tipo de habitacion (faltan " ++ show faltan ++ "):")
    adultos <- leerNatural "Adultos en esa habitacion:"
    ninos <- leerNatural "Ninos en esa habitacion:"
    case find ((== limpiar tipoNombre) . nombreTipo) tipos of
        Nothing -> do
            putStrLn "Tipo desconocido."
            return Nothing

        Just tipoElegido -> do
            let total = adultos + ninos
            let libres =
                    [h | h <- habitaciones,
                         nombreHabitacion h == nombreTipo tipoElegido,
                         estaDisponible reservas ingreso salida (idHabitacion h),
                         not (idHabitacion h `elem` map idOcupacion elegidas)]

            if total < 1 || total > limiteTipo tipoElegido then do
                putStrLn "Cantidad de personas invalida para ese tipo."
                return Nothing
            else
                case libres of
                    [] -> do
                        putStrLn "No hay suficientes habitaciones libres de ese tipo."
                        return Nothing

                    (h:_) ->
                        pedirOcupaciones (faltan - 1) tipos habitaciones reservas ingreso salida
                            (elegidas ++ [Ocupacion (idHabitacion h)
                                                   (nombreTipo tipoElegido) adultos ninos])


-- Creo una copia de la reserva con el estado nuevo cuando coincide su codigo.
cambiarEstado :: Int -> String -> Reserva -> Reserva
cambiarEstado buscado nuevo reserva@(Reserva codigo nombre momento ingreso salida os total estado)
    | codigo == buscado = Reserva codigo nombre momento ingreso salida os total nuevo
    | otherwise = reserva


-- Busco una reserva activa, la marco como cancelada y libero sus habitaciones.
anular :: Sistema -> IO Sistema
anular sis@(Sistema hotel tipos hab tarifas reservas facturas) = do
    codigo <- leerNatural "Codigo de reserva a cancelar:"
    case find (\(Reserva i _ _ _ _ _ _ _) -> i == codigo) reservas of
        Just (Reserva _ _ _ _ _ _ _ "Activa") -> do
            putStrLn "Reserva anulada: sus habitaciones quedan liberadas."
            return (Sistema hotel tipos hab tarifas
                    (map (cambiarEstado codigo "Cancelada") reservas) facturas)

        _ -> do
            putStrLn "Reserva inexistente o no esta activa."
            return sis


-- Genero una factura para una reserva activa y le agrego el 13 por ciento de IVA.
-- Conservo el subtotal calculado al reservar, aunque luego cambien las tarifas.
facturar :: Sistema -> IO Sistema
facturar sis@(Sistema hotel tipos hab tarifas reservas facturas) = do
    codigo <- leerNatural "Codigo de reserva a facturar:"
    case find (\(Reserva i _ _ _ _ _ _ _) -> i == codigo) reservas of
        Just reserva@(Reserva _ _ _ _ _ _ subtotal "Activa") -> do
            let consecutivo = 1 + maximum (0 : [i | Factura i _ _ _ _ <- facturas])
            let impuesto = subtotal * 0.13
            let total = subtotal + impuesto
            putStrLn "=========== FACTURA ==========="
            let Hotel nombre cedula sitio telefono pais provincia = hotel
            putStrLn (nombre ++ " | Cedula juridica: " ++ cedula)
            putStrLn (sitio ++ " | " ++ telefono ++ " | " ++ pais ++ ", " ++ provincia)
            putStrLn ("Factura: " ++ show consecutivo ++ " / Reserva: " ++ show codigo)
            imprimirReserva reserva
            putStrLn ("Subtotal: " ++ show subtotal ++ " | IVA (13%): " ++ show impuesto ++ " | TOTAL: " ++ show total)
            return (Sistema hotel tipos hab tarifas
                    (map (cambiarEstado codigo "Facturada") reservas)
                    (facturas ++ [Factura consecutivo codigo subtotal impuesto total]))

        _ -> do
            putStrLn "Reserva inexistente o no esta activa."
            return sis


-- ==========================================
-- CONSULTAS Y ESTADISTICAS
-- ==========================================


-- Recorro todas las reservas registradas y muestro sus datos en la consola.
historial :: Sistema -> IO ()
historial (Sistema _ _ _ _ reservas _) = do
    if null reservas then
        putStrLn "Todavia no hay reservaciones."
    else
        mapM_ imprimirReserva reservas


-- Cuento las noches de cada huesped que corresponden al mes y ano consultados.
huespedesMes :: [Reserva] -> Integer -> Int -> Int
huespedesMes reservas anio mes =
    sum
        [ adultosOcupacion o + ninosOcupacion o
        | Reserva _ _ _ inicio salida ocupaciones _ estado <- reservas
        , estado /= "Cancelada"
        , dia <- take (fromIntegral (diffDays salida inicio))
                      (iterate (addDays 1) inicio)
        , let (a, m, _) = toGregorian dia
        , a == anio
        , m == mes
        , o <- ocupaciones
        ]


-- Cuento noches de ocupacion por dia de semana y muestro los tres con mas uso.
topDias :: [Reserva] -> [(String, Int)]
topDias reservas =
    take 3 (sortBy comparar [(dia, contar dia) | dia <- dias])
  where
    dias = ["Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"]

    contar nombre = length
        [() | Reserva _ _ _ ini fin os _ estado <- reservas,
              estado /= "Cancelada",
              dia <- take (fromIntegral (diffDays fin ini))
                          (iterate (addDays 1) ini),
              let numero = read (formatTime defaultTimeLocale "%u" dia) :: Int,
              dias !! (numero - 1) == nombre,
              _ <- os]

    comparar (_,a) (_,b) = compare b a


-- Presento las consultas de estadisticas y regreso al mismo menu al terminar.
estadisticas :: Sistema -> IO ()
estadisticas sis@(Sistema _ _ _ _ reservas facturas) = do
    putStrLn "\n===== ESTADISTICAS ====="
    putStrLn "1. Huespedes por mes y ano (noches-huesped)"
    putStrLn "2. Habitaciones ocupadas un dia"
    putStrLn "3. Cantidad sin ocupar un dia"
    putStrLn "4. Monto recaudado con impuestos"
    putStrLn "5. Top 3 dias semanales con ocupaciones"
    putStrLn "6. Volver"

    opcion <- getLine

    case opcion of
        "1" -> do
            anio <- leerNatural "Ano (AAAA):"
            mes <- leerNatural "Mes (1-12):"

            if mes < 1 || mes > 12 then
                putStrLn "Mes invalido."
            else
                putStrLn ("Total noches-huesped: " ++
                          show (huespedesMes reservas (fromIntegral anio) mes))

            estadisticas sis

        "2" -> do
            dia <- leerFecha "Dia (AAAA-MM-DD):"
            let ocupadas = [(h, i) | (h, Just i) <- estadoDia sis dia]

            if null ocupadas then
                putStrLn "Ninguna habitacion ocupada."
            else
                mapM_ (\(Habitacion i n, codigo) ->
                    putStrLn (show i ++ " - " ++ n ++
                              " / reserva " ++ show codigo)) ocupadas

            estadisticas sis

        "3" -> do
            dia <- leerFecha "Dia (AAAA-MM-DD):"
            putStrLn ("Habitaciones libres: " ++
                      show (length [() | (_, Nothing) <- estadoDia sis dia]))
            estadisticas sis

        "4" -> do
            putStrLn ("Total efectivamente facturado con IVA: " ++
                      show (sum [t | Factura _ _ _ _ t <- facturas]))
            putStrLn "Se contabilizan facturas emitidas; las reservas activas no son cobros realizados."
            estadisticas sis

        "5" -> do
            mapM_ (\(dia, cantidad) ->
                putStrLn (dia ++ ": " ++ show cantidad)) (topDias reservas)
            estadisticas sis

        "6" -> return ()

        _ -> do
            putStrLn "Opcion invalida."
            estadisticas sis


-- ==========================================
-- MENUS DEL PROGRAMA
-- ==========================================


-- Muestro las opciones administrativas y conservo los cambios del sistema.
menuAdmin :: Sistema -> IO Sistema
menuAdmin sis = do
    putStrLn "\n===== MENU ADMINISTRATIVO ====="
    putStrLn "1. Cargar tipos de habitaciones"
    putStrLn "2. Asignar habitaciones por tipo"
    putStrLn "3. Ver habitaciones"
    putStrLn "4. Cargar tarifas"
    putStrLn "5. Consultar reservaciones"
    putStrLn "6. Estado de ocupacion"
    putStrLn "7. Estadisticas"
    putStrLn "8. Volver"

    opcion <- getLine

    case opcion of
        "1" -> do
            nuevo <- cargarTipos sis
            menuAdmin nuevo

        "2" -> do
            nuevo <- asignarHabitaciones sis
            menuAdmin nuevo

        "3" -> do
            verHabitaciones sis
            menuAdmin sis

        "4" -> do
            nuevo <- cargarTarifas sis
            menuAdmin nuevo

        "5" -> do
            historial sis
            menuAdmin sis

        "6" -> do
            consultarOcupacion sis
            menuAdmin sis

        "7" -> do
            estadisticas sis
            menuAdmin sis

        "8" -> return sis

        _ -> do
            putStrLn "Opcion invalida."
            menuAdmin sis


-- Muestro las opciones generales para consultar, reservar, cancelar y facturar.
menuGeneral :: Sistema -> IO Sistema
menuGeneral sis = do
    putStrLn "\n===== MENU GENERAL ====="
    putStrLn "1. Ver disponibilidad"
    putStrLn "2. Reservar"
    putStrLn "3. Anular reserva"
    putStrLn "4. Facturar reserva"
    putStrLn "5. Volver"

    opcion <- getLine

    case opcion of
        "1" -> do
            mostrarDisponibilidad sis
            menuGeneral sis

        "2" -> do
            nuevo <- reservar sis
            menuGeneral nuevo

        "3" -> do
            nuevo <- anular sis
            menuGeneral nuevo

        "4" -> do
            nuevo <- facturar sis
            menuGeneral nuevo

        "5" -> return sis

        _ -> do
            putStrLn "Opcion invalida."
            menuGeneral sis


-- Mantengo abierto el menu principal hasta que se seleccione la opcion de salir.
menu :: Sistema -> IO ()
menu sis = do
    putStrLn "\n===== GESTION HOTELERA ====="
    putStrLn "1. Administracion"
    putStrLn "2. Opciones generales"
    putStrLn "3. Salir"

    opcion <- getLine

    case opcion of
        "1" -> do
            nuevo <- menuAdmin sis
            menu nuevo

        "2" -> do
            nuevo <- menuGeneral sis
            menu nuevo

        "3" -> putStrLn "Programa finalizado. Los datos en memoria se liberan."

        _ -> do
            putStrLn "Opcion invalida."
            menu sis


-- ==========================================
-- FUNCION PRINCIPAL Y CARGA DEL HOTEL
-- ==========================================


-- Intento leer los datos de hotel.csv; si falla, uso los datos de ejemplo.
cargarHotel :: IO Sistema
cargarHotel = do
    archivo <- leerArchivo "datos/hotel.csv"
    case archivo of
        Right contenido ->
            case map limpiar (separar (takeWhile (/= '\n') contenido)) of
                [a, b, c, d, e, f] | all (/= "") [a, b, c, d, e, f] ->
                    return (Sistema (Hotel a b c d e f) [] [] [] [] [])

                _ -> do
                    putStrLn "Datos de hotel invalidos. Se usaran datos de ejemplo."
                    return sistemaInicial

        Left _ -> do
            putStrLn "No se encontro datos/hotel.csv. Se usaran datos de ejemplo."
            return sistemaInicial


-- Inicio el programa, cargo el hotel y paso la informacion al menu principal.
main :: IO ()
main = do
    putStrLn "Sistema de Gestion Hotelera - Proyecto Programado 2"
    sis <- cargarHotel
    menu sis
