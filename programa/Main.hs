import Data.Time (Day, UTCTime, getCurrentTime, utctDay, addDays, diffDays, toGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime)
import Data.List (find, nub, sortBy, group, sort)
import Data.Char (isSpace)
import Control.Exception (IOException, try, evaluate)
import Text.Read (readMaybe)

-- cada constructor representa una entidad del hotel.
data Tipo = Tipo String String Int deriving (Show)
data Habitacion = Habitacion Int String deriving (Show)
data Ocupacion = Ocupacion Int String Int Int deriving (Show)
data Reserva = Reserva Int String UTCTime Day Day [Ocupacion] Double String deriving (Show)
data Factura = Factura Int Int Double Double Double deriving (Show)
data Hotel = Hotel String String String String String String deriving (Show)
data Sistema = Sistema Hotel [Tipo] [Habitacion] [Double] [Reserva] [Factura] deriving (Show)

sistemaInicial :: Sistema
sistemaInicial = Sistema (Hotel "Hotel de Ejemplo" "0000000000" "www.ejemplo.com" "0000-0000" "Costa Rica" "Limon") [] [] [] [] []

-- Entrada: texto. Salida: texto sin espacios al inicio o al final.
limpiar :: String -> String
limpiar = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Entrada: texto con campos separados por comas. Salida: lista de campos.
separar :: String -> [String]
separar [] = [""]
separar (',':xs) = "" : separar xs
separar (x:xs) = let (a:as) = separar xs in (x:a):as

-- Entrada: texto. Salida: numero entero valido o Nothing.
entero :: String -> Maybe Int
entero = readMaybe . limpiar

-- Entrada: texto. Salida: numero decimal valido o Nothing.
decimal :: String -> Maybe Double
decimal = readMaybe . limpiar

-- Entrada: pregunta. Salida: respuesta leida desde la consola.
preguntar :: String -> IO String
preguntar pregunta = do
    putStrLn pregunta
    getLine

-- Entrada: pregunta. Salida: entero no negativo; repite si hay error.
leerNatural :: String -> IO Int
leerNatural mensaje = do
    valor <- preguntar mensaje
    case entero valor of
        Just n | n >= 0 -> return n
        _ -> do
            putStrLn "Ingrese un numero entero mayor o igual a cero."
            leerNatural mensaje

-- Entrada: fecha yyyy-mm-dd. Salida: Just dia o Nothing.
fecha :: String -> Maybe Day
fecha = parseTimeM True defaultTimeLocale "%Y-%m-%d" . limpiar

-- Entrada: mensaje. Salida: fecha valida, repitiendo si es necesario.
leerFecha :: String -> IO Day
leerFecha mensaje = do
    texto <- preguntar mensaje
    case fecha texto of
        Just d -> return d
        Nothing -> putStrLn "Fecha invalida. Use AAAA-MM-DD." >> leerFecha mensaje

-- Entrada: ruta. Salida: contenido del archivo o mensaje de error.
-- Restriccion: el archivo debe existir y ser legible.
leerArchivo :: FilePath -> IO (Either String String)
leerArchivo ruta = do
    resultado <- try (do
        contenido <- readFile ruta
        _ <- evaluate (length contenido)
        return contenido) :: IO (Either IOException String)
    return (case resultado of
        Left e -> Left (show e)
        Right texto -> Right texto)

-- Entrada: linea de tipo nombre,descripcion,maximo. Salida: tipo valido.
convertirTipo :: String -> Maybe Tipo
convertirTipo linea = case map limpiar (separar linea) of
    [nombre, descripcion, maximo] -> case entero maximo of
        Just n | n > 0 && nombre /= "" && descripcion /= "" -> Just (Tipo nombre descripcion n)
        _ -> Nothing
    _ -> Nothing

nombreTipo :: Tipo -> String
nombreTipo (Tipo n _ _) = n

limiteTipo :: Tipo -> Int
limiteTipo (Tipo _ _ n) = n

nombreHabitacion :: Habitacion -> String
nombreHabitacion (Habitacion _ n) = n

idHabitacion :: Habitacion -> Int
idHabitacion (Habitacion i _) = i

idOcupacion :: Ocupacion -> Int
idOcupacion (Ocupacion i _ _ _) = i

adultosOcupacion :: Ocupacion -> Int
adultosOcupacion (Ocupacion _ _ a _) = a

ninosOcupacion :: Ocupacion -> Int
ninosOcupacion (Ocupacion _ _ _ n) = n

-- Entrada: sistema y archivo CSV. Salida: sistema con tipos nuevos sin duplicados.
-- Restriccion: se descartan lineas mal formadas y nombres repetidos.
cargarTipos :: Sistema -> IO Sistema
cargarTipos sis@(Sistema hotel tipos hab tarifas reservas facturas) = do
    ruta <- preguntar "Ruta del archivo de tipos (ej: datos/tipos.csv):"
    archivo <- leerArchivo ruta
    case archivo of
        Left errorTexto -> putStrLn ("No se pudo leer: " ++ errorTexto) >> return sis
        Right contenido -> do
            let nuevos = [t | linea <- lines contenido, Just t <- [convertirTipo linea]]
            let agregar ts t = if any ((== nombreTipo t) . nombreTipo) ts then ts else ts ++ [t]
            let finales = foldl agregar tipos nuevos
            putStrLn ("Tipos nuevos agregados: " ++ show (length finales - length tipos))
            return (Sistema hotel finales hab tarifas reservas facturas)

-- Entrada: sistema y cantidades por tipo. Salida: habitaciones identificadas.
-- Restriccion: solamente se permite la generacion una vez.
asignarHabitaciones :: Sistema -> IO Sistema
asignarHabitaciones sis@(Sistema hotel tipos hab tarifas reservas facturas)
    | not (null hab) = putStrLn "Las habitaciones ya fueron generadas." >> return sis
    | null tipos = putStrLn "Primero debe cargar tipos." >> return sis
    | otherwise = do
        putStrLn "Indique la cantidad para cada tipo."
        cantidades <- mapM (\t -> leerNatural (nombreTipo t ++ ":")) tipos
        let nombres = concat [replicate cantidad (nombreTipo t) | (t, cantidad) <- zip tipos cantidades]
        if null nombres then putStrLn "Debe generar al menos una habitacion." >> return sis
        else do
            let nuevas = zipWith Habitacion [1..] nombres
            putStrLn ("Habitaciones generadas: " ++ show (length nuevas))
            return (Sistema hotel tipos nuevas tarifas reservas facturas)

verHabitaciones :: Sistema -> IO ()
verHabitaciones (Sistema _ _ habitaciones _ _ _) = do
    if null habitaciones then putStrLn "No hay habitaciones." else
        mapM_ (\(Habitacion i n) -> putStrLn (show i ++ " - " ++ n)) habitaciones

-- Entrada: numero de tarifa,valor. Salida: par valido entre 1 y 8.
convertirTarifa :: String -> Maybe (Int, Double)
convertirTarifa linea = case map limpiar (separar linea) of
    [codigo, precio] -> case (entero codigo, decimal precio) of
        (Just i, Just p) | i >= 1 && i <= 8 && p >= 0 -> Just (i,p)
        _ -> Nothing
    _ -> Nothing

-- Entrada: archivo con exactamente ocho codigos unicos. Salida: tarifas reemplazadas.
cargarTarifas :: Sistema -> IO Sistema
cargarTarifas sis@(Sistema hotel tipos hab _ reservas facturas) = do
    ruta <- preguntar "Ruta del archivo de tarifas (ej: datos/tarifas.csv):"
    archivo <- leerArchivo ruta
    case archivo of
        Left errorTexto -> putStrLn ("No se pudo leer: " ++ errorTexto) >> return sis
        Right contenido -> do
            let lineas = filter (not . null . limpiar) (lines contenido)
            let pares = [p | linea <- lineas, Just p <- [convertirTarifa linea]]
            if length pares /= 8 || sort (map fst pares) /= [1..8] then do
                putStrLn "Archivo invalido: debe contener exactamente los codigos 1 al 8, sin repetidos."
                return sis
            else do
                let valores = [snd (head (filter ((== i) . fst) pares)) | i <- [1..8]]
                putStrLn "Tarifas cargadas y reemplazadas."
                return (Sistema hotel tipos hab valores reservas facturas)

-- Entrada: dia. Salida: codigo base de tarifa segun temporada y dia.
-- 1-4 temporada verde y 5-8 temporada alta; par adulto, impar siguiente nino.
indiceTarifa :: Day -> Int
indiceTarifa dia =
    let (_, mes, _) = toGregorian dia
        semana = read (formatTime defaultTimeLocale "%u" dia) :: Int
        alta = mes >= 11 || mes <= 3
        finSemana = semana == 5 || semana == 6
    in 1 + (if alta then 4 else 0) + (if finSemana then 2 else 0)

-- Entrada: tarifas, dias y ocupaciones. Salida: subtotal sin IVA.
-- Restriccion: salida no esta incluida (intervalo [entrada,salida)).
calcularSubtotal :: [Double] -> Day -> Day -> [Ocupacion] -> Double
calcularSubtotal tarifas entrada salida ocupaciones = sum
    [ fromIntegral (adultosOcupacion o) * (tarifas !! (codigo - 1))
      + fromIntegral (ninosOcupacion o) * (tarifas !! codigo)
    | dia <- take (fromIntegral (diffDays salida entrada)) (iterate (addDays 1) entrada)
    , o <- ocupaciones
    , let codigo = indiceTarifa dia ]

-- Entrada: intervalo de dos reservas. Salida: True si se cruzan noches.
cruzan :: Day -> Day -> Day -> Day -> Bool
cruzan inicio1 fin1 inicio2 fin2 = inicio1 < fin2 && inicio2 < fin1

-- Entrada: reserva. Salida: True para reservas que aun bloquean habitacion.
bloquea :: Reserva -> Bool
bloquea (Reserva _ _ _ _ _ _ _ estado) = estado /= "Cancelada"

-- Entrada: lista reservas, fechas, habitacion. Salida: disponibilidad.
estaDisponible :: [Reserva] -> Day -> Day -> Int -> Bool
estaDisponible reservas entrada salida codigo = not (any ocupada reservas)
  where
    ocupada (Reserva _ _ _ inicio fin ocupaciones _ estado) =
        estado /= "Cancelada" && cruzan entrada salida inicio fin &&
        any ((== codigo) . idOcupacion) ocupaciones

-- Entrada: sistema y fecha. Salida: pares habitacion/codigo de reserva o libre.
estadoDia :: Sistema -> Day -> [(Habitacion, Maybe Int)]
estadoDia (Sistema _ _ habitaciones _ reservas _) dia =
    [(h, reservaDia (idHabitacion h)) | h <- habitaciones]
  where
    reservaDia codigo = case [i | Reserva i _ _ ini fin os _ est <- reservas,
                                  est /= "Cancelada", dia >= ini, dia < fin,
                                  any ((== codigo) . idOcupacion) os] of
        (x:_) -> Just x
        [] -> Nothing

mostrarDisponibilidad :: Sistema -> IO ()
mostrarDisponibilidad sis = do
    dia <- leerFecha "Dia a consultar (AAAA-MM-DD):"
    let disponibles = [(h, codigo) | (h, codigo) <- estadoDia sis dia, codigo == Nothing]
    if null disponibles then putStrLn "No hay habitaciones disponibles." else
        mapM_ (\(Habitacion i nombre, _) -> putStrLn (show i ++ " - " ++ nombre)) disponibles

consultarOcupacion :: Sistema -> IO ()
consultarOcupacion sis = do
    dia <- leerFecha "Dia a consultar (AAAA-MM-DD):"
    mapM_ imprimir (estadoDia sis dia)
  where
    imprimir (Habitacion i nombre, Nothing) = putStrLn (show i ++ " - " ++ nombre ++ " - Disponible")
    imprimir (Habitacion i nombre, Just codigo) = putStrLn (show i ++ " - " ++ nombre ++ " - Ocupada / reserva " ++ show codigo)

-- Entrada: una ocupacion. Salida: detalle legible.
imprimirOcupacion :: Ocupacion -> IO ()
imprimirOcupacion (Ocupacion i tipo a n) =
    putStrLn ("  Habitacion " ++ show i ++ " (" ++ tipo ++ ") - adultos: " ++ show a ++ ", ninos: " ++ show n)

-- Entrada: una reserva. Salida: comprobante por consola.
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

-- Entrada: sistema y datos de usuario. Salida: reserva activa si todo es valido.
-- Validaciones: periodo, capacidad, personas y disponibilidad simultanea.
reservar :: Sistema -> IO Sistema
reservar sis@(Sistema hotel tipos habitaciones tarifas reservas facturas)
    | null habitaciones = putStrLn "Primero genere habitaciones." >> return sis
    | length tarifas /= 8 = putStrLn "Primero cargue las ocho tarifas." >> return sis
    | otherwise = do
        nombre <- preguntar "Nombre de quien reserva:"
        ingreso <- leerFecha "Fecha de ingreso (AAAA-MM-DD):"
        salida <- leerFecha "Fecha de salida (AAAA-MM-DD):"
        if limpiar nombre == "" || ingreso >= salida then do
            putStrLn "Nombre y fechas invalidas: salida debe ser posterior a ingreso."
            return sis
        else do
            putStrLn "Tipos disponibles:"
            mapM_ (\t -> putStrLn (nombreTipo t ++ " (max " ++ show (limiteTipo t) ++ ")")) tipos
            cantidad <- leerNatural "Cantidad de habitaciones que desea reservar:"
            if cantidad == 0 then putStrLn "Debe solicitar al menos una habitacion." >> return sis
            else do
                pedido <- pedirOcupaciones cantidad tipos habitaciones reservas ingreso salida []
                case pedido of
                    Nothing -> putStrLn "Reservacion cancelada por datos invalidos o falta de espacio." >> return sis
                    Just ocupaciones -> do
                        let total = calcularSubtotal tarifas ingreso salida ocupaciones
                        momento <- getCurrentTime
                        let codigo = 1 + maximum (0 : [i | Reserva i _ _ _ _ _ _ _ <- reservas])
                        let nueva = Reserva codigo (limpiar nombre) momento ingreso salida ocupaciones total "Activa"
                        imprimirReserva nueva
                        return (Sistema hotel tipos habitaciones tarifas (reservas ++ [nueva]) facturas)

-- Entrada: cantidad y habitaciones ya elegidas. Salida: ocupaciones o Nothing.
-- Restriccion: no repite habitacion dentro de una misma solicitud.
pedirOcupaciones :: Int -> [Tipo] -> [Habitacion] -> [Reserva] -> Day -> Day -> [Ocupacion] -> IO (Maybe [Ocupacion])
pedirOcupaciones 0 _ _ _ _ _ elegidas = return (Just elegidas)
pedirOcupaciones faltan tipos habitaciones reservas ingreso salida elegidas = do
    tipoNombre <- preguntar ("Tipo de habitacion (faltan " ++ show faltan ++ "):")
    adultos <- leerNatural "Adultos en esa habitacion:"
    ninos <- leerNatural "Ninos en esa habitacion:"
    case find ((== limpiar tipoNombre) . nombreTipo) tipos of
        Nothing -> putStrLn "Tipo desconocido." >> return Nothing
        Just tipoElegido -> do
            let total = adultos + ninos
            let libres = [h | h <- habitaciones, nombreHabitacion h == nombreTipo tipoElegido,
                                  estaDisponible reservas ingreso salida (idHabitacion h),
                                  not (idHabitacion h `elem` map idOcupacion elegidas)]
            if total < 1 || total > limiteTipo tipoElegido then
                putStrLn "Cantidad de personas invalida para ese tipo." >> return Nothing
            else case libres of
                [] -> putStrLn "No hay suficientes habitaciones libres de ese tipo." >> return Nothing
                (h:_) -> pedirOcupaciones (faltan - 1) tipos habitaciones reservas ingreso salida
                    (elegidas ++ [Ocupacion (idHabitacion h) (nombreTipo tipoElegido) adultos ninos])

-- Entrada: reserva y nuevo estado. Salida: copia actualizada.
cambiarEstado :: Int -> String -> Reserva -> Reserva
cambiarEstado buscado nuevo reserva@(Reserva codigo nombre momento ingreso salida os total estado)
    | codigo == buscado = Reserva codigo nombre momento ingreso salida os total nuevo
    | otherwise = reserva

anular :: Sistema -> IO Sistema
anular sis@(Sistema hotel tipos hab tarifas reservas facturas) = do
    codigo <- leerNatural "Codigo de reserva a cancelar:"
    case find (\(Reserva i _ _ _ _ _ _ _) -> i == codigo) reservas of
        Just (Reserva _ _ _ _ _ _ _ "Activa") -> do
            putStrLn "Reserva anulada: sus habitaciones quedan liberadas."
            return (Sistema hotel tipos hab tarifas (map (cambiarEstado codigo "Cancelada") reservas) facturas)
        _ -> putStrLn "Reserva inexistente o no esta activa." >> return sis

-- Entrada: reserva activa. Salida: factura con IVA del 13%.
-- La tarifa se almacena al reservar para que la recarga futura no la cambie.
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
            return (Sistema hotel tipos hab tarifas (map (cambiarEstado codigo "Facturada") reservas)
                                (facturas ++ [Factura consecutivo codigo subtotal impuesto total]))
        _ -> putStrLn "Reserva inexistente o no esta activa." >> return sis

historial :: Sistema -> IO ()
historial (Sistema _ _ _ _ reservas _) = do
    if null reservas then putStrLn "Todavia no hay reservaciones." else mapM_ imprimirReserva reservas

-- Entrada: un mes y un ano. Salida: noches-huesped contabilizadas por mes.
-- Se cuentan noches realmente comprendidas en el mes para evitar duplicaciones.
huespedesMes :: [Reserva] -> Integer -> Int -> Int
huespedesMes reservas anio mes = sum
    [ adultosOcupacion o + ninosOcupacion o
    | Reserva _ _ _ inicio salida ocupaciones _ estado <- reservas
    , estado /= "Cancelada"
    , dia <- take (fromIntegral (diffDays salida inicio)) (iterate (addDays 1) inicio)
    , let (a,m,_) = toGregorian dia
    , a == anio, m == mes, o <- ocupaciones ]

-- Entrada: ocupaciones de todas las fechas. Salida: top tres por dia semanal.
-- Se cuentan noches-habitacion, no reservas completas.
topDias :: [Reserva] -> [(String, Int)]
topDias reservas = take 3 (sortBy comparar [(dia, contar dia) | dia <- dias])
  where
    dias = ["Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"]
    contar nombre = length
        [() | Reserva _ _ _ ini fin os _ estado <- reservas, estado /= "Cancelada",
              dia <- take (fromIntegral (diffDays fin ini)) (iterate (addDays 1) ini),
              let numero = read (formatTime defaultTimeLocale "%u" dia) :: Int,
              dias !! (numero - 1) == nombre, _ <- os]
    comparar (_,a) (_,b) = compare b a

estadisticas :: Sistema -> IO ()
estadisticas sis@(Sistema _ _ _ _ reservas facturas) = do
    putStrLn "\nESTADISTICAS\n1. Huespedes por mes y ano (noches-huesped)\n2. Habitaciones ocupadas un dia\n3. Cantidad sin ocupar un dia\n4. Monto recaudado con impuestos\n5. Top 3 dias semanales con ocupaciones\n6. Volver"
    opcion <- getLine
    case opcion of
        "1" -> do
            anio <- leerNatural "Ano (AAAA):"
            mes <- leerNatural "Mes (1-12):"
            if mes < 1 || mes > 12 then putStrLn "Mes invalido." else
                putStrLn ("Total noches-huesped: " ++ show (huespedesMes reservas (fromIntegral anio) mes))
            estadisticas sis
        "2" -> do
            dia <- leerFecha "Dia (AAAA-MM-DD):"
            let ocupadas = [(h,i) | (h,Just i) <- estadoDia sis dia]
            if null ocupadas then putStrLn "Ninguna habitacion ocupada." else
                mapM_ (\(Habitacion i n,codigo) -> putStrLn (show i ++ " - " ++ n ++ " / reserva " ++ show codigo)) ocupadas
            estadisticas sis
        "3" -> do
            dia <- leerFecha "Dia (AAAA-MM-DD):"
            putStrLn ("Habitaciones libres: " ++ show (length [() | (_,Nothing) <- estadoDia sis dia]))
            estadisticas sis
        "4" -> do
            putStrLn ("Total efectivamente facturado con IVA: " ++ show (sum [t | Factura _ _ _ _ t <- facturas]))
            putStrLn "Se contabilizan facturas emitidas; las reservas activas no son cobros realizados."
            estadisticas sis
        "5" -> do
            mapM_ (\(dia,cantidad) -> putStrLn (dia ++ ": " ++ show cantidad)) (topDias reservas)
            estadisticas sis
        "6" -> return ()
        _ -> putStrLn "Opcion invalida." >> estadisticas sis

-- Entrada: sistema actual. Salida: sistema actualizado al regresar al menu principal.
menuAdmin :: Sistema -> IO Sistema
menuAdmin sis = do
    putStrLn "\nMENU ADMINISTRATIVO\n1. Cargar tipos de habitaciones\n2. Asignar habitaciones por tipo\n3. Ver habitaciones\n4. Cargar tarifas\n5. Consultar reservaciones\n6. Estado de ocupacion\n7. Estadisticas\n8. Volver"
    opcion <- getLine
    case opcion of
        "1" -> cargarTipos sis >>= menuAdmin
        "2" -> asignarHabitaciones sis >>= menuAdmin
        "3" -> verHabitaciones sis >> menuAdmin sis
        "4" -> cargarTarifas sis >>= menuAdmin
        "5" -> historial sis >> menuAdmin sis
        "6" -> consultarOcupacion sis >> menuAdmin sis
        "7" -> estadisticas sis >> menuAdmin sis
        "8" -> return sis
        _ -> putStrLn "Opcion invalida." >> menuAdmin sis

-- Entrada: sistema actual. Salida: sistema despues de reservas/cancelaciones/facturas.
menuGeneral :: Sistema -> IO Sistema
menuGeneral sis = do
    putStrLn "\nMENU GENERAL\n1. Ver disponibilidad\n2. Reservar\n3. Anular reserva\n4. Facturar reserva\n5. Volver"
    opcion <- getLine
    case opcion of
        "1" -> mostrarDisponibilidad sis >> menuGeneral sis
        "2" -> reservar sis >>= menuGeneral
        "3" -> anular sis >>= menuGeneral
        "4" -> facturar sis >>= menuGeneral
        "5" -> return sis
        _ -> putStrLn "Opcion invalida." >> menuGeneral sis

-- Entrada: sistema. Salida: IO (); procesa las opciones hasta salir.
menu :: Sistema -> IO ()
menu sis = do
    putStrLn "\n====== GESTION HOTELERA ======\n1. Administracion\n2. Opciones generales\n3. Salir"
    opcion <- getLine
    case opcion of
        "1" -> menuAdmin sis >>= menu
        "2" -> menuGeneral sis >>= menu
        "3" -> putStrLn "Programa finalizado. Los datos en memoria se liberan."
        _ -> putStrLn "Opcion invalida." >> menu sis

-- Entrada: datos/hotel.csv. Salida: sistema con datos del hotel.
-- Si no existe el archivo o su formato es invalido, se usan datos de ejemplo.
cargarHotel :: IO Sistema
cargarHotel = do
    archivo <- leerArchivo "datos/hotel.csv"
    case archivo of
        Right contenido -> case map limpiar (separar (takeWhile (/= '\n') contenido)) of
            [a,b,c,d,e,f] | all (/= "") [a,b,c,d,e,f] ->
                return (Sistema (Hotel a b c d e f) [] [] [] [] [])
            _ -> putStrLn "Datos de hotel invalidos. Se usaran datos de ejemplo." >> return sistemaInicial
        Left _ -> putStrLn "No se encontro datos/hotel.csv. Se usaran datos de ejemplo." >> return sistemaInicial

main :: IO ()
main = do
    putStrLn "Sistema de Gestion Hotelera - Proyecto Programado 2"
    sis <- cargarHotel
    menu sis
