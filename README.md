# Proyecto Programado 2 - Sistema de Gestion Hotelera

## 1. Portada

**Instituto Tecnologico de Costa Rica**  
**Carrera:** Ingenieria en Computacion  
**Curso:** Lenguajes de Programacion, Grupo 60  
**Semestre:** II - 2026  
**Profesor:** Allan Rodriguez Davila  
**Proyecto:** Proyecto Programado #2 - Sistema de Gestion Hotelera  
**Integrantes y carnets:** Dilan Zamora-2019167464, Daryll Martinez-2019271231, Ian Samuels
**Fecha:** 15/10/2026

## 2. Descripcion del problema

Se requiere un sistema en consola que permita administrar los tipos y la cantidad fija de habitaciones de un hotel, cargar ocho tarifas, consultar ocupacion por fechas, generar reservas, anular reservas, emitir facturas y mostrar estadisticas. Se utilizan listas de valores y tipos de datos definidos con `data`.

## 3. Estructura de los archivos

```text
PP2_GesionHotelera/
  info.txt
  README.md
  programa/
    Main.hs
    datos/
      hotel.csv
      tipos.csv
      tarifas.csv
```

## 4. Compilar y ejecutar

Instalar GHC (Glasgow Haskell Compiler). Abrir terminal **dentro de la carpeta `programa`**.

Opcion directa (sin producir ejecutable):

```bash
runghc Main.hs
```

Opcion compilada:

```bash
ghc Main.hs -o hotel
./hotel              # Linux/macOS
hotel.exe            # Windows (PowerShell: .\\hotel.exe)
```

Si `runghc` o `ghc` no se reconocen, revisar la instalacion y PATH. Solo se usan paquetes habituales incluidos en GHC: base y time.

## 5. Manual de usuario y recorrido recomendado

1. Elegir `1. Administracion`.
2. Elegir `1. Cargar tipos` e introducir `datos/tipos.csv`.
3. Elegir `2. Asignar habitaciones` e indicar cantidades (por ejemplo 3, 2 y 1).
4. Elegir `3. Ver habitaciones` para confirmar los ID generados.
5. Elegir `4. Cargar tarifas` e introducir `datos/tarifas.csv`.
6. Regresar al menu principal y entrar en `2. Opciones generales`.
7. Consultar una fecha libre. Registrar una reserva de ejemplo usando fechas en formato `AAAA-MM-DD`.
8. Anotar el codigo mostrado. Consultar ocupacion desde Administracion.
9. Facturar o cancelar una reserva activa (son operaciones excluyentes).
10. Consultar historial y estadisticas.

### Formatos CSV

`hotel.csv`: una linea: `nombre,cedula,sitio,telefono,pais,provincia`.

`tipos.csv`: cada linea: `nombre,descripcion,maximo_de_personas`; ej. `Estandar,Habitacion queen,3`. No utilizar comas dentro de la descripcion.

`tarifas.csv`: exactamente una linea por codigo del 1 al 8, `codigo,precio`, donde los precios son por persona y noche, no por habitacion.

| Codigo | Temporada              | Dias           | Huesped |
| ------ | ---------------------- | -------------- | ------- |
| 1      | Verde (abril-octubre)  | Domingo-jueves | Adulto  |
| 2      | Verde (abril-octubre)  | Domingo-jueves | Nino    |
| 3      | Verde (abril-octubre)  | Viernes-sabado | Adulto  |
| 4      | Verde (abril-octubre)  | Viernes-sabado | Nino    |
| 5      | Alta (noviembre-marzo) | Domingo-jueves | Adulto  |
| 6      | Alta (noviembre-marzo) | Domingo-jueves | Nino    |
| 7      | Alta (noviembre-marzo) | Viernes-sabado | Adulto  |
| 8      | Alta (noviembre-marzo) | Viernes-sabado | Nino    |

## 6. Diseno del programa

Se usa el tipo `Sistema` como contenedor de hotel, tipos, habitaciones, tarifas, reservas y facturas. Cada menu recibe un `Sistema` y retorna otro actualizado. Al volver al menu principal se conserva el estado en memoria. Al salir se termina la ejecucion y no se guardan reservas ni facturas.

`Tipo`: nombre, descripcion y capacidad. `Habitacion`: codigo y tipo. `Ocupacion`: habitacion, tipo, adultos, ninos. `Reserva`: codigo, cliente, momento de registro, entrada, salida, ocupaciones, subtotal y estado. `Factura`: consecutivo, codigo de reserva, subtotal, impuesto, total.

### Algoritmos y funciones principales

| Funcion               | Entrada                                         | Salida                      | Funcionamiento y restricciones                                            |
| --------------------- | ----------------------------------------------- | --------------------------- | ------------------------------------------------------------------------- |
| `convertirTipo`       | linea CSV                                       | `Maybe Tipo`                | Valida 3 campos, nombre, descripcion y capacidad positiva.                |
| `cargarTipos`         | Sistema y archivo                               | `IO Sistema`                | Lee el CSV y agrega nombres que no existan.                               |
| `asignarHabitaciones` | Sistema y cantidades                            | `IO Sistema`                | Genera ID 1..N; solo se ejecuta una vez cuando se generan habitaciones.   |
| `cargarTarifas`       | Sistema y archivo                               | `IO Sistema`                | Exige 8 codigos unicos y precios no negativos; reemplaza las tarifas.     |
| `indiceTarifa`        | `Day`                                           | `Int`                       | Determina temporada y si es viernes/sabado.                               |
| `calcularSubtotal`    | tarifas, fecha inicio, fin, ocupaciones         | `Double`                    | Suma tarifas por persona de cada noche; excluye el dia de salida.         |
| `cruzan`              | dos intervalos                                  | `Bool`                      | Comprueba si los periodos se solapan.                                     |
| `estaDisponible`      | reservas, fechas, habitacion                    | `Bool`                      | Impide reservar un cuarto en fechas ya bloqueadas.                        |
| `pedirOcupaciones`    | cantidad, tipos, habitaciones, reservas, fechas | `IO (Maybe [Ocupacion])`    | Valida capacidad y asigna IDs libres sin repetirlos.                      |
| `reservar`            | Sistema                                         | `IO Sistema`                | Captura solicitud y genera reserva con subtotal y estado Activa.          |
| `anular`              | Sistema y codigo solicitado                     | `IO Sistema`                | Solo cancela reservas activas y libera fechas.                            |
| `facturar`            | Sistema y codigo solicitado                     | `IO Sistema`                | Solo factura una reserva activa; aplica IVA 13% y guarda comprobante.     |
| `estadoDia`           | Sistema y fecha                                 | `[(Habitacion, Maybe Int)]` | Indica si esta libre u ocupada y reserva que la ocupa.                    |
| `huespedesMes`        | reservas, ano, mes                              | `Int`                       | Cuenta noches-huesped para el mes; no cuenta canceladas.                  |
| `topDias`             | reservas                                        | `[(String,Int)]`            | Cuenta noches-habitacion por dia de la semana y retorna los tres mayores. |

### Ejemplo de encadenamiento

`main` -> `cargarHotel` -> `menu` -> `menuAdmin` -> `cargarTipos` -> `menuAdmin` -> `menu`.

Para una reserva: `menuGeneral` -> `reservar` -> `pedirOcupaciones` -> `estaDisponible` -> `calcularSubtotal` -> `imprimirReserva` -> `menuGeneral`.

### Funciones de orden superior utilizadas

- `map`: transforma un conjunto de reservas para cambiar el estado de una reserva concreta; tambien extrae cantidades por ocupacion.
- `mapM_`: ejecuta una accion de impresion por cada habitacion o reserva.
- `mapM`: solicita cantidades a partir de todos los tipos.
- `filter`: extrae habitaciones de un tipo disponible o busca tarifas.
- `any`: comprueba si existe una reserva que bloquea una habitacion o un tipo duplicado.
- `find`: encuentra el tipo o reserva con el codigo indicado.
- `foldl`: acumula los tipos nuevos, evitando repetidos.
- `sortBy`: ordena los dias de la semana de mayor a menor ocupacion.

## 7. Librerias y sus usos

- `Data.Time`: fechas, dias, operaciones entre fechas, fecha y hora de reserva.
- `Data.Time.Format`: convertir texto a fechas y obtener dia semanal.
- `Data.List`: `find`, `sortBy`, `sort`.
- `Data.Char`: `isSpace`.
- `Control.Exception`: manejar errores de lectura.
- `Text.Read`: `readMaybe`, conversion segura de texto a numeros.
- `Prelude`: IO, listas, comparacion, conversion y operaciones comunes.

## 8. Validaciones y restricciones conocidas

- Tipos con mismo nombre se omiten. La comparacion distingue mayusculas/minusculas.
- Las descripciones CSV no admiten comas internas ni comillas CSV especiales.
- La asignacion no puede repetirse despues de generar habitaciones.
- Las tarifas nuevas afectan reservas futuras; las reservas ya creadas conservan su subtotal.
- Se rechaza salida menor o igual a entrada, habitaciones sin espacio y ocupacion superior al maximo.
- Reservas facturadas siguen ocupando espacio en sus fechas. Reservas canceladas no.
- Solo una reserva Activa puede anularse o facturarse.
- El monto recaudado corresponde a facturas emitidas, no reservas pendientes.
- La estadistica de huespedes por mes esta definida como **noches-huesped**; una persona con tres noches suma tres.
- No hay usuario/clave: el enunciado solo pide menus administrativos y normales.
- Datos volatiles: la persistencia no fue implementada (es punto extra).
- Montos `Double`: para una solucion financiera de produccion convendria centavos enteros.

## 9. Pruebas de funcionalidad

Consultar `Pruebas_Manual.md` para casos y resultados esperados. **Capturas reales pendientes:** ejecutar el programa en su computadora, comprobar cada escenario, insertar screenshots en el documento de entrega y anotar el resultado real. No se adjuntan pantallazos fabricados.

## 10. Analisis de resultados (estado preliminar)

**Implementado en codigo:** menus, carga desde archivos, asignacion unica, consultas, reserva, anulacion, factura, estadisticas y validaciones esenciales.

**No alcanzado:**
