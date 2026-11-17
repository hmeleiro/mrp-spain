------------------------------------------------------------------------

editor_options: markdown: wrap: 72 ---

# Configuración de MRP Spain

`config/mrp.yml` es el único fichero de configuración del pipeline soportado. Este documento distingue entre:

1.  **Parámetros de ejecución**, que se pueden cambiar entre estimaciones.
2.  **Contratos internos**, que describen datos, recodificaciones y backtests.

La distinción es importante. Cambiar `run` selecciona una ejecución dentro del modelo soportado. Cambiar `domain`, `schemas` o `backtests` equivale a modificar la definición del modelo o el contrato de sus datos y debe revisarse como un cambio de código.

## Ejemplo mínimo

El fragmento siguiente es ilustrativo. Los valores efectivos son siempre los que figuren en `config/mrp.yml` en el momento de ejecutar.

``` yaml
run:
  poststrat_source: censo
  recent_studies: 2
  inference: fast
  temporal:
    unit: month
    structure: none
  vote_sampling:
    adapt_delta: 0.9
    max_treedepth: 10
    chains: 4
  calibration:
    turnout: 0.71
    recall: 0.50
  national_calibration:
    enabled: false
    draws_path: data-raw/national-vote-draws.csv
    strength: 1.0
    otbl_strength: null
```

Estas son las decisiones públicas:

| Clave                                 | Tipo   | Valores válidos               |
|---------------------------------------|--------|-------------------------------|
| `run.poststrat_source`                | texto  | `censo`, `epa`                |
| `run.recent_studies`                  | entero | `1` o mayor                   |
| `run.inference`                       | texto  | `fast`, `full`                |
| `run.temporal.unit`                   | texto  | `month`, `day`                |
| `run.temporal.structure`              | texto  | `none`, `linear_varying`      |
| `run.vote_sampling.adapt_delta`       | número | entre `0.8` y `0.999`         |
| `run.vote_sampling.max_treedepth`     | entero | entre `8` y `15`              |
| `run.vote_sampling.chains`            | entero | entre `2` y `8`               |
| `run.calibration.turnout`             | número | estrictamente entre `0` y `1` |
| `run.calibration.recall`              | número | entre `0` y `1`               |
| `run.national_calibration.enabled`    | lógico | `true`, `false`               |
| `run.national_calibration.draws_path` | ruta   | CSV largo de objetivos        |
| `run.national_calibration.strength`   | número | entre `0` y `1`               |
| `run.national_calibration.otbl_strength` | número o `null` | entre `strength` y `1`; hereda `strength` si se omite o es `null` |

Las claves desconocidas dentro de `run` y `run.calibration`, los valores fuera de rango y los tipos incorrectos producen un error al cargar el YAML.

## `run.poststrat_source`

Selecciona el marco con el que se proyectan los modelos:

``` yaml
poststrat_source: censo
```

### `censo`

Lee `data-raw/CensoPersonas_2021.parquet`, filtra personas de 18 o más años y construye celdas agregadas. El peso `FACTOREL` es el número de personas de cada celda.

Variables disponibles:

- sexo;
- grupo de edad;
- estudios;
- situación laboral (`sitlab` y `sitlab2`);
- tamaño de municipio (`tamuni`);
- CCAA y provincia.

### `epa`

Lee todos los ficheros que cumplen `sav$` dentro de `data-raw/epa/`. Conserva la EPA como microdatos y utiliza `FACTOREL` como peso poblacional, dividido entre el número de ficheros combinados.

Variables disponibles:

- sexo;
- grupo de edad;
- estudios, derivados de `NFORMA`;
- situación laboral, derivada de `SIDAC1`, `SIDI1` y `NBUSCA`;
- CCAA y provincia.

La EPA no aporta `tamuni`.

### Efecto sobre las fórmulas

Ambos adaptadores producen `estudios` y `sitlab2`. Por tanto, seleccionar EPA no elimina estudios y seleccionar Censo no elimina situación laboral.

| Modelo | Términos demográficos relevantes |
|----|----|
| Participación | `sexo + edad_grupo + estudios + sitlab2 + [tamuni] + recuerdo` |
| Recuerdo | `sexo + edad_grupo + estudios + sitlab2 + [tamuni]` |
| Voto | `sexo + edad_grupo + sexo:edad_grupo + estudios + recuerdo + [tamuni]` |

`[tamuni]` significa que el término solo se añade cuando está presente en el marco; actualmente, solo con Censo. El modelo de voto no utiliza `sitlab2`.

La fuente también cambia:

- la distribución y granularidad de las celdas;
- los pesos poblacionales;
- las imputaciones de recuerdo y participación;
- la población sobre la que se agregan voto y escaños;
- la firma del marco y todos sus modelos dependientes.

Incluso usando EPA, el pipeline sigue necesitando `data-raw/CensoPersonas_2021.parquet` para construir determinadas covariables provinciales. `poststrat_source: epa` no elimina esa dependencia.

## `run.recent_studies`

Indica cuántos estudios CIS recientes entran en el modelo de voto:

``` yaml
recent_studies: 2
```

El adaptador:

1.  elimina observaciones sin intención directa de voto;
2.  convierte `estudio` a texto;
3.  ordena los códigos de estudio;
4.  selecciona los últimos `N`;
5.  aplica recodificaciones, casos completos y candidaturas.

Si se solicita un número mayor que los estudios disponibles, se utilizan todos los disponibles. El valor afecta también a participación cuando su fuente contiene `estudio`, como ocurre con el tracking longitudinal; no cambia la encuesta histórica utilizada para recuerdo. La estructura temporal ya no se activa implícitamente por combinar estudios: depende de `run.temporal`.

Más estudios proporcionan mayor muestra y estabilidad, pero mezclan periodos más alejados. Menos estudios reflejan mejor el momento reciente, a costa de más varianza y menor cobertura provincial.

## `run.temporal`

Controla exclusivamente la estructura temporal del modelo de voto:

``` yaml
temporal:
  unit: day
  structure: linear_varying
```

`unit` define cómo se agrupan y distancian las entrevistas:

- `month`: periodos `YYYY-MM` y distancias en meses de calendario;
- `day`: periodos `YYYY-MM-DD` y distancias en días de calendario.

Los huecos del calendario se conservan. La tendencia se normaliza sobre la ventana seleccionada: el primer periodo vale `-1` y el último `0`, por lo que su prior tiene la misma escala con meses y días.

`structure` admite:

- `none`: no incorpora tiempo en la fórmula;
- `linear_varying`: incorpora `periodo_tendencia + (1 | periodo)`.

`linear_varying` exige fechas completas y al menos cinco periodos distintos; el pipeline falla explícitamente si no se cumplen esos contratos. La predicción se sitúa en el último periodo observado. El perfil de tracking diario usa `day` y `linear_varying`; los perfiles longitudinal y flash parten de `month` y `none`.

## `run.vote_sampling`

Configura únicamente el muestreador MCMC del modelo de voto:

``` yaml
vote_sampling:
  adapt_delta: 0.9
  max_treedepth: 10
  chains: 4
```

Los valores solo se aplican con `run.inference: full`; el perfil `fast` conserva sus controles variacionales. Participación y recuerdo mantienen cuatro cadenas, `adapt_delta = 0.8` y `max_treedepth = 10`.

Reducir `chains` a `2` permite ejecuciones MCMC exploratorias, aunque la validación definitiva debe usar normalmente `4`. `cores` no forma parte de la interfaz pública: el pipeline usa como máximo cuatro y nunca más que el número de cadenas de voto. Cambiar cualquiera de estas claves invalida la estimación y el checkpoint de voto, pero no el marco ni sus modelos auxiliares.

### Backtests

Los backtests no heredan actualmente esta clave. `R/backtest.R` fija `recent_studies = 1` para cada elección histórica. Modificar `run.recent_studies` solo cambia la ejecución productiva. En el bundle de 2023, voto y participación leen el mismo longitudinal pre-23J; esa selección conserva exclusivamente el tracking 3416.

## `run.inference`

Selecciona el método de inferencia de los tres modelos:

``` yaml
inference: fast
```

### Perfil `fast`

- inferencia variacional `meanfield`;
- backend `cmdstanr`;
- semilla `1234`;
- una cadena y un core;
- 200 draws usados en imputación y predicción;
- checks posteriores sobre 20 draws;
- controles VI fijos: `eta = 0.05`, evaluación del ELBO cada 100 iteraciones y tolerancia relativa `0.001`.

Presupuestos máximos:

| Modelo        | Iteraciones |
|---------------|------------:|
| Participación |       2.000 |
| Recuerdo      |       5.000 |
| Voto          |      10.000 |

Es el perfil operativo y sirve para iteración rápida. `meanfield` puede infraestimar incertidumbre y alcanzar el máximo de iteraciones sin converger. El pipeline comprueba que las probabilidades categóricas sean finitas y sumen uno, pero no aplica gates MCMC.

### Perfil `full`

- muestreo MCMC;
- cuatro cadenas y cuatro cores para participación y recuerdo;
- semilla `1234`;
- 500 draws usados en imputación y predicción;
- checks posteriores sobre 100 draws;
- controles de voto tomados de `run.vote_sampling`;
- `adapt_delta = 0.8` y profundidad máxima `10` para participación y recuerdo.

Presupuestos por cadena:

| Modelo        | Iteraciones | Warmup |
|---------------|------------:|-------:|
| Participación |       1.500 |    750 |
| Recuerdo      |       2.000 |    400 |
| Voto          |       2.000 |    400 |

La ejecución falla si se incumple cualquiera de estos gates:

- Rhat máximo mayor que `1.05`;
- ESS bulk mínimo menor que `400`;
- ESS tail mínimo menor que `400`;
- alguna divergencia;
- alguna transición que alcance la profundidad máxima del árbol.

El YAML no permite personalizar cadenas, cores, semilla, iteraciones, priors o controles de Stan. Son decisiones internas deliberadamente fijas.

## Calibración del marco

La calibración se aplica después de ajustar participación y recuerdo y antes de predecir voto. No modifica los coeficientes de los modelos; modifica la masa poblacional que se postestratifica.

### `run.calibration.recall`

``` yaml
recall: 0.50
```

Controla la mezcla draw a draw entre dos predicciones completas:

- la distribución de recuerdo estimada por el modelo;
- la distribución objetivo derivada del resultado oficial anterior.

Cada escenario pasa separadamente por el modelo y la calibración de participación. Sus votos provinciales se mezclan antes de la calibración nacional y de D'Hondt:

``` text
votos finales =
  recall × votos con recuerdo oficial +
  (1 - recall) × votos con recuerdo modelado
```

- `0`: no utiliza el resultado anterior para calibrar el recuerdo.
- `1`: reproduce exactamente su distribución provincial agregada.
- `0.50`: pondera por igual ambos escenarios.

El resultado anterior recodifica partidos regionales en `Nac_izq` y `Nac_der`, blancos en `OTBL`, y abstención/nulos en `ABNL`.

### `run.calibration.turnout`

``` yaml
turnout: 0.71
```

El modelo utiliza una respuesta ordinal 0--10 cuando la fuente ofrece `prob_voto`, y Bernoulli cuando existe participación histórica observada. Para cada draw desplaza las probabilidades en escala logit hasta que:

``` text
suma(votantes esperados) / suma(población postestratificada) = turnout
```

El desplazamiento conserva el orden relativo de propensión y garantiza probabilidades estrictamente acotadas. Los extremos `0` y `1` no son válidos.

### `run.national_calibration`

La calibración nacional es opcional; su activación efectiva se lee del YAML. El CSV debe contener `draw`, `partido` y `voto_pct` en escala 0--1. Puede listar solo algunos partidos; las categorías no listadas forman un residual y conservan sus proporciones MRP dentro de cada provincia al repartirlo.

`strength` controla la mezcla geométrica general entre la predicción nacional y el objetivo externo. `otbl_strength` es opcional: omitido o `null`, hereda `strength` y reproduce el comportamiento anterior. Su valor debe ser finito y cumplir `strength <= otbl_strength <= 1`.

Cuando `otbl_strength > strength`, OTBL debe estar modelado y figurar explícitamente en todos los draws del CSV. No se deduce su objetivo del residual. Para cada draw se calcula primero el vector `b` con la mezcla general. Si `s = strength`, `u = otbl_strength` y `t` es el objetivo externo de OTBL, el objetivo final es:

``` text
a = (u - s) / (1 - s)
q_OTBL = (1 - a) * b_OTBL + a * t
q_k = b_k * (1 - q_OTBL) / (1 - b_OTBL), para k distinto de OTBL
```

Las fórmulas del refuerzo se aplican solo si `u > s`. Con intensidades iguales se mantiene la mezcla general sin cambios; con `u = 1`, se asigna directamente `q_OTBL = t`. Esta segunda mezcla es lineal: aumentar `u` manteniendo `s` fijo aproxima OTBL al objetivo, tanto para reducirlo como para aumentarlo. Las demás categorías, incluido el residual, conservan sus cocientes nacionales respecto a `b`, pero sus cuotas absolutas cambian. Sus cocientes provinciales pueden cambiar durante el ajuste.

Con ambas intensidades a cero se devuelve la predicción original, validando el CSV si `enabled` es verdadero. `strength = 0` y `otbl_strength > 0` permiten calibrar exclusivamente OTBL. Con `enabled: false` no se lee el CSV. Un ejemplo ilustrativo de refuerzo, sin recomendar esos valores para producción:

``` yaml
national_calibration:
  enabled: true
  draws_path: data-raw/national-vote-draws.csv
  strength: 0.1
  otbl_strength: 0.8
```

Se realiza un único ajuste provincial con el objetivo final `q`, conservando los totales provinciales y ceros estructurales antes de recalcular D'Hondt. Si no existe soporte o los márgenes son inviables, se produce un error; no se crean masas artificiales. Tampoco se inventa una composición para un complemento de OTBL vacío que deba recibir masa positiva.

Los diagnósticos de `run_metadata.rds` identifican el método `otbl_reinforcement_v1`, ambas intensidades efectivas y `target_hash` (SHA-256 del CSV leído). Conservan `achieved` por partido y añaden `margins` por draw y categoría de calibración: `initial_pct`, `external_pct`, `base_pct`, `final_pct` y `achieved_pct`. `external_pct` es el objetivo externo; `final_pct` es el margen solicitado después de las dos mezclas. Las categorías no listadas se registran juntas como `residual`. Con calibración desactivada no se generan estos márgenes ni una huella del CSV.

Cambiar estas intensidades no invalida el marco, los modelos ni sus checkpoints: la predicción y calibración se ejecutan con la configuración actual. La opción se mantiene en `null` hasta disponer de objetivos válidos de OTBL. Elegir una intensidad para producción requiere análisis de sensibilidad y validación histórica con información disponible antes de cada elección.

### Orden de aplicación

``` text
modelo de recuerdo
  → escenarios modelado y oficial
  → participación ordinal/binaria y calibración logit
  → modelo de voto por escenario
  → mezcla de recuerdo (`recall`)
  → calibración nacional opcional
  → D'Hondt
```

## Firmas, caché y `--rebuild`

Existen dos firmas:

1.  **Marco**: postestratificación, recuerdo y participación.
2.  **Estimación**: CIS preparado y modelo de voto.

Cada firma contiene sólo los datos, esquemas, controles y versiones de caché relevantes para su etapa. La firma de marco excluye la calibración nacional y los controles exclusivos de voto. La firma de estimación incluye la firma del marco y la especificación efectiva de voto. La predicción, calibración nacional y asignación de escaños se ejecutan siempre con la configuración y los inputs actuales, aunque ambos ajustes se reutilicen.

Los checkpoints usan una firma adicional basada en los datos Stan canónicos, fórmula, familia, priors, presupuesto, semilla, algoritmo, controles y versiones. Columnas no utilizadas y atributos de importación no invalidan el ajuste. Los checkpoints de la versión anterior se adoptan y reescriben sólo cuando todos esos componentes pueden validarse.

Las cachés principales se separan por perfil:

- producción: `data-processed/<perfil>/` y `models/<perfil>/`;
- backtests: `data-processed/validation/<elección>/<perfil>/` y `models/validation/<elección>/<perfil>/`;
- checkpoints: `models[/validation/<elección>]/checkpoints/<perfil>/`.

Una caché incompatible informa qué componente superior de su manifiesto ha cambiado. Los artefactos principales sin perfil de la arquitectura anterior no se migran automáticamente.

`MRP_FRAME_CACHE_VERSION`, `MRP_ESTIMATE_CACHE_VERSION` y `MRP_CHECKPOINT_VERSION` son contratos internos. Cualquier cambio de código que altere la semántica de una etapa debe incrementar únicamente la versión correspondiente.

``` powershell
# Reutiliza únicamente artefactos con firma idéntica
Rscript R/run.R

# Ignora las firmas y reconstruye todo
Rscript R/run.R --rebuild
```

No es necesario borrar manualmente RDS o modelos.

## Contrato `domain`

Estas claves no son parámetros ordinarios:

### `domain.reference_party`

Categoría de referencia del modelo categórico de voto. Debe existir en la encuesta y estar disponible en las 52 provincias de la cuadrícula de candidaturas. Cambiarla modifica la parametrización completa del modelo.

### `domain.vote_geography`

Define cómo se incorpora la CCAA en cada categoría de voto:

``` yaml
vote_geography:
  random_min_ccaa: 5
  support_exclusions:
    - {partido: PNV, ccaa: "15"}
```

El soporte se deriva del cruce provincia–CCAA del marco y de las provincias con
`se_presenta_fit = true`, no de los votos observados. Con al menos
`random_min_ccaa` CCAA se usa un intercepto
aleatorio cuyos niveles se limitan al soporte del partido; con dos a cuatro se
usan contrastes fijos regularizados, y con una no se añade término CCAA. La
referencia de los contrastes es la CCAA con más provincias disponibles, con
desempate por código.

`support_exclusions` elimina una CCAA únicamente del componente geográfico. No
modifica `candidaturas.csv`, los offsets ni el soporte de predicción. En la
configuración actual PNV sigue disponible en Navarra (`15`), pero no recibe un
efecto navarro propio; EH Bildu conserva Navarra y se modela mediante un
contraste fijo frente a Euskadi (`16`). Esta sección afecta a la firma de
estimación, no a la del marco.

### `domain.unavailable_offset`

Offset añadido al predictor lineal de una candidatura no disponible. Debe ser negativo; `-1000000` hace que su probabilidad sea numéricamente cero tanto en el likelihood como en la predicción.

No debe utilizarse para ajustar la intensidad de una penalización. Representa una imposibilidad estructural.

### `domain.availability_tolerance`

Máxima probabilidad permitida en candidaturas no disponibles después de la predicción. Si se supera, el pipeline falla. El valor habitual es `1e-10`.

### `domain.district_threshold`

Proporción mínima de voto válido provincial que debe alcanzar una candidatura para entrar en el reparto D'Hondt. El contrato productivo utiliza `0.03`, de acuerdo con el artículo 163.1.a de la [LOREG](https://www.boe.es/buscar/act.php?id=BOE-A-1985-11672).

La proporción se calcula antes de excluir `OTBL`, de modo que el denominador incluye el voto en blanco y el resto de voto válido agregado. Una candidatura con exactamente el 3 % es elegible; `OTBL` nunca recibe escaños.

### `domain.expected_seats`

Número total de escaños que debe repartir cada draw. Se utiliza como invariante posterior a D'Hondt; no modifica el fichero provincial `nseats.xlsx`.

### `domain.left_parties` y `domain.right_parties`

Listas utilizadas para agregar el voto anterior de los partidos en covariables provinciales de bloque ideológico. No controlan gráficos ni coaliciones posteriores.

## Contrato `paths`

``` yaml
paths:
  raw_root: data-raw
  processed_root: data-processed
  models_root: models
  output_root: output
```

Solo se pueden cambiar los cuatro directorios raíz. Los nombres concretos de los inputs y outputs productivos son convenciones fijas:

| Tipo                 | Ruta relativa al directorio raíz |
|----------------------|----------------------------------|
| CIS longitudinal     | `cis-longitudinal_23.parquet`    |
| CIS anterior         | `3420.sav`                       |
| Censo                | `CensoPersonas_2021.parquet`     |
| EPA                  | `epa/`                           |
| Resultado anterior   | `resultados_cer_provincias.csv`  |
| Candidaturas         | `candidaturas.csv`               |
| Escaños provinciales | `nseats.xlsx`                    |
| DEGURBA              | `EU-27-LAU-2025-NUTS-2024.xlsx`  |

No existen overrides individuales de rutas en la interfaz pública. Los artefactos intermedios y modelos se escriben bajo un subdirectorio `fast` o `full`; los resultados finales permanecen bajo `output_root` y representan la última ejecución.

## Contratos `schemas`

Los esquemas traducen fuentes externas a las variables canónicas del modelo. No están pensados para ajustarse entre ejecuciones.

### Estructuras comunes

#### `column_map`

Mapea nombres de la fuente a nombres internos:

``` yaml
column_map:
  CPRO: provincia
  SEXO: sexo_raw
```

La clave es la columna original; el valor, la columna canónica. Una columna obligatoria ausente provoca un error al preparar los datos.

#### Recodificaciones agrupadas

``` yaml
estudios_recode:
  "2": [FP]
  "3": [Superiores]
  default: 1
```

Cada clave de destino contiene los códigos o etiquetas de origen que deben agruparse. `default` recibe cualquier valor no enumerado. El adaptador compara tanto el código crudo como su etiqueta importada.

#### Recodificaciones planas

``` yaml
voto_recode:
  remap:
    Compromís: Sumar
    EH Bildu: EH_Bildu
  default: OTBL
```

`remap` establece correspondencias explícitas. `default` absorbe categorías no reconocidas. Añadir o eliminar partidos exige revisar también `voto_levels` y la cuadrícula completa de candidaturas.

### `schemas.poststrat.censo`

Define columnas, filtro de nacionalidad y recodificaciones de sexo, estudios y situación laboral del Censo. `source` debe ser `censo`.

`tamuni` no se configura mediante una tabla: se deriva internamente de `CMUN`.

### `schemas.poststrat.epa`

Define el patrón de ficheros, columnas, filtros de edad/nacionalidad, correcciones de códigos de CCAA y recodificaciones de sexo y estudios. `source` debe ser `epa`.

La situación laboral se deriva internamente de las variables de actividad y búsqueda de empleo; no tiene una tabla YAML independiente.

### `schemas.vote`

Controla la encuesta CIS usada para intención de voto:

- `column_map`: columnas del parquet longitudinal;
- `voto_levels`: universo ordenado de respuestas y partidos;
- `recuerdo_levels`: categorías permitidas de recuerdo;
- `voto_recode`: intención de voto;
- `voto_simp`/`voto_aux`: simpatía usada solo por el experimento de indecisos;
- `participation`: escala ordinal de `prob_voto` cuando esta encuesta alimenta el modelo de participación;
- `recuerdo_recode`: recuerdo;
- recodificaciones de estudios, situación laboral y municipio.

`ABNL`, `<18` e `Indecisos` pueden existir durante la limpieza, pero se excluyen antes de ajustar el modelo de voto. Las observaciones que contradicen la disponibilidad provincial también se descartan.

El orden y contenido de `voto_levels` afecta:

- los niveles de la respuesta categórica;
- los offsets del choice set;
- la cuadrícula provincia--partido exigida a `candidaturas.csv`;
- la forma de las predicciones.

### `schemas.previous`

Controla el CIS histórico utilizado para recuerdo y, en los bundles que no ofrecen una escala prospectiva, para participación observada:

- columnas canónicas;
- filtro de recuerdo;
- códigos que representan participación o no participación;
- niveles y recodificación de recuerdo;
- recodificación del resultado oficial anterior;
- estudios, situación laboral y tamaño municipal.

`results_recode` debe ser coherente con `recuerdo_levels`, ya que proporciona los objetivos provinciales usados por `calibration.recall`.

## Contrato `backtests`

Los backtests son escenarios históricos fijos, no una rejilla de tuning.

El CLI permite limitar las variantes sin modificar el YAML:

``` powershell
Rscript R/backtest.R 2023 --variant=production
Rscript R/backtest.R 2023 --variant=production,geography --rebuild
```

Si se omite `--variant`, se ejecutan todas las variantes configuradas.

### `backtests.elections`

Lista de identificadores aceptados por `R/backtest.R`. Debe coincidir exactamente con los nombres presentes en `backtests.bundles`.

### `backtests.bundles`

Cada bundle declara:

| Clave | Significado |
|----|----|
| `root` | Directorio bajo `raw_root` |
| `previous` | Encuesta para recuerdo |
| `turnout` | Encuesta para participación |
| `vote` | Encuesta longitudinal anterior a la elección |
| `poststrat_source` | `censo` o `epa` histórico |
| `poststrat` | Fichero o directorio del marco |
| `previous_results` | Resultado anterior usado por el pipeline |
| `target_results` | Resultado que solo se usa para evaluar |
| `candidates` | Cuadrícula histórica provincia--partido |
| `seats` | Escaños provinciales históricos |
| `previous_schema` | Esquema de la encuesta anterior |
| `turnout_schema` | Esquema de participación |
| `vote_schema` | Esquema del CIS de voto |
| `fit_overrides` | Excepciones históricas aplicadas solo durante el ajuste |

Los ficheros se resuelven dentro de `root`. Para `poststrat`, si la ruta no existe dentro del bundle se intenta resolver bajo `raw_root`.

`fit_overrides` corrige inconsistencias conocidas entre la encuesta histórica y la cuadrícula de candidaturas durante el ajuste. No modifica la disponibilidad usada en la predicción final.

### `backtests.variants`

Las variantes actuales son:

``` yaml
production:
  weight_scheme: sample
  geo_effect: ccaa

recency:
  weight_scheme: sample_recency
  recency_lambda: 0.35
  geo_effect: ccaa

geography:
  weight_scheme: sample
  geo_effect: ccaa_provincia
```

La variante `production` también produce una ablación secuencial. La etapa `actual` usa recuerdo modelado y participación sin el desplazamiento logit; después se añaden la mezcla de recuerdo, el ajuste de participación y, si está habilitada, la calibración nacional. Los indecisos solo aparecerán como etapa después de una integración explícita; el experimento actual no supera su puerta de entrada y por eso se omiten.

#### `weight_scheme`

- `sample`: utiliza únicamente el peso muestral normalizado.
- `sample_recency`: multiplica el peso muestral por `exp(-recency_lambda × periodo_edad)`; la edad se mide en `run.temporal.unit`.

#### `recency_lambda`

Controla la velocidad de decaimiento temporal:

- `0`: equivale a no penalizar observaciones antiguas;
- valores mayores concentran más peso en el último periodo;
- `0.35` multiplica el peso por aproximadamente `0.70` por cada unidad temporal de antigüedad.

Solo tiene efecto con `weight_scheme: sample_recency`.

#### `geo_effect`

- `ccaa`: intercepto aleatorio por CCAA.
- `ccaa_provincia`: interceptos aleatorios por CCAA y provincia.

Estas variantes se comparan con los baselines de encuesta ponderada y uniform swing. No se selecciona automáticamente una variante a partir del resultado.

## Qué cambios requieren especial cuidado

| Cambio | Riesgo principal |
|----|----|
| Fuente Censo/EPA | Cambia población, pesos y presencia de `tamuni` |
| Número de estudios | Cambia la muestra y los periodos disponibles |
| Unidad/estructura temporal | Cambia fórmula, escala de recencia y predicción |
| Calibraciones | Cambia directamente la composición del marco final |
| Niveles de voto | Cambia respuesta, choice set y candidaturas exigidas |
| Recodificaciones | Puede mover masa entre partidos o estratos |
| Partido de referencia | Reparametriza todo el modelo categórico |
| Offset/tolerancia | Puede romper la garantía de candidaturas |
| Bundles históricos | Puede introducir información posterior a la elección |

Después de cambiar contratos internos deben ejecutarse:

``` powershell
Rscript validate_syntax.R
Rscript tests/run-tests.R
Rscript R/run.R --rebuild
Rscript R/backtest.R all --rebuild
```

Los backtests nunca deben ajustarse mirando el resultado objetivo.
