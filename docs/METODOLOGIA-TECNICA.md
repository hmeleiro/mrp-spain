# Metodología estadística y de datos

Este documento describe el pipeline con un nivel intermedio: presupone que el
lector conoce la idea de MRP, modelos categóricos y distribuciones posteriores,
pero no entra en una derivación académica completa.

Para una explicación conceptual, véase
[Cómo funciona la estimación electoral](METODOLOGIA-INTRODUCCION.md). Las
opciones operativas se documentan aparte en la
[referencia de configuración](CONFIGURATION.md).

## 1. Objeto de la estimación

La unidad electoral es la provincia. Para cada draw \(s\), provincia \(p\) y
partido \(k\), el pipeline produce:

- votos esperados \(V_{spk}\);
- proporción provincial \(V_{spk}/\sum_k V_{spk}\);
- escaños obtenidos al aplicar D'Hondt de forma determinista.

La salida no es una muestra de papeletas individuales. Cada draw combina
incertidumbre paramétrica de tres modelos ajustados por separado —recuerdo,
participación y voto— y, cuando está habilitada, una distribución nacional
externa. No se añade una capa multinomial de realización electoral.

## 2. Linaje de datos

| Artefacto | Contenido | Papel metodológico |
|---|---|---|
| `data-raw/cis-longitudinal_23.parquet` | Tracking CIS | Participación declarada e intención directa de voto |
| `data-raw/3420.sav` | CIS histórico | Modelo de recuerdo de voto |
| `data-raw/CensoPersonas_2021.parquet` | Microdatos censales | Marco alternativo y covariables provinciales |
| `data-raw/epa/*.sav` | Ficheros EPA | Marco alternativo con factores de elevación |
| `data-raw/resultados_cer_provincias.csv` | Elección anterior | Márgenes oficiales de recuerdo, voto y participación previos |
| `data-raw/EU-27-LAU-2025-NUTS-2024.xlsx` | Población municipal por grado de urbanización | Covariables provinciales |
| `data-raw/candidaturas.csv` | Rejilla provincia–partido | Soporte del modelo de elección |
| `data-raw/nseats.xlsx` | Escaños por provincia | Asignación D'Hondt |
| `data-raw/national-vote-draws.csv` | Draws nacionales opcionales | Calibración nacional |

Las columnas se adaptan mediante los contratos de `config/mrp.yml`. Las
etiquetas electorales se normalizan antes de agrupar partidos y opciones.
`OTBL` recoge voto en blanco y, según la fuente, opciones no modeladas;
`ABNL` agrupa abstención y voto nulo en las etapas de recuerdo y
participación. Estas agregaciones no tienen exactamente el mismo significado
sustantivo y deben interpretarse como categorías operativas.

### 2.1. Marco de postestratificación

Los dos adaptadores terminan en un esquema común:

- `sexo`;
- `edad_grupo`: 18–24, 25–34, 35–44, 45–54, 55–64 y 65+;
- `estudios`;
- `sitlab2`, una situación laboral reducida;
- `ccaa` y `provincia`;
- `tamuni`, solo con Censo;
- `FACTOREL`, masa poblacional.

Con **Censo**, se conservan personas de 18 o más años que cumplen el filtro de
nacionalidad configurado. Las filas se agregan por la combinación completa de
variables y `FACTOREL` es el recuento de personas.

Con **EPA**, se concatenan todos los `.sav` del directorio. Se aplican los
filtros de edad y nacionalidad y se divide cada factor de elevación por el
número de ficheros, de modo que combinar periodos representa una población
media, no la suma de sus poblaciones. La primera banda EPA disponible agrupa
códigos de edad que se etiquetan como 18–24; esta aproximación puede incorporar
parte del tramo 16–17 y es una limitación del adaptador actual.

Para modelar recuerdo se agregan después las filas del marco por:

```text
CCAA × provincia × [tamaño de municipio] × sexo × edad
× estudios × situación laboral
```

El peso de la celda \(c\) es:

\[
N_c = \sum_{j \in c} \text{FACTOREL}_j.
\]

### 2.2. Covariables provinciales

Se construyen y estandarizan entre provincias:

- margen previo derecha menos izquierda;
- proporción de hogares en alquiler;
- proporción de mayores de 65 años;
- proporciones en los tres grados de urbanización;
- participación previa, solo para el modelo de participación.

El margen político depende de las listas de partidos de izquierda y derecha
definidas en `domain`. Todas estas variables se tratan como datos fijos.

## 3. Modelo de recuerdo

Sea \(R_i\) la categoría de recuerdo declarada por la persona \(i\). Se ajusta
una regresión logística categórica con:

```text
recuerdo ~ sexo + edad_grupo + estudios + sitlab2 + [tamuni]
           + margen_der_izq_prev + pct_alquiler + pct_mayores65
           + degurba1 + degurba2 + degurba3
           + (1 | provincia)
```

Los términos provinciales que no existan en los datos se omiten. El intercepto
aleatorio provincial introduce *partial pooling*. Aunque el adaptador conserva
`ponde`, el likelihood de recuerdo no usa actualmente pesos muestrales.

Para cada draw \(s\), celda \(c\) y recuerdo \(r\), se obtiene
\(q^{R}_{scr}\). El escenario puramente modelado contiene:

\[
M_{scr} = N_c q^{R}_{scr}.
\]

### 3.1. Escenario ajustado al resultado anterior

Los resultados anteriores se recodifican a las mismas categorías de recuerdo.
Para cada provincia se calculan proporciones oficiales \(T_{pr}\), incluyendo
`OTBL` y `ABNL`.

En cada draw y provincia se aplica ajuste biproporcional a la matriz
celda × recuerdo, usando \(M_{scr}\) como semilla. El algoritmo conserva
simultáneamente:

\[
\sum_r O_{scr}=N_c
\quad\text{y}\quad
\sum_{c \in p} O_{scr}=N_p T_{pr}.
\]

Así se obtienen dos poblaciones completas, `model` y `official`. La segunda
cuadra exactamente con los márgenes provinciales anteriores, pero conserva en
lo posible las asociaciones demográficas aprendidas por el modelo.

## 4. Modelo y calibración de participación

La fórmula es:

```text
participa ~ sexo + edad_grupo + estudios + sitlab2 + [tamuni]
            + margen_der_izq_prev + pct_alquiler + pct_mayores65
            + degurba1 + degurba2 + degurba3 + participacion_prev
            + recuerdo + (1 | provincia)
```

En producción, `participa` es una escala ordenada 0–10 y se utiliza un modelo
ordinal acumulativo con enlace logit. Cuando una fuente histórica contiene
participación observada, se usa Bernoulli con enlace logit. El likelihood de
participación tampoco aplica actualmente `ponde`.

Para la respuesta ordinal, la predicción es la esperanza de la categoría
reescalada:

\[
\pi_{sc}
= \sum_{h=0}^{10} \Pr(Y_{sc}=h)\frac{h}{10}.
\]

No se interpreta como la probabilidad calibrada final de votar, sino como una
propensión inicial. La misma matriz \(\pi_{sc}\) se aplica a los escenarios de
recuerdo modelado y oficial.

### 4.1. Calibración logit

Para cada draw y cada escenario de recuerdo se busca un desplazamiento
\(\delta_s\) que satisfaga:

\[
\frac{\sum_c W_{sc}
  \operatorname{logit}^{-1}\{\operatorname{logit}(\pi_{sc})+\delta_s\}}
 {\sum_c W_{sc}}
= \tau,
\]

donde \(W_{sc}\) es la población de la celda-recuerdo y \(\tau\) es
`run.calibration.turnout`.

El número esperado de votantes es:

\[
E_{sc}=W_{sc}
\operatorname{logit}^{-1}\{\operatorname{logit}(\pi_{sc})+\delta_s\}.
\]

La calibración fija exactamente la tasa nacional de cada draw. Preserva el
ranking de propensión, pero no su distancia absoluta, y trata \(\tau\) como
conocido sin error.

## 5. Modelo categórico de voto

La muestra se restringe a casos completos con intención directa de voto. Se
eliminan `ABNL`, `<18` e `Indecisos`; `OTBL` permanece. `voto_aux`, que recoge
simpatía u otra medida auxiliar cuando existe, no interviene en producción.

El modelo es categórico con PP como referencia. Para cada partido distinto de
PP utiliza:

```text
voto | weights(peso_modelo) ~
  sexo + edad_grupo + sexo:edad_grupo + estudios + recuerdo + [tamuni]
  + [periodo_tendencia]
  + margen_der_izq_prev + pct_alquiler + pct_mayores65
  + degurba1 + degurba2 + degurba3
  + geografia_ccaa(partido) + [(1 | provincia)] + [(1 | periodo)]
  + offset_disponibilidad
```

La geografía de CCAA se construye por partido desde el cruce provincia–CCAA del
marco y la cuadrícula de candidaturas. Con cinco o más CCAA disponibles se usa un intercepto aleatorio
cuyos niveles se restringen al soporte electoral; con dos a cuatro se usan
contrastes fijos, y con una se omite el término. Así no se estiman efectos CCAA
para combinaciones estructuralmente prohibidas. Las exclusiones geográficas
declaradas en `domain.vote_geography` no cambian los offsets de disponibilidad.
El efecto provincial se estudia como variante de backtest. La configuración temporal elige periodos
mensuales o diarios y puede desactivar el tiempo. Con `linear_varying` se
incluyen una tendencia normalizada y un intercepto parcialmente agrupado por
periodo; se requieren al menos cinco periodos y la predicción se sitúa en el
último observado.

El peso ordinario es:

\[
w_i=\frac{\text{ponde}_i}{\overline{\text{ponde}}}.
\]

La normalización mantiene un peso medio de uno. La variante experimental de
recencia multiplica además por
\(\exp(-\lambda a_i)\), donde \(a_i\) es la antigüedad en la unidad temporal
configurada, meses o días; no es la variante productiva.

### 5.1. Priors

Los tres modelos utilizan priors generados por clase y parámetro:

- coeficientes: \(\mathcal N(0,0.5)\);
- interceptos: Student-\(t(3,0,2)\);
- desviaciones de efectos aleatorios: Exponencial(3).

Son priors regularizadores comunes a todas las categorías compatibles con la
fórmula de `brms`.

### 5.2. Disponibilidad provincial

Para un partido \(k\) no disponible en la provincia \(p\), el predictor lineal
recibe un offset de \(-10^6\); en otro caso, cero. PP debe estar disponible en
las 52 provincias porque es la categoría de referencia.

Antes del ajuste se eliminan observaciones que contradigan la rejilla de
candidaturas. Tras predecir, un *guardrail* verifica que la probabilidad total
asignada a candidaturas ausentes no supere \(10^{-10}\). La misma comprobación
se repite después de la calibración nacional.

## 6. Predicción y postestratificación

`posterior_epred()` devuelve \(q^{V}_{sck}\), la probabilidad esperada de cada
partido para cada celda-recuerdo. Incluye todos los efectos aleatorios y
permite niveles nuevos muestreados con incertidumbre. Si hay varios meses, la
celda se evalúa en el último.

Para cada escenario \(a \in \{\text{model},\text{official}\}\):

\[
V^{(a)}_{spk}
= \sum_{c \in p} q^{V}_{sck} E^{(a)}_{sc}.
\]

Los dos resultados se mezclan con
\(\alpha=\texttt{run.calibration.recall}\):

\[
V_{spk}
=(1-\alpha)V^{(\text{model})}_{spk}
+\alpha V^{(\text{official})}_{spk}.
\]

Esta mezcla se realiza sobre conteos esperados draw a draw y se renormalizan
después las proporciones provinciales.

Los draws de los tres ajustes se obtienen por separado y se emparejan por su
posición en las matrices. Esto propaga la dispersión marginal de cada modelo,
pero no representa una distribución posterior conjunta que estime
correlaciones entre sus parámetros.

## 7. Calibración nacional opcional

El fichero externo debe contener `draw`, `partido` y `voto_pct`. Puede omitir
partidos; estos se tratan conjuntamente como una categoría residual.

Sea \(m_{sk}\) la cuota nacional MRP y \(t_{sk}\) el objetivo externo. Con
intensidad \(\gamma\), el margen deseado se interpola geométricamente:

\[
g_{sk}
\propto m_{sk}^{1-\gamma}t_{sk}^{\gamma}.
\]

Por tanto, \(\gamma=0\) no modifica el MRP y \(\gamma=1\) reproduce el objetivo.
Cada draw de predicción se empareja secuencialmente con un draw objetivo; si
hay menos objetivos, se reciclan cíclicamente.

Después se ajusta la matriz provincia × categoría mediante *raking* para
conservar:

- los totales de voto de cada provincia;
- los márgenes nacionales \(g_{sk}\);
- los ceros estructurales;
- la composición relativa de partidos no listados dentro del residual de cada
  provincia.

### 7.1. Generación del objetivo disponible en el repositorio

`tools/estimate-poll-of-polls.R` es un proceso separado, no una dependencia
automática de `R/run.R`. El script:

1. consulta encuestas mediante `spainpolls`;
2. selecciona trabajo de campo de los últimos 30 días;
3. pondera por recencia, con vida media de 10 días, y por la raíz del tamaño
   muestral relativo;
4. imputa un partido ausente en una encuesta con su media entre las demás;
5. aplica un *Bayesian bootstrap* con pesos exponenciales;
6. genera 500 draws con semilla 1234.

Este procedimiento representa variación entre encuestas y sensibilidad a su
peso, pero no es un modelo explícito de *house effects*, correlación temporal
o error de muestreo dentro de cada encuesta. El CSV debe actualizarse y
auditarse por separado.

## 8. Conversión a escaños

Para cada provincia y draw se calcula primero:

\[
h_{spk}=\frac{V_{spk}}{\sum_j V_{spj}}.
\]

El denominador incluye `OTBL`, que representa voto válido en blanco y otras
opciones agregadas. Una candidatura entra en el reparto si
\(h_{spk}\geq\theta\), donde
\(\theta=\texttt{domain.district_threshold}=0.03\). La comparación es
inclusiva: exactamente el 3 % es elegible. `OTBL` se excluye siempre de la
asignación. Esto implementa los artículos 163.1.a y 96.5 de la
[LOREG](https://www.boe.es/buscar/act.php?id=BOE-A-1985-11672): la barrera es
el 3 % de los votos válidos y el voto en blanco es válido.

Sobre las candidaturas elegibles se llama a `vota::fast_dhondt()` en cada
provincia y draw, con la magnitud de distrito de `nseats.xlsx`. Los votos
esperados pueden ser no enteros; esto no afecta al orden de cocientes.

La implementación no modela empates ni otras incidencias administrativas. Tras
el reparto se exige:

\[
\sum_{p,k} S_{spk}=350
\]

para todo draw, y que las cuotas de voto sumen uno dentro de cada provincia.

## 9. Inferencia y diagnósticos

| Perfil | Método | Draws de predicción | Presupuesto participación / recuerdo / voto |
|---|---|---:|---|
| `fast` | Variacional `meanfield` | 200 | 2.000 / 5.000 / 10.000 iteraciones |
| `full` | MCMC; cadenas de voto configurables | 500 | 1.500 / 2.000 / 2.000 iteraciones |

En `full`, los warmups son 750, 400 y 400 respectivamente. La ejecución falla
si Rhat máximo supera 1,05, el ESS bulk o tail mínimo baja de 400, aparece
alguna divergencia o se alcanza la profundidad máxima del árbol.

Para los modelos categóricos se comprueba que las probabilidades sean finitas
y sumen uno. Los *posterior predictive checks* comparan distribuciones
marginales observadas y predichas sobre una submuestra. Son controles de
sanidad, no una evaluación completa de calibración predictiva.

`meanfield` no dispone de los gates MCMC y tiende a subestimar dependencia y
dispersión posterior. Los intervalos del perfil `fast` deben tratarse como
aproximados.

## 10. Validación histórica

Los backtests soportan 2023 y 2019-10N. Cada elección usa datos anteriores al
objetivo, fija un estudio reciente y evalúa:

1. `production`: pesos muestrales y efecto de CCAA;
2. `recency`: añade decaimiento temporal mensual con \(\lambda=0.35\);
3. `geography`: añade efecto aleatorio provincial.

Se comparan con:

- la distribución nacional ponderada de la encuesta, replicada en todas las
  provincias;
- un *uniform swing* que suma a cada resultado provincial anterior el cambio
  nacional observado en la encuesta.

Las métricas son MAE de voto y escaños, provincial y nacional, y cobertura del
intervalo del 95 % para voto provincial. El promedio provincial se calcula
sobre pares provincia–partido, incluidos muchos ceros estructurales; por ello
conviene leerlo junto con resultados desagregados y no como único criterio.

La ablación separa recuerdo robusto, calibración de participación y calibración
nacional. La imputación de indecisos tiene una evaluación y una puerta de
entrada independientes y no forma parte del pipeline productivo.

Los informes viven en `output/validation/`. Antes de interpretar un backtest
con calibración nacional debe comprobarse en su `run_metadata.rds` que el
fichero de objetivos corresponde realmente a la fecha histórica evaluada.

## 11. Salidas y trazabilidad

`output/sims_est.rds` contiene una fila por draw, provincia y partido:

```text
draw | provincia | partido | votos | voto_pct | seats
```

Los informes nacionales calculan medias e intervalos entre los percentiles
20 y 80. Las salidas provinciales publican medias. `run_metadata.rds`
registra configuración, firmas, diagnósticos, artefactos y sesión de R.

La suite de diagnóstico bajo `output/diagnostics/voto/` añade intervalos
P20--P80, auditoría de la calibración nacional, evolución temporal
estandarizada, fortaleza y cambio por grupos sociodemográficos, estabilidad
del último escaño, bloques configurados, efectos geográficos, ajuste predictivo
y un panel técnico. El diagnóstico sociodemográfico publica la cuota dentro de
cada grupo y su sobreíndice frente al total nacional. Se calcula después de las
calibraciones de participación y recuerdo, pero antes del objetivo nacional
externo, que no identifica cómo repartir su corrección entre grupos. Sus vistas principales
usan los colores convencionales y se centran en PP, PSOE, Vox, FA, Podemos y
SALF; los CSV conservan todos los partidos. El análisis marginal distingue,
en cada draw, al defensor del último escaño y al aspirante de otro partido,
resume conjuntamente la pareja y calcula la probabilidad de involucrarse en
cualquiera de los dos roles. El informe de recuerdo
comprueba además que el escenario oficial reproduce por provincia todos sus
márgenes; la ejecución aborta si una categoría con votos positivos desaparece
o supera la tolerancia.

La fórmula de voto actual no contiene interacciones entre el tiempo y los
grupos. Por tanto, el cambio sociodemográfico es una evolución estandarizada
implícita en los niveles de apoyo de cada grupo bajo la tendencia común del
partido; no identifica pendientes temporales específicas por sexo, edad,
estudios, situación laboral o tamaño municipal, ni efectos causales.

Las cachés se firman por etapa a partir de sus datos, esquemas, controles y
versiones de contrato. Los checkpoints usan además los datos Stan canónicos,
la fórmula, la familia, los priors y los parámetros efectivos de ajuste.
Columnas ajenas al modelo no alteran esa firma. Esta es una garantía de
reproducibilidad computacional, no de vigencia sustantiva de las fuentes.

## 12. Supuestos y limitaciones principales

1. **Ignorabilidad condicional.** Dentro de las variables incluidas, las
   respuestas del CIS se tratan como informativas de la población. Sesgos no
   explicados por esas variables pueden permanecer.
2. **Armonización.** Estudios, situación laboral, edad y partidos se reducen a
   categorías comunes; la equivalencia entre fuentes es aproximada.
3. **Pesos auxiliares.** Recuerdo y participación no incorporan `ponde` en el
   likelihood; su corrección poblacional se produce principalmente al
   postestratificar. Voto sí utiliza el peso CIS.
4. **Indecisos.** Se condiciona el modelo de partido a intención decidida; no
   se redistribuye la masa indecisa en producción.
5. **Objetivos fijos.** Participación, mezcla de recuerdo e intensidad nacional
   son parámetros sin distribución de incertidumbre propia.
6. **Marcos fijos.** Factores poblacionales, resultados anteriores,
   candidaturas y magnitudes de distrito se consideran conocidos.
7. **Incertidumbre parcial.** No hay ajuste conjunto de los tres modelos ni
   simulación adicional de conducta individual el día de la elección.
8. **Calibración externa.** Si está activa, la estimación deja de ser un MRP
   autónomo: el nivel nacional depende sustantivamente del fichero externo.
9. **Denominador del umbral.** `OTBL` agrupa voto en blanco y otras opciones no
   modeladas. Su masa entra correctamente como voto válido en el denominador,
   pero no permite distinguir si alguna candidatura agregada alcanzaría por sí
   sola el 3 %.
10. **Evidencia histórica limitada.** Dos elecciones no bastan para demostrar
    estabilidad frente a todos los contextos políticos posibles.

Estas limitaciones no invalidan el método, pero delimitan qué incertidumbre
representan sus intervalos y qué afirmaciones pueden apoyarse en sus
resultados.
