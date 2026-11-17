# Cómo funciona la estimación electoral

Esta guía explica el pipeline sin presuponer conocimientos de estadística. La
idea central es sencilla: una encuesta dice cómo suelen votar distintos tipos
de personas y el Censo o la EPA dicen cuántas personas de cada tipo viven en
cada provincia. El pipeline combina ambas cosas y aplica después las reglas de
reparto de escaños.

Para una descripción con fórmulas, decisiones de modelado y limitaciones
técnicas, véase la
[guía metodológica técnica](METODOLOGIA-TECNICA.md).

## Qué problema intenta resolver

Una encuesta nacional contiene pocas entrevistas a nivel provincial. Además,
su composición nunca coincide exactamente con la población: puede haber, por
ejemplo, demasiadas personas de cierta edad y pocas de otra.

Usar directamente el porcentaje de respuestas de la encuesta sería
especialmente problemático en España, porque los 350 escaños no se reparten en
una única circunscripción nacional. Se asignan provincia a provincia.

MRP aborda ambos problemas en dos pasos:

1. **Regresión multinivel:** estima cómo cambian la participación y el voto
   entre grupos demográficos y territorios, compartiendo información entre
   territorios parecidos.
2. **Postestratificación:** aplica esas estimaciones a la composición real de
   cada provincia.

El pipeline añade dos elementos más: calibra algunas magnitudes conocidas o
fijadas externamente y convierte los votos provinciales en escaños mediante
D'Hondt.

## Qué datos utiliza

| Fuente | Para qué se usa |
|---|---|
| Tracking longitudinal del CIS | Intención directa de voto y propensión declarada a participar |
| Estudio CIS 3420 | Relación entre perfil demográfico y recuerdo de voto |
| EPA o Censo de Población | Número de personas en cada grupo demográfico y provincia |
| Resultados de la elección anterior | Recuerdo de voto oficial y características políticas provinciales |
| Censo y clasificación europea de urbanización | Alquiler, población mayor y grado de urbanización por provincia |
| Tabla de candidaturas | Partidos que se presentan en cada provincia |
| Número de escaños por provincia | Reparto territorial de los 350 diputados |
| Distribuciones nacionales externas, si se activan | Anclaje parcial a una estimación nacional agregada |

Las fuentes no se unen persona a persona. Se armonizan variables como sexo,
edad, estudios, situación laboral, provincia y recuerdo de voto, y se trabaja
con grupos o **celdas** que comparten esas características.

## El recorrido de los datos

```text
EPA o Censo
  → grupos de población por provincia
  → recuerdo de voto probable
  → probabilidad de participar
  → probabilidad de votar a cada partido
  → votos esperados por provincia
  → calibración nacional opcional
  → escaños por D'Hondt
```

### 1. Construir una versión resumida de la población

El pipeline divide la población en grupos como este:

> Mujer, 35–44 años, estudios superiores, ocupada, residente en una provincia
> determinada.

Con el Censo también puede distinguir el tamaño del municipio. La EPA no
contiene esa variable en el marco utilizado. Cada grupo recibe un peso que
representa cuántas personas hay en él.

La edad se agrupa en seis tramos: 18–24, 25–34, 35–44, 45–54, 55–64 y 65 o
más años. Estudios y situación laboral también se reducen a categorías
comparables entre las distintas fuentes.

### 2. Completar el recuerdo de voto

La población del Censo o de la EPA no informa de qué votó cada persona en la
elección anterior. Un primer modelo estima esa información a partir del estudio postelectoral del CIS
(3420), el perfil demográfico y el contexto provincial.

El pipeline conserva dos versiones:

- una distribución de recuerdo estimada íntegramente por el modelo;
- otra que mantiene las diferencias internas del modelo, pero cuadra con el
  resultado oficial anterior de cada provincia.

Ambas versiones se usan hasta la estimación de voto y se mezclan al final. El
peso de la segunda se controla con `run.calibration.recall`. Así se evita
depender por completo tanto de un recuerdo declarado potencialmente sesgado
como de una reconstrucción rígida del pasado.

### 3. Estimar quién acudirá a votar

Un segundo modelo aprende qué perfiles declaran mayor o menor propensión a
participar. En producción, los estudios preelectorales del CIS suelen ofrecer una escala de 0 a 10; el
modelo la traduce a una probabilidad esperada. En backtests con participación
observada puede utilizar una respuesta de sí/no.

Después se aplica el mismo desplazamiento, en escala logit, a todas las
probabilidades hasta alcanzar una participación nacional fijada en la
configuración. Este ajuste conserva el orden relativo: un grupo inicialmente
más propenso a votar sigue siéndolo después de calibrar.

El resultado de esta etapa no es una selección tajante de votantes y
abstencionistas. Es el **número esperado de votantes** de cada grupo.

### 4. Estimar el voto actual

El modelo principal utiliza las entrevistas con intención directa de voto
decidida. Aprende la relación entre partido y:

- sexo y edad, incluida su interacción;
- nivel de estudios;
- recuerdo de voto;
- tamaño de municipio, cuando está disponible;
- características políticas y sociales de la provincia;
- comunidad autónoma;
- tendencia y variación del periodo de entrevista cuando la configuración
  temporal está activa; el periodo puede ser un mes o un día.

Las entrevistas reciben el peso muestral del CIS. El componente multinivel
permite que territorios con pocas entrevistas tomen información del conjunto
sin obligarlos a ser idénticos.

Los indecisos, los menores de edad y la abstención declarada no forman parte de
este modelo de elección de partido. La abstención ya se trata en el modelo de
participación. El voto en blanco, agrupado con otras opciones no modeladas bajo
la etiqueta `OTBL`, sí permanece en la distribución de voto. La imputación de
indecisos se evalúa por separado y actualmente no está incorporada a producción por no proporcionar cambios sustantivos en la intención de voto.

### 5. Respetar las candidaturas provinciales

No todos los partidos se presentan en todas las provincias. La tabla
`data-raw/candidaturas.csv` forma parte del propio modelo:

- se descartan respuestas incompatibles con la candidatura disponible en la
  provincia declarada;
- una candidatura ausente recibe una probabilidad prácticamente nula durante
  el ajuste y la predicción;
- la ejecución falla si queda una masa superior a `1e-10` en candidaturas no
  disponibles.

No hay una corrección manual posterior que redistribuya esos votos.

### 6. Postestratificar

Para cada grupo de población, el pipeline multiplica:

```text
personas del grupo
  × probabilidad de participar
  × probabilidad de votar a cada partido
```

Al sumar los grupos de una provincia se obtienen sus votos esperados por
partido. Esta suma da el peso correcto a grupos muy numerosos aunque en la
encuesta hayan aparecido pocas veces.

### 7. Aplicar, si procede, una referencia nacional

La calibración nacional es opcional. Cuando está activa, acerca el total
nacional del MRP a distribuciones externas de voto con una intensidad entre 0
y 1. El ajuste conserva:

- el total de votantes estimado en cada provincia;
- los ceros de partidos que no se presentan;
- la estructura territorial producida por el MRP.

Por tanto, la fuente externa determina parcialmente el nivel nacional y el MRP
determina cómo se reparte territorialmente. El fichero externo se prepara de
forma independiente; ejecutar `R/run.R` no lo actualiza.

### 8. Convertir votos en escaños

En cada provincia se calcula primero qué candidaturas alcanzan al menos el 3 %
de los votos válidos. El denominador incluye `OTBL`, porque contiene el voto en
blanco y otras opciones válidas, aunque esa categoría no puede recibir
escaños. Las candidaturas por debajo del umbral se excluyen.

Después, cada simulación se procesa provincia a provincia con D'Hondt y el
número de diputados indicado en `data-raw/nseats.xlsx`. La salida se comprueba
para asegurar que cada simulación suma 350 escaños.

## De dónde sale la incertidumbre

Los modelos no producen un único resultado. Generan muchos *draws* o
escenarios plausibles. En cada uno cambian las probabilidades de recuerdo,
participación y voto, la posible referencia nacional y, en consecuencia, los
escaños.

Los informes resumen esos escenarios con una media y, normalmente, los
percentiles 20 y 80. Este intervalo central del 60% expresa incertidumbre
condicionada a los datos, modelos y calibraciones elegidos; no garantiza que
el resultado real quede dentro.

El perfil `fast` usa una aproximación variacional y 200 draws. Es útil para
iterar, pero puede producir intervalos demasiado estrechos. El perfil `full`
usa MCMC, 500 draws de predicción y comprobaciones de convergencia más
exigentes.

## Cómo interpretar el resultado

- `votos` es masa de voto esperada, no una predicción del número exacto de
  papeletas.
- `voto_pct` suma uno dentro de cada provincia e incluye `OTBL`.
- `seats` es el resultado determinista de aplicar D'Hondt a cada draw.
- La dispersión entre draws refleja incertidumbre del modelo y, si está
  activada, de la referencia nacional. No simula además un resultado electoral
  persona a persona.
- Una estimación provincial combina información local y nacional; no equivale
  al porcentaje bruto de entrevistas recogidas en esa provincia.

La salida de detalle es `output/sims_est.rds`. Los resúmenes se publican en
`output/estimacion.xlsx` y `output/estimacion.docx`.

## Comprobaciones y límites

El pipeline comprueba datos incompletos, probabilidades inválidas,
candidaturas imposibles, márgenes de calibración, sumas provinciales y el total
de escaños. También dispone de backtests para 2023 y noviembre de 2019, con
comparaciones frente a la encuesta ponderada y un *uniform swing*.

Estas comprobaciones no eliminan varias fuentes de error:

- sesgo de cobertura o respuesta del CIS;
- recodificaciones imperfectas entre fuentes;
- cambios políticos no capturados por las variables;
- participación nacional fijada, no estimada con incertidumbre externa;
- tratamiento de indecisos fuera de producción;
- marco poblacional y resultados anteriores que pueden quedar desactualizados;
- agregación del voto en blanco y otras opciones minoritarias dentro de
  `OTBL`.

Los resultados deben leerse como una estimación dependiente de supuestos, no
como una previsión infalible.

## Configuración activa al redactar esta guía

El comportamiento puede cambiar mediante `config/mrp.yml`. En la versión
revisada para esta documentación, la ejecución productiva usa:

| Decisión | Valor |
|---|---|
| Marco de postestratificación | EPA |
| Estudios CIS recientes | 1 |
| Inferencia | `fast` |
| Participación nacional objetivo | 0,71 |
| Peso del recuerdo oficial | 0,50 |
| Calibración nacional | activada, intensidad 0,50 |

La [referencia de configuración](CONFIGURATION.md) explica cómo modificar
estos valores. Para entender sus consecuencias estadísticas, continúe con la
[guía metodológica técnica](METODOLOGIA-TECNICA.md).
