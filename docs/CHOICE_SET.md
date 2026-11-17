# Disponibilidad provincial

`data-raw/candidaturas.csv` es la fuente de verdad de disponibilidad electoral.

También delimita el soporte de los términos geográficos por partido. Este
soporte puede excluir una CCAA sólo para modelado mediante
`domain.vote_geography.support_exclusions`; esa excepción no cambia la
candidatura, el offset ni la disponibilidad usada en predicción.
Debe contener exactamente una fila por combinación de las 52 provincias y
cada partido modelado, con:

- `provincia`: código INE de dos dígitos.
- `partido`: etiqueta canónica del modelo.
- `se_presenta`: `TRUE` o `FALSE`, sin ausencias.

`PP` es la categoría de referencia y debe estar disponible en las 52
provincias. Cada partido restante debe presentarse al menos en una.

## Uso en el modelo

Antes del ajuste se eliminan respuestas CIS que asignan voto a una candidatura
no disponible en su provincia. El modelo categórico recibe un offset por
partido:

- `0` si la candidatura está disponible.
- `-1e6` si no está disponible.

Los mismos offsets se usan al predecir el marco postestratificado. No se
aplica una máscara posterior ni se redistribuyen votos.

## Guardrail

Tras `posterior_epred()`, el pipeline suma la probabilidad asignada a
candidaturas no disponibles para cada draw y celda. La ejecución falla si el
máximo supera `1e-10`.

El informe `output/informe_candidaturas.md` registra:

- Cobertura de la tabla de candidaturas.
- Observaciones CIS descartadas.
- Pares no disponibles.
- Máxima masa prohibida y tolerancia.
- Resultado `PASS`.

Los fingerprints, manifests, overrides productivos y el procedimiento de
migración `choice-set-v1` fueron retirados. La caché general del pipeline se
invalida mediante firmas sencillas de datos, configuración y código.
