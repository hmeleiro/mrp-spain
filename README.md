# MRP Spain

Estimación electoral para elecciones generales españolas mediante regresión multinivel y postestratificación (MRP). El proyecto combina encuestas CIS, Censo o EPA, resultados anteriores y candidaturas provinciales para producir distribuciones posteriores de voto y escaños.

## Ejecución

La configuración ordinaria vive exclusivamente en `config/mrp.yml`.

``` powershell
# Reutiliza modelos y marco si sus firmas siguen vigentes
Rscript R/run.R

# Reconstruye marco y los tres modelos
Rscript R/run.R --rebuild
```

Desde una sesión R:

``` r
source("R/run.R")
result <- run_mrp()
```

El pipeline reconstruye automáticamente una caché cuando cambia alguno de los datos, esquemas, controles o contratos de versión relevantes para esa etapa. Las cachés `fast` y `full` se almacenan por separado.

Cada muestreo terminado se guarda atómicamente en `models/checkpoints/<perfil>/` antes de ejecutar diagnósticos, comprobaciones predictivas o informes. Una ejecución ordinaria reutiliza esos ajustes cuando coinciden los datos, la fórmula, los priors y los parámetros de muestreo. `--rebuild` es la única opción que fuerza su repetición. Los checkpoints antiguos se migran cuando el contenido Stan y los parámetros efectivos pueden validarse contra el contrato actual.

## Configuración pública

Las decisiones ordinarias están en `run`:

``` yaml
run:
  poststrat_source: censo  # censo | epa
  recent_studies: 2
  inference: fast          # fast | full
  temporal:
    unit: month            # month | day
    structure: none        # none | linear_varying
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
```

- `fast`: inferencia variacional `meanfield`, semilla 1234 y 200 draws.
- `full`: MCMC con gates obligatorios de Rhat, ESS, divergencias y treedepth. Los controles de voto se configuran en `run.vote_sampling`; recuerdo y participación conservan cuatro cadenas, `adapt_delta = 0.8` y `max_treedepth = 10`.

El resto del YAML contiene contratos de datos, recodificaciones y las tres variantes fijas de backtest. Las claves públicas desconocidas producen un error.

La referencia completa de cada clave, su efecto sobre fórmulas, datos, calibración y cachés está en [`docs/CONFIGURATION.md`](docs/CONFIGURATION.md).

La metodología se documenta a dos niveles:

- [introducción sin conocimientos estadísticos previos](docs/METODOLOGIA-INTRODUCCION.md);
- [descripción estadística y de datos para lectores familiarizados con MRP](docs/METODOLOGIA-TECNICA.md).

Los inputs y outputs siguen convenciones fijas:

- Inputs en `data-raw/`.
- Intermedios en `data-processed/<perfil>/`.
- Modelos en `models/<perfil>/`.
- Checkpoints en `models/checkpoints/<perfil>/`.
- Resultados en `output/`.

## Arquitectura

El código soportado se concentra en diez módulos:

1.  `config.R`: configuración, perfiles, rutas, firmas y caché.
2.  `data.R`: adaptadores Censo/EPA/CIS y recodificaciones.
3.  `availability.R`: candidaturas, offsets y guardrail.
4.  `calibration.R`: calibración nacional preservando márgenes provinciales.
5.  `models.R`: fórmulas, pesos, ajustes y diagnósticos.
6.  `pipeline.R`: marco postestratificado, imputaciones, predicción y escaños.
7.  `reporting.R`: RDS, Word, Excel e informes de diagnóstico.
8.  `participation.R`: agregados, gráficos e informe de participación.
9.  `backtest.R`: validación histórica fija.
10. `undecided.R`: evaluación aislada de la imputación de indecisos.

El flujo productivo es:

``` text
inputs
  -> marco Censo/EPA + datos provinciales
  -> escenarios de recuerdo modelado y oficial
  -> participación ordinal/binaria calibrada en escala logit
  -> encuesta CIS + modelo de voto
  -> mezcla de recuerdo + calibración nacional opcional
  -> D'Hondt
  -> informes
```

La disponibilidad provincial forma parte del likelihood categórico mediante offsets. No existe una segunda máscara que corrija resultados: el guardrail falla si la probabilidad asignada a candidaturas no disponibles supera `1e-10`. Los efectos CCAA se construyen además por partido: sólo contienen territorios donde compite y se sustituyen por contrastes fijos u omiten cuando hay menos de cinco CCAA disponibles.

## Salidas

| Ruta | Contenido |
|----|----|
| `output/sims_est.rds` | `draw`, `provincia`, `partido`, `votos`, `voto_pct`, `seats` |
| `output/estimacion.xlsx` | Resumen nacional, provincial y candidaturas |
| `output/estimacion.docx` | Tablas nacional y provincial |
| `output/informe_candidaturas.md` | Filtro CIS y guardrail de disponibilidad |
| `output/informe_recuerdo.md` | Escenarios de recuerdo, flujos y auditoría contra resultados oficiales |
| `output/diagnostics/voto/informe_estimacion.md` | Voto y escaños con intervalos posteriores |
| `output/diagnostics/voto/informe_calibracion_nacional.md` | Auditoría del objetivo externo y sus efectos |
| `output/diagnostics/voto/informe_temporal.md` | Índice temporal estandarizado por periodo y partido |
| `output/diagnostics/voto/informe_grupos_sociodemograficos.md` | Fortaleza, cambio y evolución del voto por grupo |
| `output/diagnostics/voto/informe_ultimo_escano.md` | Defensores, aspirantes y batalla marginal D'Hondt |
| `output/diagnostics/voto/informe_bloques.md` | Sumas aritméticas configuradas, draw a draw |
| `output/diagnostics/voto/informe_geografia_voto.md` | Soportes y efectos CCAA específicos por partido |
| `output/diagnostics/voto/informe_ajuste_voto.md` | Diagnósticos MCMC y residuos agregados |
| `output/diagnostics/voto/informe_ejecucion.md` | Configuración, inputs, firmas e invariantes |
| `output/diagnostics/participacion/informe_participacion.md` | Participación, calibración, sensibilidad, demografía y geografía |
| `output/diagnostics/participacion/participacion_resumen.csv` | Resumen P20--P80 por variable, escenario y etapa |
| `output/diagnostics/participacion/participacion_correlaciones.csv` | Correlaciones descriptivas de predictores provinciales |
| `output/diagnostics/participacion/participacion_draws.rds` | Agregados compactos por draw y metadatos |
| `output/run_metadata.rds` | Firmas, perfil, configuración, diagnósticos y sesión |

Cada draw debe sumar 350 escaños y cada distribución provincial de voto debe sumar uno.

El diagnóstico de participación se regenera en cada ejecución con informes, aunque se reutilice el marco postestratificado. También puede ejecutarse sin ajustar modelos ni producir la estimación de voto:

``` powershell
Rscript analysis/analisis-participacion.R
Rscript analysis/analisis-participacion.R --config=config/mrp.yml
```

El comando independiente exige que `data-processed/<perfil>/poststrat_frame.rds` exista y que su firma coincida con los datos y contratos actuales. Si no coincide, indica cómo reconstruirlo y termina sin modificar modelos.

## Backtests

Se soportan las elecciones de 2023 y 2019-10N:

``` powershell
Rscript R/backtest.R 2023
Rscript R/backtest.R 2019-10N
Rscript R/backtest.R all
Rscript R/backtest.R 2023 --variant=production
Rscript R/backtest.R 2023 --variant=production,geography
```

Sin `--variant`, cada elección ejecuta las tres variantes:

1.  Producción: pesos muestrales y efecto CCAA.
2.  Pesos muestrales con recencia (`lambda = 0.35`) y efecto CCAA.
3.  Pesos muestrales y efectos CCAA + provincia.

Se comparan con encuesta ponderada y uniform swing. Las salidas incluyen `backtest_metrics.rds`, `backtest_summary.csv` y `backtest_report.md`. La variante de producción genera además `ablation_metrics.rds`, `ablation_summary.csv` y `ablation_report.md`, con las etapas `actual`, `+recuerdo_robusto`, `+participacion` y, cuando esté habilitada, `+calibracion_nacional`. La etapa de indecisos no se incluye porque el experimento actual no supera su puerta de entrada. No hay rejillas de hiperparámetros ni selección automática.

La imputación de indecisos no forma parte de producción. Se evalúa mediante:

``` powershell
Rscript analysis/evaluate-undecided.R
```

El informe y la puerta de entrada se guardan en `output/validation/undecided/`.

## Dependencias

Paquetes principales:

``` r
install.packages(c(
  "arrow", "brms", "digest", "dplyr", "flextable", "flexlsx", "ggplot2", "haven",
  "janitor", "officer", "openxlsx2", "posterior", "purrr", "readr",
  "readxl", "stringr", "tidyr", "yaml"
))
```

También se requieren `cmdstanr`, CmdStan, `vota` e `infoelectoral`.

## Comprobaciones

``` powershell
Rscript validate_syntax.R
Rscript tests/run-tests.R
```

Los scripts de `analysis/` son exploratorios salvo `analysis/analisis-participacion.R`, que reutiliza el generador soportado por el pipeline. `tools/download-electoral-results.R` actualiza los resultados electorales.
