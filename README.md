# triangdist

Paquete de R para trabajar con la **Distribución Triangular**, desarrollado como parte de la asignatura de Probabilidad.

## Funciones incluidas
El paquete implementa las cuatro funciones estándar de una distribución en R:
* `dtriang()`: Densidad de probabilidad.
* `ptriang()`: Función de distribución acumulada (CDF).
* `qtriang()`: Función de cuantiles.
* `rtriang()`: Generación de números aleatorios (vía método de inversión).

## Instalación
Puedes instalarlo directamente desde GitHub usando:
```r
install.packages("remotes")
remotes::install_github("caralmel/triangdist")
