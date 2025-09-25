# ==============================================================================
# DOCKERFILE PARA LA APLICACIÓN SHINY ICFES
# Objetivo: Crear un entorno reproducible y aislado para ejecutar la aplicación.
# Cambio clave: Se añade 'texlive-latex-recommended' para soportar tablas
#               complejas en los reportes PDF generados con R Markdown.
# ==============================================================================

# Usamos una imagen base oficial de R mantenida por el proyecto Rocker.
# Se especifica una versión concreta (4.4.1) para asegurar la reproducibilidad.
FROM rocker/r-ver:4.4.1

# 1. Instalar dependencias del sistema operativo (APT)
#    - pandoc: Necesario para R Markdown.
#    - texlive-*: Dependencias de LaTeX para generar PDFs.
#      'texlive-latex-recommended' es la adición clave que incluye el paquete
#      'booktabs' necesario para las tablas con kable().
#    - lib*: Librerías de desarrollo para compilar paquetes de R.
RUN apt-get update && apt-get install -y \
    pandoc \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libjpeg-dev \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-extra \
    && rm -rf /var/lib/apt/lists/*

# 2. Establecer un directorio de trabajo limpio dentro del contenedor.
#    Todos los comandos siguientes se ejecutarán desde /app.
WORKDIR /app

# 3. Copiar TODO el proyecto al directorio de trabajo del contenedor.
#    Esto incluye app.R, reporte.Rmd, renv.lock, .Rprofile y la carpeta de datos.
COPY . .

# 4. Instalar 'renv' y restaurar las dependencias del proyecto.
#    'renv::restore()' leerá el archivo 'renv.lock' y instalará las versiones
#    exactas de los paquetes de R, garantizando la reproducibilidad.
RUN R -e "install.packages('renv')"
RUN R -e "renv::restore()"

# 5. Exponer el puerto que Shiny usará.
#    Esto informa a Docker que el contenedor escuchará en el puerto 3838,
#    pero no lo publica automáticamente. Se debe usar 'docker run -p'.
EXPOSE 3838

# 6. Comando para ejecutar la aplicación al iniciar el contenedor.
#    - host = '0.0.0.0' permite que la app sea accesible desde fuera del contenedor.
#    - port = 3838 coincide con el puerto expuesto.
CMD ["R", "-e", "shiny::runApp('app_reportes.R', host = '0.0.0.0', port = 3838)"]
