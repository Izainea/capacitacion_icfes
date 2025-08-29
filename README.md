# Repositorio Base para Aplicaciones R/Shiny

Este repositorio contiene una estructura estandarizada y un punto de partida para desarrollar aplicaciones de datos robustas y desplegables con R y Shiny.

---

## 🚀 ¿Cómo Lanzar la Aplicación?

Para ejecutar esta aplicación en su entorno local, siga los siguientes pasos.

### Prerrequisitos

Asegúrese de tener instalado lo siguiente:
1.  **R** (versión 4.2 o superior)
2.  **RStudio Desktop**
3.  La librería `shiny` en R. Si no la tiene, instálela ejecutando este comando en la consola de R:
    ```r
    install.packages("shiny")
    ```

### Pasos para la Ejecución

1.  **Clonar el Repositorio (si no lo ha hecho):**
    ```bash
    git clone <URL_DEL_REPOSITORIO>
    ```

2.  **Abrir el Proyecto:**
    Navegue a la carpeta del proyecto y abra el archivo `.Rproj` con RStudio (o simplemente abra la carpeta con VSCode).

3.  **Ejecutar la Aplicación:**
    -   En RStudio, abra el archivo `app.R` y haga clic en el botón **"Run App"**.
    -   Desde una terminal, puede ejecutar: `R -e "shiny::runApp('app.R')"`

¡La aplicación Shiny debería lanzarse en una nueva ventana!

---

## 🏛️ Estructura del Proyecto

Este proyecto sigue una arquitectura estándar para facilitar la mantenibilidad y el despliegue:

-   `/data`: Contiene los datos crudos y procesados.
-   `/R`: Aloja scripts modulares para lógica de negocio (procesamiento, modelado).
-   `/models`: Almacena los artefactos de modelos entrenados (ej. archivos `.rds`).
-   `/www`: Guarda recursos estáticos para la UI (CSS, imágenes, JS).
-   `app.R`: El archivo principal que contiene la lógica de la UI y el Servidor de la aplicación Shiny.
-   `.gitignore`: Define qué archivos no deben ser rastreados por Git.
-   `README.md`: Documentación principal del proyecto (este archivo).
