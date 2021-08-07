# Contribuyendo a Pyre

Queremos hacer que la contribución a este proyecto sea tan fácil y transparente como
posible.

*Lea esto en otros idiomas: [Inglés](CONTRIBUTING.md)*

## Pull Requests

Aceptamos activamente sus solicitudes de extracción.

1. Fork el repositorio y crear su rama de `main`.
2. Si ha agregado un código que debe probarse, agregue pruebas.
3. Si ha cambiado las APIs, actualice la documentación.
4. Asegúrese de que el conjunto de pruebas pase (consulte [la siguiente sección](#ejecución-de-pruebas)).
5. Asegúrese de que su código se filtre (consulte [la siguiente sección](#estilo-de-codificación)).
6. Si aún no lo ha hecho, complete el Acuerdo de Licencia del Colaborador ("CLA").

## Ejecución de pruebas

Pyre ejecuta dos suites de prueba diferentes:

* las pruebas unitarias en OCaml cubren el binario principal (`pyre.bin`); estas pruebas
  se ejecutan a través de `make test` después de que las fuentes hayan sido
  configurado. Consulte [Cómo obtener
  Comenzó] (https://pyre-check.org/docs/installation.html) página para
  más información sobre cómo arrancar el proceso.

* Las pruebas de Python para los wrappers se ejecutan a través de `make python_tests`.

## Acuerdo de licencia del colaborador ("CLA")

Para aceptar su solicitud de extracción, necesitamos que envíe un CLA. Solo necesitas
hacer esto una vez para trabajar en cualquiera de los proyectos de código abierto de Facebook.

Completa su CLA aquí: <https://code.facebook.com/cla>. Si tienes alguna pregunta,
por favor escríbanos a cla@fb.com.

También se espera que siga el [Código de conducta](CODE_OF_CONDUCT.md), así que lea eso si es un colaborador nuevo.

## Cuestiones

Usamos problemas de GitHub para rastrear errores públicos. Por favor, asegúrese de que su descripción sea
claro y tiene instrucciones suficientes para poder reproducir el problema.

## Estilo de codificación

Valoramos el código consistente. Por favor, sigue el estilo del código que lo rodea. Reglas útiles para todos los idiomas son
* evitar abreviaturas.
* evitar indentaciones variables (por ejemplo alinear parámetros con el paréntesis de apertura de una función). Tiende a echar a perder la culpa de git y resulta rápidamente con conflictos con el límite de longitud de línea.
* agregue un punto de partida / punto y coma para listas y registros de múltiples líneas, y tenga el corchete de cierre en la línea después del punto y coma.
* prefiera snake_case sobre camelCase para variables y nombres de funciones, y prefiera CamelCase sobre Snake_case para módulos y clases.

### Python
<p>
  <a href="https://github.com/ambv/black"><img alt="Code style: black" src="https://img.shields.io/badge/code%20style-black-000000.svg"></a>
</p>

Usamos el formateador de código `Black` para todos los archivos de Python.
Puede instalar la última versión a través de `pip install black` y ejecutarlo sobre los archivos del cliente como` black pyre-check/client`.
Más información disponible en: https://github.com/ambv/black

### OCaml

- no utilizamos anotaciones de tipo fuera de los archivos de interfaz a menos que sea necesario
- usa ocp-indent para formatear

En un nivel alto, Pyre realiza los siguientes pasos para cuando se llama "pira" desde la línea de comando:

1. Lea una .pyre_configuration para determinar qué raíces de origen analizar, así como también para qué paquetes de python analizar anotaciones y qué comando de pyre ejecutar. Esta información se usa para determinar qué banderas pasan a `pyre.bin`, y envían al binario de OCaml. La implementación de este paso se puede encontrar en `client/`.

2. Determine qué comando de piratería ejecutar. Los comandos incluyen algunos que manejan el tiempo de vida y el estado de un servidor de piro persistente para un proyecto, así como un comando de ejecución independiente denominado `pyre check`. La implementación de estos comandos se encuentra en `commands/`. `main.ml` agrega estos comandos y maneja el análisis de los argumentos de la línea de comandos. La mayoría de los pasos eventualmente llamarán a TypeCheckService.

3. El módulo TypeCheckService llamará a ParseService, que localizará todas las fuentes en la raíz de origen determinada y todas las dependencias, y analizará, procesará previamente y agregará las fuentes a la memoria compartida. ParseService y TypeCheckService viven bajo `service/`, mientras que el analizador se encuentra en `parser/`, y el preprocesador está bajo `analysis/analysisPreprocessing.ml`. El AST (árbol de sintaxis abstracta) en el que se analiza el código fuente se especifica en los archivos del directorio `ast/`.

4. A continuación, pyre completará el entorno de tipo global con las fuentes del proyecto y las dependencias. Tenga en cuenta que no analizamos recursivamente las importaciones de los archivos del proyecto, sino que agregamos todas las fuentes al entorno a la vez. Esta elección facilita la paralelización de la construcción del entorno de tipo, pero tiene el costo de no poder depender de las dependencias de un archivo que se analizan al agregarlo al entorno de tipo. El entorno de tipos se puede considerar como una colección de tablas hash, nombres de funciones de asignación, clases, globales, etc. en sus implementaciones a las que se puede acceder desde la memoria compartida. El orden de tipo, que se explica con más detalle en una sección a continuación, también se construye aquí.

Los módulos que hacen el trabajo pesado aquí se pueden encontrar en `analysis/analysisEnvironment.ml` y` analysis/analysisTypeOrder.ml`.

5. Una vez que se construye el entorno, Pyre comenzará a escribir y comprobará todos los archivos bajo la raíz de origen en paralelo. La propiedad clave aquí es que construir el entorno en el paso anterior permite que Pyre pueda verificar cada función en paralelo. El comprobador de tipos no irá más allá de los límites de llamada de función, lo cual es posible porque habremos acumulado el parámetro y devuelto las anotaciones de todas las funciones antes de este paso.

Durante el análisis, cada función se procesará en un diagrama de flujo de control para representar el flujo de información de escritura (https://en.wikipedia.org/wiki/Control_flow_graph es una buena introducción a CFG). La vista de pájaro del algoritmo es que inicializamos el análisis con la información de tipo del parámetro de la función, y seguimos el flujo de control de la función para anotar las variables locales que se encuentran. Cuando se encuentra con un acceso de atributo, llamada, etc., la información de tipo propagado se compara con la firma ya presente, y se genera un error si los dos no son compatibles. La sección `Abstract Interpretation` proporciona una base teórica para el análisis.

6. TypeCheckService recogerá todos los errores y los devolverá a la persona que llama. En el caso de `pyre check`, todos los errores serán reportados a stdout.

## Licencia
Al contribuir con Pyre, usted acepta que sus contribuciones serán licenciadas
bajo el archivo LICENCIA en el directorio raíz de este árbol fuente.
