# Monad Core 2D engine

## Configurazione di Ollama

L'applicazione usa Ollama per eseguire un LLM in locale. Prima di avviarla,
installare Ollama e scaricare il modello predefinito:

```bash
ollama pull gemma4:e4b
```
Ollama deve essere in ascolto su `http://localhost:11434`.

### Configurazione predefinita e personalizzata

Per impostazione predefinita, Monad Core usa l'endpoint `http://localhost:11434` e il modello `gemma4:e4b`. È possibile sostituire entrambi tramite variabili d'ambiente:

- `MONAD_CORE_OLLAMA_URL`: URL del server Ollama.
- `MONAD_CORE_MODEL_NAME`: nome di un modello già scaricato dal server configurato.

Ad esempio, per avviare l'applicazione con un server o un modello diversi:

```bash
MONAD_CORE_OLLAMA_URL=http://localhost:11434 \
MONAD_CORE_MODEL_NAME=<nome-modello> \
sbt run
```

## What to do before a new release
### Update the version in designated file
In ```project/build.properties``` the ```release_version``` field must be changed to a higher version than the one in the last release on github.

If the version is invalid the release will not be made and the GitHub action for the release will fail.

## Project Formatter Settings

### IntelliJ 

Go to the IDE settings and type ```formatter``` in the searchbar, then select ```Editor > Scala```, you should see something like the provided image: 

![img.png](readmeImgs/img.png)

In Formatter select Scalafmt and you are all set!

### Any other IDE or Terminal
You can run in any terminal the following command to format all the project files:
```bash
sbt scalafmtAll
```

There are also the following counterparts:

```bash
sbt scalafmt              # formats only the files in the main package
sbt test:scalafmt         # formats only the files in test package
sbt scalafmtSbt           # formats only the build files like build.sbt or project/*.scala
```
