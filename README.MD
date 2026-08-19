## What to do before a new release
### Update the version in designated file
In ```project/build.properties``` the ```release_version``` field must be changed to a higher version than the one in the last release on github.

If the version is invalid the release will not be made and the github action for the release will fail.

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