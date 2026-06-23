## What to do before a new release
### Update the version in designated file
In ```project/build.properties``` the ```release_version``` field must be changed to a higher version than the one in the last release on github.

If the version is invalid the release will not be made and the github action for the release will fail. 