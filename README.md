# Purescript-inspection

purescript-inspection is a service which is supposed to coordinate a continuously
going health-inspection of the library ecosystem, gather the results, present
them in a meaningful way, and use it to produce verified package sets.

An inspected list of packages is configured through `inspection.yaml`.
Server handles the information about whether a given version of given package
builds with given compiler version.

Server doesn't perform the builds, but only provides an API which allows
to query the packages which hasn't yet been built and report whether the result of building it
was successful. Package maintainers have to take care of building and reporting
themselves, but it's easy to automate it using on-release travis builds.

## Examples of the flow

### New version of purescript (compiler) gets released

1. Server gets notified about a new release through PubSubHubbub subscription and
   adds a new compiler column to a build matrix of every package.
2. A travis build starts (it does happen every time a new release is published)
3. `after_success` a travis build is configured to run a `purescript-inspection-worker` script.
   This script would do something like:

   ```shell
   ~ $ # Query the tasks that involve the newly released compiler
   ~ $ http GET "inspection.purescript.org/tasks?compilerVersion=${TRAVIS_TAG}"
   [
     {
       "buildConfig": {
           "compiler": "purescript",
           "compilerRelease": "v0.7.6.1"
       },
       "target": [
           "purescript-validation",
           "v0.2.1"
       ]
     },
     {
       "buildConfig": {
           "compiler": "purescript",
           "compilerRelease": "v0.7.6.1"
       },
       "target": [
           "purescript-validation",
           "v0.2.0"
       ]
     },
     ...
     ...
   ]

   ~ $ # Get the compiler
   ~ $ wget -O purescript.tar.gz https://github.com/purescript/purescript/releases/v0.7.6.1/linux64.tar.gz
   ~ $ tar -xvf purescript.tar.gz

   ~ $ # Get the package and it's dependencies using bower
   ~ $ bower install --save purescript-validation#v0.2.1

   ~ $ # Compile the package
   ~ $ ./purescript/psc 'bower_components/purescript-*/src/**/*.purs' -f 'bower_components/purescript-*/src/**/*.js'
       Compiling Prelude
       Compiling Control.Alt
       Compiling Data.Functor
       Compiling Control.Lazy
       Compiling Control.Apply
       Compiling Control.Monad
       Compiling Control.Bind
       Compiling Control.Extend
       Compiling Data.Validation
       Compiling Control.Plus
       Compiling Control.Comonad
       Compiling Control.Alternative
       Compiling Control.MonadPlus
       Compiling Data.Validation.Semiring

   ~ $ # Report that everything went allright
   ~ $ http POST "inspection.purescript.org/matrix/purescript-validation/v0.2.1/purescript/v0.7.6.1/success"

   ~ $ # Repeat for the rest of the tasks
   ```

### New version of purescript-foobar (package) gets released

1. Server gets notified about a new release through PubSubHubbub subscription and
   adds a new package version row to a build matrix of `purescript-foobar`.
2. A travis build starts (it does happen every time a new release is published)
3. `after_success` a travis build is configured to run a `purescript-inspection-worker` script.
   This script would do something like:

   ```shell
   ~ $ # Query the tasks that involve the newly released purescript-foobar
   ~ $ http GET "inspection.purescript.org/tasks?packageName=purescript-foobar&packageVersion=${TRAVIS_TAG}"
   [
     {
       "buildConfig": {
           "compiler": "purescript",
           "compilerRelease": "v0.7.6.1"
       },
       "target": [
           "purescript-foobar",
           "v9.9.9"
       ]
     },
     {
       "buildConfig": {
           "compiler": "purescript",
           "compilerRelease": "v0.7.6.0"
       },
       "target": [
           "purescript-foobar",
           "v9.9.9"
       ]
     },
     ...
     ...
   ]

   ~ $ # Get the compiler
   ~ $ wget -O purescript.tar.gz https://github.com/purescript/purescript/releases/v0.7.6.1/linux64.tar.gz
   ~ $ tar -xvf purescript.tar.gz

   ~ $ # Get the package and it's dependencies using bower
   ~ $ bower install --save purescript-foobar#v9.9.9

   ~ $ # Compile the package
   ~ $ ./purescript/psc 'bower_components/purescript-*/src/**/*.purs' -f 'bower_components/purescript-*/src/**/*.js'
       Compiling Foo
       Compiling Bar

   ~ $ # Report that everything went allright
   ~ $ http POST "inspection.purescript.org/matrix/purescript-foobar/v9.9.9/purescript/v0.7.6.1/success"

   ~ $ # Repeat for the rest of the tasks
   ```

# Current status

Largely work in progress. A running instance with a crude frontend can (sometimes)
be seen at `http://inspector.zudov.me`.

TODO:

- Use Github's PubSubHubbub
- Authenticate status submissions using Github's tokens.
- Improve UI
- Use gathered information to produce verified package sets
- Improve `purescript-inspection-worker` and provide instructions on how to
  run it from the `travis.yml`
- Keep more info in a `BuildResult` (warnings, logs)
