var Promise = require('bluebird');
var request = require('request');
var process = require('process');
var fs = require('fs');
var _ = require('lodash');
var child_process = require('child_process');

var HOST = 'http://inspector.zudov.me'

var execFileAsync = Promise.promisify(child_process.execFile);

var query = {};

if (process.env.COMPILER) {
  query.compiler = process.env.COMPILER;
}

if (process.env.COMPILER_VERSION) {
  query.compilerVersion = process.env.COMPILER_VERSION;
}

if (process.env.PACKAGE_NAME) {
  query.packageName = process.env.PACKAGE_NAME;
}

if (process.env.PACKAGE_VERSION) {
  query.packageVersion = process.env.PACKAGE_VERSION;
}

request({ uri: HOST + '/tasks'
        , qs: query
        }, (error, response, body) => {
  var tasks = JSON.parse(body);
  //Promise.map(compilers(tasks), getCompiler, { concurrency: 5 })
  //  .then(() =>
      Promise.mapSeries(tasks, performTask);
  //  );
});

var compilers = (tasks) => _(tasks).pluck('buildConfig.compilerRelease').uniq().value()

function getCompiler(compilerTag) {
  function fetchCompilerTar(compilerTag) {
    var releaseUrl = 
      'https://github.com/purescript/purescript/releases/download/' + compilerTag + '/linux64.tar.gz'
    var compilerTar = 'purescript-' + compilerTag + '.tar.gz';
    if (fs.existsSync(compilerTar)) {
      return new Promise((resolve) => resolve(compilerTar));
    } else {
       return execFileAsync('wget', [ '-O', compilerTar, releaseUrl])
                .then(() => compilerTar);
    }
  }

  return fetchCompilerTar(compilerTag)
    .then((compilerTar) => {
      var compilerDir = 'purescript-' + compilerTag;
      if (fs.existsSync(compilerDir)) {
        return './' + compilerDir + '/psc';
      } else {
        return execFileAsync('mkdir', [compilerDir])
          .then(() => {
            child_process.execFileSync('tar', ['-xvf', compilerTar, '-C', compilerDir, '--strip-components=1']);
            return './' + compilerDir + '/psc';
          })
      }
    });
}

function clean() {
  child_process.execFileSync('rm', ['-rf', './bower_components/', './output/']);
}

function performTask(task) {
  console.log(task);
  return new Promise((resolve, reject) => {
    clean();

    getCompiler(task.buildConfig.compilerRelease)
      .then((pscPath) => {
        try {
          child_process.execFileSync('bower', ['install', task.target[0] + '#' + task.target[1]]);

          child_process.execFileSync(pscPath, [ "bower_components/purescript-*/src/**/*.purs"
                                              , "-f", "bower_components/purescript-*/src/**/*.js"
                                              ]);

          console.log("Reporting success");
          request.post(HOST + '/matrix/' + task.target[0] + '/' + task.target[1] + '/'
                      + task.buildConfig.compiler + '/' + task.buildConfig.compilerRelease + '/success'
                      , (error, response, body) => resolve());
        }
        catch (e) {
          console.log("Reporting error");
          console.log(e.stderr.toString());
          request.post(HOST + '/matrix/' + task.target[0] + '/' + task.target[1] + '/'
                       + task.buildConfig.compiler + '/' + task.buildConfig.compilerRelease + '/failure'
                      , (error, response, body) => resolve());
        }
      });
  });
};

