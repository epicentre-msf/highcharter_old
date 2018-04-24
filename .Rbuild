
options(
  browserNLdisabled = FALSE,
  deparse.max.lines = 2,
  help_type = 'html',
  repos = c(
    CRAN = 'https://cran.rstudio.com/',
    MSF  = 'https://cran.msf.fr'),
  stringsAsFactors = FALSE)

require(utils)

systemFC <- function(...) {
  system2(...)
  flush.console()
}

getPackage <- function(pkg, pkg.github = NULL, load = TRUE, silent = FALSE, force = FALSE) {
  if(force | !suppressMessages(suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE)))) {
    if(is.null(pkg.github)) {
      try(          install.packages(pkg)       , silent = TRUE)
    } else {
      try(devtools::install_github(  pkg.github), silent = TRUE)
    }
  }
  if(load) suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
  if(load & !silent) message("Loaded ", pkg)
  flush.console()
}

getPackages <- function() {
  pkgs <- gsub('\\(.*| ', '', unlist(strsplit(
    paste0(gsub('\n', '', as.package('.')$imports), ',',
           gsub('\n', '', as.package('.')$suggest)), ',')))

  for(pkg in pkgs)
    getPackage(pkg)
}

version.r <- gsub('.*version ([0-9]*).([0-9]*).*', '\\1.\\2', R.Version()$version.string)

dev.host     <- Sys.getenv('DEV_HOST')
dev.user     <- Sys.getenv('DEV_USER')
dev.password <- Sys.getenv('DEV_PWD')

dir.path        <- Sys.getenv('PATH')
dir.root        <- gsub('\\\\', '/', Sys.getenv('ROOT'))
dir.r           <- sprintf('c:/progra~1/r/r-%s', version.r)

dir.dev         <- file.path(dir.root     , 'development')
dir.dev.lib     <- file.path(dir.dev      , 'lib')
dir.dev.lib.r   <- file.path(dir.dev.lib  , 'r', version.r)

Sys.setenv( R_LIBS      = dir.dev.lib.r)
Sys.setenv( R_LIBS_SITE = dir.dev.lib.r)
Sys.setenv( R_LIBS_USER = dir.dev.lib.r)
Sys.setenv( RTOOLS = paste0(dir.r, '-tools'))

Sys.setenv(    JAVA_HOME  = 'C:/Program Files/java/jdk')
Sys.setenv(RSTUDIO_PANDOC = 'C:/Program Files (x86)/pandoc')

Sys.setenv( PATH   = paste(
  file.path(Sys.getenv('RTOOLS'), 'bin'),
  file.path(Sys.getenv('RTOOLS'), 'mingw_32', 'bin'),
  file.path(Sys.getenv('RTOOLS'), 'mingw_64', 'bin'), Sys.getenv('PATH'), sep = ';'))

assign(".lib.loc", c(dir.dev.lib.r, .Library), envir = environment(.libPaths))

arguments <- commandArgs()
arg.target <- match('--target', arguments)
if(!is.na(arg.target)) {
  arg.target <- arguments[arg.target + 1]
} else {
  arg.target <- NA
}

cat('\n....................................\n')
print(.libPaths())
cat('\n....................................\n')
print(sessionInfo())
cat('\n....................................\n')

Sys.setenv( PATH   = paste(
  'C:/Program Files (x86)/git/bin',
  'C:/Program Files (x86)/PuTTY',
  'C:/Program Files/Beyond Compare 4',
  'C:/HashiCorp/Vagrant/bin',
            Sys.getenv('PATH'), sep = ';'))

getPackage('devtools')
getPackage('roxygen2')
getPackage('testthat')

if(!file.exists('.git/hooks/pre-commit.r')) {
  file.copy('tools/pre-commit'  , '.git/hooks/pre-commit', overwrite = T)
  file.copy('tools/pre-commit.r', '.git/hooks/pre-commit.r', overwrite = T)
}

usethis::use_build_ignore('.gitignore' )
usethis::use_build_ignore('.Rbuild' )
usethis::use_build_ignore('.Rprofile' )
usethis::use_build_ignore('build.properties' )
usethis::use_build_ignore('dist' )
usethis::use_build_ignore('Jenkinsfile' )
usethis::use_build_ignore('revdep') 
usethis::use_build_ignore('tools')

.First <- function(){
  x <- read.dcf(file = "DESCRIPTION", fields = c("Package", "Version", "Title"))
  cat('\nWelcome at', date(), '\n')
  cat(x[1], '\n')
}

.Last <- function(){
  cat('\nGoodbye at ', date(), '\n')
}

command <- function(){
  cat('\nGoodbye at ', date(), '\n')
}

cmd <- function()  {
  system('cmd')
}

installPackages <- function() {
  getPackages()
}

.s <- function() {
  devtools::install_dev_deps()
}

clean <- function(){
  unlink('src-i386', T, T)
  unlink('src-x64', T, T)
}

.d <- function() {
  devtools::document()
}

.l <- function() {
  devtools::load_all()
}

.la <- function() {

  devtools::load_all()
}

.i <- function() {
  devtools::install()
}

check <- function() {
  clean()
  devtools::check()
}

.t <- function() {
  devtools::test()
}

.style <- function(strict = FALSE) {
  require(styler)
  style_dir('inst/shiny/default', strict = strict)
  style_dir('r', strict = strict)
  style_dir('tests', strict = strict)
}

.sinew <- function() {
  lf <- list.files('R', full.names = TRUE)
  for(f in lf)
    try(makeOxyFile(f, verbose = FALSE, overwrite = TRUE))
}

build <- function(binary = FALSE) {
  clean()
  cat('\n....................................\n')
  if(binary)
    dir.dist <- sprintf('dist/%s', version.r) else
    dir.dist <- 'dist'
  dir.create(dir.dist, F, T)
  devtools::build(path = dir.dist, binary = binary)
  flush.console()
}

release <- function() {
  clean()

  build()
  build(binary = TRUE)

  systemFC('pscp' , sprintf('-P 22 -l %s -pw %s -r ./dist/*.gz      %s@%s:/var/r/src/contrib'           , dev.user, dev.password           , dev.user, dev.host))
  systemFC('pscp' , sprintf('-P 22 -l %s -pw %s -r ./dist/%s/*.zip %s@%s:/var/r/bin/windows/contrib/%s', dev.user, dev.password, version.r, dev.user, dev.host, version.r))

  systemFC('plink', sprintf('-ssh %s@%s -pw %s Rscript --vanilla -e "tools::write_PACKAGES\\(\\\'/var/r/src/contrib\\\',verbose=T\\)"', dev.user, dev.host, dev.password))
  systemFC('plink', sprintf('-ssh %s@%s -pw %s Rscript --vanilla -e "tools::write_PACKAGES\\(\\\'/var/r/bin/windows/contrib/%s\\\',verbose=T,type=\\\'win.binary\\\'\\)"', dev.user, dev.host, dev.password, version.r))
}

compare <- function() {
  systemFC('bcompare', sprintf('. sftp://%s:%s@%s:22//home/s-balandine/%s', dev.user, dev.password, dev.host, pkgname), wait = FALSE)
}

switchR <- function(version = NULL) {
  if(version == '3.2')
    compiler <- 'i386' else
    compiler <- 'x64'
  if(!is.null(version))
    systemFC(command = sprintf('c:/program files/r/r-%s/bin/%s/rgui.exe', version, compiler), args = '--args --file .Rbuild', wait = FALSE) else
    systemFC(command = sprintf('c:/program files/r/r/bin/%s/rgui.exe'            , compiler), args = '--args --file .Rbuild', wait = FALSE) 
}

switch(arg.target,
  'build'     = build(),
  'build-bin' = build(binary = TRUE),
)