
options(
  browserNLdisabled = FALSE,
  deparse.max.lines = 2,
  help_type = 'html',
  repos = c(
    MSF  = 'https://cran.msf.fr',
    CRAN = 'https://cran.rstudio.com/'),
  stringsAsFactors = FALSE,
  production = FALSE)

require(utils)

version.r <- gsub('.*version ([0-9]*).([0-9]*).*', '\\1.\\2', R.Version()$version.string)

dev.host     <- Sys.getenv('DEV_HOST')
dev.user     <- Sys.getenv('DEV_USER')
dev.password <- Sys.getenv('DEV_PWD')

dir.path        <- Sys.getenv('PATH')
dir.root        <- gsub('\\\\', '/', Sys.getenv('ROOT'))
dir.r           <- sprintf('c:/progra~1/r/r-%s', version.r)
dir.rtool       <- sprintf('c:/progra~1/r/r-%s-tools', version.r)

dir.dev         <- file.path(dir.root     , 'development')
dir.dev.lib     <- file.path(dir.dev      , 'lib')
dir.dev.lib.r   <- file.path(dir.dev.lib  , 'r', version.r)

Sys.setenv( R_LIBS      = dir.dev.lib.r)
Sys.setenv( R_LIBS_SITE = dir.dev.lib.r)
Sys.setenv( R_LIBS_USER = dir.dev.lib.r)
Sys.setenv( RTOOLS      = dir.rtool)

Sys.setenv(    JAVA_HOME  = 'C:/Program Files/java/jdk')
Sys.setenv(RSTUDIO_PANDOC = 'C:/Program Files (x86)/pandoc')

Sys.setenv( PATH   = paste(
  file.path(dir.rtool, 'bin'),
  file.path(dir.rtool, 'mingw_32', 'bin'),
  file.path(dir.rtool, 'mingw_64', 'bin'), Sys.getenv('PATH'), sep = ';'))

assign(".lib.loc", c(dir.dev.lib.r, .Library), envir = environment(.libPaths))

arguments <- commandArgs()
arg.target <- match('--target', arguments)
if(!is.na(arg.target)) {
  arg.target <- arguments[arg.target + 1]
} else {
  arg.target <- NA
}

Sys.setenv( PATH   = paste(
  'C:/Program Files (x86)/git/bin',
  'C:/Program Files (x86)/PuTTY',
  'C:/Program Files/Beyond Compare 4',
  'C:/HashiCorp/Vagrant/bin',
            Sys.getenv('PATH'), sep = ';'))

cat('\n....................................\n')
print(getwd())
print(Sys.getenv('PATH'))
cat('\n....................................\n')
print(.libPaths())
cat('\n....................................\n')
print(sessionInfo())
cat('\n....................................\n')

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
  cat('\nWelcome at', date(), '\n')
  if(file.exists("DESCRIPTION")) {
    x <- read.dcf(file = "DESCRIPTION", fields = c("Package", "Version", "Title"))
    cat(x[1], '\n')
  }
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

clean <- function(){
  unlink('src-i386', T, T)
  unlink('src-x64', T, T)
}

# Alias pour fonction devtools
# ...................................................

alias <- new.env()

assign('cc' , structure('no', class = 'cc'), envir = alias)
assign('dd' , structure('no', class = 'dd'), envir = alias)
assign('ii' , structure('no', class = 'ii'), envir = alias)
assign('id' , structure('no', class = 'id'), envir = alias)
assign('ll' , structure('no', class = 'll'), envir = alias)
assign('lll', structure('no', class = 'lll'), envir = alias)
assign('tt' , structure('no', class = 'tt'), envir = alias)

assign('print.cc', function(x) {
  devtools::check()}   , envir = alias)
  
assign('print.dd', function(x) {
  devtools::document()}, envir = alias)
  
assign('print.ii', function(x) {
  devtools::install()} , envir = alias)
  
assign('print.id', function(x) {
  devtools::install_deps(dependencies = TRUE)}   , envir = alias)

assign('print.ll', function(x) {
  devtools::load_all()}, envir = alias)

assign('print.lll', function(x) {
  devtools::load_all()}, envir = alias)

assign('print.tt', function(x) {
  devtools::test()}    , envir = alias)

attach(alias)

style <- function(strict = FALSE) {
  require(styler)
  style_dir('inst/shiny/default', strict = strict)
  style_dir('r', strict = strict)
  style_dir('tests', strict = strict)
}

sinew <- function() {
  lf <- list.files('R', full.names = TRUE)
  for(f in lf)
    try(makeOxyFile(f, verbose = FALSE, overwrite = TRUE))
}

.pretty <- function() {
  getPackage('sinew')
  lf <- list.files(file.path(getwd(), 'R'), full.names = TRUE)
  for(f in lf)
    try({pretty_namespace(f, overwrite = TRUE, sos=T)})
}

build <- function() {
  clean()

  dir.create(sprintf('dist/%s', version.r), FALSE, TRUE)

  # devtools::update_packages(TRUE)
  # devtools::install_dev_deps()

  devtools::document()
  # devtools::install()

  # devtools::check()
  
  devtools::build(path =         'dist', vignettes = TRUE)
  devtools::build(path = sprintf('dist/%s', version.r), binary = TRUE, vignettes = TRUE)

  system2('pscp' , sprintf('-P 22 -l %s -pw %s -batch -r ./dist/*.gz      %s@%s:/var/r/src/contrib'          , dev.user, dev.password           , dev.user, dev.host))
  system2('pscp' , sprintf('-P 22 -l %s -pw %s -batch -r ./dist/%s/*.zip %s@%s:/var/r/bin/windows/contrib/%s', dev.user, dev.password, version.r, dev.user, dev.host, version.r))

  system2('plink', sprintf('-ssh %s@%s -pw %s -batch Rscript --vanilla -e "tools::write_PACKAGES\\(\\\'/var/r/src/contrib\\\',verbose=T\\)"', dev.user, dev.host, dev.password))
  system2('plink', sprintf('-ssh %s@%s -pw %s -batch Rscript --vanilla -e "tools::write_PACKAGES\\(\\\'/var/r/bin/windows/contrib/%s\\\',verbose=T,type=\\\'win.binary\\\'\\)"', dev.user, dev.host, dev.password, version.r))
}

compare <- function() {
  system2('bcompare', sprintf('. sftp://%s:%s@%s:22//home/s-balandine/%s', dev.user, dev.password, dev.host, pkgname), wait = FALSE)
}

switch(arg.target,
  'build'     = build()
)