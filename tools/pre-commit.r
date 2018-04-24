if(file.exists('build.properties')) {
  inifile <- file('build.properties') 
  inifilelines  <- readLines(inifile) 
  close(inifile) 

  version.major.ind <- which(grepl('version.major=', inifilelines))
  version.major     <- gsub('.*=', '', inifilelines[version.major.ind])

  version.minor.ind <- which(grepl('version.minor=', inifilelines))
  version.minor     <- gsub('.*=', '', inifilelines[version.minor.ind])

  version.release.ind <- which(grepl('version.release=', inifilelines))
  version.release     <- gsub('.*=', '', inifilelines[version.release.ind])

  version.build.ind <- which(grepl('version.build=', inifilelines))
  version.build     <- gsub('.*=', '', inifilelines[version.build.ind])
  version.build     <- as.integer(version.build) + 1

  version.build.date.ind  <- which(grepl('version.build.date=', inifilelines))
  version.build.date      <- Sys.time()

  inifilelines[version.build.ind]      <- sprintf('version.build=%s'                  , version.build)
  inifilelines[version.build.date.ind] <- sprintf('version.build.date=%s', as.character(version.build.date))

  cat(inifilelines, sep='\n', file='build.properties')
}

dcffile <- file('DESCRIPTION') 
dcffilelines  <- readLines(dcffile) 
close(dcffile) 

dcf <- read.dcf('DESCRIPTION')

version           <- gsub('(.*)(-|\\.)+([0-9]*)$' , '\\1', dcf[1, 'Version'])
version.build     <- gsub('(.*)(-|\\.)+([0-9]*)$' , '\\3', dcf[1, 'Version'])
version.build     <- as.integer(version.build) + 1

dcffilelines[which(grepl('Version: ', dcffilelines))] <- sprintf('Version: %s-%s', version, version.build)
dcffilelines[which(grepl('Date: '   , dcffilelines))] <- sprintf('Date: %s', as.character(Sys.time()))

cat(dcffilelines, sep='\n', file='DESCRIPTION')
