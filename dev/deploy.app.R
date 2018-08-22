library ('rsconnect')

rsconnect::deployApp (
               appDir='./outlier-explorer.app/',
               appFiles=c(
                   'server.R',
                   'ui.R',
                   'data.cube.R',
                   'data/guardian.small.csv'
               ),
               appName='outlier-explorer',
               appTitle='Outlier Explorer',
               launch.browser=FALSE
           )
