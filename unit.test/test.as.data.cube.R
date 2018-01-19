
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), c('obs1','obs2'))

test.data.cube (dc, 'example.as.data.cube.csv')
