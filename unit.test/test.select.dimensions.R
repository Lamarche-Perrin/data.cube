
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), c('obs1','obs2'))

dc <- select.dims (dc, c('dim1','dim3'))

test.data.cube (dc, 'example.select.dimensions.csv')
