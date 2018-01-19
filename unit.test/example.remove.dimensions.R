
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), c('obs1','obs2'))

dc <- remove.dimensions (dc, 'dim1','dim3')

test.data.cube (dc, 'example.remove.dimensions.csv')
