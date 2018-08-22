
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), dim = list (dim1, dim2, dim3), var = list (obs1))

dc <- select (dc, dim1, dim3)

test.data.cube (dc, 'example.select.dim.csv')
