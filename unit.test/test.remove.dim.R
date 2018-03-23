
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), dim = list (dim1, dim2, dim3), var = list (obs1))

dc <- select (dc, dim2)

test.data.cube (dc, 'example.remove.dim.csv')
