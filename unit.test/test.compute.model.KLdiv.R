
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), dim = list (dim1, dim2, dim3), var = list (obs1))

dc <- compute.model (dc, dim3, dim1, deviation.type = 'KLdiv')

test.data.cube (dc, 'example.compute.model.KLdiv.csv')
