
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), dim = list (dim1, dim2, dim3), var = list (obs1))

dc <- compute.model (dc, dim1, dim3, deviation.type = 'ratio')

dc <- compute.model (dc, dim1, dim3, deviation.type = 'ratio')
print (dc)

test.data.cube (dc, 'example.compute.model.ratio.csv')
