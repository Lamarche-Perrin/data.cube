
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), dim = list (dim1, dim2, dim3), var = list (obs1))

dc <- aggregate.elm (dc, list(dim1=list(a13=c('a1','a3'), a24=c('a2','a4')), dim2=list(b12=c('b1','b2'))))

test.data.cube (dc, 'example.aggregate.elements.csv')
