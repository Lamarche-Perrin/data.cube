
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), c('obs1','obs2'))

dc <- aggregate.elems (dc, list(dim1=list(a13=c('a1','a3'), a24=c('a2','a4')), dim2=list(b12=c('b1','b2'))))

test.data.cube (dc, 'example.aggregate.elements.csv')
