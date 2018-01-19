
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), c('obs1','obs2'))

dc <- remove.elems (dc, list(dim1=c('a1','a4'), dim2=c('b1')))

test.data.cube (dc, 'example.remove.elements.csv')
