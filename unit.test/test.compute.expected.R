
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), c('obs1','obs2'))

dc <- compute.expected (dc, 'obs1', 'exp1', c('dim1','dim3'))
dc <- compute.deviated (dc, 'obs1', 'exp1', 'dev1', 'KLdiv')

dc <- compute.expected (dc, 'obs1', 'exp2')
dc <- compute.deviated (dc, 'obs1', 'exp2', 'dev2', 'poisson')

test.data.cube (dc, 'example.compute.expected.csv')
