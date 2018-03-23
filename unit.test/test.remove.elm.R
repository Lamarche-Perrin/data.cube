
dc <- as.data.cube (read.csv ('example.csv', stringsAsFactors=FALSE), dim = list (dim1, dim2, dim3), var = list (obs1))

dc <- remove.elm (dc, dim1, c ('a1', 'a4'))
dc <- remove.elm (dc, dim2, 'b1')

test.data.cube (dc, 'example.remove.elm.csv')
