data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node _ list) = 1 + longitud list

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void _ = False
estaContenido (Node x rest) elem = 
    if elem == x
    then True
    else estaContenido rest elem
    
convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void  = []
convertirALista (Node x rest ) = x : (convertirALista rest )  

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x rest) = 
    if estaContenido rest x
    then conjunto rest 
    else Node x (conjunto rest)
    
eliminarIndice :: List a -> Int -> List a
eliminarIndice Void _ = error "Índice fuera del rango permitido."
eliminarIndice (Node _ rest) 0 = rest  -- Caso cuando índice es 0
eliminarIndice (Node x rest) n = 
    if n < 0 || n >= longitud (Node x rest)
    then error "Índice fuera del rango permitido."
    else Node x (eliminarIndice rest (n-1))

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void 0 nuevoElem = Node nuevoElem Void
insertarIndice Void _ _ = error "Índice fuera del rango permitido."
insertarIndice (Node x rest) 0 nuevoElem = Node nuevoElem (Node x rest)
insertarIndice (Node x rest) n = 
    if n < 0 || n > longitud (Node x rest)
    then error "Índice fuera del rango permitido."
    else Node x (insertarIndice rest (n-1) nuevoElem)
    
recorrerLista :: List a -> Int -> List a
recorrerLista Void _ = Void 
recorrerLista lista 0 = lista 
recorrerLista (Node x rest) n = recorrerLista (insertarIndice rest (longitud rest) x) (n-1)



