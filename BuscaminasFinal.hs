--Buscaminas realizado por Antonio Borrego y Elia Fernandez.

module Main (main) where
    
    import Data.Array
    import Data.List
    import Data.Char
    import System.Random
    
    -- Funcion principal. Pide la dificultad, crea un tablero inicial y luego comienza el juego guardando en un fichero externo el historial de la partida
    main:: IO ()
    main = do putStr "Gracias por jugar a nuestro buscaminas. Vas a generar un tablero y a la hora de jugar las dimensiones van desde 0 hasta la dimension elegida menos 1. Mucha suerte.\n\n"
              dif <- dificultad
              tablero <- tableroInicial dif
              putStr "Dame el nombre del fichero.\n"
              nombre <- getLine
              writeFile (nombre ++ ".txt") ("Vas a jugar al buscaminas con dimensiones " ++ (numToString $ fst $ fst dif) ++ "x" ++ (numToString $ snd $ fst dif) ++ " con " ++ (numToString $ snd dif) ++ " minas.\n\n")
              putStr $ mostrar tablero
              final <- juego tablero nombre dif
              appendFile (nombre ++ ".txt") (fst final)
              appendFile (nombre ++ ".txt") (snd final)
              putStr $ fst final
              putStr $ snd final
    
    -- Tipo para la dificultad. La primera tupla es la dimension tablero alto x ancho y el ultimo valor es el numero de minas
    type Dif = ((Int, Int), Int)

    -- Tipo para el tablero. Es una lista de listas de casillas
    type Tablero = [[Casilla]]
    
    -- Tipo para la casilla. Tiene dos valores: 1- un número entre 0 y 9 / 2- Cavada, Bandera o Lisa
    type Casilla = (Int, Estado)
    
    -- Tipo de dato para determinar el estado de la casilla
    data Estado = Cavada | Bandera | Lisa
        deriving (Show, Read, Eq)
    
    -- Quick sort
    qsort :: Ord a => [a] -> [a]
    qsort [] = []
    qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y > x]
    
    --Pasa un numero entero a String
    numToString :: Int -> String
    numToString n
        |div n 10 == 0 = [intToDigit n]
        |otherwise = (numToString $ div n 10) ++ [intToDigit $ mod n 10]
    
    -- Crea un número a partir de un string para crear la semilla del numero aleatorio
    crearSemilla :: String -> Int
    crearSemilla str = (foldr (+) 0 (map ord str))^2    
    
    -- Pregunta si quieres un tablero predeterminado
    dificultad:: IO Dif
    dificultad = do putStr "Quieres un tablero predeterminado? (si/no)\n"
                    pred <- getLine
                    ajustarDif pred
                    
    -- Establece la dificultad, si es predeterminado es un tablero 10x10 con 10 minas
    ajustarDif:: String -> IO Dif
    ajustarDif "no" = preguntarDif
    ajustarDif str = return ((10,10),10)

    -- Dificultad personalizada, pregunta el alto, ancho y numero de minas
    preguntarDif:: IO Dif
    preguntarDif = do putStr "Que dimensiones quieres que tenga el tablero?\n"
                      putStr "Alto: "
                      ancho <- getLine
                      putStr "Ancho: "
                      largo <- getLine
                      let dim1 = read ancho :: Int
                          dim2 = read largo :: Int
                      tnt <- preguntarMinas (dim1,dim2)
                      return ((dim1, dim2), tnt)
                      
    -- Pregunta el numero de minas, y mira que sea menor que el numero de casillas
    preguntarMinas :: (Int,Int) -> IO Int
    preguntarMinas (alt,anc) = do putStr "Cuantas minas quieres que tenga el tablero?\n"
                                  minas <- getLine
                                  let tnt = read minas :: Int
                                  if (tnt >= alt*anc) then preguntarOtrasMinas (alt,anc) else return tnt
    
    -- Te lo pregunta otra vez en caso de que elijas tantas o mas minas como numero de casillas del tablero
    preguntarOtrasMinas :: (Int,Int) -> IO Int
    preguntarOtrasMinas (alt,anc) = do let min = alt*anc
                                       putStr ("Por favor, indica menos de " ++ (numToString min) ++ " minas.\n")
                                       preguntarMinas (alt,anc)
    
    -- Crea el tablero inicial
    tableroInicial:: Dif -> IO Tablero
    tableroInicial dif = do let tablero = [[(0, Lisa) | x <- [1.. snd $ fst dif]] | x <- [1.. fst $ fst dif]]
                            tabMinas <- rellenarMinas tablero dif
                            return $ rellenarNumTab (0,0) dif tabMinas
                            
    -- Rellena el tablero inicial con las minas, generadas las posiciones de estas de manera aleatoria
    rellenarMinas :: Tablero -> Dif -> IO Tablero
    rellenarMinas tab dif = do putStr "Dime algo bonito:\n" -- se usa para dar una semilla al generador de numeros aleatorios
                               semilla <- getLine
                               let sem = (crearSemilla semilla)^(crearSemilla semilla)
                                   dim = (fst $ fst dif) * (snd $ fst dif)
                                   numMinas = qsort $ take (snd dif) . nub $ (randomRs (1, dim) (mkStdGen sem) :: [Int])
                                   posMinas = [((div (x - 1) (fst $ fst dif)), (mod (x - 1) (snd $ fst dif))) | x <- numMinas]
                               return $ colocarMinas 0 0 tab posMinas
    
    -- Coloca las minas dentro del tablero
    colocarMinas :: Int -> Int -> Tablero -> [(Int, Int)] -> Tablero
    colocarMinas _ _ tab [] = tab
    colocarMinas fil col (ts:tab) (x:xs)
        |fil /= (fst x) = ts : (colocarMinas (fil + 1) 0 tab (x:xs))
        |otherwise = colocarMinas fil (col + 1) ((reemplazarCasilla (snd x) (9, Lisa) ts):tab) xs
    
    -- Reemplaza una casilla por otra dada en la posicion indicada dentro de una fila dada
    reemplazarCasilla :: Int -> Casilla -> [Casilla] -> [Casilla]
    reemplazarCasilla _ _ [] = []
    reemplazarCasilla n cas (x:xs)
        | n == 0 = cas:xs
        | otherwise = x:reemplazarCasilla (n-1) cas xs
    
     --Cambia una fila dada de un tablero
    reemplazarFila :: Int -> [Casilla] -> Tablero -> Tablero
    reemplazarFila _ _ [] = []
    reemplazarFila n fil (t:ts)
        |n == 0 = fil:ts
        |otherwise = t:reemplazarFila (n-1) fil ts
        
    -- Rellena el tablero con los numeros mirando las minas de alrededor
    rellenarNumTab :: (Int,Int) -> Dif -> Tablero -> Tablero
    rellenarNumTab (fil,col) dif tab
        |fil == (fst $ fst dif) = []
        |otherwise = (rellenarNumFila (fil,col) dif tab (tab!!fil)):rellenarNumTab (fil+1,0) dif tab
        
    rellenarNumFila :: (Int,Int) -> Dif -> Tablero -> [Casilla] -> [Casilla]
    rellenarNumFila _ _ _ [] = []
    rellenarNumFila (fil,col) dif tab (t:ts) = (numerar (fil,col) dif tab t):rellenarNumFila (fil,col+1) dif tab ts
    
    -- Sustituye una casilla en blanco por una con el número de minas que tiene a su alrededor
    numerar :: (Int, Int) -> Dif -> Tablero -> Casilla -> Casilla
    numerar _ _ _ (9, Lisa) = (9, Lisa)
    numerar pos dif tab _ = (contador, Lisa)
        where contador = contMinas pos tab dif
            
    -- Cuenta las minas al rededor de una casilla
    contMinas :: (Int,Int) -> Tablero -> Dif -> Int
    contMinas = posicionesLado cuenta
    
    cuenta :: [(Int,Int)] -> Tablero -> Dif -> Int
    cuenta [] _ _ = 0
    cuenta (x:xs) tab dif
        |(tab!!fil!!col) == (9, Lisa) = 1 + cuenta xs tab dif
        |otherwise = cuenta xs tab dif
            where fil = fst x
                  col = snd x
        
    -- Convierte el tablero de juego en un String para que el jugador pueda verlo
    mostrar :: Tablero -> String
    mostrar tab = concat $ map mostrarFila tab

    mostrarFila :: [Casilla] -> String
    mostrarFila ts = (concat $ map mostrarEstado ts) ++ "|" ++ "\n"
    
    -- Son las opciones que se pueden ver en el tablero que se presenta al jugador
    mostrarEstado :: Casilla -> String
    mostrarEstado (n,est)
        |est == Lisa = "| "
        |est == Cavada = "|" ++ [num]
        |otherwise = "|P"
            where num = camNum n
            
    camNum :: Int -> Char
    camNum 9 = 'X'
    camNum 0 = '-'
    camNum n = intToDigit n
    
    -- Dado una casilla y una acción realiza una jugada, muestra el tablero con la accion ejecutada y devuelve dicho tablero
    jugadaIOTab :: (Int,Int) -> String -> Tablero -> Dif -> IO Tablero
    jugadaIOTab (fila,columna) acc tab dif = do let cas = tab!!fila!!columna
                                                    newtab = jugadaTab (fila,columna) acc tab dif
                                                putStr $ mostrar newtab
                                                return newtab
    
    -- Ejecuta la accion en el tablero
    jugadaTab :: (Int,Int) -> String -> Tablero -> Dif -> Tablero
    jugadaTab (fila,columna) acc tab dif
        |snd cas == Cavada = tab
        |acc == "Abanderar" = cambiarBandera (fila,columna) tab
        |otherwise = cavarTab (fila,columna) tab dif
            where cas = tab!!fila!!columna

    -- Cambia una casilla lisa a bandera y viceversa cuando la accion es Abanderar
    cambiarBandera :: (Int,Int) -> Tablero -> Tablero
    cambiarBandera _ [] = []
    cambiarBandera (fila,columna) tab
        |snd cas == Lisa = reemplazarFila fila fBand tab
        |snd cas == Bandera = reemplazarFila fila fLisa tab
        |otherwise = tab
            where cas = tab!!fila!!columna
                  fBand = reemplazarCasilla columna (fst cas,Bandera) (tab!!fila)
                  fLisa = reemplazarCasilla columna (fst cas,Lisa) (tab!!fila)

    -- Cava la posición dada en el tablero cuando la accion es Cavar
    cavarTab :: (Int,Int) -> Tablero -> Dif -> Tablero
    cavarTab (fila,columna) tab dif
        |fst cas /= 0 = tCav
        |otherwise = posicionesLado cavarCero (fila,columna) tCav dif
            where cas = tab!!fila!!columna
                  fCav = reemplazarCasilla columna (fst cas,Cavada) (tab!!fila)
                  tCav = reemplazarFila fila fCav tab

    -- Cava todas las posiciones de alrededor de un cero cavado
    cavarCero :: [(Int,Int)] -> Tablero -> Dif -> Tablero
    cavarCero [] tab _ = tab
    cavarCero (x:xs) tab dif = jugadaTab x "Cavar" tCav dif
        where tCav = cavarCero xs tab dif

    -- Realiza juego de manera iterativa hasta que se acabe
    juego :: Tablero -> String -> Dif -> IO (String, String)
    juego tab nom dif = do acc <- pedirAccion
                           cas <- pedirCasilla tab
                           appendFile (nom ++ ".txt") ("Vas a " ++ acc ++ " en la casilla " ++ (numToString $ fst cas) ++ "x" ++ (numToString $ snd cas) ++ ".\n")
                           newtab <- jugadaIOTab cas acc tab dif
                           appendFile (nom ++ ".txt") (mostrar newtab ++ "\n")
                           let finPerder = finalPerder newtab
                               finGanar = finalGanar newtab
                           if finPerder then return (autoPerder newtab) else (if finGanar then return (autoGanar newtab) else juego newtab nom dif)
    
    -- Pide una accion al jugador               
    pedirAccion :: IO String
    pedirAccion = do putStr "Que quieres hacer? (Cavar/Abanderar)\n" 
                     acc <- getLine
                     if (acc /= "Cavar" && acc /= "Abanderar") then pedirAccion else return acc
    
    -- Pide una casilla al jugador
    pedirCasilla :: Tablero -> IO (Int,Int)
    pedirCasilla tab = do putStr "Que casilla quieres? \n"
                          putStr "Fila: "
                          fil <- getLine
                          putStr "Columna: "
                          col <- getLine
                          let fila = read fil :: Int
                              columna = read col :: Int
                          if (fila >= length(tab) || columna >= length(tab!!0)) then pedirOtraCas tab else return (fila, columna)
    
    -- Pide otra casilla al jugador en el caso de que este indique alguna posicion fuera del tablero
    pedirOtraCas :: Tablero -> IO (Int,Int)
    pedirOtraCas tab = do putStr "Por favor indica una casilla dentro de las dimensiones del tablero.\n"
                          pedirCasilla tab
                     
    -- Condicion de acabar el juego porque se pica una mina
    finalPerder :: Tablero -> Bool
    finalPerder tab = or $ map finalFilaPerder tab
    
    finalFilaPerder :: [Casilla] -> Bool
    finalFilaPerder xs = or $ map (==(9,Cavada)) xs
    
    -- Muestra todas las minas del tablero cuando se pierde
    autoPerder :: Tablero -> (String,String)
    autoPerder tab = ("HAS PERDIDO.\n", mostrar $ map autoPerderFila tab)
    
    autoPerderFila :: [Casilla] -> [Casilla]
    autoPerderFila xs = map autoPerderCasilla xs
    
    autoPerderCasilla :: Casilla -> Casilla
    autoPerderCasilla (9,Lisa) = (9,Cavada)
    autoPerderCasilla cas = cas
    
    -- Condición de acabar el juego porque se ha desbloqueado todo menos las minas
    finalGanar :: Tablero -> Bool
    finalGanar tab = and $ map finalFilaGanar tab
    
    finalFilaGanar :: [Casilla] -> Bool
    finalFilaGanar xs = and $ map comprobar xs
    
    -- Comprueba que las casillas sin mina estan cavadas
    comprobar :: Casilla -> Bool
    comprobar (n,est)
        |n==(9) && est==Lisa = True
        |n==(9) && est==Bandera = True
        |n/=(9) && est==Cavada = True
        |otherwise = False
       
    -- Muestra todas las minas del tablero abanderadas cuando se gana
    autoGanar :: Tablero -> (String,String)
    autoGanar tab = ("HAS GANADO.\n", mostrar $ map autoGanarFila tab)
    
    autoGanarFila :: [Casilla] -> [Casilla]
    autoGanarFila xs = map autoGanarCasilla xs
    
    autoGanarCasilla :: Casilla -> Casilla
    autoGanarCasilla (9,Lisa) = (9,Bandera)
    autoGanarCasilla cas = cas
    
    -- Cambia el estado de una casilla
    jugarCasilla :: String -> Casilla -> Casilla
    jugarCasilla _ (n,Cavada) = (n,Cavada)
    jugarCasilla "Cavar" (n,_) = (n,Cavada)
    jugarCasilla "Abanderar" (n,Lisa) = (n,Bandera)
    jugarCasilla "Abanderar" (n,Bandera) = (n,Lisa)
    jugarCasilla _ cas = cas
     
    -- Recorre las posiciones que rodean a una casilla dada y realiza la operación indicada con ellas (version 2 reducida)
    posicionesLado :: ([(Int,Int)] -> Tablero -> Dif -> a) -> (Int,Int) -> Tablero -> Dif -> a
    posicionesLado func (fil, col) tab dif
        |(fil == 0) && (col == 0) = func [(fil,col+1),(fil+1,col),(fil+1,col+1)] tab dif
        |(fil == 0) && (col == ((snd $ fst dif) - 1)) = func [(fil,col-1),(fil+1,col-1),(fil+1,col)] tab dif
        |(fil == 0) = func [(fil,col-1),(fil,col+1),(fil+1,col-1),(fil+1,col),(fil+1,col+1)] tab dif
        |(fil == ((fst $ fst dif) - 1)) && (col == 0) = func [(fil-1,col),(fil-1,col+1),(fil,col+1)] tab dif
        |(fil == ((fst $ fst dif) - 1)) && (col == ((snd $ fst dif) - 1)) = func [(fil-1,col-1),(fil-1,col),(fil,col-1)] tab dif
        |(fil == ((fst $ fst dif) - 1)) = func [(fil-1,col-1),(fil-1,col),(fil-1,col+1),(fil,col-1),(fil,col+1)] tab dif
        |(col == 0) = func [(fil-1,col),(fil-1,col+1),(fil,col+1),(fil+1,col),(fil+1,col+1)] tab dif
        |(col == ((snd $ fst dif) - 1)) = func [(fil-1,col-1),(fil-1,col),(fil,col-1),(fil+1,col-1),(fil+1,col)] tab dif
        |otherwise = func [(fil-1,col-1),(fil-1,col),(fil-1,col+1),(fil,col-1),(fil,col+1),(fil+1,col-1),(fil+1,col),(fil+1,col+1)] tab dif