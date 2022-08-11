-- PEDRO GONZAGA L. COUTINHO - 2022000557
-- ATIVIDADE AVALIATIVA 2

type TITULO = String
type AUTOR = String
type NOTA = Float
type ANO = Int

type Livro = (TITULO, AUTOR, NOTA, ANO)
type Database = [Livro]

insere :: Database -> Livro -> Database
insere listagem book = book : listagem

removeTitulo :: Database -> String -> Database
removeTitulo [] title = []
removeTitulo ((titulo, autor, nota, ano):t) title = if title == titulo
  then removeTitulo t title
  else (titulo, autor, nota, ano) : removeTitulo t title

removeAutor :: Database -> String -> Database
removeAutor [] author = []
removeAutor ((titulo, autor, nota, ano):t) author = if author == autor
  then removeAutor t author
  else (titulo, autor, nota, ano) : removeAutor t author

removeAno :: Database -> Int -> Database
removeAno [] year = []
removeAno ((titulo, autor, nota, ano):t) year = if year == ano
  then removeAno t year
  else (titulo, autor, nota, ano) : removeAno t year

buscaTitulo :: Database -> String -> IO()
buscaTitulo [] title = do putStrLn ""
buscaTitulo ((titulo, autor, nota, ano):t) title = if title == titulo
  then do
    putStrLn (titulo ++ "(" ++ show(ano) ++ ")")
    putStrLn (autor)
    putStrLn ("NOTA: " ++ show(nota))
  else buscaTitulo t title

buscaAutor :: Database -> String -> IO()
buscaAutor [] author = do putStrLn ""
buscaAutor ((titulo, autor, nota, ano):t) author = if author == autor
  then do
    putStrLn (titulo ++ "(" ++ show(ano) ++ ")")
    putStrLn (autor)
    putStrLn ("NOTA: " ++ show(nota))
    putStrLn ""
    buscaAutor t author
  else buscaAutor t author

buscaAno :: Database -> Int -> IO()
buscaAno [] year = do putStrLn ""
buscaAno ((titulo, autor, nota, ano):t) year = if year == ano
  then do
    putStrLn (titulo ++ "(" ++ show(ano) ++ ")")
    putStrLn (autor)
    putStrLn ("NOTA: " ++ show(nota))
    putStrLn ""
    buscaAno t year
  else buscaAno t year

imprimir :: Database -> IO ()
imprimir [] = do putStrLn ""
imprimir ((titulo, autor, nota, ano):t) = do
  putStrLn (titulo ++ "(" ++ show(ano) ++ ")")
  putStrLn (autor)
  putStrLn ("NOTA: " ++ show(nota))
  putStrLn ""
  imprimir t

main = do
  let db = []
  menu db

menu :: Database -> IO ()
menu dtb = do
  putStrLn "1 - Inserir Novo Livro"
  putStrLn "2 - Remover Livro por Titulo"
  putStrLn "3 - Remover Livros por Autor"
  putStrLn "4 - Buscar Livro por Titulo"
  putStrLn "5 - Buscar Livro por Autor"
  putStrLn "6 - Buscar Livro por Ano"
  putStrLn "7 - Exibir Todos os Livros"
  putStrLn "8 - Sair do Programa"
  putStrLn "Digite a opcao"
  opt <- readLn
  dataatual <- carryout dtb opt
  if opt == 8 then return()
    else menu dataatual

perguntainsere :: IO Livro
perguntainsere = do
  putStrLn "titulo do Livro?"
  titulo <- getLine
  putStrLn "Autor do Livro?"
  autor <- getLine
  putStrLn "Nota do Livro?"
  nota <- readLn
  putStrLn "Ano de lancamento?"
  ano <- readLn
  return(titulo, autor, nota, ano)

perguntaString :: IO TITULO
perguntaString = do
  titulo <- getLine
  return (titulo)

perguntaAno :: IO ANO
perguntaAno = do
  putStrLn "Qual o Ano de lancamento?"
  ano <- readLn
  return(ano)

carryout :: Database -> Int -> IO Database
carryout db opcao = if opcao == 1
  then
    do
      putStrLn "Inserindo Livro!"
      bookinsere <- perguntainsere
      let dataatual = insere db bookinsere
      return dataatual
    else if opcao == 2
      then
        do
          putStrLn "Qual o titulo do Livro?"
          bookremove <- perguntaString
          let dataatual = removeTitulo db bookremove
          putStrLn "Removendo Livro!"
          return dataatual
        else if opcao == 3
          then do
            putStrLn "Qual nome do autor?"
            bookremove2 <- perguntaString
            let dataatual = removeAutor db bookremove2
            putStrLn "Removendo Livro(s)!"
            return dataatual
          else if opcao == 4
            then do
              putStrLn "Qual nome do Titulo?"
              bookbusca <- perguntaString
              buscaTitulo db bookbusca
              return db
            else if opcao == 5
              then do
                putStrLn "Qual nome do Autor?"
                bookbusca2 <- perguntaString
                buscaAutor db bookbusca2
                return db
              else if opcao == 6
                then do
                  bookbusca3 <- perguntaAno
                  buscaAno db bookbusca3
                  return db
                else if opcao == 7
                  then do
                    putStrLn "Lista de Livro:"
                    imprimir db
                    return db
                  else do
                    putStrLn "Terminando o Programa..."
                    return db
