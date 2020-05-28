# Programming Assignment 2 - Lexical Scoping
# Assignment: Caching the inverse ofa matrix

## Resolver o inverso de uma matrix, armazenando o resultado em cache
## Cache = Evitar excesso do uso de memoria
## Lexical Scopes = Permitem criar funções dentro de uma função e novos objetos "definidos pelo usuário" (tipos de dados) 
##                  para armazenar dados em vários ambientes


##############################################################################################

## A função "makeCacheMatrix" cria um novo e unico ambiente
## A matriz inversa é armazenada em cache dentro do objeto m, dentro do ambiente principal, unico para cada instancia que a função é chamada
## O output da função é uma lista com 5 elementos, funções criadas aqui: setmatrix, getmatrix, setinverse, getinverse, getenv

##############################################################################################


makeCacheMatrix <- function(x = matrix()) {

# Exemplo Input) 
# Inserir matrix: x <- matrix(nnorm(64), 8,8)

# Checar valores armazenados em cache)
# xMat <- makeCacheMatrix(x)                #Roda a Função 
# parent.env(xMat$getenv())$m               #Verifica o cache
# environment(xMat$getmean)                 #Ambiente "m" criado

m <- NULL                                   #Atribui NULL a uma variavel no ambiente atual
env <-  environment()                       #Salvar ambiente
y <-  NULL
  

setmatrix <- function(y){                   #Definir valor da matriz
  x <<- y                                   #Armazenar em cache a matriz - atribui o valor y do ambiente pai (parent environment)
  m <<- NULL                                #Procura nos ambientes pai (parent environment) uma definição existente da variável e definida como NULL
}  

## Os operadores << - e - >> normalmente são usados apenas em funções e fazem com que uma pesquisa seja feita nos ambientes pai (parent environment)  
## por uma definição existente da variável que está sendo atribuída. Se uma variável desse tipo for encontrada (e sua ligação não estiver bloqueada), 
## seu valor será redefinido; caso contrário, a atribuição ocorrerá no ambiente global. Observe que a semântica deles difere da linguagem S, 
## mas é útil em conjunto com as regras de escopo de R. Consulte o manual "Definição da linguagem R" para obter mais detalhes e exemplos.
## https://stackoverflow.com/questions/13273002/what-does-mean-in-r


getmatrix <- function() x                   #Obter o valor da matriz armazenada em cache com "setmatrix"
setinverse <- function(solve) m <<- solve   #O valor em cache da matriz inversar é salvo em "m"
getinverse <- function() m                  #Obter o valor salvo da matriz inversa "m" que foi salva com "setinverse"
getenv <- function() environment()
  

                                            #Cria uma lista para armazenar as funções  
list (setmatrix=setmatrix, 
      getmatrix=getmatrix,
      setinverse=setinverse,
      getinverse=getinverse,
      getenv=getenv)  

}


##############################################################################################

## A função "cacheSolve" retorna o inverso da matriz criada pela função "makeCacheMatrix"
## Se o inverso já tiver sido calculado (e a matriz não tiver sido alterada), ele deverá recuperar o inverso do cache.

##############################################################################################

cacheSolve <- function(xMat = m(), ...) {

# Retorna uma matrix que é o inverso de "x" 
# Rodar a função compara a matrix com a que havia antes. Exemplo) minv<-cacheSolve(xMat = m)

  m <- xMat$getinverse()                   #"If" Verifica se o inverso ja foi calculado(..): 
    
  if (!is.null(m)) {                       #Verifica se o "cacheSolve" ja foi executado antes
    
    
                                           #verifica se a matriz não mudou e, se não mudou, envia uma mensagem de texto e retorna a matriz em cache
    if(xMat$setmatrix() == xMat$getmatrix()) {
                                           
      message("getting cached data")
      matrix <- xMat$get()
      m <- solve(matrix, ...)
      xMat$setmatrix(m)
      return(m)
    }
  
                                            #(..)Se não foi calculado:
    y <- xMat$getmatrix()                   #Executa a função "getmatrix" para onter o calor da matriz entrada
    xMat$setmatrix(y)                       #Executa a função "setmatrix" da matriz input para armazena-la em cache
    m <-  solve(y,...)                      #Calcula o valor inverso da matriz input
    xMat$setinverse(m)                      #Executa a função "setinverse" para armazernar em cache o inverso
    m                                       #Retorna a matriz inversa
  }
                                            #Fim
}
