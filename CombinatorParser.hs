-- Bernardo Pinto de Alkmim
-- 1712658
-- Exercícios em Haskell para INF2811 - Programação Funcional
-- Professor Roberto Ierusalimschy
-- Parser de λ-calculus para Combinadores.
module CombinatorParser (
        CAST(S, K, I, B, C, (:$:), CVar, CAbs, CApplist, Varlist, Root, Error),
        runParser,
        translate
) where

import Parser

data CAST = S
        | K
        | I
        | B
        | C
        | CAST :$: CAST
        | CVar String
        | CAbs CAST CAST
        | CApplist [CAST]
        | Varlist [CAST]
        | Root CAST
        | Error String -- Para mensagens de erro
        deriving Eq

instance Show CAST where
        show (CVar x) = x
        show (e :$: e'@(_ :$: _)) = show e ++ " (" ++ show e' ++ ")"
        show (e :$: e') = show e ++ " " ++ show e'
        show S = "S"
        show K = "K"
        show I = "I"
        show B = "B"
        show C = "C"
        show (Root x) = show x
        show (CAbs v e) = "(\\" ++ show v ++ "-> " ++ show e ++ ")"
        show (Varlist (v:[])) = show v
        show (Varlist (v:vs)) = show v ++ " " ++ show (Varlist vs)
        show (CApplist (e:[])) = show e
        show (CApplist (e:es)) = show e ++ " " ++ show (CApplist es)
        show (Error s) = s

kwS :: Parser CAST
kwS = do
        symbol "S"
        return S

kwB :: Parser CAST
kwB = do
        symbol "B"
        return B

kwC :: Parser CAST
kwC = do
        symbol "C"
        return C

kwK :: Parser CAST
kwK = do
        symbol "K"
        return K

kwI :: Parser CAST
kwI = do
        symbol "I"
        return I

keywords :: Parser CAST
keywords = do
        w <-    kwS
                `por1` kwB
                `por1` kwC
                `por1` kwK
                `por1` kwI
        return w

identifier :: Parser CAST
identifier = do
        c <- alpha
        cs <- many alphanum
        spaces
        return (CVar (c:cs))

-- Símbolos não terminais mais gerais da gramática.
parenthesized :: Parser CAST
parenthesized = do
        symbol "("
        e <- expression
        symbol ")"
        return e

varlist :: Parser CAST
varlist = do
        vars <- many2 identifier
        return (Varlist vars)

abstraction :: Parser CAST
abstraction = do
        symbol "\\"
        var <- varlist `por1` identifier
        symbol "->"
        e <- expression
        return (CAbs var e)

application :: Parser CAST
application = do
        aps <- many2 simple
        return (CApplist aps)

simple :: Parser CAST
simple = do
        e <-    parenthesized
                `por1` keywords
                `por1` identifier
        return e

expression :: Parser CAST
expression = do
        e <-    abstraction
                `por1` application
                `por1` simple
        spaces
        return e

root :: Parser CAST
root = do
        spaces
        p <- expression
        return (Root p)

-- Função que roda o parser.
runParser :: String -> CAST
runParser s =
        case s of
                []      -> Error "Entrada vazia."
                _       -> case parse root s of
                        [(x, [])]       -> x
                        [(_, (_:_))]    -> Error "Nao aceitou a entrada."
                        _               -> Error "Erro no parser."

-- Evita repetições em uma lista
noRepeat :: (Eq a) => [a] -> [a]
noRepeat [] = []
noRepeat (x:xs) = x:(filter (/= x) (noRepeat xs))

-- Mostra as variáveis livres de uma λ-expressão.
freeVar :: CAST -> [String]
freeVar (CVar x) = [x]
freeVar (CAbs (CVar x) e) = filter (/= x) (freeVar e)
freeVar (CAbs (Varlist (v:[])) e) = freeVar (CAbs v e)
freeVar (CAbs (Varlist (v:vs)) e) = freeVar (CAbs v (CAbs (Varlist vs) e))
freeVar (e1 :$: e2) = noRepeat ((freeVar e1) ++ (freeVar e2))
freeVar (CApplist (e:[])) = freeVar e
freeVar (CApplist (e:(e':es))) = freeVar (CApplist ((e :$: e'):es))
-- Demais combinadores
freeVar _ = []

-- Gera sempre que possível uma variável da forma "x" concatenado com n "'".
-- n, no caso, é o menor número de "'" necessário para que a variável nova seja livre
-- na nova λ-expressão gerada.
-- Pior caso: complexidade O(n^2) de tempo, O(n) espaço.
newVar :: [String] -> String
newVar [] = "x"
newVar (y:ys) = case filter (== y++"'") ys of
                        [] -> y++"'"
                        _ -> newVar ((y++"'"):ys)

-- Função que substitui uma variável por outra em uma λ-expressão.
subst :: CAST -> String -> CAST -> CAST
subst (Root e') x e = Root (subst e' x e)
subst (CVar y) x e = case x of
                        _       | x == y -> e
                                | otherwise -> CVar y
subst (CAbs (CVar y) e') x e = case x of
                        _       | x == y -> CAbs (CVar x) e'
                                | otherwise -> case (filter (== y) (freeVar e)) of
                                        [] -> CAbs (CVar y) (subst e' x e)
                                        _ -> CAbs (CVar z) (subst (subst e' y (CVar z)) x e)
                                                where z = newVar (x:((freeVar e) ++ (freeVar e')))
subst (CAbs (Varlist (v:[])) e') x e = subst (CAbs v e') x e
subst (CAbs (Varlist (v:vs)) e') x e = subst (CAbs v (CAbs (Varlist vs) e')) x e
subst (e' :$: e'') x e = (subst e' x e) :$: (subst e'' x e)
subst (CApplist (e':[])) x e = subst e' x e
subst (CApplist (e':(e'':es))) x e = subst (CApplist ((e' :$: e''):es)) x e
-- Demais combinadores
subst comb _ _ = comb

-- Função de tradução, que traduz sempre rendo uma variável como referência.
-- Só é utilizada dentro de λ-abstrações.
translate' :: String -> CAST -> CAST
translate' x (CVar y) = case x of
        _       | x == y -> I
                | otherwise -> K :$: (CVar y)
translate' x (CAbs v e) = case v of
        CVar y -> case x of
                _       | x == y -> translate' x (translate' (newVar (freeVar e)) (subst e x (CVar (newVar (freeVar e)))))
                        | otherwise -> translate' x (translate' y e)
        Varlist ((CVar y):[]) -> translate' x (CAbs (CVar y) e)
        Varlist ((CVar y):vs) -> translate' x (CAbs (CVar y) (CAbs (Varlist vs) e))
-- Múltiplas aplicações seguidas
translate' x (CApplist (e:[])) = translate' x e
translate' x (CApplist (e:(e':es))) = translate' x (CApplist ((e :$: e'):es))
-- Aplicação geral
translate' x (e :$: e') = case e' of
        CVar y -> case x of
                _       | x == y -> e -- lei de abstração
                        | otherwise -> S :$: translate' x e :$: translate' x e'
        _ -> S :$: translate' x e :$: translate' x e'
translate' x (Root e) = Root (translate' x e)
translate' _ e@(Error _) = e
-- Combinadores S K B C I
translate' _ comb = K :$: comb

-- Função de tradução quando não estamos dentro de uma λ-abstração.
translate :: CAST -> CAST
translate (e :$: e') = translate e :$: translate e'
translate (CApplist (e:[])) = translate e
translate (CApplist (e:(e':es))) = translate (CApplist ((e :$: e'):es))
translate (CAbs v (expr :$: expr')) = s (tv expr) (tv expr') where
        tv e = translate (CAbs v e)
        -- Otimizações
        s (K :$: e) (K :$: e') = K :$: (e :$: e')
        s (K :$: e) I = e
        s (K :$: e) e' = B :$: e :$: e'
        s e (K :$: e') = C :$: e :$: e'
translate (CAbs v e) = case v of
        CVar x -> translate' x e
        Varlist ((CVar x):[]) -> translate (CAbs (CVar x) e)
        Varlist ((CVar x):vs) -> translate (CAbs (CVar x) (CAbs (Varlist vs) e))
translate (Root e) = Root (translate e)
-- Error, Combinadores e Variáveis
translate e = e

main :: IO ()
main = do
        putStrLn "\nTestes do Parser de Expressoes CAST para Combinadores\n"
        -- x
        print (CVar "x")
        print (translate (runParser "x"))
        putStrLn ""
        -- K
        print (K)
        print (translate (runParser "K"))
        putStrLn ""
        -- K I
        print (K :$: I)
        print (translate (runParser "K I"))
        putStrLn ""
        -- K x
        print (K :$: (CVar "x"))
        print (translate (runParser "K x"))
        putStrLn ""
        -- I x
        print (I :$: (CVar "x"))
        print (translate (runParser "I x"))
        putStrLn ""
        -- λx. x
        print (CAbs (CVar "x") (CVar "x"))
        print (translate (runParser "\\x-> x"))
        putStrLn ""
        -- λx. y
        print (CAbs (CVar "x") (CVar "y"))
        print (translate (runParser "\\x-> y"))
        putStrLn ""
        -- λx. (λy. x y)
        print (CAbs (CVar "x") (CAbs (CVar "y") ((CVar "x") :$: (CVar "y"))))
        print (translate (runParser "\\x-> (\\y-> x y)"))
        putStrLn ""
        -- λx. (λy. x)
        print (CAbs (CVar "x") (CAbs (CVar "y") (CVar "x")))
        print (translate (runParser "\\x-> (\\y-> x)"))
        putStrLn ""
        -- λx. (λy. y)
        print (CAbs (CVar "x") (CAbs (CVar "y") (CVar "y")))
        print (translate (runParser "\\x-> (\\y-> y)"))
        putStrLn ""
        -- λx. (λx. x)
        print (CAbs (CVar "x") (CAbs (CVar "x") (CVar "x")))
        print (translate (runParser "\\x-> (\\x-> x)"))
        putStrLn ""
        -- λx. (λy. z)
        print (CAbs (CVar "x") (CAbs (CVar "y") (CVar "z")))
        print (translate (runParser "\\x-> (\\y-> z)"))
        putStrLn ""
        -- λx. (λy. (λz. (x z (y z))))
        print (CAbs (CVar "x") (CAbs (CVar "y") (CAbs (CVar "z") (((CVar "x") :$: (CVar "z")) :$: ((CVar "y") :$: (CVar "z"))))))
        print (translate (runParser "\\x-> (\\y-> (\\z-> (x z (y z))))"))
        putStrLn ""
        -- λx. (λy. (λz. (x (y z))))
        print (CAbs (CVar "x") (CAbs (CVar "y") (CAbs (CVar "z") ((CVar "x") :$: ((CVar "y") :$: (CVar "z"))))))
        print (translate (runParser "\\x-> (\\y-> (\\z-> (x (y z))))"))
        putStrLn ""
        -- λx. (λy. (λz. (x z y))
        print (CAbs (CVar "x") (CAbs (CVar "y") (CAbs (CVar "z") ((CVar "x") :$: (CVar "z") :$: (CVar "y")))))
        print (translate (runParser "\\x-> (\\y-> (\\z-> (x z y)))"))
        putStrLn ""
