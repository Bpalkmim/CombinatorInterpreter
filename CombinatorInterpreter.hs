-- Bernardo Pinto de Alkmim
-- 1712658
-- Exercícios em Haskell para INF2811 - Programação Funcional
-- Professor Roberto Ierusalimschy
-- Interpretador de Combinadores.
import CombinatorParser

-- Monta uma lista de aplicações de CASTs.
spine :: CAST -> [CAST]
spine (e :$: e') = spine e ++ [e']
spine (Root e) = spine e
spine e = [e]

-- Interpreta uma lista de aplicações de CASTs.
evaluate :: [CAST] -> [CAST]
evaluate (S:(e:(e':(e'':es)))) = evaluate (spine e ++ (e'':((e' :$: e''):es)))
evaluate (B:(e:(e':(e'':es)))) = evaluate (spine e ++ ((e' :$: e''):es))
evaluate (C:(e:(e':(e'':es)))) = evaluate (spine e ++ (e'':(e':es)))
evaluate (K:(e:(_:es))) = evaluate (spine e ++ es)
evaluate (I:(e:es)) = evaluate (spine e ++ es)
-- Caso em que a espinha principal já não pode ser aplicada, entramos nas
-- "subespinhas".
evaluate [] = []
evaluate (e:[]) = case e of
        (_ :$: _) -> evaluate (spine e)
        _ -> [e]
evaluate (e:es) = [e] ++ evaluate es

-- Recebe uma lista de aplicações de CASTs e remonta uma aplicação só.
rebuild :: [CAST] -> CAST
rebuild (e:[]) = Root e
rebuild (e:(e':es)) = rebuild ((e :$: e'):es)

main :: IO ()
main = do
        putStrLn "\nTestes do Interpretador de Combinadores\n"
        -- x
        print (CVar "x")
        print (rebuild (evaluate (spine (translate (runParser "x")))))
        print (rebuild (evaluate (spine (CVar "x"))))
        putStrLn ""
        -- K
        print (K)
        print (rebuild (evaluate (spine (translate (runParser "K")))))
        print (rebuild (evaluate (spine (K))))
        putStrLn ""
        -- K I
        print (K :$: I)
        print (rebuild (evaluate (spine (translate (runParser "K I")))))
        print (rebuild (evaluate (spine (K :$: I))))
        putStrLn ""
        -- K x
        print (K :$: (CVar "x"))
        print (rebuild (evaluate (spine (translate (runParser "K x")))))
        print (rebuild (evaluate (spine (K :$: (CVar "x")))))
        putStrLn ""
        -- K x y
        print (K :$: (CVar "x") :$: (CVar "y"))
        print (rebuild (evaluate (spine (translate (runParser "K x y")))))
        print (rebuild (evaluate (spine (K :$: (CVar "x") :$: (CVar "y")))))
        putStrLn ""
        -- K I y
        print (K :$: I :$: (CVar "y"))
        print (rebuild (evaluate (spine (translate (runParser "K I y")))))
        print (rebuild (evaluate (spine (K :$: I :$: (CVar "y")))))
        putStrLn ""
        -- I x
        print (I :$: (CVar "x"))
        print (rebuild (evaluate (spine (translate (runParser "I x")))))
        print (rebuild (evaluate (spine (I :$: (CVar "x")))))
        putStrLn ""
        -- I (K x) y
        print (I :$: (K :$: (CVar "x")) :$: (CVar "y"))
        print (rebuild (evaluate (spine (translate (runParser "I (K x) y")))))
        print (rebuild (evaluate (spine (I :$: (K :$: (CVar "x")) :$: (CVar "y")))))
        putStrLn ""
        -- S a b c
        print (S :$: (CVar "a") :$: (CVar "b") :$: (CVar "c"))
        print (rebuild (evaluate (spine (translate (runParser "S a b c")))))
        print (rebuild (evaluate (spine (S :$: (CVar "a") :$: (CVar "b") :$: (CVar "c")))))
        putStrLn ""
        -- B a b c
        print (B :$: (CVar "a") :$: (CVar "b") :$: (CVar "c"))
        print (rebuild (evaluate (spine (translate (runParser "B a b c")))))
        print (rebuild (evaluate (spine (B :$: (CVar "a") :$: (CVar "b") :$: (CVar "c")))))
        putStrLn ""
        -- (λx. (λy. (λz. x (y z))) a b c
        -- Equivalente a "B a b c"
        print ((CAbs (CVar "x") (CAbs (CVar "y") (CAbs (CVar "z") ((CVar "x") :$: ((CVar "y") :$: (CVar "z")))))) :$: (CVar "a") :$: (CVar "b") :$: (CVar "c"))
        print (rebuild (evaluate (spine (translate (runParser "(\\x-> (\\y-> (\\z-> (x (y z))))) a b c")))))
        print (rebuild (evaluate (spine (translate ((CAbs (CVar "x") (CAbs (CVar "y") (CAbs (CVar "z") ((CVar "x") :$: ((CVar "y") :$: (CVar "z")))))) :$: (CVar "a") :$: (CVar "b") :$: (CVar "c"))))))
        putStrLn ""
        -- S (K S) K a b c d e f
        -- Equivalente a "B a b c d e f"
        print (S :$: (K :$: S) :$: K :$: (CVar "a") :$: (CVar "b") :$: (CVar "c") :$: (CVar "d") :$: (CVar "e") :$: (CVar "f"))
        print (rebuild (evaluate (spine (translate (runParser "S (K S) K a b c d e f")))))
        print (rebuild (evaluate (spine (S :$: (K :$: S) :$: K :$: (CVar "a") :$: (CVar "b") :$: (CVar "c") :$: (CVar "d") :$: (CVar "e") :$: (CVar "f")))))
        putStrLn ""
        -- S (K S) K a b c (K I f)
        print (S :$: (K :$: S) :$: K :$: (CVar "a") :$: (CVar "b") :$: (CVar "c") :$: (K :$: I :$: (CVar "f")))
        print (rebuild (evaluate (spine (translate (runParser "S (K S) K a b c (K I f)")))))
        print (rebuild (evaluate (spine (S :$: (K :$: S) :$: K :$: (CVar "a") :$: (CVar "b") :$: (CVar "c") :$: (K :$: I :$: (CVar "f"))))))
        putStrLn ""
        -- S (K S) K a b c (K (K S g) f)
        print (S :$: (K :$: S) :$: K :$: (CVar "a") :$: (CVar "b") :$: (CVar "c") :$: (K :$: (K :$: S :$: (CVar "g")) :$: (CVar "f")))
        print (rebuild (evaluate (spine (translate (runParser "S (K S) K a b c (K (K S g) f)")))))
        print (rebuild (evaluate (spine (S :$: (K :$: S) :$: K :$: (CVar "a") :$: (CVar "b") :$: (CVar "c") :$: (K :$: (K :$: S :$: (CVar "g")) :$: (CVar "f"))))))
        putStrLn ""
        -- C a b c
        print (C :$: (CVar "a") :$: (CVar "b") :$: (CVar "c"))
        print (rebuild (evaluate (spine (translate (runParser "C a b c")))))
        print (rebuild (evaluate (spine (C :$: (CVar "a") :$: (CVar "b") :$: (CVar "c")))))
        putStrLn ""
        -- (λx. (λy. (λz. x z y)) a b c
        -- Equivalente a "C a b c"
        print ((CAbs (CVar "x") (CAbs (CVar "y") (CAbs (CVar "z") ((CVar "x") :$: (CVar "z") :$: (CVar "y"))))) :$: (CVar "a") :$: (CVar "b") :$: (CVar "c"))
        print (rebuild (evaluate (spine (translate (runParser "(\\x-> (\\y-> (\\z-> (x z y)))) a b c")))))
        print (rebuild (evaluate (spine (translate ((CAbs (CVar "x") (CAbs (CVar "y") (CAbs (CVar "z") ((CVar "x") :$: (CVar "z") :$: (CVar "y"))))) :$: (CVar "a") :$: (CVar "b") :$: (CVar "c"))))))
        putStrLn ""
