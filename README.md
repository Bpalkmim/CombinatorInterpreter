# CombinatorInterpreter
Um parser para expressões Lambda estendidas (que incluem combinadores S, K, I, C e B), que elimina as Lambda abstrações (essencialmente transformando-as em expressões puramente em lógica combinatória) e um interpretador/avaliador de expressões em lógica combinatória.

O módulo Parser.hs contém a definição do monad de parsing e algumas funções básicas para parsing. Utiliza-se uma gramática estilo PEG.

Em CombinatorParser.hs temos a definição da AST de combinadores/λ-calculus estendido e as funções de parsing específicas. Aqui há a remoção de λ-abstrações, deixando o resultado final como pura lógica combinatória. Este módulo depende de Parser.hs.

Finalmente, em CombinatorInterpreter.hs temos o interpretador das expressões de lógica combinatória, de acordo com CombinatorParser.hs. Este módulo depende de CombinatorParser.hs.