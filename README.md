Para compilar o programa primeiramente é necessario obter ter instalado na máquina o "ghc"
Com o compilador devidamente instalado, siga os seguintes passos:
-- Compilação
1 - Abrir o terminal na pasta que contém o arquivo fonte "jogo_da_vida.hs"
2 - Rodar o comando "ghc .\jogo_da_vida.hs" no terminal

-- Rodar Manualmente
3 - Ainda no terminal, após executar o comando do passo 2, rode ".\jogo_da_vida.exe"
4 - Insira o tamanho do tabuleiro n
5 - Insira as n linhas separado por espaço, onde 0 é célula morta, 1 é célula viva e 2 é célula morta
6 - Insira a quantidade de iterações

-- Rodar algum caso de teste pré-definido
7 - Rode o seguinte comando no terminal, na pasta que foi rodado o comando do passo 2 "cat .\tests\case1Input.txt | .\jogo_da_vida.exe" (Ou o caso de teste desejado)
8 - Verifique se a saida está compativel ou não