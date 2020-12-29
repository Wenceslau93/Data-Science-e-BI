Esta tarefa é referente a algumas das aulas do curso Engenharia de Dados com Hadoop e Spark da Data Science Academy.
Através do framework Spark, é possível usar o MapReduce para verificar a quantidade de repetições
que uma palavra aparece no texto. Neste exemplo é considerado a palavra se estiver seguido de .,!? ou de outro
caractere parecido.

Dentro do arquivo input.txt, contém um trecho do artigo:
http://datascienceacademy.com.br/blog/cientista-de-dados-por-onde-comecar-em-8-passos/

Segue o trecho abaixo:

Eles também são um sinal dos tempos modernos. Cientistas de dados não estavam no radar há uma década,
mas sua taxa de arrependimento reflete como empresas agora pensadas sobre Big Data.
Essa incrível massa de informações não estruturadas já não pode ser mais ignorada e esquecida.
É uma mina de ouro virtual que ajuda a aumentar receitas - contanto que haja alguém que escaneie e desenterre
insights que não foram considerados em procurar. Entra em cena o Cientista de Dados.

Para uma comunidade em geral, um Cientista de Dados é um desses “Magos de Dados”,
que pode adquirir massas de dados de diversas fontes e depois limpar, tratar,
organizar e preparar os dados; e, em seguida, explore como suas habilidades em Matemática,
Estatística e Aprendizado de Máquina para descobrir insights ocultos de negócios e gerar inteligência.

Os dados usados ​​por um cientista de dados podem ser tanto estruturados
(bancos de dados transacionais de sistemas ERP ou CRM, por exemplo)
e não estruturados (e-mails, imagens, vídeos ou dados de redes sociais).
O Cientista de Dados cria algoritmos para extrair insights desses dados.
Em seguida, inclua os dados de cientista, apresente estes dados,
de forma que os tomadores de decisão podem usar o resultado da análise e definir
como as estatísticas ou o mesmo para criar novos produtos ou serviços utilizados em dados.





Após realizar o MapReduce através do código app.py feito em Python, dentro do arquivo parte-00000, 
é possível verificar o resultado.

Segue o resultado abaixo:


('Eles', 1)
('também', 1)
('são', 1)
('hum', 4)
('sinal', 1)
(dos, 1)
('tempos', 1)
(modernos, 1)
(Cientistas, 1)
('de', 17)
(dados, 5)
('não', 4)
(estavam, 1)
('não', 1)
(radar, 1)
('há', 1)
(uma, 2)
('década', 1)
(mas, 1)
('sua', 1)
('popularidade', 1)
(repentina, 1)
(refletir, 1)
('como', 1)
('como', 3)
('empresas', 1)
('agora', 1)
(pensam, 1)
('sobre', 1)
('Grande', 1)
("Dados", 1)
(Essa, 1)
('incrível', 1)
('massa', 1)
('informações', 1)
('estruturadas', 1)
('já', 1)
('pode', 2)
(mais, 1)
('ser', 2)
(ignorada, 1)
('e', 7)
('esquecida'. 1)
(É, 1)
(mina, 1)
('ouro', 1)
('virtual', 1)
('que', 6)
('ajuda', 1)
('a', 2)
('aumentar', 1)
('receitas', 1)
('-', 1)
(contanto, 1)
(haja 1)
('alguém', 1)
(escave, 1)
(desenterre, 1)
('insights', 3)
('fotografia', 2)
('ninguém', 1)
(havia, 1)
(pensado, 1)
('em', 6)
('procurar.', 1)
(Entra, 1)
('cena', 1)
('o', 2)
('Cientista', 5)
(Dados., 1)
('', 2)
('Pará', 1)
('comunidade', 1)
('geral', 1)
(Dados, 3)
('é', 1)
(desses, 2)
('Magos', 1)
('Dados', ', 1)
('adquirir', 1)
(massas, 1)
(diversas, 1)
(fontes, 1)
('então', 1)
('limpar', 1)
('tratar', 1)
('organizar', 1)
(preparar, 1)
('os', 2)
('dados;', 1)
('e', 1)
('seguido', 2)
('explorar', 1)
(suas, 1)
('habilidades', 1)
('Matemática', 1)
(Estatística, 1)
('Máquina', 1)
('Aprendizado', 1)
('para', 3)
('descobrir', 1)
(ocultos, 1)
(negócios, 1)
('gerar', 1)
('inteligência', 1)
('Os', 1)
(utilizado, 1)
('por', 2)
('podem', 1)
('tanto', 1)
('estruturados', 2)
((bancos, 1)
('transacionais', 1)
(sistemas, 1)
(ERP, 1)
('ou', 4)
('CRM', 1)
('exemplo)', 1)
('(e-mails', 1)
('imagens', 1)
('vídeos', 1)
(redes, 1)
('sociais).', 1)
(O, 1)
(cria, 1)
('algoritmos', 1)
('extrair', 1)
(dados., 2)
('Em', 1)
('cabe', 1)
('ao', 2)
('Dados', 1)
('apresentação', 1)
(estes, 1)
('dados', 1)
(forma, 1)
('tomadores', 1)
(decisão, 1)
(podem, 1)
('utilizar', 1)
('resultado', 1)
('da', 1)
('análise', 1)
(definir, 1)
('estratégias', 1)
('mesmo', 1)
('criar', 1)
(novos, 1)
(produtos, 1)
('serviços', 1)
('fundados', 1)