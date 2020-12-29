Esta tarefa é referente a algumas das aulas do curso Engenharia de Dados com Hadoop e Spark da Data Science Academy.
Através do framework Spark, é possível usar o MapReduce para verificar a quantidade de repetições
que uma palavra aparece no texto. Neste exemplo é considerado a palavra se estiver seguido de .,!? ou de outro
caractere parecido.

Dentro do arquivo input.txt, contém um trecho do artigo:</br>
http://datascienceacademy.com.br/blog/cientista-de-dados-por-onde-comecar-em-8-passos/

<b>Segue o trecho abaixo:</b>

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


</br>


Após realizar o MapReduce através do código app.py feito em Python, dentro do arquivo parte-00000, 
é possível verificar o resultado.

<b>Segue o resultado abaixo:</b>


('Eles', 1)</br>
('também', 1)</br>
('são', 1)</br>
('hum', 4)</br>
('sinal', 1)</br>
(dos, 1)</br>
('tempos', 1)</br>
(modernos, 1)</br>
(Cientistas, 1)</br>
('de', 17)</br>
(dados, 5)</br>
('não', 4)</br>
(estavam, 1)</br>
('não', 1)</br>
(radar, 1)</br>
('há', 1)</br>
(uma, 2)</br>
('década', 1)</br>
(mas, 1)</br>
('sua', 1)</br>
('popularidade', 1)</br>
(repentina, 1)</br>
(refletir, 1)</br>
('como', 1)</br>
('como', 3)</br>
('empresas', 1)</br>
('agora', 1)</br>
(pensam, 1)</br>
('sobre', 1)</br>
('Grande', 1)</br>
("Dados", 1)</br>
(Essa, 1)</br>
('incrível', 1)</br>
('massa', 1)</br>
('informações', 1)</br>
('estruturadas', 1)</br>
('já', 1)</br>
('pode', 2)</br>
(mais, 1)</br>
('ser', 2)</br>
(ignorada, 1)</br>
('e', 7)</br>
('esquecida'. 1)</br>
(É, 1)</br>
(mina, 1)</br>
('ouro', 1)</br>
('virtual', 1)</br>
('que', 6)</br>
('ajuda', 1)</br>
('a', 2)</br>
('aumentar', 1)</br>
('receitas', 1)</br>
('-', 1)</br>
(contanto, 1)</br>
(haja 1)</br>
('alguém', 1)</br>
(escave, 1)</br>
(desenterre, 1)</br>
('insights', 3)</br>
('fotografia', 2)</br>
('ninguém', 1)</br>
(havia, 1)</br>
(pensado, 1)</br>
('em', 6)</br>
('procurar.', 1)</br>
(Entra, 1)</br>
('cena', 1)</br>
('o', 2)</br>
('Cientista', 5)</br>
(Dados., 1)</br>
('', 2)</br>
('Pará', 1)</br>
('comunidade', 1)</br>
('geral', 1)</br>
(Dados, 3)</br>
('é', 1)</br>
(desses, 2)</br>
('Magos', 1)</br>
('Dados', ', 1)</br>
('adquirir', 1)</br>
(massas, 1)</br>
(diversas, 1)</br>
(fontes, 1)</br>
('então', 1)</br>
('limpar', 1)</br>
('tratar', 1)</br>
('organizar', 1)</br>
(preparar, 1)</br>
('os', 2)</br>
('dados;', 1)</br>
('e', 1)</br>
('seguido', 2</br>)
('explorar', 1)</br>
(suas, 1)</br>
('habilidades', 1)</br>
('Matemática', 1)</br>
(Estatística, 1)</br>
('Máquina', 1)</br>
('Aprendizado', 1)</br>
('para', 3)</br></br>
('descobrir', 1)</br>
(ocultos, 1)</br>
(negócios, 1)</br>
('gerar', 1)</br>
('inteligência', 1)</br>
('Os', 1)</br>
(utilizado, 1)</br>
('por', 2)</br>
('podem', 1)</br>
('tanto', 1)</br>
('estruturados', 2)</br>
((bancos, 1)</br>
('transacionais', 1)</br>
(sistemas, 1)</br>
(ERP, 1)</br>
('ou', 4)</br>
('CRM', 1)</br>
('exemplo)', 1)</br>
('(e-mails', 1)</br>
('imagens', 1)</br>
('vídeos', 1)</br>
(redes, 1)</br>
('sociais).', 1)</br>
(O, 1)</br>
(cria, 1)</br>
('algoritmos', 1)</br>
('extrair', 1)</br>
(dados., 2)</br>
('Em', 1)</br>
('cabe', 1)</br>
('ao', 2)</br>
('Dados', 1)</br>
('apresentação', 1)</br>
(estes, 1)</br>
('dados', 1)</br>
(forma, 1)</br>
('tomadores', 1)</br>
(decisão, 1)</br>
(podem, 1)</br>
('utilizar', 1)</br>
('resultado', 1)</br>
('da', 1)</br>
('análise', 1)</br>
(definir, 1)</br>
('estratégias', 1)</br>
('mesmo', 1)</br>
('criar', 1)</br>
(novos, 1)</br>
(produtos, 1)</br>
('serviços', 1)</br>
('fundados', 1)</br>
