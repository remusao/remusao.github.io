---
title: Pypy, « Make Python faster ! »
date: 2011-11-10
logo: python
lang: fr
---

J’inaugure ce blog avec un article sur Pypy, une implémentation de
Python très intéressante du point de vue des performances puisque,
comme nous le verrons un peu plus bas, il est possible de rivaliser avec
des langages compilés statiquement tels que le C ou le C++. L’article
se décomposera de la façon suivante :

1. Qu’est-ce que Python.
2. Quelles sont les différentes implémentation ?
3. Qu’est-ce qui caractérise Pypy ?
4. La compilation JIT (Just-In-Time).
5. Compiler et utiliser Pypy.
6. Bref aperçu des performances.

## Qu’est ce que Python

Python est un langage de programmation moderne classé dans la
catégorie des langages de scripts (il est interprété, du moins dans
son implémentation officielle, CPython). Étant très haut niveau, il
permet aux développeurs de gagner en productivité en leur permettant
de se détacher au maximum des contraintes bas niveaux que l’on peut
retrouver sur d’autres langages tels que le C, ce qui en fait un
langage adapté pour l’apprentissage de la programmation. En effet,
il est facile d’accès et rapide à apprendre. Sa syntaxe, basée
sur l’indentation (pas d’accolades ni de mot-clef pour séparer
les « blocs » d’instruction) le rend plus lisible que la plupart
des langages. Pour terminer, voici quelques spécificités du langage
: Inférence de type, fonctionnalités objet avancées, type entier de
taille arbitraire, compréhension de listes permettant de créer des
listes de manière élégante, etc ..

## Quelles sont les différentes implémentations

Le langage Python étant placé sous licence libre, il en existe de
nombreuses implémentations ayant chacune leurs spécificités. En voici
quelques-unes :

1. *CPython* : Implémentation officielle (la plus connue) écrite en C.
2. *Pypy* : Implémentation disposant d’un compilateur JIT (Just-In-Time).
3. *ShedSkin* : Implémentation disposant d’un compilateur
Python-to-C++ et offrant des performances très intéressantes du point
de vu du temps d’exécution.
4. *Stackless* : Implémentation qui se passe de la pile d’appels du
C en la remplaçant par une structure de données propre au Python.
Cette implémentation est grandement basée sur le paradigme de la
concurrence, faisant appel à des Tasklets (sorte de micro-threads)
afin de simuler une exécution parallèle des programmes. Les Tasklets
peuvent être créées par centaines (voire milliers), communiquer entre
elles facilement, être mise en pause ou réveillées instantanément,
etc ..
5. *Jython* : Implémentation écrite en Java.
6. *Iron Python* : Implémentation écrite en C#.

## Qu’est-ce qui caractérise Pypy

Pypy est une implémentation de Python, écrite en Python, et proposant
(entre autres), un compilateur JIT (cf paragraphe suivant) lui
permettant de délivrer des performances dignes de la plupart des
langages compilés statiquement (C, C++, etc..). Pypy offre également
une consommation mémoire réduite par rapport à CPython, et supporte
la plupart des bibliothèques disponibles avec l’implémentation
officielle.

Il est également possible de compiler Pypy pour obtenir une version
SandBox ou Stackless de Python. D’autres langages sont également
supportés par Pypy (Javascript, Scheme, etc..) puisque ce dernier
utilise une chaine de traitement qui peut analyser des programmes
écrits en RPython (un subset réduit du langage Python), les
traduire en code C puis les compiler en code machine, ce qui permet
à n’importe quel langage d’être utilisé avec Pypy à condition
qu’ils puissent être traduit en RPython. Pour plus d’informations,
je vous invite à consulter le site officiel de Pypy dont le lien est
communiqué en fin d’article.

## La compilation JIT (Just-In-Time)

Le langage Python offrant des outils très haut niveau (tels que des
fonctionnalités Objet avancées), il est difficile et globalement
inefficace (comparativement à des langages plus bas niveaux tels que le
C) de le compiler statiquement, on ne se tournera donc pas vers de la
compilation statique si l’on recherche des performances honorables. Il
existe heureusement une alternative très performante à la compilation
statique, il s’agit de la compilation JIT (Just-In-Time, ou « à la
volée »). C’est cette technologie qui est utilisée dans le projet
Pypy, ce qui permet d’offrir au langage Python des performances dignes
de langages compilés statiquement tels que le C ou le C++. Voyons de
quoi il s’agit.

La compilation Just-In-Time (ou dynamique) représente une approche
hybride entre la compilation statique et l’interprétation, elle a
pour but d’obtenir des performance égales, voire supérieures, à la
compilation statique classique. Les compilateurs JIT convertissent du
Bytecode en code machine à la volée (au moment de l’exécution).
Mais pour limiter la dégradation des performances, plusieurs techniques
sont utilisées. Premièrement, il est possible de mettre en cache
certaines parties d’un programme afin de ne pas avoir à les
retraduire inutilement lors d’exécutions ultérieures si leur code
source n’a pas été modifié entre temps. Deuxièmement, grâce à
un système de détection des points chauds (parties d’un programmes
étant très sollicitées lors de l’exécution), le compilateur peut
décider de traduire une partie du programme en code machine si cela
s’avère plus intéressant qu’une simple interprétation en terme de
temps d’exécution.

De plus, de nombreuses optimisations ne sont possibles qu’au moment
de l’exécution du programme. Un compilateur JIT peut donc utiliser
les informations du contexte d’exécution afin d’améliorer les
performance d’un programme. Notamment, il est possible d’adapter le
code machine produit en fonction de l’architecture du processeur ou
encore d’effectuer de l’inlining de bibliothèques dynamiques, sans
perdre pour autant les avantages du linkage dynamique.

La compilation JIT est utilisée par la machine virtuelle du Java ou
encore le C#.

## Compiler et utiliser Pypy

Il existe deux solutions pour utiliser Pypy. Soit votre distribution
Linux favorite vous le propose directement depuis ses dépôts, auquel
cas il vous suffit de l’installer, soit vous pouvez télécharger
les sources depuis le mercurial de Pypy. Attention néanmoins, si
vous installez Pypy depuis les dépôts de votre distribution, il est
possible qu’il remplace une version existante de Python sur votre
ordinateur.

*Building from sources* : Nous allons voir ici comment télécharger les
sources de Pypy, les compiler puis utiliser le binaire produit. Avant
toute chose, il faut savoir que la compilation de Pypy est extrêmement
gourmande en ressources (mémoire vive et CPU), si vous n’avez pas
4 Go de RAM sur votre PC, il faudra passer par les dépôts de votre
distribution, sans quoi vous risquez de partir en Swap infini ..

*Liste des dépendances* : http://pypy.readthedocs.org/en/latest/getting-started-python.html#translating-the-pypy-python-interpreter

Une fois toutes les dépendances installées, récupérez les sources de
Pypy en lançant la commande suivant :

```sh
$ hg clone https://bitbucket.org/pypy/pypy
```

Rendez-vous ensuite dans le répertoire goal :

```sh
$ cd pypy/pypy/translator/goal
```

Lancez enfin le script avec Python (ou avec Pypy lui-même si vous
l’avez déjà installé, ce qui vous fera économiser du temps et de
la mémoire) :

```sh
$ python2 translate.py -Ojit
```

Vous apprécierez au passage les fractales de Mandelbrot qui seront
dessinées dans votre terminal pendant la compilation.

Une fois le long processus terminé, un binaire nommé pypy-c est
normalement présent dans le répertoire. Vous pouvez le renommer et/ou
le déplacer afin de l’utiliser comme bon vous semble. Il remplace le
binaire Python traditionnel, vous pouvez donc l’utiliser de la façon
suivante :

```sh
$ ./pypy mon_script.py # votre script python
```

## Bref aperçu des performances

Pour terminer, voici un bref aperçu des performances de Pypy
comparativement à d’autres implémentations/langages disponible sur
la page Performance officielle du site de Pypy : http://speed.pypy.org/

## Bibliographie

* http://pypy.org/index.html
* http://fr.wikipedia.org/wiki/PyPy
* http://en.wikipedia.org/wiki/PyPy
* http://en.wikipedia.org/wiki/Just-in-time_compilation
* http://www.python.org/
* http://fr.wikibooks.org/wiki/Python

Le mot de la fin – Toutes les propositions et remarques sont bien
évidemment les bienvenues :)
