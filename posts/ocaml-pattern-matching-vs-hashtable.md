---
title: Ocaml—Hash table vs. pattern matching
date: 2012-11-01
logo: ocaml
lang: fr
---

*Disclaimer*: This is an old article and the information might be out-dated.

Dans le cadre d’un projet personnel j’en suis venu à réaliser un
lexer avec l’excellent Ocamllex. Afin de matcher les mots clés du
langage source (le langage Python), deux solutions se sont offertes à
moi afin d’associer un token à chaque string représentant un mot
clé :

1. Utiliser une Hashtable dans laquelle chaque entrée (clé, valeur) représente un mot clé et son token associé.
2. Utiliser le pattern matching où chaque entrée correspond à un token.

Les deux solutions ayant une « verbosité » équivalente (j’ai tout
de même une légère préférence pour le pattern matching, que je
trouve un peu plus lisible), je me suis demandé si les performances
étaient équivalentes. J’ai donc réalisé un petit benchmark afin de
comparer les deux solutions.

Les deux méthodes ont donc été testées avec une entrée de `3.000.000`
de chaines de caractères contenant, à fréquences d’apparition
égales, tous les mots clés possibles ainsi que des mots-clés non
existants.

Le programme a été compilé sans flag d’optimisation particulier. La
machine de test était équipée d’un processeur Atom 1,2 Ghz et 1 Go
de mémoire vive. Voici le résultat:

* Hashtable :  `1,259` secondes.
* Pattern-matching : `2,265` secondes.

Victoire pour les Hashtables. Dommage que le compilateur n’optimise
pas le pattern-matching en Hash table lorsque c’est possible.
