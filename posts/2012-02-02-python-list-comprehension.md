---
title: Python, construction des listes
date: 2012-02-02
logo: python
---

Le type liste du langage python est une des structures les plus utilises
(et utilisées). Elles sont polyvalentes et efficaces. En effet, on
peut aussi bien s’en servir comme tableau, file, pile, listes, etc..
L’outil à tout faire en quelques sorte. Mais leur utilité et leur «
beauté » ne s’arrête pas là, car en plus de fournir de nombreuses
méthodes utiles, le programmeur a la possibilité de construire des
listes de manière très élégante. Voyons voir de plus près de quoi
il s’agit !

Les listes étant représentées entre crochet ‘[' et ']‘, c’est
tout naturellement avec ce formalisme que nous allons les créer, en
plaçant entre les crochets des « motifs » décrivant les éléments
contenus dans la liste. Voyons sans plus tarder un exemple :

```python
l = [x for x in range(1, 10)]
>> [1, 2, 3, 4, 5, 6, 7, 8, 9]
```

Nous pouvons bien sûr imaginer des exemples plus complexes comme la
génération de tous les couples de nombres (x,y) avec x et y compris
entre 1 et 9 :

```python
l = [(x, y) for x in range(1, 10) for y in range(1, 10)]
```

Il est également possible de rajouter des conditions sur les éléments
avec lesquels nous construisons les listes :

```python
l = [x*x for x in range(1, 10) if x % 2 == 0]
```

Ce dernier exemple génère la liste contenant les carrés de tous les
nombres pairs compris entre 1 et 10. Vous commencez à comprendre ? Ce
qui est drôle, c’est qu’on peut utiliser des constructions de liste
dans des constructions de liste. Ainsi on peut facilement générer des
matrice (listes de listes) :

```python
l = [[x * y for x in range(1, 10)] for y in range(1, 10)]
```

Je vous laisse entrer cette ligne dans un interpréteur Python pour
voir ce que cela donne. On peut ainsi créer des listes contenant a
peu près n’importe quoi avec cette notation. Cette dernière étant
beaucoup plus compacte qu’une ou plusieurs boucles ‘for’, on gagne
beaucoup en lisibilité et compacité en les utilisant. Je précise
également que ce genre de constructions est également disponible pour
d’autres structures de données du Python (notamment les set et les
dictionnaires) :

```python
s = {x for x in range(1, 10)}    # crée un set
d = {x : x * x for x in range(1, 10)}  # crée un dictionnaire
```

Il y a donc de quoi s’amuser :) Toutefois, attention à ne pas
les utiliser lorsque ce n’est pas nécessaire. Imaginons que vous
souhaitiez initialiser une liste avec 1M éléments ayant tous la même
valeur (disons True). Les deux manières suivantes sont équivalentes :

```python
l = [True for x in range(0, 1000000)]
l = [True] * 100000
```

Bien que produisant le même résultat, la deuxième méthode est
plus rapide que la première, attention donc de ne pas abuser des
constructions de liste :)

**Édition du 5 février 2012** :

Dans tous les exemples précédents, c’est la fonction range qui a
été utilisée pour illustrer les constructions de listes. Sachez
qu’il est possible d’utiliser n’importe quel générateur à la
place, et même n’importe quelle structure de données itérable (par
exemple : une liste, un set, etc..).
