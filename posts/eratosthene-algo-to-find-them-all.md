---
title: Crible d'Ératosthène, Un algo pour les trouver tous
date: 2011-11-15
logo: c++
lang: fr
---
 	
Il y a quelques temps je me suis intéressé au crible
d’Ératosthène, qui permet de trouver l’ensemble des nombres
premiers inférieurs à une borne donnée. L’application naïve
de l’algorithme dans sa version originale n’étant pas très
performante, j’ai cherché à l’optimiser afin d’obtenir des
performances correctes. Voici donc les différents moyens de rendre cet
algorithme plus efficace. Des fichiers sources en C ou en Python seront
fournis tout au long de l’article.

1. Qu’est-ce qu’un nombre premier ?
2. Le crible d’Ératosthène.
3. Optimisations.
4. Performances.
5. Conclusion.
 

*Qu’est-ce qu’un nombre premier* – Un nombre premier est un entier
naturel qui n’admet que deux diviseurs, 1 et lui-même. Ce qui exclut
tous les autres entiers naturels (c’est sur cette remarque qu’est
basé le crible d’Ératosthène). Il existe une infinité de nombre
premiers.

*Le crible d’Ératosthène* – L’algorithme du crible
d’Ératosthène est très simple. Prenons un tableau contenant les
entiers de 2 a n (si l’on désire connaitre tous les nombres premiers
inférieurs à n) que l’on suppose tous premiers. Ensuite il suffit,
pour chaque élément du tableau, de supprimer tous ses multiples. Le
pseudo code pourrait donc être le suivant :

```python
Crible(entier n):
  Tableau = entiers de 2 a n
  Pour chaque i dans Tableau
    supprimer_multiples de i dans Tableau
  Fin Pour Fin Crible
```

Le pseudo-code suivant peut aisément être implémenté en Python :

```python
def Supprimer_multiples(i, tableau):
  for x in tableau:
    if x > i and x % i == 0:
      tableau.remove(x)

def Crible(n):
  tableau = [i for i in range(2, n)]
  for i in tableau:
    Supprimer_multiples(i, tableau)
  return tableau

def main():
  tableau = Crible(100)
  print tableau

main()
```

On remarque que la complexité de cette implémentation n’est pas
du tout adaptée à des `n` très grands (essayez avec `n > 10000`, le
résultat va commencer à mettre un certain temps à être calculé).
Voyons comment l’optimiser.

## Optimisations

*Première remarque*, si le fait de stocker tous les nombres dans un
tableau puis de les supprimer au fur et à mesure peut sembler plus
lisible, ce n’est pas une bonne méthode si l’on recherche les
performances. Une première optimisation est de déclarer un tableau
de n cases qui peuvent chacune contenir soit 1 soit 0 en fonction de
la primalité (ou non-primalité) du nombre correspondant à l’index
considéré dans le tableau. Nous allons voir qu’avec cette technique
nous allons pouvoir nous passer complètement de comparaisons et de
tests (sauf pour les cas d’arrêt des boucles bien évidemment), ce
qui va diminuer grandement le temps de calcul.

*Deuxièmement*, on remarque que pour obtenir tous les multiples d’un
nombre i dans notre tableau dont les index correspondent aux nombres
dont on veut connaitre la primalité, il n’est pas nécessaire de
parcourir tout le tableau en testant à chaque fois le modulo. Pour cela
il suffit de partir de l’index i puis d’aller de i en i dans le
tableau, nous sommes ainsi assurés de passer sur tous les multiples de
i (y compris lui-même). Ceci va grandement améliorer notre algorithme.

Une autre remarque que l’on peut faire est que pour tester la
primalité d’un nombre n, il n’est pas nécessaire de tester la
division par tous les nombres de `2` a `n`, il suffit d’aller jusqu’à
la racine carrée de n (au delà on ne peut pas trouver de diviseur
entier).

Enfin, une petite amélioration liée à l’implémentation en C de
l’algorithme, puisqu’on va devoir initialiser notre tableau avec
toutes les cases à 1, autant éliminer tous les nombres pairs (qui ne
sont pas premier puisque divisibles par `2`). On peut donc initialiser
notre tableau avec deux boucles distinctes, une qui part de 3 et
qui va jusqu’à n de deux en deux en initialisant à 1 les cases
visitées et une autre qui part de 4 et qui va jusqu’à n de deux
en deux en initialisant à 0 les cases parcourues. Voici ce que donne
l’algorithme implémenté en C avec les optimisations précédentes
(sources : <http://www.pythux.com/exemples/erato/crible_1.c>). Pour compiler le
fichier source, vous pouvez utiliser soit clang soit gcc (ou un autre
compilateur C) comme ceci :

```sh
clang -lm -O3 crible_1.c
gcc -lm -o3 crible_1.c
```

Nous pourrions nous arrêter là, puisque les performances sont déjà
très bonnes (11 ms pour un crible jusqu’à 1.000.000 et 40-45
secondes jusqu’à 1.000.000.000), mais nous allons voir qu’il est
encore possible de diminuer drastiquement le temps de calcul. En effet,
nous savons qu’à part le nombre 2, aucun nombre pair n’est premier.
Nous pourrions donc chercher un moyen de ne pas les stocker dans le
tableau, afin de ne garder que les nombres impairs. Nous diviserions
donc dans un premier temps l’espace mémoire occupé par deux. Pour
un crible jusqu’à n = 30, nous aurions à stocker les éléments
suivants :

```sh
Les nombres -> [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29]
Les index   -> [0, 1, 2, 3, 4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14]
```

Hors, nous ne pouvons plus simplement utiliser les index dans le tableau
pour connaitre les nombres associés. En fait, on peut remarquer que
pour retrouver le nombre associé a chaque case du tableau, il nous
suffit de calculer : `index * 2 + 1` . Deuxièmement, pour retrouver
les multiples d’un nombre N dans le tableau, il nous suffit d’aller
de N en N dans le tableau en partant de la case `N / 2`. Enfin, on
remarque qu’avec cette technique, l’algorithme est plus compact
et nous pouvons nous contenter d’initialiser toutes les cases du
tableau à 1 (avec la fonction memset de la bibliothèque `<string>` du
langage C). Voici le code source de cette implémentation (sources :
<http://www.pythux.com/exemples/erato/crible.c>) :

```c
char *erato_opti(int n)
{
  char  *tab = malloc(n / 2);
  int   i = 1, j, borne = sqrt(n / 2), step;

  tab = memset(tab, 1, n / 2);

  for (; i <= borne; i++)
  {
    step = 2 * i + 1;
    for (j = i + step; j < n / 2; j += step)
      tab[j] = 0;
  }

  return (tab);
}
```

Si nous voulons afficher les nombres premiers produits par la fonction
erato_opti il nous suffit d’utiliser l’astuce du `nombre = index *
2 + 1` et de ne pas oublier d’afficher le nombre 2 (qui n’est pas
contenu dans le tableau) :

```c
void print_erato_space(char *tab, int n)
{
  int i = 1;

  printf("2\n");
  for (; i < n / 2; i++)
    if (tab[i])
      printf("%i\n", i * 2 + 1);
}
```

Vous pouvez compiler les sources précédentes avec l’une ou l’autre
des commandes suivantes :

```sh
clang -lm -O3 crible.c
gcc -lm -O3 crible.c
```

## Performances

Voici un récapitulatif des performances obtenues avec les différentes
implémentations :


Valeur de N              | 1000  | 10.000     | 1.000.000  | 1.000.000.000
---                      | :---:   | :---:        | :---:        | :---:
Version Python (Pypy)    | 30ms  |  370ms     |  /         |  /
Version Python (CPython) | 42ms  |  710ms     |  /         |  /
Version C intermédiaire  |  1ms  |    1ms     | 7ms        | 40s
Version C opti           |  1ms  |    1ms     | 6ms        | 32s

## Conclusion

Nous avons vu quelques optimisations possibles sur l’algorithme du
crible d’Ératosthène. Cet article est loin d’être exhaustif,
vous pourrez trouver d’autres optimisations plus ou moins efficaces.
Néanmoins, la plupart des implémentations trouvables sur internet et
qui sont plus performante que celle proposée ici sont moins lisibles et
plus complexes que celle-ci (qui ne fait qu’une dizaine de ligne). Si
vous connaissez d’autres optimisations, n’hésitez pas a poster un
commentaire :)
