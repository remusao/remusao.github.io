---
title: C++, taille d’un tableau static en temps constant
date: 2012-12-25
logo: c++
lang: fr
---

J’ai découvert cette semaine une petite astuce que je trouve assez
élégante, même si son utilité est assez limitée. Il est possible
grâce à un template de garder la trace de la taille d’un tableau
static entre des appels de fonctions. Voyez plutôt :

```c++
template <typename T, int N>
int size(T (&)[N])
{
    return N;
}

int main()
{
    int tab[42];
    cout << size(tab) << endl;
    return 0;
}
```

Notre fonction size est une fonction template qui prend en paramètre
le type `T` des éléments stockés dans le tableau, ainsi que sa taille.
Ainsi lorsqu’on appelle notre fonction `size` sur un tableau, cette
fonction est spécialisée à la compilation avec la bonne valeur de `N`
et le bon type `T` et chaque appel sera remplacé par la valeur `N`, qui est
la taille du tableau.

Notez également que pour garder l’information de la taille du
tableau, il faut passer une référence sur le pointeur du tableau.
Si notre fonction `size` avait simplement pris un pointeur en argument
ça n’aurait pas marché puisque le compilateur ne garde pas
l’information de la taille, et considère l’argument comme un simple
pointeur.
