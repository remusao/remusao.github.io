---
title: Python—Lister le contenu des répertoires
date: 2012-02-02
logo: python
lang: fr
---

**Disclaimer** : Il est probablement plus simple d'utiliser la fonction
[os.walk](https://docs.python.org/2/library/os.html#os.walk) pour lister
récursivement le contenu d'un dossier.

Python est un langage très haut niveau qui permet de faire beaucoup de
choses très facilement. Notamment, il est très facile de manipuler les
fichiers et les dossiers grâce au module os. Les lecteurs connaissant
le langage C remarqueront que les fonctions présentes dans ce module
sont les mêmes que celles de la bibliothèque standard du C (exceptées
certaines fonctions un peu plus « haut-niveau » qui sont seulement
disponibles dans le module os de Python).

Dans cet articles nous allons simplement voir comment lister
récursivement le contenu des dossiers à partir de la position
actuelle. Commençons tout d’abord par inclure le module os comme ceci :

```python
import os
```

Nous utiliserons ensuite les fonctions suivantes :

* `os.listdir(dossier)` : Liste le contenu de `dossier` (fichiers et dossiers)
* `os.chdir(dossier)` : On se déplace dans `dossier`.
* `os.path.isdir(chemin)` : Détermine si le `chemin` donné en paramètre
pointe vers un dossier ou un simple fichier.
* `os.getcwd()` : Retourne une chaine de caractères représentant la
position actuelle dans le système de fichiers (sous forme de chemin
absolu).

Avec ces quelques fonctions , on peu facilement lister le contenu des
sous-dossiers à partir de la position courante comme ceci :

```python
def parse():
  """
    Liste récursivement le contenu des sous-répertoires
  """
  for f in os.listdir(os.getcwd()):
      if os.path.isdir(f): # si f est un dossier
        os.chdir(f) # On va lister son contenu
        parse()
        os.chdir('../') # On revient au répertoire précédent
      else:
        # Traitement sur le fichier f
```
