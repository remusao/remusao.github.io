---
title: Python, Le saviez-vous ?
date: 2012-02-05
logo: python
---

J’en apprend tous les jours sur le Python, et tous les jours je
suis émerveillé par la richesse de ce langage. Je fais actuellement
quelques recherches sur des utilisations avancées des décorateurs
(afin de faire de la méta-programmation par exemple), j’ai découvert
deux chose :

1. Il est tout à fait possible en Python de déclarer des fonctions à
l’intérieur d’autres fonctions. Par exemple, le code suivant est
tout à fait valide :

```python
def main():
  def toto():
    print('toto')
    return 1
  return toto()
```

2. Il est également possible de créer des classes à l’intérieur de fonctions :

```python
def dessine_moi_une_classe():
  class Classe():
    def dessine(self):
      print('classe')

  c = Classe()
  return c
```

Bref, tout ceci pour dire que le Python est un langage d’une richesse
insoupçonnée :)
