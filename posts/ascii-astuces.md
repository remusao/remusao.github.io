---
title: Ascii, astuces
date: 2012-12-25
logo: cpp
lang: fr
---

Voici quelques petites astuces afin de manipuler plus simplement des
caractères encodés en ASCII.

## Changer la casse

Cette astuce est indiquée dans le page de `man(1)` ascii, mais je l’ai
découverte très récemment car je n’avais pas eu le réflexe de lire
les paragraphes se trouvant à la suite de la table ASCII sur cette
page. Afin de changer la casse d’une lettre (c’est à dire
passer de minuscule à majuscule, ou l’inverse), il suffit de changer
le 5ième bit. Celui-ci est à 1 pour les minuscules et à 0 pour les
majuscules. Donc en effectuant un `XOR 32` avec une lettre on change sa
casse. C’est tout de même pratique !

```c
'A' ^ 32 == 'a'
'a' ^ 32 == 'A'
```

## Manipuler les chiffres

Afin de convertir un entier entre 0 et 9 en son équivalent en
caractère ASCII ('0' à '9'), on ajoute (ou soustrait selon le
sens de la conversion) la valeur de '0'. On peut aussi remarquer
qu’entre un entier et son équivalent en ASCII seuls 2 bits changent.
Il s’agit donc de mettre les 6ième et 5ième bits à 1 (ou 0 si on
désire passer d’un caractère à un entier). Il suffit donc d’un
simple `XOR 48`, comme ceci :

```c
'9' ^ 48 == 9
```

Ces deux astuces sont assez basiques, mais cela peut toujours servir !
