# minijeu

lien du github :https://github.com/Vyslon/PAF_city
Mini-jeu en Haskell-SDL2 à compléter.
(En cas d'erreur, commenter la ligne "arch: i386" dans stack.yaml et décommenter la ligne "compiler: ghc-8.10.7")


## Compilation

Ce projet requiert la bibliothèque sdl2 (Simple Media Libary, v2.0).

Sous Linux ou MacOS, il suffit d'installer la dépendance associée
(par exemple `libsdl2-dev` sous Debian/Ubuntu).

Sous Windows, c'est un peu plus complexe (comme d'habitude).  Le plus simple est de passer par *msys2* dont une version est installée par *stack*.  Normalement, la commande suivante devrait suffire :

```
stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2
```

Dans tous les cas, on utilisera :

```
stack build
```

Pour construire le projet.

et :

```
stack run
```

Pour le lancer...

