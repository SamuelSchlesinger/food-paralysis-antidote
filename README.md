# Food Paralysis Antidote

To install, run `cabal install --installdir=.` and the executable will be
built inside this directory.

To use, observe the help texts from the command lines:

```
$ food-paralysis-antidote

Generate Dinner Ideas

Usage: food-paralysis-antidote COMMAND
  Outputs dinner ideas from a list of recipes as well as lists of proteins,
  veggies, and carbs

Available commands:
  pvc                      Outputs a dinner idea with a random protein, veggie,
                           and carb
  recipe                   Outputs a more specific recipe idea
  any                      Outputs a completely random dinner idea

$ food-paralysis-antidote pvc --help

Usage: food-paralysis-antidote pvc [-p|--protein ARG] [-v|--veggie ARG]
                                   [-c|--carb ARG]
  Outputs a dinner idea with a random protein, veggie, and carb

Available options:
  -h,--help                Show this help text

$ food-paralysis-antidote recipe --help

Usage: food-paralysis-antidote pvc [-p|--protein ARG] [-v|--veggie ARG]
                                   [-c|--carb ARG]
  Outputs a dinner idea with a random protein, veggie, and carb

Available options:
  -h,--help                Show this help text

$ food-paralysis-antidote any --help

Usage: food-paralysis-antidote any a more specific recipe idea
  Outputs a completely random dinner idealetely random dinner idea

Available options: Samuel Schlesinger
  -h,--help                Show this help textantidote % vim README.md 
```

To modify the set of recipes, proteins, carbs, or veggies, modify `app/Settings.hs`
and rebuild.
