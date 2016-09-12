# Innovation

### A input of a game might look like

```
admin: init
... seed is [only visible for admin]
... generate inital permutations
admin: addPlayer "player1"
admin: addPlayer "playerFoo"
playerFoo: play age1:3
player1: play age1:12
... staring player is playerFoo

playerFoo: draw

player1: draw
player1: activate yellow
... playerFoo is affected by the dogma.
... playerFoo has to choose an active card from the board to recycle:
playerFoo: choose age1:3

playerFoo: play age1:2
playerFoo: activate green
... player1 also used the cooperative dogma
... playerFoo: draw

player1: dominate age1
player1: draw
[...]
```

where
- `admin` is the user who has created this game
- `age1:3` is an id of an card **in the current game**, where the first part
  determines the age of a card and the second part is the id of the card after
  applying the inital pormutation for this age

### More information:
- official rules (of the base game):
  - http://www.iellogames.com/downloads/Innovation_rules.pdf
  - http://asmadigames.com/innovation/FiguresRules.pdf
  - http://www.asmadigames.com/innovation/InnovationRulesWeb.pdf
- list of cards and more information: https://boardgamegeek.com/filepage/67894/innovation-official-faq-and-cards-list
