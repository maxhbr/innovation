# Innovation

### A input of a game might look like
This is what player1 would see:
```
admin: init
... seed is [only visible for admin]
... generate inital permutations
admin: addPlayer "player1"
admin: addPlayer "playerFoo"

playerFoo: play age1:agriculture
player1: play age1:clothing
... staring player is playerFoo

playerFoo: draw

player1: draw [got age1:metalworking]
player1: activate yellow
... playerFoo is affected by the dogma.
... playerFoo has to choose an card from the influence to recycle:
playerFoo: choose [an age1 card]

playerFoo: play age1:codeOfLaws
playerFoo: activate purple
... player1 also used the cooperative dogma
... playerFoo: draw [got age1 card]

player1: dominate age1
player1: draw [got age1:oars]

[...]
```

where
- parts in brackets (`[`..`]`) depend on the viewing user, everything else is
  visible to everyone
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
