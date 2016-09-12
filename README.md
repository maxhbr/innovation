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

playerFoo: draw [got age1 card]

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
- everything starting with `...` is generated output which is not saved
- `age1:agriculture` is an id of a card
- `age1:agriculture:1` is an id of a dogma on a card

The internal representation might be of type 
```
type History = [(UserId,UserAction)]
```
and might look like
```
admin, p1, p2 :: UserId
seed :: Integer

[(admin, Init seed)
,(admin, AddPlayer p1)
,(admin, AddPlayer p2)
,(p2, Play age1:agriculture)
,(p1, Play age1:clothing)
,(p2, Draw)
,(p1, Draw)
,(p1, Activate Yellow)
,(p2. Choose age1:someCard)
,(p2, Play age1:codeOfLaws)
,(p2, Activate Purple)
,(p1, Dominate Age1)
,(p1, Draw)
,[...]
]
```

### More information:
- official rules (of the base game):
  - http://www.iellogames.com/downloads/Innovation_rules.pdf
  - http://asmadigames.com/innovation/FiguresRules.pdf
  - http://www.asmadigames.com/innovation/InnovationRulesWeb.pdf
- list of cards and more information: https://boardgamegeek.com/filepage/67894/innovation-official-faq-and-cards-list
