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
```haskell
type History = [UserAction State]
```
and might look like
```haskell
deckName :: String
Admin, p1, p2 :: UserId
p1 = U "player1"
p2 = U "Player2"
seed :: Integer
seed = 12345

G [ Admin `does` Init deckName
  , Admin `does` AddPlayer p1
  , Admin `does` AddPlayer p2
  , Admin `does` StartGame seed
  , p2    `chooses` age1:agriculture
  , p1    `chooses` age1:clothing
  , Admin `does` DeterminesTurnOrder
  , p2    `does` Draw
  , p1    `does` Draw
  , p1    `does` Activate Yellow
  , p2    `does` Choose age1:someCard
  , p2    `chooses` ToPlayMaybe
  , p1    `chooses` ToPlayMaybe
  , p2    `does` Play age1:codeOfLaws
  , p2    `does` Activate Purple
  , p1    `does` Dominate Age1
  , p1    `does` Draw
  , [...]
  ]
```

### More information:
- official rules (of the base game):
  - http://www.iellogames.com/downloads/Innovation_rules.pdf
  - http://asmadigames.com/innovation/FiguresRules.pdf
  - http://www.asmadigames.com/innovation/InnovationRulesWeb.pdf
- list of cards and more information: https://boardgamegeek.com/filepage/67894/innovation-official-faq-and-cards-list
