; to do: implenment having a population of X and then doign tournaments by picking 3 at a time and save best and make it a parent


breed [dogs dog]
breed [sheeps sheep]

globals
[
  currentGeneration

  no-agent-actions
  no-agent-states
  no-agent-views
  chromosome-length
  total-chromosome-length

  currentChromosome
  tournamentChromosomes
  tournamentNumber
  tournamentResults

  dogPositions
  sheepPositions

  score
  bestScore
  bestChromosome
  allScores

]

dogs-own
[
  chromosome ;; this is a list of lists each containing list-pairs of:
           ;; one of 5 actions (encoded from 0 to 4)
           ;; one of 16 states to change to (encoded from 0 to 15)
           ;; In total a chromosome contains 128 numbers to encode agent behaviour
  generation ;; what generation they are from
  nextPatch
  currentState ;; what is the current state they are in
  sheepCentre
  number
]

sheeps-own
[
  nextPatch
  prevMove
  number

]

to setup
  clear-all
  reset-ticks
  random-seed 42
  set currentGeneration 0
  set bestScore 1000000
  set tournamentResults []
  set allScores []
  ;;these are defined by the design (agents can move/rotate, and can encounter 4 types of space so 16 states. 4 * 16 * 2 -> 128
  set no-agent-actions 5
  set no-agent-views 4
  set no-agent-states 16
  set chromosome-length (no-agent-views * no-agent-states)
  set total-chromosome-length chromosome-length * dogPop
  ask patches [set pcolor green]
  setup-walls

  set tournamentChromosomes []
  repeat tournamentSize [
    let chromosomeNew n-values total-chromosome-length [randomState]
    set tournamentChromosomes fput chromosomeNew tournamentChromosomes
  ]

  set currentChromosome item 0 tournamentChromosomes

  set dogPositions get-random-patches dogPop
  set sheepPositions get-random-patches sheepPop
  create-sheeps sheepPop [setup-sheep]

  create-dogs dogPop [setup-dogs]



  let n 0
  ask dogs [
    set label n     ; Sets the label of each turtle to its number
    set number n
    set n n + 1    ; Increment the number for the next turtle
  ]
  ask dogs [setup-chromosomes]
  set n 0
  ask sheeps [
    set label n     ; Sets the label of each turtle to its number
    set number n
    set n n + 1    ; Increment the number for the next turtle
  ]
  set tournamentNumber 0

   ask dogs [

    move-to item number dogPositions
  ]

  ask sheeps [
    move-to item number sheepPositions
  ]

end

to setup-chromosomes
  set currentState 0

  set chromosome sublist currentChromosome (chromosome-length * number) (chromosome-length * (number + 1))
end

to-report randomState
  report list (random no-agent-actions) (random no-agent-states)
end

to-report get-random-patches [n]
  let randomPatches []
  repeat n [
    let randomPatch patch (random-between min-pxcor max-pxcor) (random-between min-pycor max-pycor) ; Adjust ranges as needed
    while [([pcolor] of randomPatch = white)] [
      set randomPatch patch (random-between min-pxcor max-pxcor) (random-between min-pycor max-pycor) ; Adjust ranges as needed
    ]
    set randomPatches lput randomPatch randomPatches
  ]
  report randomPatches
end

to-report random-between [minval maxval]

  report minval + random abs ( minval - maxval)

end

to setup-walls
    ask patches
  [
    set pcolor green
    if pxcor = max-pxcor or pxcor = min-pxcor
    [ set pcolor white ]
    if pycor = max-pycor or pycor = min-pycor
    [set pcolor white]

  ]
end

to setup-sheep
  set shape "sheep"
  set color white
  setxy 0 0
  set heading 0

end

to setup-dogs
  set generation currentGeneration
  set size 1
  set shape "arrow"
  set color black

end

to go

  if ticks < cycleTime
  [
    tick-sheeps
    tick-dogs
    ask turtles [move-to nextPatch]
    tick

  ]

;; once we reach cycleTime ticks we move to the next generation
  if ticks = cycleTime
  [
    ifelse tournamentNumber < tournamentSize[


      let meanX mean [pxcor] of sheeps
      let meanY mean [pycor] of sheeps
;
;
;
;      print "TOURNAMENT Finishing:"
;      print tournamentNumber


      set tournamentResults lput (list (currentChromosome) (checkFitness sheeps meanX meanY )) tournamentResults

;      print tournamentResults

      set currentChromosome item (tournamentNumber) tournamentChromosomes
      ask dogs [
        setup-chromosomes
        move-to item number dogPositions
      ]

      ask sheeps [
        move-to item number sheepPositions
      ]
      reset-ticks
      set tournamentNumber tournamentNumber + 1
    ][
      reset-ticks
      hatchNextGeneration
      set tournamentNumber 0
      set tournamentResults []
      ask dogs [
        setup-chromosomes
        move-to item number dogPositions
      ]

      ask sheeps [
        move-to item number sheepPositions
      ]
    ]

  ]
end


to tick-sheeps
  ask sheeps
  [



    set nextPatch moveSheep

    set prevMove calculate-direction patch-here nextPatch
;    set nextPatch calculate-direction patch-here nextPatch


  ]
end


to tick-dogs
  ask dogs
  [

;    set nextPatch patch-here

    set nextPatch moveDog

;    set nextPatch one-of neighbors4 with [pcolor != white]

    if [pcolor = white] of nextPatch [


      set nextPatch one-of neighbors4 with [pcolor != white]


    ]

  ]
end


to-report moveSheep

  let surrounding-tiles neighbors4 with [pcolor != white] ; Adjust the condition as needed
  let current-tile patch-here
  let no-dogs surrounding-tiles with [count dogs-here = 0]
  let no-sheep surrounding-tiles with [count sheeps-here = 0 and count dogs-here = 0]
  let currentSheep self
  let with-dogs surrounding-tiles with [count dogs-here > 0]

  ; 1. If there is a dog in your current patch, move, if possible, to a patch without a dog;
  if count dogs-here > 0 [
    if count no-dogs > 0 [
      report one-of no-dogs
    ]


  ]
  ; 2. If there is a dog in any of the four adjacent patches (i.e. North, South, East or West of the current one), move,
  ; if possible, to an adjacent patch that does not contain a dog;
  if count with-dogs > 0 and count no-dogs > 0 [
    report  one-of no-dogs
  ]

  ; Move to a patch with no sheep, but which is adjacent to a patch with sheep;


  let options no-sheep with [count neighbors4 with [count sheeps-here > 0 and self != current-tile] > 0]
  if count options > 0[
    report one-of options
  ]



  ; Move to an adjacent patch containing fewer sheep than the current patch;
  set options no-dogs with [(count sheeps-here < (count [sheeps-here] of current-tile))]

  if count options > 0[
    report one-of options
  ]
;   Make a stochastic choice of action as follows: choose the same action as the last one with 50% probability,
;   or choose one of the remaining four actions, each with 12.5% probability. For the first move, assume for all sheep
;   that their previous move was to stay put.

  set options (list 0 1 2 3 4 prevMove prevMove prevMove)

  let newMove one-of options

  let newPositions filter [x -> calculate-direction current-tile x = newMove] sort surrounding-tiles

  if length newPositions > 0 [
    report item 0 newPositions
  ]

  report current-tile


end


to-report mutate-chromosome [c]
  let j 0
  let stateBlock c
  while [j < total-chromosome-length]
  [
    if mutationChance > random-float 1
    [
      set stateBlock replace-item j stateBlock randomState
    ]
  set j ( j + 1 )
  ]
  report stateBlock
end



to-report crossOver [firstChrom secondChrom]
  let tempSet tournamentChromosomes
  print "crossing over"
  print firstChrom
  print secondChrom

  while[0.8 > random-float 1]
  [
    let dogNum (random dogPop)

    ;; pick a random number between 0 and 32 and swap chromosome block at that point
    let slicePoint random (round (chromosome-length / 2))

    let slice1Start sublist firstChrom 0 (dogNum * chromosome-length)
    let slice1 sublist firstChrom (dogNum * chromosome-length) ((dogNum * chromosome-length) + slicePoint )
    let slice1End sublist firstChrom ((dogNum * chromosome-length) + slicePoint ) total-chromosome-length

    let slice2Start sublist secondChrom 0 (dogNum * chromosome-length)
    let slice2 sublist secondChrom (dogNum * chromosome-length) ((dogNum * chromosome-length) + slicePoint )
    let slice2End sublist secondChrom ((dogNum * chromosome-length) + slicePoint ) total-chromosome-length

    print slice2


    set firstChrom (sentence slice1Start slice2 slice1End)
    set secondChrom (sentence slice2Start slice1 slice2End)

    ]
    ;; make a copy of that state block for both agents
    ;; then swap them and make a new list for each agents
  print firstChrom
  report firstChrom
end


to-report join-lists[list1 list2]
  let jMax length list2
  let j 0
    while [j < jMax]
    [
      set list1 lput (item j list2) list1
      set j ( j + 1 )
    ]
  report list1
end

to hatchNextGeneration
  set currentGeneration currentGeneration + 1
  let sortedResults sort-by [[x y] -> item 1 x < item 1 y] tournamentResults
  set score item 1 item 0 sortedResults
  if score < bestScore [
    set bestScore score
    set bestChromosome item 0 item 0 sortedResults
  ]
  set allScores lput score allScores


  set tournamentChromosomes []
  repeat tournamentSize [
    set tournamentChromosomes fput ( crossOver (mutate-chromosome (item 0 item 0 sortedResults)) (item 0 item 1 sortedResults)) tournamentChromosomes
    ;  set tournamentChromosomes fput (  (mutate-chromosome (item 0 item 0 sortedResults))) tournamentChromosomes]

;    let chromosomeNew n-values total-chromosome-length [randomState]
;    set tournamentChromosomes fput chromosomeNew tournamentChromosomes
  ]

;  let sorted-items sort-by [[item1 item2] -> item1 <= item2] (list 4 3 2 1) (list "D" "C" "B" "A")

end

to-report move [i]

    if i = 1 [report moveNorth]
    if i = 2 [report moveEast]
    if i = 3 [report moveSouth]
    if i = 4 [report moveWest]
    if i = 0 [report patch-here]

end

to-report checkIsWhite [i]
  if i = 1 [report ([pcolor] of moveNorth = white)]
  if i = 2 [report ([pcolor] of moveEast = white)]
  if i = 3 [report ([pcolor] of moveSouth = white)]
  if i = 4 [report ([pcolor] of moveWest = white)]
  if i = 0 [report [pcolor] of patch-here = white]

end

to-report moveDog
  let meanX mean [pxcor] of sheeps
  let meanY mean [pycor] of sheeps
  let distanceFromMean ([ distance patch meanX meanY ] of patch-here) / 2

  let angleToMean 0
  ifelse patch-here = patch meanX meanY [
    set sheepCentre 0
    set heading 0
  ][
    set angleToMean towards patch meanX meanY
    set heading angleToMean
    let sheepsInfront sheeps in-cone distanceFromMean 180

    set heading angleToMean + 180
    let sheepsBehind sheeps in-cone 10000 180

    ifelse (count sheepsInfront > 1) and (count sheepsBehind > 1)[
      set color pink
      set sheepCentre 0


      ][ifelse (count sheepsInfront > 1) and (count sheepsBehind = 0)[
        set color yellow
        set sheepCentre 1

        ][ifelse (count sheepsInfront = 0) and (count sheepsBehind > 1)[

        set color blue
        set sheepCentre 2

        ][
        set color black
        set sheepCentre 3
    ]]]
  ]
  set heading 0

  let state item ((sheepCentre * no-agent-states) + currentState) chromosome

  let moveType (item 0 state)
  set currentState (item 1 state)

  if moveType = 0[
    report patch-here
  ]
  if moveType = 1[
    report move calculate-direction patch-here patch meanX meanY
  ]

  let moveDir (calculate-direction patch-here patch meanX meanY) + moveType

  if moveDir > 4 [
    set moveDir moveDir - 4
  ]

;  report move calculate-direction patch-here patch meanX meanY




  report move movedir
end


to-report moveEast

    set heading 90
    report patch (xcor + 1) ycor

end

to-report moveWest

  set heading 270
  report patch (xcor - 1) ycor

end

to-report moveSouth

    set heading 180
    report patch xcor (ycor - 1)

end

to-report moveNorth

  set heading 0
  report patch xcor (ycor + 1)

end

to-report calculate-direction [prevPatch newPatch]
  if prevPatch = newPatch [
    report 0
  ]

  let directions []
  if [pycor] of prevPatch < ([pycor] of newPatch) [
    set directions lput 1 directions
  ]
  if [pxcor] of prevPatch < ([pxcor] of newPatch) [
    set directions lput 2 directions
  ]
  if [pycor] of prevPatch > ([pycor] of newPatch) [
    set directions lput 3 directions
  ]
  if [pxcor] of prevPatch > ([pxcor] of newPatch) [
    set directions lput 4 directions
  ]
  report one-of directions
end


to-report checkFitness [sheepInput meanXIn meanYIn]

  let sheep-set turtles with [member? self sheepInput]

  let x-coordinates [xcor] of sheep-set
  let y-coordinates [ycor] of sheep-set
  let n count sheep-set

  let squared-diff-sum-x sum map [x -> (x - meanXIn) ^ 2] x-coordinates


  let squared-diff-sum-y sum map [y -> (y - meanYIn) ^ 2] y-coordinates


  let total-variance squared-diff-sum-x + squared-diff-sum-y



  if n = 0 [
    report 0
  ]

  report total-variance / n


end
@#$#@#$#@
GRAPHICS-WINDOW
199
10
717
529
-1
-1
10.0
1
10
1
1
1
0
0
0
1
-25
25
-25
25
1
1
1
ticks
30.0

BUTTON
125
31
188
64
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
23
31
89
64
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
14
97
186
130
sheepPop
sheepPop
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
15
167
187
200
dogPop
dogPop
1
50
5.0
1
1
NIL
HORIZONTAL

SLIDER
13
230
185
263
cycleTime
cycleTime
500
20000
4000.0
500
1
NIL
HORIZONTAL

CHOOSER
17
283
155
328
tournamentSize
tournamentSize
3 5 7
0

SLIDER
14
352
186
385
mutationChance
mutationChance
0
0.5
0.03
0.01
1
NIL
HORIZONTAL

PLOT
18
419
218
569
plot 1
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot score"

MONITOR
709
23
836
68
NIL
currentGeneration
17
1
11

BUTTON
757
117
820
150
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>currentGeneration = 20</exitCondition>
    <metric>bestScore</metric>
    <metric>bestChromosome</metric>
    <enumeratedValueSet variable="sheepPop">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournamentSize">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dogPop">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutationChance">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cycleTime">
      <value value="1500"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
