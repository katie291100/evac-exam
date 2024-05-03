;
;adapted from Jonathan Levinkind, 2018
;
breed [animals animal]
breed [sheeps sheep]
globals
[
  currentGeneration
  averageFitness
  lineList
  highest-avg-fitness
  highest-individual-score
  no-agent-actions
  no-agent-states
  chromosome-length
  count-sheep-all
  count-notsheep
]

animals-own
[
  chromosome ;; this is a list of 4 lists each containing list-pairs of:
             ;; one of 4 actions (encoded from 0 to 3)
             ;; one of 16 states to change to (encoded from 0 to 15)
             ;; In total a chromosome contains 128 numbers to encode agent behaviour
  currentState ;; what is the current state they are in
  points ;; how much sheep have they eaten this cycle
  generation ;; what generation they are from
  notpoints
]


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  reset-ticks
  set currentGeneration 0

  ;;these are defined by the design (agents can move/rotate, and can encounter 4 types of space so 16 states. 4 * 16 * 2 -> 128
  set no-agent-actions 4
  set no-agent-states 16
  set chromosome-length (no-agent-actions * no-agent-states)

  set lineList (list (random min-pycor) (random min-pycor) (random max-pycor) (random max-pycor)) ;; used to create rows sheep placement
  if walls-on [setup-walls]
  create-sheeps sheepPop [setup-sheep]
  create-animals population [setup-animals]


end

to setup-walls
    ask patches
  [ if pxcor = max-pxcor or pxcor = min-pxcor
    [ set pcolor white ]
    if pycor = max-pycor or pycor = min-pycor
    [set pcolor white]
  ]
end

to setup-sheep
  set shape "sheep"
  set color white
  setxy 0 0
  set heading random 360

  setxy random-pxcor random-pycor
  while [any? other turtles-here or pcolor = white]
  [
    set heading random 360
    fd 1
  ]

end

to setup-animals
  set generation currentGeneration
  set size 1
  set color yellow
  set heading 90 * (random 4)
  set points 0
  set notpoints 0

  while [any? other turtles-here or pcolor = white]
  [
    setxy random-pxcor random-pycor
  ]

  ;; when a new agent is created during the tournament phase it will have a chance to mutate chromosome data
  ifelse generation = 0
  [
    setup-chromosomes
  ]
  [
    mutate-chromosomes
  ]
end

to setup-chromosomes
  let statesBlank n-values chromosome-length [randomState]

  set chromosome statesBlank

  set currentState random no-agent-states
end

to go
  if ticks < cycleTime
  [
    tick-animals
    tick-sheeps
    tick
  ]
;; once we reach cycleTime ticks we move to the next generation
  if ticks = cycleTime
  [
    endOfCycle
    set lineList (list (random min-pycor) (random min-pycor) (random max-pycor) (random max-pycor))
    reset-ticks
  ]
end

;;;;;;;;;;;;;;;;;;;;;
;;; GA procedures ;;;
;;;;;;;;;;;;;;;;;;;;;

to endOfCycle
  let sumFitness 0
  ask animals [set sumFitness (sumFitness + points)]
  set averageFitness (sumFitness / population)
  print "Average Fitness: "
  print averageFitness
  if averageFitness > highest-avg-fitness [set highest-avg-fitness averageFitness]
  hatchNextGeneration
end

to hatchNextGeneration

  if currentGeneration = 10 or currentGeneration = 50 [
    let sheepall-count 0
    let notsheep-count 0
    let countanimals 0
    ask animals [
      set sheepall-count (sheepall-count + points)
      set notsheep-count (notsheep-count + notpoints)
      set countanimals (countanimals + 1)
    ]
    print "The values for the generation "
    print (currentGeneration)
    print sheepall-count
    print notsheep-count
    print countanimals
  ]


  let tempSet (animals with [generation = currentGeneration])

  set currentGeneration (currentGeneration + 1)


  ask tempSet
  [
    if points > highest-individual-score [set highest-individual-score points]
    if averageFitness = 0 [set points 1]
  ]

  if averageFitness = 0 [ set averageFitness 1]

  while[ count animals < (population * 2)]
  [
    ask tempSet
    [
      if count animals < (population * 2)
      [

        if (points / averageFitness) > random-float 1
        [
          hatch-animals 1 [setup-animals]
        ]
      ]
    ]
  ]
  ask tempSet [die]
  ask sheeps [die]
  create-sheeps (round (world-width * world-height * 0.04)) [setup-sheep]

  crossOver
end

to mutate-chromosomes

 ;; set chromosome (map (ifelse (mutationChance > random 1) [?][? -> randomState]) chromosome)

 ;; conditional mappings don't work in netlogo 6.x for some reason
 ;; having to use a while loop to replicate mapping

  set chromosome mutate-chromosome

end

to-report mutate-chromosome
  let j 0
  let stateBlock chromosome
    while [j < (no-agent-actions * no-agent-states)]
    [
      if mutationChance > random-float 1
      [
        set stateBlock replace-item j stateBlock randomState
      ]
    set j ( j + 1 )
    ]
  report stateBlock
end


to crossOver
  let tempSet (animals with [generation = currentGeneration])

  while[0.8 > random-float 1]
  [
    let newSet (n-of 2 tempSet)
    ;; pick a random number between 0 and 32 and swap chromosome block at that point
    let slicePoint random (round (chromosome-length / 2))

    let agent1 one-of newSet

    ask agent1
    [
      let agent2 other newSet
      let slice sublist chromosome 0 slicePoint
      let slice1 sublist chromosome slicePoint (chromosome-length)

      ask agent2
      [
        let slice2 sublist chromosome 0 slicePoint
        let slice3 sublist chromosome slicePoint (chromosome-length)
        set chromosome join-lists slice2 slice1
        ask agent1
        [
          set chromosome join-lists slice slice3
        ]
      ]

    ]
    ;; make a copy of that state block for both agents
    ;; then swap them and make a new list for each agents
  ]
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


to-report randomState
  report list (random no-agent-actions) (random no-agent-states)
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Agent procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to tick-sheeps
  ask sheeps
  [
  fd 1
    let ahead look-here
  bk 1


  ]
end

to tick-animals
  ask animals
  [
    action
  ]
end

to action

  ;; what is infront of you?
  fd 1
  let ahead look-here
  bk 1

  ;;pair-value for what is infront of you and what state you are in
  ;; gets correct point in the chromosome list
  let state item ((ahead * no-agent-states) + currentState) chromosome

  let move (item 0 state)
  let newState (item 1 state)

  ifelse move = 0 [ moveFwd ]
[ ifelse move = 1 [ moveBkwd ]
[ ifelse move = 2 [ rotateLeft ]
[ ifelse move = 3 [ rotateRight]
[ ;; default case
  ]]]]

  set currentState newState
end

to-report look-here

  ifelse count animals-here > 0 [report 1]
  [ ifelse count sheeps-here > 1 [report 2]
  [ ifelse pcolor = white [report 3]
  [ ifelse pcolor = black [report 0]
  [ ;; default case
  ]]]]
 end

to moveFwd
  fd 1
  checkForsheep
  if count animals-here > 1 [bk 1]
  if pcolor = white [bk 1]
end

to rotateLeft
  lt 90
end

to rotateRight
  rt 90
end

to moveBkwd
  bk 1
  checkForsheep
  if count animals-here > 1 [fd 1]
  if pcolor = white [fd 1]
end

to checkForsheep
  ifelse count sheeps-here > 0
  [
    set points (points + 1)
    ask sheeps-here [die]
    hatch-sheeps 1 [setup-sheep]
    set count-sheep-all (count-sheep-all + 1)

  ]
  [
    set notpoints (notpoints + 1)
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
491
292
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-10
10
-10
10
0
0
1
ticks
30.0

SLIDER
11
271
183
304
population
population
2
50
4.0
1
1
NIL
HORIZONTAL

BUTTON
4
43
67
76
NIL
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

BUTTON
71
43
134
76
NIL
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

CHOOSER
10
146
148
191
foodSpawnPattern
foodSpawnPattern
"random" "circular" "rows"
0

MONITOR
654
12
728
57
Generation
currentGeneration
17
1
11

SLIDER
11
196
183
229
cycleTime
cycleTime
1000
10000
4000.0
1000
1
NIL
HORIZONTAL

SLIDER
11
233
183
266
mutationChance
mutationChance
0
0.25
0.02
0.01
1
NIL
HORIZONTAL

MONITOR
654
62
802
107
Highest Average Fitness
highest-avg-fitness
17
1
11

MONITOR
655
113
809
158
Highest Individual Score
highest-individual-score
17
1
11

SWITCH
12
312
115
345
walls-on
walls-on
0
1
-1000

SLIDER
13
357
185
390
sheepPop
sheepPop
0
100
11.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

In this project, agents search for food. Over generations agents adapt to improve at this task using a simple set of behaviours and states encoded into a list of 128 numbers.

## HOW IT WORKS

The project is an implementation of a genetic algorithm. It is based off the project by David Eck found here:

http://math.hws.edu/eck/jsdemo/jsGeneticAlgorithm.html

His explaination is also worth reading to understand the project which is here:

http://math.hws.edu/eck/jsdemo/ga-info.html

## HOW TO USE IT

To run a simulation use the setup button, then the go button. The go button can be pressed again to pause or unpause the simulation.

There are a few variables that can be altered from the interface panel:

1. foodSpawnPattern allows you to choose from three possible variations of food locations on the map

2. cycleTime is the the number of ticks a generation has to run before the tournamnet of GA takes place. Too short or too long a cycleTime might result in poor results as highly random agents could benefit from opportune coniditons to succeed

3. mutatuionChance is the probability a chromosome pair will mutate in the mutation phase of the genetic algorithm. A high mutation chance (even above 10%) can lead to poor results.

4. Population is the number of agents spawned for a cycle.

5. wallsOn sets whether a wall will be placed around the perimiter of the map preventing agents from freely moving around the looping map. Additionally, by having walls the encoding of behvaiours related to walls will be utilised.


## EXTENDING THE MODEL

To increase performance of agents, extending the behaviou-space r of agents would be ideal. As of now, agents can only see ahead of themselves and only move in four directions. Increasing agents awareness of the environment or increasing the action-set of agents would allow for more complex behaviour. In order to do this the chormosome of the agent would have to be increased in size (at an exponential rate).
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
