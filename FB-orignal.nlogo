extensions [ cf ]

globals [
  startup-counter
  changed-job-counter
  counter-parent
  died-counter
  counter-parent-city
  left-city-counter
  died-city-counter
  newcity-counter
]

breed [ pd-s a-pd  ]       ;; pd-s stands for Product Divisions
breed [ firms firm ]
breed [ cities city ]

turtles-own [ t-index ]
firms-own   [ firm-size parent ]
cities-own  [ city-size ]
pd-s-own    [
  parent-firm
  parent-city
]


to setup
  ca

  create-pd-s initial-pds [            ;; creates pd-s
    setup-new-pd
    set t-index random product-space-size
  ]

  create-firms initial-firms [         ;; creates firms
    setup-new-firm
  ]

  create-cities initial-cities [       ;; creates firms
   setup-new-city
  ]

  ask pd-s [
    create-link-with one-of firms      ;; pd-s determine their home firm
    set parent-firm  one-of link-neighbors with [ breed = firms]
    create-link-with one-of cities     ;; pd-s determine their home city
    set parent-city  one-of link-neighbors with [ breed = cities]
  ]

  ask firms [
    if count my-links = 0 [die]        ;; firms are an aggregation of pd-s, it's impossible to have a firm with no pd-s
    set-t-index
    set firm-size count [my-links] of self
  ]

  ask cities [
    if count my-links = 0 [die]        ;; cities are an aggregation of pd-s, it's impossible to have a city with no pd-s
    set-t-index
    set city-size count [my-links] of self
  ]

  ask patch -15 100 [ set plabel "Cities"]
  ask patch 0 100   [ set plabel "PDs"]
  ask patch 15 100  [ set plabel "Firms"]

update-positions
reset-ticks
end

to go

 ask one-of pd-s [

    hatch-pd-s 1 [

      set parent-city [one-of link-neighbors with [ breed = cities]] of myself
      set parent-firm [one-of link-neighbors with [ breed = firms]] of myself
      set t-index ((random-normal [t-index] of parent-firm p-of-radical-innovation) mod product-space-size) ;; I would have to change it here too

      find-firm
      find-city

    ]

   ask firms [                              ;; Firms update their link count - size
     set firm-size count [my-links] of self
     set-t-index
   ]

   ask cities [                             ;; Cities update their link count - size
     set city-size count [my-links] of self
     set-t-index
   ]

 ]
 update-positions
 if ticks >= 2000 [stop]
 tick
end


to-report pd-city
  report one-of link-neighbors with [ breed = cities]
end


to-report pd-firm
  report one-of link-neighbors with [ breed = firms]
end

to setup-new-pd
  set xcor 0
  set size 0.8
  set shape "circle"
end

to setup-new-firm
  set xcor 15
  set size 0.8
  set shape "circle"
  set color [ 120 30 235 ]
end


to setup-new-city
  set xcor -15
  set size 0.8
  set shape "circle"
  set color [ 55 200 50 ]

end

to find-firm
  let p-of-home-firm random 100
  let b ( 1 - (distance-to-best-match / 250))
  let c (p-of-home-firm + b) ;; some will be > than 1 but do I have to normalize it if p-of-parent-city will be max 1 or 0.9

  cf:when
  cf:case [ c <= p-of-parent-firm ] [
    create-link-with parent-firm
    set counter-parent counter-parent + 1
  ]

  cf:case [ p-of-home-firm <= p-of-other-firm ] [
    let other-firms [other firms] of parent-firm
    let best-firm-match max-one-of other-firms [(ifelse-value (tech-relatedness?) [distance-to-best-match] [1]) * firm-size]
    if best-firm-match != nobody [
    create-link-with best-firm-match
    set changed-job-counter changed-job-counter + 1
    ]
  ]

  cf:else [ hatch-firms 1 [
    set parent myself
    let parent-of-firm myself
    create-link-with parent-of-firm
    set startup-counter startup-counter + 1
    setup-new-firm
    set-t-index
    ]
  ]
end


to find-city

  let p-of-home-city random 100
  let p ( 1 - (distance-to-best-match / 250))
  let a (p-of-home-city + p) ;; some will be > than 1 but do I have to normalize it if p-of-parent-city will be max 1 or 0.9

  cf:when
  cf:case [ a <= p-of-parent-city ] [
    create-link-with parent-city
    set counter-parent-city counter-parent-city + 1
  ]

  cf:case [ p-of-home-city <= p-of-other-city ] [
    let other-cities [other cities] of parent-city
    let best-city-match max-one-of other-cities [(ifelse-value (tech-relatedness?) [distance-to-best-match] [1]) * city-size] ;min-one-of other-cities [(1 - (distance-to-best-match / p-of-radical-innovation )) * city-size] ;
    if best-city-match != nobody [
    create-link-with best-city-match
    set left-city-counter left-city-counter + 1
    ]
  ]

  cf:else [ hatch-cities 1 [
    let parent-of-city myself
    create-link-with parent-of-city
    set newcity-counter newcity-counter + 1
    setup-new-city
    set-t-index]
  ]

end

to set-t-index
  set t-index mean [t-index] of link-neighbors with [ breed = pd-s]
end

to arrange-in-column [ agentset x ] ; give argument agent set firms and x is the xcor
  ask agentset [ setxy x max-pycor - 2]
  foreach but-first sort agentset [
    ask ? [
      set ycor min [ ycor ] of agentset - ( world-height - 3) / count agentset ]
  ]
end


to update-positions
  arrange-in-column pd-s  0
  arrange-in-column firms  15
  arrange-in-column cities  -15
end

;Jason and me - Aug.24.16
; to compute distance-to-best-match we subtract my t-index from every other firms' t-index not including my parent firm. I want to minimize the technological distance, therefore I take the absolute value, however,
; because t-index is one value and it needs to wrap around so the distance between the min and the max has to be the same as max and max - 1,
; we try all three of the following cases:
;  * add 0 (which will be optimal when the two numbers are already near one another)
;  * add -p (which will be optimal when the comparer's t-index is low and the comparee's t-index is high)
;  * add p (which will be optimal when the comparer's t-index is high and the comparee's t-index is low)



to-report distance-to-best-match
 report min map [ abs (t-index - ([t-index] of myself + ? )) ]  (list 0 (- product-space-size) product-space-size)






 ;;1 - distance-to-best-match + p  or 1 - distance-to-best-match * p6
end
@#$#@#$#@
GRAPHICS-WINDOW
255
10
500
549
16
-1
5.03030303030303
1
10
1
1
1
0
0
0
1
-16
16
0
100
1
1
1
ticks
30.0

BUTTON
107
21
173
54
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

SLIDER
48
94
220
127
initial-cities
initial-cities
0
60
10
1
1
NIL
HORIZONTAL

SLIDER
48
135
220
168
initial-pds
initial-pds
0
60
10
1
1
NIL
HORIZONTAL

SLIDER
48
178
220
211
initial-firms
initial-firms
0
100
5
1
1
NIL
HORIZONTAL

PLOT
505
582
1009
792
Firm Size Distribution
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
"default" 1.0 1 -16777216 true "histogram [firm-size] of firms\nset-plot-x-range 1 15\nset-histogram-num-bars 20" "histogram [firm-size] of firms"

SLIDER
49
260
221
293
p-of-parent-firm
p-of-parent-firm
1
100
50
1
1
%
HORIZONTAL

SLIDER
49
302
221
335
p-of-other-firm
p-of-other-firm
p-of-parent-firm
100
83
1
1
%
HORIZONTAL

SLIDER
50
344
222
377
p-of-parent-city
p-of-parent-city
1
100
50
1
1
%
HORIZONTAL

SLIDER
51
387
223
420
p-of-other-city
p-of-other-city
p-of-parent-city
100
80
1
1
%
HORIZONTAL

BUTTON
35
20
98
53
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

PLOT
506
52
1009
302
PDs choice in Firm
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Parent-firm" 1.0 0 -16777216 true "" "plot counter-parent"
"changed-job" 1.0 0 -7500403 true "" "plot changed-job-counter"
"startup" 1.0 0 -2674135 true "" "plot startup-counter"

SLIDER
505
10
854
43
Product-space-size
Product-space-size
0
500
500
5
1
NIL
HORIZONTAL

SLIDER
39
495
241
528
p-of-radical-innovation
p-of-radical-innovation
10
500
80
1
1
NIL
HORIZONTAL

PLOT
505
311
1009
569
PDs Choice in City
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"parent-city" 1.0 0 -16777216 true "" "plot counter-parent-city"
"dif-city" 1.0 0 -7500403 true "" "plot left-city-counter"
"New" 1.0 0 -955883 true "" "plot newcity-counter"

PLOT
505
799
1010
1018
City Size Distribution
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
"default" 1.0 1 -16777216 true "histogram [city-size] of cities\nset-plot-x-range 1 15\nset-histogram-num-bars 20" "histogram [city-size] of cities"

PLOT
5
619
499
894
plot 1
NIL
NIL
0.0
2.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "if ticks mod 50 = 0 [\n  clear-plot\n  (foreach (n-values count firms [ 1 + ? ]) (reverse sort [ firm-size ] of firms) [\n    plotxy (log ?1 10) (log ?2 10)\n  ])\n]"

SWITCH
1136
144
1303
177
tech-relatedness?
tech-relatedness?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

This model reproduces exogenous growth model by Frenken and Boschma (2007) in the paper "Notes on a complexity theory of economic development".

## HOW IT WORKS


The model has three kinds of agents, namely: product divisions, firms and cities.

Product divisions can belong to one firm and one city, but firms and and cities can be made up of several different product divisions.

At each tick, one product division will produce a new product division which will have the choice to etiher stay at its parent firm, move to a different existing firm or create a new firm. Similarly, the new product division will have to make a choice in regards to which city they want to be in. In the original version, pd-s chose a firm or city only interms of its size. In this version, the choice incorporates technological relatedness. This last criteria can be turned on or off with the switch on the interface.


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
NetLogo 6.0-M6
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
