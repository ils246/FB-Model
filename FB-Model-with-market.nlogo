extensions [cf]

globals [
  startup-counter
  changed-job-counter
  counter-parent
  died-counter
  counter-parent-city
  left-city-counter
  died-city-counter
  newcity-counter
  number-of-sectors
  base-demand
]


breed [ pd-s a-pd   ]       ;; pd-s stands for Product Divisions
breed [ firms firm  ]
breed [ cities city ]

turtles-own [ t-index ]     ;; technological signature used to show in to which sectors does a particular pd / firm belong to
firms-own   [ firm-size parent ]
cities-own  [ city-size ]
pd-s-own    [
  parent-firm
  parent-city
  cost-of-operation
  revenue
]


to setup
  ca
  set number-of-sectors 20
  let resources 50000
  set base-demand n-values number-of-sectors [ (random-float 1 * resources) + 5 ]

  create-pd-s initial-pds [                             ;; creates pd-s
    create-t-index
    setup-new-pd
    set cost-of-operation random 10
  ]

  create-firms initial-firms   [                        ;; creates firm
    setup-new-firm
  ]

  create-cities initial-cities [                        ;; creates firms
    setup-new-city
  ]

  ask pd-s [
    create-link-with one-of firms                       ;; pd-s determine their home firm
    set parent-firm  one-of link-neighbors with [ breed = firms ]
    create-link-with one-of cities                      ;; pd-s determine their home city
    set parent-city  one-of link-neighbors with [ breed = cities ]
  ]

  ask firms [
    if count my-links = 0 [die]                         ;; Firms are an aggregation of pd-s, it's impossible to have a firm with no pd-s
    set t-index sector-affiliation
    set firm-size count [my-links] of self
  ]

  ask cities [
    if count my-links = 0 [die]                         ;; Cities are an aggregation of pd-s, it's impossible to have a city with no pd-s
    set t-index sector-affiliation
    set city-size count [my-links] of self
  ]

  ask patch -11 (max-pycor - 1) [ set plabel "Cities"]  ;;labels agents
  ask patch 0   (max-pycor - 1) [ set plabel "PDs"   ]  ;;labels agents
  ask patch 13  (max-pycor - 1) [ set plabel "Firms" ]  ;;labels agents

  update-positions                                      ;; Re-arranges the layout as new agents are created to fit in the world
  reset-ticks
end

to go

  let pds-per-sector count-pds-per-sector
  let demand-of-sectors overall-sector-demand
  let agent-demand individual-demand demand-of-sectors count-pds-per-sector
  let sector-agentset agentset-per-sector

  ask one-of pd-s  [
    hatch-pd-s 1 [
      set parent-city [one-of link-neighbors with [ breed = cities]] of myself
      set parent-firm [one-of link-neighbors with [ breed = firms ]] of myself
      set cost-of-operation random 100
      set t-index t-index-similar-to-parent-firm [t-index] of myself
      find-firm
      find-city
    ]
  ]

  ask firms [
    set firm-size count [my-links] of self              ;; Firms update their size and their t-index
    set t-index sector-affiliation
  ]

  ask cities [
    set city-size count [my-links] of self              ;; Cities update their size and t-index
    set t-index sector-affiliation
  ]

  update-positions                                      ;; Re-arranges the layout as new agents are created to fit in the world
  compute-revenue agent-demand sector-agentset

  ask pd-s   [ if revenue < 0 [die]]
  ask firms  [ if not any? link-neighbors with [ breed = pd-s] [ die ]]   ;; Firms are an aggregation of pd-s, it's impossible to have a firm with no pd-s
  ask cities [ if not any? link-neighbors with [ breed = pd-s] [ die ]]   ;; Cities are an aggregation of pd-s, it's impossible to have a firm with no pd-s
  tick
end


;;; Market procedures

to-report overall-sector-demand                         ;; Computes the demand for a whole sector
  let range-of-variation  (1 - random-float 2)          ;; Range of variation - demand is history dependent
  let demand map [ (1 + range-of-variation ) * ? ] base-demand
  report demand
end

to-report count-pds-per-sector                          ;; counts pd-s in each secyor in order to determine the quantity of demand to be assigned to each one
  let indices n-values number-of-sectors [?]
  let pd-count map [ count pd-s with [ item ? t-index = 1 ]] indices
  report pd-count
end

to-report agentset-per-sector                           ;; builds an thr agentsets of pd-s in each sector
  let indices n-values number-of-sectors [?]
  let my-pds map [ pd-s with [ (item ? t-index) = 1 ]] indices
  report my-pds
end

to-report individual-demand [sector-demands count-pds]                          ;; computes the individual demand that is assigned to each pd depending on the sector
  report (map [ ifelse-value (?2 = 0) [0] [?1 / ?2] ] sector-demands count-pds) ;; takes the overall demand computed in overall-sector demand and divides it by the number of
end                                                                             ;; pd-s in each sector as reported by count-pds-per-sector

to compute-revenue [ my-demand agentset ]                ;; Computes revenue for pd-s according to their sector and cost of operation, if revenue < 0, they die
   (foreach my-demand agentset [ ask ?2 [ set revenue ?1 - cost-of-operation  ]])
end


;;; Firm / City relocation procedures

to-report sector-affiliation                             ;; computes the averageof pd-s t-index to set i-index for firms and cities
  report n-values number-of-sectors [ precision ( mean [item ? t-index] of link-neighbors) 3 ]
end

to find-firm                                             ;; when each pd is hatched it decides to which firm it is tobe affiliated with
  let p-of-home-firm random 100

  cf:when
  cf:case [ p-of-home-firm <= p-of-parent-firm ] [
    create-link-with parent-firm
    set counter-parent counter-parent + 1
  ]

  cf:case [ p-of-home-firm <= p-of-other-firm ] [
    let other-firms [other firms] of parent-firm
    let best-firm-match max-one-of other-firms [firm-size]
    if best-firm-match != nobody [
    create-link-with best-firm-match
    set changed-job-counter changed-job-counter + 1
    ]
  ]

  cf:case [ p-of-home-firm <= p-of-other-firm and tech-relatedness?] [
    let other-firms [other firms] of parent-firm
    let best-firm-match min-one-of other-firms [
      (1 - ( (sum (map [?1 * ?2] t-index ([t-index] of myself))) / product-space-size )) * firm-size]
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
    set t-index sector-affiliation]
  ]

end


to find-city
  let p-of-home-city random 100

  cf:when
  cf:case [ p-of-home-city <= p-for-parent-city ] [
    create-link-with parent-city
    set counter-parent-city counter-parent-city + 1
  ]

 cf:case [ p-of-home-city <= p-for-other-city ] [
    let other-cities [other cities] of parent-city
    let best-city-match  max-one-of other-cities [city-size]
    if best-city-match != nobody [
    create-link-with best-city-match
    set left-city-counter left-city-counter + 1
    ]
  ]

  cf:case [ p-of-home-city <= p-for-other-city and tech-relatedness? ] [
    let other-cities [other cities] of parent-city
    let best-city-match  max-one-of other-cities [
      (1 - ( (sum (map [?1 * ?2] t-index ([t-index] of myself))) / product-space-size )) * city-size]
    if best-city-match != nobody [
    create-link-with best-city-match
    set left-city-counter left-city-counter + 1
    ]
  ]

  cf:else [ hatch-cities 1 [
    let parent-of-city myself
    create-link-with parent-of-city
    set newcity-counter  newcity-counter + 1
    setup-new-city
    set t-index sector-affiliation]
  ]
end

to-report t-index-similar-to-parent-firm [ parent-t-index ]
  let first-change replace-item (random length (parent-t-index)) parent-t-index 1
  report replace-item (random length (first-change)) first-change 0
end

to-report pd-city
  report one-of link-neighbors with [ breed = cities ]
end

to-report pd-firm
  report one-of link-neighbors with [ breed = firms ]
end


;;;  New agent procedures

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

to create-t-index                              ;; Creates technological signature to keep track of which sector agents belong to
  set t-index n-values number-of-sectors [0]
  repeat (random 2 + 1) [set t-index replace-item random number-of-sectors t-index 1]
end

;;; Layout procedures

to arrange-in-column [ agentset x ]
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


to-report diversity
    report length remove-duplicates [t-index] of pd-s
end
@#$#@#$#@
GRAPHICS-WINDOW
282
11
527
638
16
-1
5.91
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
4
60
176
93
initial-cities
initial-cities
0
60
8
1
1
NIL
HORIZONTAL

SLIDER
4
101
176
134
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
4
144
176
177
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
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "if ticks mod 20 = 0 [\n  clear-plot\n  (foreach (n-values count firms [ 1 + ? ]) (reverse sort [ firm-size ] of firms) [\n    plotxy (log ?1 10) (log ?2 10)\n  ])\n    \n]"

SLIDER
4
186
176
219
p-of-parent-firm
p-of-parent-firm
1
100
60
1
1
%
HORIZONTAL

SLIDER
4
228
176
261
p-of-other-firm
p-of-other-firm
p-of-parent-firm + 1
100
90
1
1
%
HORIZONTAL

SLIDER
5
270
179
303
p-for-parent-city
p-for-parent-city
1
100
60
1
1
%
HORIZONTAL

SLIDER
6
313
178
346
p-for-other-city
p-for-other-city
p-for-parent-city + 1
100
90
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
883
259
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
8000
100
50
1
NIL
HORIZONTAL

PLOT
506
263
882
471
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
2.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "if ticks mod 50 = 0 [\n \n clear-plot\n  (foreach (n-values count firms [ 1 + ? ]) (reverse sort [ firm-size ] of firms) [\n    plotxy (log ?1 10) (log ?2 10)\n  ])\n  ]"

PLOT
906
63
1239
213
PD count
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
"default" 1.0 0 -16777216 true "" "plot count pd-s"

PLOT
884
215
1541
577
Machine Sector
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
"default" 1.0 0 -5298144 true "" "plot count pd-s with [ item 15 t-index = 1 ]"
"pen-1" 1.0 0 -1872023 true "" "plot count pd-s with [ item 0 t-index = 1 ]"
"pen-2" 1.0 0 -2269166 true "" "plot count pd-s with [ item 1 t-index = 1 ]"
"pen-3" 1.0 0 -6459832 true "" "plot count pd-s with [ item 2 t-index = 1 ]"
"pen-4" 1.0 0 -2500309 true "" "plot count pd-s with [ item 3 t-index = 1 ]"
"pen-5" 1.0 0 -12616150 true "" "plot count pd-s with [ item 4 t-index = 1 ]"
"pen-6" 1.0 0 -6565750 true "" "plot count pd-s with [ item 5 t-index = 1 ]"
"pen-7" 1.0 0 -11816013 true "" "plot count pd-s with [ item 6 t-index = 1 ]"
"pen-8" 1.0 0 -6759204 true "" "plot count pd-s with [ item 7 t-index = 1 ]"
"pen-9" 1.0 0 -14122834 true "" "plot count pd-s with [ item 8 t-index = 1 ]"
"pen-10" 1.0 0 -8275240 true "" "plot count pd-s with [ item 9 t-index = 1 ]"
"pen-11" 1.0 0 -13740902 true "" "plot count pd-s with [ item 10 t-index = 1 ]"

SWITCH
1333
111
1500
144
tech-relatedness?
tech-relatedness?
1
1
-1000

@#$#@#$#@
## WHAT IS IT?
This is a model of exogenous economic growth. It was based on the model put forth by Frenken and Boschman (2007).

## HOW IT WORKS

The model has three kinds of agents, namely: product divisions, firms and cities.

Product divisions can belong to one firm and one city, but firms and and cities can be made up of several different product divisions.

At each tick, one product division will produce a new product division which will have the choice to etiher stay at its parent firm, move to a different existing firm or create a new firm. Similarly, the new product division will have to make a choice in regards to which city they want to be in. In the original version, pd-s chose a firm or city only interms of its size. In this version, the choice incorporates technological relatedness. This last criteria can be turned on or off with the switch on the interface.

Also in this version, a market mechanism was put in place in order to enable product division interaction and agent death.



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

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

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
<experiments>
  <experiment name="City/Firm Size Distribution" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>[firm-size] of firms</metric>
    <metric>max firm-size</metric>
    <metric>min firm-size</metric>
    <metric>[city-size] of cities</metric>
    <metric>max city-size</metric>
    <metric>min city-size</metric>
    <enumeratedValueSet variable="initial-cities">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tech-relatedness?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-pds">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-of-other-firm">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-for-parent-city">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-firms">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-for-other-city">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-of-parent-firm">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Product-space-size">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Diversity" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>diversity</metric>
    <metric>count-pds-per-sector</metric>
    <metric>max [city-size] of cities</metric>
    <metric>min [city-size] of cities</metric>
    <metric>max [firm-size] of firms</metric>
    <metric>min [firm-size] of firms</metric>
    <enumeratedValueSet variable="initial-cities">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tech-relatedness?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-pds">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-of-other-firm">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-for-parent-city">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-firms">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-for-other-city">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-of-parent-firm">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Product-space-size">
      <value value="100"/>
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
