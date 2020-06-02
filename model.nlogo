globals [
  SS-contacts
  SE-contacts
  SI-contacts
  SR-contacts
  EE-contacts
  EI-contacts
  ER-contacts
  II-contacts
  IR-contacts
  RR-contacts

  first-lockdown?
  already-locked?
]

breed [susceptibles susceptible]  ;; can be infected
breed [exposeds exposed]          ;; infectious but asymptomatic
breed [infecteds infected]        ;; infectious and symptomatic
breed [removeds removed]          ;; recovered and immune
breed [deads dead]                ;; removed from population

turtles-own [
  z-contact                ;; radius of contact neighbourhood
  age                      ;; age range of person (0-29, 30-59, 60+)
]

susceptibles-own [
  to-become-exposed?       ;; whether an S will turn into E
  p-infect                 ;; probability of being infected by contact
]

exposeds-own [
  to-become-infected?      ;; whether an E will turn into I
  inc-countdown            ;; time until E develops symptoms
]

infecteds-own [
  removal-or-death?        ;; whether an I will turn into R or D
  rec-countdown            ;; time until I becomes R or D
]

removeds-own [
  to-become-susceptible?   ;; whether an R will turn into S
  imm-countdown            ;; time until R returns to S
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  setup-globals
  setup-turtles
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; SETUP PROCEDURES ;;;;;;;;;;;;;;;;;

to setup-globals ;; no contacts to begin with
  set SS-contacts 0
  set SE-contacts 0
  set SI-contacts 0
  set SR-contacts 0
  set EE-contacts 0
  set EI-contacts 0
  set ER-contacts 0
  set II-contacts 0
  set IR-contacts 0
  set RR-contacts 0

  set first-lockdown? false
  set already-locked? false
end

to setup-turtles
  set-default-shape turtles "person"
  ask patches
     [
       set pcolor white
       sprout-susceptibles 1 [                  ;; place a susceptible on each patch
          set color green
          set to-become-exposed? false
          set p-infect p-infect-init
          set z-contact z-contact-init
          set-age
        ]
     ]
   ask turtles-on (n-of initial-inf patches)    ;; infect n random susceptibles
     [
      set breed infecteds
      set color red
      set rec-countdown round (normal-dist recovery-mean recovery-stdev)
      set removal-or-death? false
      set z-contact z-contact-init
     ]
end

to set-age
  let p (random 100) + 1
  if (p <= 40) [set age "30-59"]                ;; 40% (30-59)
  if (p > 40 and p <= 77) [set age "0-29"]      ;; 37% (0-29)
  if (p > 77) [set age "60+"]                   ;; 23% (60+)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  ifelse (ticks < (duration * 365))
;  and ((count infecteds + count exposeds) > 0)  ;; uncomment to stop simulation when virus stops circulating
  [
    count-contacts       ;; update the number of contacts made before with the ones made at this step after lockdown was modified
    expose-susceptibles  ;; turn susceptibles into exposeds if they had contact with an infected or exposed with probability p-infect
    infect-exposeds      ;; turn exposeds into infecteds after inc-countdown ticks
    remove-infecteds     ;; turn infecteds into removeds or deads after rec-countdown ticks, with p-death probability of becoming deads instead of removeds
    lose-immunity        ;; turn removeds back into susceptibles after imm-countdown ticks
    update-breeds        ;; update conditions as necessary
    modify-lockdown      ;; modify the lockdown depending on the new number of infecteds
    tick                 ;; go to next day
  ][
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; GO PROCEDURES ;;;;;;;;;;;;;;;;;;;

to count-contacts
  let SS-tick 0
  let SE-tick 0
  let SI-tick 0
  let SR-tick 0
  let EE-tick 0
  let EI-tick 0
  let ER-tick 0
  let II-tick 0
  let IR-tick 0
  let RR-tick 0

  ask susceptibles [
    set SS-tick (SS-tick + ((count other susceptibles in-radius z-contact with [z-contact >= distance myself])))
    set SE-tick (SE-tick + ((count exposeds in-radius z-contact)))
    set SI-tick (SI-tick + ((count infecteds in-radius z-contact)))
    set SR-tick (SR-tick + ((count removeds in-radius z-contact)))

  ]
  set SS-tick (SS-tick / 2)

  ask exposeds [
    set EE-tick (EE-tick + ((count other exposeds in-radius z-contact with [z-contact >= distance myself])))
    set EI-tick (EI-tick + ((count infecteds in-radius z-contact-init)))
    set ER-tick (ER-tick + ((count removeds in-radius z-contact-init)))
  ]
  set EE-tick (EE-tick / 2)

  ask infecteds [
    set II-tick (II-tick + (count other infecteds in-radius z-contact with [z-contact >= distance myself]))
    set IR-tick (IR-tick + (count removeds in-radius z-contact-init))
  ]
  set II-tick (II-tick / 2)

  ask removeds [
    set RR-tick (RR-tick + (count other removeds in-radius z-contact with [z-contact >= distance myself]))
  ]
  set RR-tick (RR-tick / 2)

  set-current-plot "num-contacts"
  plot (SS-tick + SE-tick + SI-tick + SR-tick +
        EE-tick + EI-tick + ER-tick +
        II-tick + IR-tick +
        RR-tick)

  set SS-contacts (SS-contacts + SS-tick)
  set SE-contacts (SE-contacts + SE-tick)
  set SI-contacts (SI-contacts + SI-tick)
  set SR-contacts (SR-contacts + SR-tick)
  set EE-contacts (EE-contacts + EE-tick)
  set EI-contacts (EI-contacts + EI-tick)
  set ER-contacts (ER-contacts + ER-tick)
  set II-contacts (II-contacts + II-tick)
  set IR-contacts (IR-contacts + IR-tick)
  set RR-contacts (RR-contacts + RR-tick)
end

to expose-susceptibles
   ask susceptibles [

     let infected-contacts (                                                           ;; number of infected contacts is
      (count infecteds in-radius z-contact with [z-contact >= distance myself]) +      ;; the number of actual infecteds plus
      (count exposeds in-radius z-contact with [z-contact >= distance myself])         ;; that of exposeds in the susceptible's z-radius (if the susceptible is in their radius)
    )

    if modify-p-infect? and first-lockdown? [                                          ;; if a lockdown has occurred and the option is on
      set p-infect (1 - protection-strength / 100) * (p-infect-init / 100)             ;; p-infect is reduced depending on the protection strength (e.g. how many people use masks)
    ]

     let infection-prob 1 - ((1 - p-infect) ^ infected-contacts)                       ;; probability of at least one of these contacts causing infection is
                                                                                       ;; 1 - the probability that none of them cause infection
     let p (random 100 + 1)
     if (p <= infection-prob * 100) [
      set to-become-exposed? true
    ]
 ]

  check-travel
end

to check-travel
  if not closed-system? and (count infecteds) < lockdown-threshold [  ;; if people can travel and lockdown is not active
    let p (random 100 + 1)                                            ;; one person gets randomly infected per tick depending on travel strictness
    if p >= (travel-strictness) [                                     ;; 1% chance if 100% strictness, 100% chance if 0% strictness
      ask susceptibles-on (n-of 1 patches) [
        set breed infecteds
        set color red
        set rec-countdown round (normal-dist recovery-mean recovery-stdev)
        set removal-or-death? false
        set z-contact z-contact-init
      ]
    ]
  ]
end

to infect-exposeds
  ask exposeds [
    ifelse inc-countdown = 0
    [set to-become-infected? true]
    [set inc-countdown (inc-countdown - 1)]
  ]
end

to remove-infecteds
  ask infecteds [
    ifelse rec-countdown = 0
      [set removal-or-death? true]
      [set rec-countdown (rec-countdown - 1)]
  ]
 end

to lose-immunity
  ask removeds [
    ifelse imm-countdown = 0
    [set to-become-susceptible? true]
    [set imm-countdown (imm-countdown - 1)]
  ]
end

to update-breeds

  ask susceptibles with [to-become-exposed? = true] [
    set breed exposeds
    set color yellow
    set inc-countdown (log-normal incubation-mean incubation-stdev)
    set to-become-infected? false
    ifelse z-contact = 0
    [set shape "person-outline"]
    [set shape "person"]
  ]

  ask exposeds with [to-become-infected? = true] [
    set breed infecteds
    set color red
    set rec-countdown (normal-dist recovery-mean recovery-stdev)
    set removal-or-death? false
    ifelse z-contact = 0
    [set shape "person-outline"]
    [set shape "person"]
  ]

  ask infecteds with [removal-or-death? = true] [
    let p (random 100 + 1)
    let p-death-here (actual-p-death age)
    ifelse (p <= p-death-here)
    [set breed deads
      set color black]
    [set breed removeds
      set color 8
      set imm-countdown (poisson-dist immunity-mean)
      set to-become-susceptible? false
      ifelse z-contact = 0
      [set shape "person-outline"]
      [set shape "person"]
    ]
  ]

  ask removeds with [to-become-susceptible? = true] [
    set breed susceptibles
    set color green
    set to-become-exposed? false
    set p-infect p-infect-init
    ifelse z-contact = 0
    [set shape "person-outline"]
    [set shape "person"]
  ]
end

to modify-lockdown
  if imposed-lockdown? [
    ifelse (count infecteds) > lockdown-threshold
    [start-lockdown]
    [end-lockdown]
  ]
end

to start-lockdown
  let alives turtles with [not member? self deads]
  if not already-locked? [
    ask alives [
      let p (random 100 + 1)
      if p <= (lockdown-strictness)
      [
        set z-contact 0
        set shape "person-outline"
      ]
      set already-locked? true
    ]
  ]
end

to end-lockdown
  let alives turtles with [not member? self deads]
  if already-locked? [
    ask alives [
      set z-contact z-contact-init
      set shape "person"
    ]
    set already-locked? false
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; UTILITIES ;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; REPORTERS ;;;;;;;;;;;;;;;;;;;;;

to-report log-normal [mu sigma]
;  let z (random-normal mu sigma) ;; this was for the original formula I thought was correct
;  let x (exp (mu + (sigma * z))) ;; but only works if mean and stdev are of the normal dist
  report round (exp random-normal mu sigma)
end

to-report normal-dist [mu sigma]
  let x round (random-normal mu sigma)
  ifelse x > 0                    ;; bad hack to prevent negative days, may need a different dist
  [report x]
  [report (x * -1)]
end

to-report poisson-dist [mu]
  report round (random-poisson mu)
end

to-report actual-p-death [#age]
  let p 0
  if #age = "0-29" [
    set p (p-death * 0.6) / (100 - p-death + (p-death * 0.6))
  ]
  if #age = "30-59" [
    set p p-death
  ]
  if #age = "60+" [
    set p (p-death * 5.1) / ((100 - p-death + p-death * 5.1))
  ]
  report p
end
@#$#@#$#@
GRAPHICS-WINDOW
513
34
1121
643
-1
-1
12.0
1
10
1
1
1
0
0
0
1
0
49
0
49
1
1
1
ticks
30.0

BUTTON
1162
307
1232
342
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
1252
305
1323
341
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

SLIDER
14
40
228
73
p-infect-init
p-infect-init
1
100
30.0
1
1
%
HORIZONTAL

SLIDER
1200
433
1393
466
initial-inf
initial-inf
0
100
10.0
1
1
NIL
HORIZONTAL

PLOT
23
421
466
721
Simulation populations
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
"susceptible" 1.0 0 -10899396 true "" "plot count susceptibles"
"exposed" 1.0 0 -1184463 true "" "plot count exposeds"
"infected" 1.0 0 -2674135 true "" "plot count infecteds"
"removed" 1.0 0 -3026479 true "" "plot count removeds"
"dead" 1.0 0 -16777216 true "" "plot count deads"
"lockdown" 1.0 0 -11221820 true "" "plot count turtles with [shape = \"person-outline\"]"

SLIDER
13
134
185
167
z-contact-init
z-contact-init
0
71
2.0
1
1
radius
HORIZONTAL

SLIDER
15
271
215
304
lockdown-strictness
lockdown-strictness
0
100
90.0
1
1
%
HORIZONTAL

PLOT
1143
35
1438
277
num-contacts
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
"default" 1.0 0 -16777216 true "" ""

TEXTBOX
62
14
212
32
infection parameters
11
0.0
1

TEXTBOX
1249
401
1399
419
model options
11
0.0
1

BUTTON
1344
305
1423
342
go once
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
14
87
186
120
p-death
p-death
0
100
3.0
1.0
1
%
HORIZONTAL

SLIDER
310
117
482
150
incubation-mean
incubation-mean
0
10
1.6
0.1
1
NIL
HORIZONTAL

SLIDER
309
159
481
192
incubation-stdev
incubation-stdev
0
10
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
308
205
480
238
recovery-mean
recovery-mean
0
20
11.5
0.1
1
NIL
HORIZONTAL

SLIDER
307
245
479
278
recovery-stdev
recovery-stdev
0
10
5.7
0.1
1
NIL
HORIZONTAL

SLIDER
1200
471
1372
504
duration
duration
0
10
5.0
1
1
years
HORIZONTAL

SLIDER
306
285
478
318
immunity-mean
immunity-mean
0
365
365.0
1
1
days
HORIZONTAL

TEXTBOX
365
94
515
112
countdowns
11
0.0
1

SLIDER
15
228
240
261
lockdown-threshold
lockdown-threshold
0
10000
100.0
100
1
infecteds
HORIZONTAL

SWITCH
1201
511
1363
544
imposed-lockdown?
imposed-lockdown?
1
1
-1000

TEXTBOX
53
204
203
222
lockdown parameters
11
0.0
1

SWITCH
1202
548
1348
581
modify-p-infect?
modify-p-infect?
1
1
-1000

SLIDER
14
311
222
344
protection-strength
protection-strength
1
100
50.0
1
1
%
HORIZONTAL

SWITCH
1202
587
1342
620
closed-system?
closed-system?
0
1
-1000

SLIDER
15
352
196
385
travel-strictness
travel-strictness
0
100
0.0
1
1
%
HORIZONTAL

MONITOR
605
658
671
703
exposeds
count exposeds
0
1
11

MONITOR
684
659
748
704
infecteds
count infecteds
17
1
11

MONITOR
513
658
593
703
susceptibles
count susceptibles
0
1
11

MONITOR
761
659
828
704
removeds
count removeds
0
1
11

MONITOR
843
660
900
705
deads
count deads
0
1
11

MONITOR
916
660
1009
705
% in lockdown
(count turtles with [shape = \"person-outline\"]) / \ncount turtles with [not member? self deads] \n* 100
0
1
11

MONITOR
1101
660
1163
705
% 30-59
count turtles with [age = \"30-59\"] /\ncount turtles * 100
1
1
11

MONITOR
1177
660
1234
705
% 60+
count turtles with [age = \"60+\"] /\ncount turtles * 100
1
1
11

MONITOR
1026
661
1083
706
% 0-29
count turtles with [age = \"0-29\"] /\ncount turtles * 100
1
1
11

@#$#@#$#@
## WHAT IS IT?

## HOW TO USE IT
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

person-outline
false
11
Polygon -16777216 true false 195 75 255 150 225 195 150 105
Polygon -16777216 true false 105 75 45 150 75 195 150 105
Polygon -16777216 true false 105 75 105 195 75 285 105 315 135 300 150 255 165 300 195 315 225 285 195 195 195 75
Circle -16777216 true false 103 -2 92
Circle -8630108 true true 110 5 80
Polygon -8630108 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -8630108 true true 127 79 172 94
Polygon -8630108 true true 105 90 60 150 75 180 135 105
Polygon -8630108 true true 195 90 240 150 225 180 165 105

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

transmitter
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Line -955883 false 210 30 285 15
Line -955883 false 210 60 285 75
Line -955883 false 90 30 15 15
Line -955883 false 90 60 15 75

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-remove">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.1"/>
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
