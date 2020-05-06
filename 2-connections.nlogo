extensions [ csv ]
globals [
  ;; used to display time...
  current-hour
  current-day
  current-week
  ;; ... and affect behaviour
  exam-weeks
  exam-time?
  busy-hours
  busy-time?
  ;; used to record details of sent messages
  msg-ledger
  msg-counter
  ;; template lists that might come in handy later on
  template-tmp-record ;; allows students to keep a personal messages record
  color-student
]

breed [ students student ]
students-own [
  msg-record
  tmp-record
  INIT-PROACTIVITY
  INIT-SUBSCRIPTIONS
  INIT-CONNECTIONS
  proactivity
  subscriptions
  connections
]

to setup
  clear-all

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; INITIALISE GLOBAL VARIABLES
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  set exam-weeks (list 12 13 14 27 28 29)
  set exam-time? false
  set busy-hours (list 2 3 4 5)
  set busy-time? false
  set msg-ledger (list)
  set msg-counter 1
  set template-tmp-record (list "_" "_" "_" "_" "_" "_")
  ;; some more variables that are only needed for setup (for now)
  set color-student hsb 240 50 80 ;; blue-ish
  let color-patches hsb 48 18 98 ;; beige-ish to hopefully have good contrast

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; INITIALISE ENVIRONMENT AND AGENTS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ask patches [
    sprout-students 1 [
      set msg-record (list) ;; students' personal record of messages
      set tmp-record template-tmp-record ;; temporary record to allow updating values
      ;; set proactivity and make sure it is bounded between 0 and 1 (inclusive)
      set INIT-PROACTIVITY mk-float-param (random-normal mean-proactivity stdev-proactivity) 1
      set INIT-SUBSCRIPTIONS mk-subscriptions INIT-PROACTIVITY "det" ;; select message sources based on proactivity

      ;; set student parameters to their initial values
      set proactivity INIT-PROACTIVITY
      set subscriptions INIT-SUBSCRIPTIONS

      ;; make aesthetically pleasing visuals
      set shape "person"
      set color color-student ;; might depend on params later on
    ]
    set pcolor color-patches ;; might depend on params later on
  ]
  ;; initialise params that require all students to exist beforehand
  ask students [
    ;; give everyone an initial random group of friends
    set INIT-CONNECTIONS mk-connections
    set connections INIT-CONNECTIONS
  ]

  reset-ticks
end

to go
  tick
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MANAGE TIME FOR BEHAVIOUR ADJUSTMENT AND REPORTING
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; simplifying assumptions:
  ;; - one tick will represent one hour
  ;; - one day is 8 hours
  ;; - one week is 40 hours (standard work hours)
  ;; - there is no communication happening on weekends or holidays
  ;; - exam weeks vs standard -> students are likely to be more proactive
  ;; - busy hours vs standard -> more messages are likely to be sent

  set current-hour ticks mod 8 ;; the hour should go up by 1 every tick
  ;; every 8 ticks the day should go up by 1
  if ticks mod 8 = 0 [ set current-day ((current-day + 1) mod 5) ]
  ;; every 40 ticks the week should go up by 1
  if ticks mod 40 = 0 [ set current-week current-week + 1 ]

  ifelse member? current-week exam-weeks [ set exam-time? true ] [ set exam-time? false ]
  ifelse member? current-hour busy-hours [ set busy-time? true ] [ set busy-time? false ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MAIN LOOP
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; check if exam time to make students be more proactive
  ifelse exam-time?
  [ ask students [ set proactivity mk-float-param INIT-PROACTIVITY exam-multiplier ] ]
  [ ask students [ set proactivity mk-float-param INIT-PROACTIVITY 1 ] ]
  ;; check if busy time to send more than one message per iteration
  let busy-counter 0
  ifelse busy-time? [ set busy-counter busy-multiplier ] [ set busy-counter 1 ]

  while [ busy-counter > 0 ] [
    ;; university sends new message and notifies students
    ;; message "object": msg { 0=tick, 1=id, 2=source, 3=priority }
    set msg-ledger insert-item (length msg-ledger) msg-ledger mk-msg
    ask students [ student-notify ]
    ;; then students receive and share the message
    ask students [ student-receive self false ]
    ask students [ student-share ]
    ;; finally students update their personal record
    ask students [ commit-tmp-record ]
    ask students [ reset-tmp-record ]

    set busy-counter busy-counter - 1
  ]
  if ticks >= max-iters [ stop ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN LOOP CORE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to student-notify
  ;; temporary record "object": tmp-record { 0=msg, 1=notified?
  ;;   , 2=received?, 3=shared?, 4=received-from, 5=shared-to }
  let msg last msg-ledger
  let src item 2 msg ;; message source from the university
  let subs [subscriptions] of self
  ask self [ set tmp-record replace-item 0 tmp-record msg ]
  ifelse member? src subs ;; notified only if subscribed to message source
    [ ask self [ set tmp-record replace-item 1 tmp-record true ] ]
    [ ask self [ set tmp-record replace-item 1 tmp-record false ] ]
end

to student-receive [ from no-check ]
  let notified? item 1 tmp-record = true
  let received? item 2 tmp-record = true

  let pro [proactivity] of self
  let thr mk-float-threshold

  ask self [
    ifelse received?
    [] ;; do not change tmp-record if message already received
    [ ;; don't check proactivity if message is shared by friend
      ifelse no-check or (notified? and pro > thr)
      [ ;; mark as received from yourself or other student...
        set tmp-record replace-item 2 tmp-record true
        set tmp-record replace-item 4 tmp-record from
      ]
      [ ;; ... unless the message was never received
        set tmp-record replace-item 2 tmp-record false
        set tmp-record replace-item 4 tmp-record "None"
      ]
    ]
  ]
end

to student-share
  let received? item 2 tmp-record = true
  let shared? item 3 tmp-record = true

  let conn [connections] of self

  ask self [
    ifelse received? and not shared?
    [ ;; share the message with your connections and record that...
      ask conn [ student-receive myself true ]
      set tmp-record replace-item 3 tmp-record true
      set tmp-record replace-item 5 tmp-record conn
    ]
    [ ;; ... or do not share and record that - there is no try!
      set tmp-record replace-item 3 tmp-record false
      set tmp-record replace-item 5 tmp-record "None"
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; message "object": msg { 0=tick, 1=id, 2=source, 3=priority }
to-report mk-msg
  let id msg-counter
  let src mk-msg-source
  let pr mk-msg-priority
  let msg (list ticks id src pr)
  set msg-counter msg-counter + 1
  report msg
end
to-report mk-msg-source
  ;; this works for now, but there's probably a better way to have different probabilities
  let src one-of [ "email" "email" "email" "email" "blackboard" "blackboard" "textmsg" ]
  report src
end

to-report mk-msg-priority
  let pr (ifelse-value
    exam-time? [ one-of [ 1 2 2 3 3 3 3 ] ]
    one-of [ 1 1 2 2 2 2 3 ] ;; no exams
  )
  report pr
end

to-report mk-subscriptions [ pa rnd ] ;; pa = proactivity
  let subs (list)
  ifelse rnd = "det"
  [ ;; deterministic based on proactivity
    set subs (ifelse-value
      pa > 0.8 [ (list "email" "blackboard" "textmsg") ]
      pa > 0.4 [ (list "email" "blackboard") ]
      [ (list "email") ] ;; students subscribed to email by default
    )
  ]
  [ ;; otherwise random
    set subs n-of ((random 3) + 1) (list "email" "blackboard" "textmsg")
  ]
  report subs
end

to-report mk-connections
  let network n-of (one-of [0 2 4]) other students
  report network
end

to-report mk-float-threshold
  let thr random-float 1
  report thr
end

to-report mk-float-param [ before factor ]
  let after before * factor
  if after > 1 [ set after 0.99999999999999999 ] ;; max precision because right now we want no zeros or ones exactly
  if after < 0 [ set after 0.00000000000000001 ]
  report after
end

to commit-tmp-record ;; append the tmp-record to the end of the students' msg-record
  ask self [ set msg-record (insert-item (length msg-record) msg-record tmp-record) ]
end

to reset-tmp-record
  ask self [ set tmp-record template-tmp-record ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA AND VISUALISATION FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ======================= WARNING! =======================
;; THIS FUNCTION IS NOT YET OPTIMISED! PLEASE DO NOT USE IT
;to output-csv
;  file-open "base_model.data.csv.tmp"
;  ask students [
;    let data csv:to-row (list ([who] of self) msg-record)
;    file-print data
;  ]
;  file-close
;end
;; ========================================================

to-report display-time
  let day-name (ifelse-value
    current-day = 0 [ "Mon" ]
    current-day = 1 [ "Tue" ]
    current-day = 2 [ "Wed" ]
    current-day = 3 [ "Thu" ]
    current-day = 4 [ "Fri" ]
  )
  report (word
    "exam? " exam-time? ", busy? " busy-time?
    ", D: " (day-name) ", W: " (current-week + 1)
    ", H: " (current-hour + 9) "-" (current-hour + 10)
  )
end

to-report percent-received
  let nofmsg length msg-ledger
  let percent 0
  ;; get the absolute number of received messages
  let nofrec length filter [ record -> item 2 record = true ] msg-record
  ;; make sure you don't divide by zero
  ifelse nofmsg > 0 [ set percent nofrec / nofmsg ] [ set percent 0 ]
  report percent
end

to-report average-percent
  let total-percent 0
  ask students [ set total-percent total-percent + percent-received ]
  report total-percent / count students
end

to show-student-colour [ param ]
  let colour-param (ifelse-value
    param = "proactivity" [ proactivity ]
    param = "subscriptions" [ (length subscriptions) / 3 ]
    ;; (conn - minconn) / (maxconn - minconn) but in expanded form
    param = "connections" [ (count connections - min [ count connections ] of students) / (max [ count connections ] of students - min [ count connections ] of students) ]
    param = "percentrec" [ percent-received ]
    param = "reset" [ 0.5 ] [ 0.5 ] ;; reset students to original colour
  )
  set color hsb 240 (100 * colour-param) 80
end

to mk-plot [ plottype param ]
  clear-plot
  (ifelse
    plottype = "hist" and param = "proactivity" [ histogram [INIT-PROACTIVITY] of students ]
    plottype = "dots" and param = "proactivity" [ ask students [ plotxy INIT-PROACTIVITY percent-received ] ]
  )
end
@#$#@#$#@
GRAPHICS-WINDOW
10
10
324
325
-1
-1
18.0
1
10
1
1
1
0
1
1
1
0
16
0
16
0
0
1
ticks
30.0

PLOT
340
10
500
130
initial proactivity
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
"default" 0.1 1 -16777216 true "" "mk-plot \"hist\" \"proactivity\""

PLOT
20
390
420
690
messages received (pro)
initial proactivity
percent received
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "mk-plot \"dots\" \"proactivity\""

BUTTON
45
335
110
375
NIL
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
125
335
190
375
NIL
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

BUTTON
205
335
280
375
go once
go
NIL
1
T
OBSERVER
NIL
F
NIL
NIL
1

MONITOR
360
330
650
375
real world time
display-time
1
1
11

SLIDER
340
135
500
168
mean-proactivity
mean-proactivity
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
340
170
500
203
stdev-proactivity
stdev-proactivity
0
0.35
0.17
0.01
1
NIL
HORIZONTAL

INPUTBOX
778
25
848
85
max-iters
1200.0
1
0
Number

SLIDER
690
100
848
133
exam-multiplier
exam-multiplier
1
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
690
185
848
218
busy-multiplier
busy-multiplier
1
5
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
689
10
852
38
suggested max-iters: 8h*5d*30w\n= 1200
11
0.0
1

BUTTON
340
210
435
243
show proactivity
ask students [ show-student-colour \"proactivity\" ]
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
340
250
420
283
reset colour
ask students [ show-student-colour \"reset\" ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
519
300
649
318
more blue == higher value
11
0.0
1

TEXTBOX
703
136
838
178
exam-multiplier is how much more proactive students become during exam weeks
11
0.0
1

TEXTBOX
701
222
841
264
busy-multiplier is the number of messages sent during the more active hours of the day
11
0.0
1

MONITOR
690
40
770
85
avg received
(word (precision (average-percent * 100) 2) \"%\")
2
1
11

BUTTON
425
250
540
283
show subscriptions
ask students [ show-student-colour \"subscriptions\" ]
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
545
250
670
283
show connections
ask students [ show-student-colour \"connections\" ]
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
340
290
497
323
show percent received
ask students [ show-student-colour \"percentrec\" ]
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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experimentProactivity" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>average-percent</metric>
    <enumeratedValueSet variable="random-seed">
      <value value="111"/>
      <value value="222"/>
      <value value="333"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="busy-multiplier">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-iters">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stdev-proactivity">
      <value value="0.17"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mean-proactivity" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="exam-multiplier">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentExams" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>average-percent</metric>
    <enumeratedValueSet variable="random-seed">
      <value value="111"/>
      <value value="222"/>
      <value value="333"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="busy-multiplier">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-iters">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stdev-proactivity">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-proactivity">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exam-multiplier" first="1" step="1" last="10"/>
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
