(deftemplate flag (slot country) (multislot colors))

(deffacts countries
   (flag (country Panama) (colors red white blue))
   (flag (country Jamaica) (colors black yellow green))
   (flag (country Colombia) (colors yellow blue red)) 
   (flag (country Italy) (colors green white red)) 
   (flag (country Ireland) (colors green white orange))
   (flag (country Greece) (colors blue white))
   (flag (country Botswana) (colors blue white black)))

(deffacts start
   (get-color))

(defrule get-colors
   ?f <- (get-color)
   =>
   (retract ?f)
   (printout t "Flag color (done to end)?")
   (assert (new-color (read))))

(defrule find-matches
   (find-flag)
   (exists (color ?))
   (flag (country ?country))
   (forall (color ?color)
           (flag (country ?country) (colors $? ?color $?)))
   =>
   (printout t ?country " 's flag contains the specified colors." crlf))

(defrule no-matches
   (find-flag)
   (not (and (flag (country ?country))
             (forall (color ?color)
                     (flag (country ?country) (colors $? ?color $?)))))
   => 
   (printout t "No country's flag contains the specified colors." crlf))

(defrule all-flags-match
   (find-flag)
   (not (color ?))
   =>
   (printout t "No search colors were specified." crlf))

(defrule look-for-flags
   ?f <- (new-color done)
   =>
   (retract ?f)
   (assert (find-flag)))

(defrule look-for-another-color
   ?f <- (new-color ?color&~done)
   =>
   (assert (color ?color))
   (retract ?f)
   (assert (get-color)))