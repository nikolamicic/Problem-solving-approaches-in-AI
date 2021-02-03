;;put command (reset) in dialog window before run!

(deftemplate floor
   (slot #)
   (slot people))

(deftemplate elevator
   (slot capacity)
   (slot occupants))
    
(deffacts initial
   (elevator (capacity 5) 
             (occupants 0))
   (floor (# ground) (people 0))
   (floor (# 1) (people 2))    
   (floor (# 2) (people 6))    
   (floor (# 3) (people 4)))  

(defrule pickUpPeople
   ;; The elevator is not full
   ?e <- (elevator (capacity ?c)
                   (occupants ?o&~?c))
   ;; There's a floor with people on it
   ?f <- (floor (# ?f1&~ground) (people ?p&~0))
   ;; There's not a higher floor with people
   (not (floor (# ?f2&~ground&:(> ?f2 ?f1)) (people ~0)))
   =>
   ;; The number of people that can enter the elevator is
   ;; the minimum of the remaining occupancy of the elevator
   ;; and the number of people on the floor
   (bind ?added-people (min (- ?c ?o) ?p))
   ;; Print a message 

   (printout t "Elevator moves to floor " ?f1
               " and picks up " ?added-people " "
               (if (= ?added-people 1) then person else people)
               crlf)
   ;; Update  the number of people in the elevator and on the floor
   (modify ?e (occupants (+ ?o ?added-people)))
   (modify ?f (people (- ?p ?added-people))))

(defrule dropOffPeople
   ;; Determine the number of people on the ground floor
   ?f <- (floor (# ground) (people ?p))
   ;; There must be people in the elevator
   ?e <- (elevator (occupants ?o&~0)
                   (capacity ?c))
   ;; There are no remaining people on any of the floors
   ;; or the elevator is at full occupancy
   (or (not (floor (# ~ground) (people ~0)))
       (test (= ?c ?o)))
   =>
   ;; Print a message
   (printout t "Elevator moves to ground floor and drops off " 
               ?o " " (if (= ?o 1) then person else people) crlf)
   ;; Update the number of people on the ground floor and 
   ;; in the elevator
   (modify ?f (people (+ ?o ?p)))
   (modify ?e (occupants 0)))