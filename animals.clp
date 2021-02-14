
(clear)

(reset)

(defglobal ?*q* = 1)

(defglobal ?*T* = 23)

(do-backward-chaining mam)       
(do-backward-chaining threetall) 
(do-backward-chaining fourlegs)  
(do-backward-chaining fur)       
(do-backward-chaining egg)       
(do-backward-chaining dom)       
(do-backward-chaining flies)     
(do-backward-chaining rep)        
(do-backward-chaining fish)      
(do-backward-chaining mars)     
(do-backward-chaining ins)      
(do-backward-chaining legs)      
(do-backward-chaining land)      
(do-backward-chaining tail)      
(do-backward-chaining vert)      
(do-backward-chaining bilat)     
(do-backward-chaining spotted)   
(do-backward-chaining carnivore) 
(do-backward-chaining venomous)  
(do-backward-chaining horns)     
(do-backward-chaining shell)      
(do-backward-chaining whiskers)  
(do-backward-chaining slithers)  

(deffunction animals ()
   (printout t "Alege un animal." crlf)
   (printout t "Orice cuvant ce incepe cu 'y' va fi interpretat ca si 'DA'" crlf)
   (printout t "Orice cuvant ce incepe cu 'n' va fi interpretat ca si 'NU'" crlf)
   (printout t "Apasa ENTER dupa fiecare raspuns." crlf)
   (run)
   (return)
)


(deffunction ask (?question)
   (printout t ?question) 
   (return (read))        
)


(deffunction validatedAsk (?question)
   (printout t ?*q*) 
   (printout t ". ") 
   (bind ?answer (ask ?question))
   (bind ?result (check ?answer))
   (while (= ?result nil) do
      (printout t "Input not valid. Please try again." crlf)
      (bind ?answer (ask ?question))
      (bind ?result (check ?answer))
   )
	(bind ?*q* (+ 1 ?*q*))
    (if (> ?*q* ?*T*) then
      (exceedThreshold)
   )
   (return ?result)
)


(deffunction check (?in)
   (if (symbolp ?in) then
      (if (or (= "y" (sub-string 1 1 ?in)) (= "Y" (sub-string 1 1 ?in))) then
         (bind ?in yes)
      elif (or (= "n" (sub-string 1 1 ?in)) (= "N" (sub-string 1 1 ?in))) then
         (bind ?in no)
      else
         (bind ?in nil)
      )
   else
      (bind ?in nil)
   )
   (return ?in)
)

(defrule noneFound "Programul se termina daca nu a fost identificat anilaul dorit"
   (declare (salience -100))
   (not (done))
   =>
   (printout t "GAME OVER" crlf)
   (printout t "NU l-am identificat cu nici un animal." crlf)
)

(deffunction exceedThreshold ()
   (halt)
   (printout t "GAME OVER" crlf)
   (printout t ?*T*)
   (printout t " Intrebarea era deja pusa." crlf)
   (return)
)

(defrule om "Este un om"
   (mam yes)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail no)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un om" crlf)
   (assert (done))
)
(defrule caine "Este un caine"
   (mam yes)
   (threetall yes)
   (fourlegs yes)
   (fur yes)
   (egg no)
   (dom yes)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un caine." crlf)
   (assert (done))
)


(defrule cameleon "Este un cameleon"
   (mam no)
   (threetall no)
   (fourlegs yes)
   (fur no)
   (egg yes)
   (dom no) ; While chameleons can be captive-bred, they are not considered domesticated
   (flies no)
   (rep yes)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns yes)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un cameleon" crlf)
   (assert (done))
)

(defrule flamingo "Este un flamingo"
   (mam no)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies yes)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land no)  ; The habitat of flamingos is usually lakes
   (tail yes) ; Short tail, but still one
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un flamingo." crlf)
   (assert (done))
)

(defrule toucan "Este un toucan"
   (mam no)
   (threetall no) ; The largest toucan is about 25 inches, so on average toucans are < 3 feet tall
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies yes)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un toucan." crlf)
   (assert (done))
)

(defrule tipar "Este un tipar"
   (mam no)
   (threetall no) ; While some eels can be longer than 3 feet, they are < 3 feet on AVERAGE.
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail no)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers yes)
   =>
   (printout t "Asa cred ca animalul tau dorit e un tipar." crlf)
   (assert (done))
)

(defrule somon "Este un somon"
   (mam no)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies no)
   (rep no)
   (fish yes)
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un somon." crlf)
   (assert (done))
)

(defrule jaguar "Este un jaguar"
   (mam yes)
   (threetall yes)
   (fourlegs yes)
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted yes)
   (carnivore yes)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers yes)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un jaguar." crlf)
   (assert (done))
)

(defrule cangur "Este un cangug"
   (mam yes)
   (threetall yes)
   (fourlegs no)
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars yes)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un cangur." crlf)
   (assert (done))
)

(defrule meduza "Este o meduza"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no) ; Jellyfish are not actually fish!
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail no)
   (vert no)
   (bilat no)
   (spotted no)
   (carnivore yes)
   (venomous yes)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e o meduza." crlf)
   (assert (done))
)

(defrule maimuta "Este o maimuta"
   (mam yes)
   (threetall no) ; While some monkeys can be taller than 3 feet, on AVERAGE they are < 3 feet.
   (fourlegs no)  ; Monkeys are considered to have 2 feet and 2 arms
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e o maimuta." crlf)
   (assert (done))
)

(defrule panda "Este o panda"
   (mam yes)
   (threetall yes)
   (fourlegs yes) ; Pandas walk on all fours so they are considered to have 4 legs
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted yes)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e o panda." crlf)
   (assert (done))
)

(defrule pismare "Este o pisică de mare"
   (mam no)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no) 
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous yes)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e o pisica de mare." crlf)
   (assert (done))
)

(defrule sarpe "Este un sarpe"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies no)
   (rep yes)
   (fish no)
   (mars no)
   (ins no)
   (legs no)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous yes)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers yes)
   =>
   (printout t "Asa cred ca animalul tau dorit e o sarpe." crlf)
   (assert (done))
)

(defrule vacă "Este o vaca"
   (mam yes)
   (threetall yes)
   (fourlegs yes)
   (fur no)
   (egg no)
   (dom yes)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers yes) ; Cows actually do have whiskers!
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e o vaca." crlf)
   (assert (done))
)

(defrule pudel "Este un pudel"
   (mam yes)
   (threetall no)
   (fourlegs yes)
   (fur yes)
   (egg no)
   (dom yes)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no) ; Poodles actually vegetables and meat
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no) ; Very short or no whiskers
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un pudel." crlf)
   (assert (done))
)

(defrule fluture "Este o fluture"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies yes)
   (rep no)
   (fish no)
   (mars no)
   (ins yes)
   (legs yes)
   (land yes)
   (tail no)
   (vert no)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e o fluture." crlf)
   (assert (done))
)

(defrule delfin "Este un delfin"
   (mam yes)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no) ; Dolphins are not fish.
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un delfin." crlf)
   (assert (done))
)

(defrule papagal "Este un papagal"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom yes)
   (flies yes)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un papagal." crlf)
   (assert (done))
)

(defrule tarantula "Este o tarantula"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur yes)
   (egg yes)
   (dom yes)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail no)
   (vert no)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous yes)
   (horns no)
   (shell no)
   (whiskers on)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e o tarantula." crlf)
   (assert (done))
)

(defrule veverita "Este o veverita"
   (mam yes)
   (threetall no)
   (fourlegs yes)
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers yes)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e o veverita." crlf)
   (assert (done))
)

(defrule melc "Este un melc"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes) ; NO legs, but snails do have one flat foot
   (land yes)
   (tail no)
   (vert no)
   (bilat no) ; Sides are mirror-images, coil opposite ways
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell yes)
   (whiskers no)
   (slithers no)
   =>
   (printout t "Asa cred ca animalul tau dorit e un melc." crlf)
   (assert (done))
)

(defrule need-mam-rule "Rule to backward chain the characteristic mam"
   (need-mam ?) ; The LHS is if mam is needed
   =>
   ; Call validatedAsk to ask if it's a mammal?, save to ?a
   (bind ?a (validatedAsk "Is it a mammal? "))
   ; If it is a mammal, we also know some more info
   (if (= ?a yes) then
      (assert (rep no))    ; no mammals are reptiles
      (assert (fish no))   ; no mammals are fish
      (assert (ins no))    ; no mammals are insects
      (assert (vert yes))  ; all mammals are vertebrates
      (assert (bilat yes)) ; all mammals have bilateral symmetry
   )
   (if (= ?a no) then
      (assert (mars no))
   )
   (assert (mam ?a)) ; assert mam with its value ?a
)

(defrule need-threetall-rule "Rule to backward chain the characteristic threetall"
   (need-threetall ?) ; The LHS is if threetall is needed
   =>
   ; Call validatedAsk to ask if it's taller/longer than 3 feet on average, save to ?a
   (bind ?a (validatedAsk "Is it taller/longer than 3 feet on average? "))
   ; If it is taller/longer than 3 feet, then we also know some more info
   (if (= ?a yes) then
      (assert (ins no)) ; insects are not taller/longer than 3 feet on average
   )
   (assert (threetall ?a)) ; assert threetall with its value ?a
)

(defrule need-fourlegs-rule "Rule to backward chain the characteristic fourlegs"
   (need-fourlegs ?) ; The LHS is if fourlegs is needed
   =>
   ; Call validatedAsk to ask if it has four legs, save to ?a
   (bind ?a (validatedAsk "Does it have four legs? "))
   ; If it has four legs, then we also know some more info
   (if (= ?a yes) then
      (assert (legs yes)) ; any animal with four legs must have legs!
   )
   (assert (fourlegs ?a)) ; assert fourlegs with its value ?a
)

(defrule need-fur-rule "Rule to backward chain the characteristic fur"
   (need-fur ?) 
   =>
   (bind ?a (validatedAsk "Does it have fur? "))
      (if (= ?a yes) then
      (assert (rep no))  
      (assert (fish no)) 
   )
   (assert (fur ?a)) 
)

(defrule need-egg-rule "Rule to backward chain the characteristic egg"
   (need-egg ?) ; The LHS is if fur is needed
   =>
   ; Call validatedAsk to ask if it lays eggs, assert egg with the output
   (assert (egg (validatedAsk "Does it lay eggs? ")))
)

(defrule need-dom-rule "Rule to backward chain the characteristic dom"
   (need-dom ?) ; The LHS is if dom is needed
   =>
   ; Call validatedAsk to ask if it is usually domesticated, assert dom with the output
   (assert (dom (validatedAsk "Is it usually domesticated? ")))
)

(defrule need-flies-rule "Rule to backward chain the characteristic flies"
   (need-flies ?) ; The LHS is if flies is needed
   =>
   ; Call validatedAsk to ask if it flies, save to ?a
   (bind ?a (validatedAsk "Does it fly? "))
   ; If it flies, then we also know some more info
   (if (= ?a yes) then
      (assert (fish no)) ; fish cannot fly (flying fish cannot technically fly)
      (assert (mars no)) ; no marsupials can fly
   )
   (assert (flies ?a)); assert flies with its value ?a
)

(defrule need-rep-rule "Rule to backward chain the characteristic rep"
   (need-rep ?) ; The LHS is if rep is needed
   =>
   ; Call validatedAsk to ask if it is a reptile, save to ?a
   (bind ?a (validatedAsk "Is it a reptile? "))
   ; If it is a reptile, then we also know some more info
   (if (= ?a yes) then
      (assert (mam no))   ; Reptiles cannot be mammals
      (assert (fur no))   ; Reptiles do not have fur
      (assert (flies no)) ; Reptiles cannot fly
      (assert (fish no))  ; Reptiles cannot be fish
      (assert (mars no))  ; Reptiles cannot be marsupials
      (assert (ins no))   ; Reptiles cannot be insects
      (assert (vert yes)) ; Reptiles are all vertebrates
      (assert (tail yes)) ; Reptiles all have tails
   )
   (assert (rep ?a)) ; assert rep with its value ?a
)

(defrule need-fish-rule "Rule to backward chain the characteristic fish"
   (need-fish ?) ; The LHS is if fish is needed
   =>
   ; Call validatedAsk to ask if it is a fish, save to ?a
   (bind ?a (validatedAsk "Is it a fish? "))
   ; If it is a fish, then we know some more info
   (if (= ?a yes) then
      (assert (mam no))    ; Fish cannot be mammals
      (assert (fur no))    ; Fish cannot have fur
      (assert (rep no))    ; Fish cannot be reptiles
      (assert (mars no))   ; Fish cannot be marsupials
      (assert (ins no))    ; Fish cannot be insects
      (assert (land no))   ; Fish cannot live on land
      (assert (tail yes))  ; Fish all have tails
      (assert (vert yes))  ; Fish are all vertebrates
      (assert (bilat yes)) ; Fish all have bilateral symmetry
   )
   (assert (fish ?a)) ; assert fish with its value ?a
)

(defrule need-mars-rule "Rule to backward chain the characteristic mars"
   (need-mars ?) ; The LHS is if mars is needed
   =>
   ; Call validatedAsk to ask if it is a marsupial, save to ?a
   (bind ?a (validatedAsk "Is it a marsupial? "))
   ; If it is a mammal, then we know some more info
   (if (= ?a yes) then
      (assert (mam yes))   ; All marsupials are mammals
      (assert (egg no))    ; Marsupials do not lay eggs
      (assert (flies no))  ; Marsupials cannot fly
      (assert (rep no))    ; Marsupials cannot be reptiles
      (assert (fish no))   ; Marsupials cannot be fish
      (assert (ins no))    ; Marsupials cannot be insects
      (assert (legs yes))  ; All marsupials have legs
      (assert (vert yes))  ; All marsupials are vertebrates
      (assert (bilat yes)) ; All marsupials have bilateral symmetry
   )
   (assert (mars ?a)) ; assert mars with its value ?a
)

(defrule need-ins-rule "Rule to backward chain the characteristic ins"
   (need-ins ?) ; The LHS is if ins is needed
   =>
   ; Call validatedAsk to ask if it is an insect, save to ?a
   (bind ?a (validatedAsk "Is it an insect? "))
   ; If it is an insect, then we know some more info
   (if (= ?a yes) then
      (assert (mam no))       ; Insects cannot be mammals
      (assert (threetall no)) ; Insects are short, so they cannot be taller/longer than 3 feet
      (assert (fourlegs no))  ; Insects have 6 legs, not 4
      (assert (fur no))       ; Insects do not have fur
      (assert (rep no))       ; Insects cannot be reptiles
      (assert (fish no))      ; Insects cannot be fish
      (assert (mars no))      ; Insects cannot be marsupials
      (assert (legs yes))     ; Insects all have legs
      (assert (vert no))      ; Insects are invertebrates - not vertebrates
      (assert (bilat yes))    ; All insects have bilateral symmetry
   )
   (assert (ins ?a)) ; assert ins with its value ?a
)

(defrule need-legs-rule "Rule to backward chain the characteristic legs"
   (need-legs ?) ; The LHS is if legs is needed
   =>
   ; Call validatedAsk to ask if it has legs, save to ?a
   (bind ?a (validatedAsk "Does it have legs? "))
   ; If it doesn't have legs, then we know some more info
   (if (= ?a no) then
      (assert (fourlegs no)) ; If it doesn't have any legs, it can't have four legs either
   )
   (assert (legs ?a)) ; assert legs with its value ?a
)

(defrule need-land-rule "Rule to backward chain the characteristic land"
   (need-land ?) ; The LHS is if land is needed
   =>
   ; Call validatedAsk to ask if it lives on land, save to ?a
   (bind ?a (validatedAsk "Does it live on land? "))
   ; If it lives on land, then we know some more info
   (if (= ?a yes) then
      (assert (fish no)) ; Fish do not live on land
   )
   (assert (land ?a)) ; assert land with its value ?a
)

(defrule need-tail-rule "Rule to backward chain the characteristic tail"
   (need-tail ?) ; The LHS is if tail is needed
   =>
   ; Call validatedAsk to ask if it is has a tail, assert tail with the output
   (assert (tail (validatedAsk "Does it have a tail? ")))
)

(defrule need-vert-rule "Rule to backward chain the characteristic vert"
   (need-vert ?) ; The LHS is if vert is needed
   =>
   ; Call validatedAsk to ask if it is a vertebrate, save to ?a
   (bind ?a (validatedAsk "Is it a vertebrate? "))
   ; If it is a vertebrate, then we know some more info
   (if (= ?a yes) then
      (assert (bilat yes)) ; Vertebrates all have bilateral symmetry
   )
   (assert (vert ?a)) ; assert vert with its value ?a
)

(defrule need-bilat-rule "Rule to backward chain the characteristic bilat"
   (need-bilat ?) ; The LHS is if bilat is needed
   =>
   ; Call validatedAsk to ask if it has bilateral symmetry, assert bilat with the output
   (assert (bilat (validatedAsk "Does it have bilateral symmetry? ")))
)

(defrule need-spotted-rule "Rule to backward chain the characteristic spotted"
   (need-spotted ?) ; The LHS is if spotted is needed
   =>
   ; Call validatedAsk to ask if it has spots, assert spotted with the output
   (assert (spotted (validatedAsk "Does it have spots? ")))
)

(defrule need-carnivore-rule "Rule to backward chain the characteristic carnivore"
   (need-carnivore ?) ; The LHS is if carnivore is needed
   =>
   ; Call validatedAsk to ask if it is a carnivore, assert carnivore with the output
   (assert (carnivore (validatedAsk "Is it a carnivore? ")))
)

(defrule need-venomous-rule "Rule to backward chain the characteristic venomous"
   (need-venomous ?) ; The LHS is if venomous is needed
   =>
   ; Call validatedAsk to ask if it is venomous, assert venomous with the output
   (assert (venomous (validatedAsk "Can it be venomous? ")))
)

(defrule need-horns-rule "Rule to backward chain the characteristic horns"
   (need-horns ?) ; The LHS is if horns is needed
   =>
   ; Call validatedAsk to ask if it is has horns, assert horns with the output
   (assert (horns (validatedAsk "Does it usually have horns? ")))
)

(defrule need-shell-rule "Rule to backward chain the characteristic shell"
   (need-shell ?) ; The LHS is if shell is needed
   =>
   ; Call validatedAsk to ask if it is has a shell, save to ?a
   (bind ?a (validatedAsk "Does it have a shell? "))
   ; If it does have a shell, then we know some more information
   (if (= ?a yes) then
      (assert (fish no)) ; No fish have shells
      (assert (mam no))  ; No mammals have shells
      (assert (mars no)) ; No marsupials have shells
   )
   (assert (shell ?a))
)

(defrule need-whiskers-rule "Rule to backward chain the characteristic whiskers"
   (need-whiskers ?) ; The LHS is if whiskers is needed
   =>
   ; Call validatedAsk to ask if it is has whiskers, assert whiskers with the output
   (assert (whiskers (validatedAsk "Does it have noticeable whiskers? ")))
)

(defrule need-slithers-rule "Rule to backward chain the characteristic slithers"
   (need-slithers ?) ; The LHS is if slithers is needed
   =>
   ; Call validatedAsk to ask if it slithers, assert slithers with the output
   (assert (slithers (validatedAsk "Does it slither (side to side movement)? ")))
)

(defrule inferNotEgg "Rule to invalidate egg if it is a mammal and it doesn't have four legs"
   (mam yes)
   (fourlegs no)
   =>
   (assert (egg no)) ; No mammals that don't have four legs lay egg
)

(animals) 
