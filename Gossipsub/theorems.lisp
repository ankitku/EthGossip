(in-package "ACL2S")

(include-book "std/alists/top" :dir :system)
(include-book "network")


;;example factors
(defconst *factors* (cons
		     (params 1 1 3 5 2 1 5 -3 4 3 5 6 1 60 5 3 120 2)
		     '((FM . (1 ;; time in mesh
                              1 ;; first message deliveries
                              -1 ;; mesh message delivery rate
                              -1 ;; mesh message delivery failures
                              -1 ;; invalid messages
                              1 ;; app specific
                              -1 ;; IP colocation factor
                              -1))
                       (PL . (1 ;; time in mesh
                              1 ;; first message deliveries
                              -1 ;; mesh message delivery rate
                              -1 ;; mesh message delivery failures
                              -1 ;; invalid messages
                              1 ;; app specific
                              -1 ;; IP colocation factor
                              -1))))) ;; behaviour penalty


;; should have a peer-state enum here

(property mesh-forwards (p :peer prcv :peer psnd :peer pld :payload-type ps
          :peer-state  s :nat)
          :PROOFS? nil
          :CHECK-CONTRACTS? nil
          (=> (^ (== pld '(PLD100 PID100 FM X))
                 (!= p prcv)
                 (in psnd (cdr (assoc-equal top (nbr-topic-state-topic-mesh
                                                 (peer-state-nts ps)))))
                 (>= (params-meshMessageDeliveriesCap (car *factors*))
                     (params-meshMessageDeliveriesThreshold (car *factors*)))
                 (<= (params-dlow (car *factors*)) (params-d (car *factors*)))
                 (>= (params-dhigh (car *factors*)) (params-d (car *factors*))))
              (member-equal `(,p SND ,psnd PAYLOAD ,pld)
                            (second (transition p ps `(,p RCV ,prcv PAYLOAD ,pld)
                                                *factors* s))))
          :debug? t)
              





          (transition p () `(,p RCV ,p2 PAYLOAD ,pld))


             run-network (gr :group evnts :loev i :nat factors :factors s

                             
             (definec topic-mesh-graph (grp :group tp :topic) :tl
  (match grp
    (() '())
    (((p . st) . rst)
     (let ((ms (cdr (assoc-equal tp (nbr-topic-state-topic-mesh
                                     (peer-state-nts st))))))
       (cons (cons p ms) (topic-mesh-graph rst tp))))))


