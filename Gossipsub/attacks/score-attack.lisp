(in-package "ACL2S")
(include-book "attack")

(definec alltopics () :lot
  '(AGG BLOCKS SUB1 SUB2 SUB3))

;Uncomment line 30 : '(BLOCKS AGG SUB1 SUB2 SUB3)) in scoring.lisp

;; ------------------ Eth2.0 Case Study (github.com/silesiacoin/prysm-spike) ------------------------------

;; Refer to https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;          beacon-chain/p2p/gossip_scoring_params.go
;;
;; d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f
;;
;;
;; GossipSub is my private version forked off of be065ce0510e81d820a2cdb9762e63fd012122ba
;; Libp2p is my private version forked off of 4400328a581babd9a196e1ddffbe996ae7b3b59

;;    IPColocationFactorThreshold = 10
;;    BehaviourPenaltyThreshold   = 6
;;    BehaviourPenaltyDecay       = 10 epochs
;;    DecayInterval               = 1 slot duration
;;    DecayToZero                 = 0.01
;;    RetainScore                 = 100 epoch durations

(defconst *decayToZero* 1/100)

;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     shared/params/config.go#L123

(defconst *seconds-per-slot* 1) ;; ARBITRARY 
(defconst *one-epoch-duration* 1) ;; ARBITRARY 

(defconst *aggregateWeight* (/ 1 2)) ;; weight for aggregate topic, see #L100
(defconst *beaconBlockWeight* (/ 8 10)) ;; weight for beacon topic, see #L77

;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     beacon-chain/p2p/gossip_scoring_params.go#L154
(defconst *slot-duration* (* *seconds-per-slot* 1))

;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;    shared/params/config.go#L48
(defconst *slots-per-epoch* 10) ;; ARBITRARY 
(defconst *blocks-per-epoch* *slots-per-epoch*) ;; #L75
(defconst *decay-epoch* 5) ;; #L74

;; In line number comments, if I write BC, I am referring to:
;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     beacon-chain/p2p/pubsub.go

;; If I write SERV, I am referring to:
;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     beacon-chain/p2p/service.go

;; In all other cases, as indicated previously, I am referring to gossip_scoring_params.go.

;; I assume all times are given in units of 1 second ...

(defconst *enable-larger-gossip-history* 'nil)

(defconst *hbmInterval* (/ 700 1000))

(defconst *topicCap* (+ 32 (/ 72 100)))

(defconst *comm-count-per-slot* 10) ;; ARBITRARY - refer to #L173 ... they have not decided yet!
;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     shared/params/config.go#L123
(defconst *target-aggregators-per-committee* 5) ;; ARBITRARY
(defconst *aggregators-per-slot* (* *comm-count-per-slot* *target-aggregators-per-committee*)) ;; #L1871
(defconst *agg-per-epoch* (* *aggregators-per-slot* *slots-per-epoch*))
(defconst *attestationTotalWeight* 1) ;; #L23
;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;; shared/params/network_config.go#L40
(defconst *AttestationSubnetCount* (- (len (alltopics)) 2))         ;; ARBITRARY 
(defconst *MinGenesisActiveValidatorCount* 300) ;; ARBITRARY -----------------|
;; -------> but currently needs to be divisible by 50 x attestationSubnetCount.

(defconst *subnet-topicWeight* (/ *attestationTotalWeight* *AttestationSubnetCount*)) ;; #L121
(defconst *activeValidators* *MinGenesisActiveValidatorCount*) ;; #L169
(defconst *subnet-subnetWeight* (/ *activeValidators* *AttestationSubnetCount*)) ;; #L122
(defconst *subnet-minimumWeight* (/ *subnet-subnetWeight* 50)) ;; #L123
(defconst *subnet-numsPerSlot* (/ *subnet-subnetWeight* *slots-per-epoch*)) ;; #L124
(defconst *subnet-comsPerSlot* *comm-count-per-slot*) ;; #L125
(defconst *subnet-exceedsThreshold*
  (>= *subnet-comsPerSlot*
      (* 2 (/ *AttestationSubnetCount* *slots-per-epoch*)))) ;; #L126
(defconst *subnet-firstDecay* (if *subnet-exceedsThreshold* 4 1)) ;; #L127, 130
(defconst *subnet-meshDecay* (if *subnet-exceedsThreshold* 16 4)) ;; #L128, 131

(defconst *eth-default-block-weights*
  (weights (/ 324 10000)  ;; w_1  = time in mesh weight                (#L78)
           1              ;; w_2  = first message deliveries weight    (#L81)
           (/ -717 1000)  ;; w_3  = mesh message deliveries weight     (#L84)
           (/ -717 1000)  ;; w_3b = mesh failure penalty weight        (#L90)
           (- -140 (/ 4475 10000)) ;; w_4  = invalid messages weight   (#L92)
           1       ;; w_5  = application-specific score weight         (#L43, global)
           (- -35 (/ 11 100))   ;; w_6  = IP-co-location factor weight (#L44, global)
           (- -15 (/ 92 100)))) ;; w_7 = behavioral penalty weight     (#L47, global)

(check= t (weightsp *eth-default-block-weights*))

(defconst *eth-default-agg-weights*
  (weights (/ 324 10000) ;; w_1  = time in mesh weight              (#L101)
           (/ 128 1000)  ;; w_2  = first message deliveries weight  (#L104)
           (/ -64 1000)  ;; w_3  = mesh message deliveries weight   (#L107)
           (/ -64 1000)  ;; w_3b = mesh failure penalty weight      (#L113)
           (- -140 (/ 4475 10000)) ;; w_4 = invalid messages weight (#L115)
           1             ;; w_5 = application-specific score weight    (#L43, global)
           (- -35 (/ 11 100))   ;; w_6  = IP-co-location factor weight (#L44, global)
           (- -15 (/ 92 100)))) ;; w_7 = behavioral penalty weight     (#L47, global)

(check= t (weightsp *eth-default-agg-weights*))

(defconst *eth-default-agg-subnet-weights*
  (weights (/ 324 10000) ;; w_1 = time in mesh weight                  (#L134)
           (/ 955 1000)  ;; w_2 = first message deliveries weight      (#L138)
           (- -37 (/ 55 100)) ;; w_3  = mesh message deliveries weight (#L141)
           (- -37 (/ 55 100)) ;; w_3b = mesh failure penalty weight    (#L147)
           -4544              ;; w_4 = invalid messages weight 
           1       ;; w_5  = application-specific score weight         (#L43, global)
           (- -35 (/ 11 100))   ;; w_6  = IP-co-location factor weight (#L44, global)
           (- -15 (/ 92 100)))) ;; w_7 = behavioral penalty weight     (#L47, global)

(check= t (weightsp *eth-default-agg-subnet-weights*))

;; ;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/beacon-chain/p2p/gossip_scoring_params.go#L162
;; (definecd scoreDecay(x :non-neg-rational) :non-neg-rational
;;   (b* (((when (= x 0)) 0)
;;        (numOfTimes (/ x *slot-duration*)))
;;     3/4))
;;     ;;(expt *decayToZero* (floor 1 numOfTimes))))

;; Currently failing ...
(defconst *eth-default-block-params*
  (params 4               ;; activationWindow    : #L89 aka MeshMessageDeliveriesActivation
	  *slot-duration* ;; meshTimeQuantum     : #L79
	  23              ;; p2cap               : #L83 aka FirstMessageDeliveriesCap
	  300             ;; timeQuantaInMeshCap : #L80
	  (* *blocks-per-epoch*
	     *decay-epoch*) ;; meshMessageDeliveriesCap : #L86
	  ;; meshMessageDeliveriesThreshold             : #L87
	  (/ (* *blocks-per-epoch* *decay-epoch*) 10)
	  *topicCap*        ;; topiccap          : #L39 (global)
	  -16000            ;; greyListThreshold : #L33 (global)
	  8                 ;; d                 : BC #L120
	  6                 ;; dlow              : BC #L119
	  12                ;; dhigh             : default in gossipsub.go
	  6                 ;; dlazy             : default in gossipsub.go
	  *hbmInterval* ;; hbmInterval (aka HeartbeatInterval) : BC #L118
	  60 ;; fanoutTTL (defaults to GossipSubFanoutTTL) : default in gossipsub.go
	  ;; mcacheLen (aka GossipSubHistoryLength) (see below)
	  (if *enable-larger-gossip-history* 12 6) ;; BC #L122, 130, 131
	  ;; mcacheGsp (aka GossipFactor) (defaults to GossipSubGossipFactor)
          ;; (see below)
	  (/ 1 1) ;; default in gossipsub.go
	  (* 500 *hbmInterval*) ;; seenTTL       : BC #L124
	  ;; https://github.com/silesiacoin/prysm-spike/blob/
	  ;; d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/beacon-chain/p2p/gossip_scoring_params.go#L35
	  5 ;; opportunisticGraftThreshold
	  ;; Let scoreDecay(x) = decayToZero^(1/(x/oneSlotDuration)).  Then, starting at:
	  ;;
	  ;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
	  ;;     beacon-chain/p2p/gossip_scoring_params.go#L73
	  ;;
	  *beaconBlockWeight*
	  ;; MeshMessageDeliveriesDecay = scoreDecay(decayEpoch * oneEpochDuration)
	  398/1000
	  ;; FirstMessageDeliveriesDecay = scoreDecay(20 * oneEpochDuration)
	  794/1000
	  63/100 ;; behaviourPenaltyDecay
	  ;;  MeshFailurePenaltyDecay = scoreDecay(decayEpoch * oneEpochDuration)
	  398/1000
	  ;; InvalidMessageDeliveriesDecay = scoreDecay(50 * oneEpochDuration)
	  912/1000
	  *decayToZero* ;; decayToZero
	  *slot-duration* ;; decayInterval
	  ))

(check= t (paramsp *eth-default-block-params*))

(defconst *eth-default-aggregate-params*
  (params (* 32 *slot-duration*) ;; activationWindow               : #L112
	  *slot-duration*        ;; meshTimeQuantum                : #L102
	  179                    ;; p2cap                          : #L106
	  300                    ;; timeQuantaInMeshCap            : #L103
	  *agg-per-epoch*        ;; meshMessageDeliveriesCap       : #L109
	  (/ *agg-per-epoch* 50) ;; meshMessageDeliveriesThreshold : #L110
	  *topicCap*             ;; topicCap,          see above (global)
	  -16000                 ;; greyListThreshold, see above (global)
	  8                      ;; d                                   : BC #L120
	  6                      ;; dlow                                : BC #L119
	  12                     ;; dhigh                               : default in gossipsub.go
	  6                      ;; dlazy                               : default in gossipsub.go
	  *hbmInterval*          ;; hbmInterval (aka HeartbeatInterval) : BC #L118
	  60                     ;; fanoutTTL (defaults to GossipSubFanoutTTL) : default in gossipsub.go
	  ;; mcacheLen (aka GossipSubHistoryLength) (see below)
	  (if *enable-larger-gossip-history* 12 6) ;; BC #L122, 130, 131
	  ;; mcacheGsp (aka GossipFactor) (defaults to GossipSubGossipFactor)
          ;; (see below)
          ;; TODO : clarify gossipfactor
	  (/ 1 1) ;; default in gossipsub.go
	  (* 500 *hbmInterval*) ;; seenTTL : BC #L124
	  5 ;; opportunisticGraftThreshold
	  *aggregateWeight* ;; topicWeight = aggregateWeight       #L100
	  1/100 ;; MeshMessageDeliveriesDecay  = scoreDecay(1 epoch) #L108
	  1/100 ;; FirstMessageDeliveriesDecay = scoreDecay(1 epoch) #L105
	  63/100 ;; behaviourPenaltyDecay
	  1/100 ;; MeshFailurePenaltyDecay     = scoreDecay(1 epoch) #L114
	  912/1000 ;; InvalidMessageDeliveriesDecay = scoreDecay(50 epochs) #L116
	  *decayToZero*
	  *slot-duration*
	  ))

(check= t (paramsp *eth-default-aggregate-params*))

(defconst *eth-default-aggregate-subnet-params*
  (params (* 17 *slot-duration*) ;; activationWindow               : #L146
	  *subnet-numsPerSlot*   ;; meshTimeQuantum                : #L136
	  24                     ;; p2cap                          : #L140
	  300                    ;; timeQuantaInMeshCap            : #L137
	  *subnet-subnetWeight*  ;; meshMessageDeliveriesCap       : #L143
	  *subnet-minimumWeight* ;; meshMessageDeliveriesThreshold : #L144
	   *topicCap*             ;; topicCap,          see above (global)
	  -16000                 ;; greyListThreshold, see above (global)
	  8                      ;; d                                   : BC #L120
	  6                      ;; dlow                                : BC #L119
	  12                     ;; dhigh                               : default in gossipsub.go
	  6                      ;; dlazy                               : default in gossipsub.go
	  *hbmInterval*          ;; hbmInterval (aka HeartbeatInterval) : BC #L118
	  60                     ;; fanoutTTL (defaults to GossipSubFanoutTTL) : default in gossipsub.go
	  ;; mcacheLen (aka GossipSubHistoryLength) (see below)
	  (if *enable-larger-gossip-history* 12 6) ;; BC #L122, 130, 131
	  ;; mcacheGsp (aka GossipFactor) (defaults to GossipSubGossipFactor)
          ;; (see below)
          ;; TODO
	  (/ 1 1) ;; default in gossipsub.go
	  (* 500 *hbmInterval*) ;; seenTTL : BC #L124
	  5 ;; OpportunisticGraftThreshold
	  ;; TODO: MeshMessageDeliveriesWindow   = 2 seconds                           #L145
	  *subnet-topicWeight*  ;; topicWeight   = *subnet-topicWeight*                      #L134
	  ;; MeshMessageDeliveriesDecay    = scoreDecay(*subnet-meshDecay* x 1 epoch)  #L142
	  749/1000
	  ;; FirstMessageDeliveriesDecay   = scoreDecay(*subnet-firstDecay* x 1 epoch) #L139
	  316/1000
	  631/1000 ;; behaviourPenaltyDecay
	  ;; MeshFailurePenaltyDecay       = scoreDecay(*subnet-meshDecay* x 1 epoch)  #L148
	  749/1000
	  ;; InvalidMessageDeliveriesDecay = scoreDecay(50 epochs)                     #L150
	  912/1000
	  *decayToZero*
	  *slot-duration*
	  ))

(check= t (paramsp *eth-default-aggregate-subnet-params*))

(defconst *eth-twp*
  `((AGG . (,*eth-default-agg-weights* . ,*eth-default-aggregate-params*))
    (BLOCKS . (,*eth-default-block-weights* . ,*eth-default-block-params*))
	;; We can have 0 or more subnet aggregator topics.  For now, let's assume 3.
    (SUB1 . (,*eth-default-agg-subnet-weights* . ,*eth-default-aggregate-subnet-params*))
    (SUB2 . (,*eth-default-agg-subnet-weights* . ,*eth-default-aggregate-subnet-params*))
    (SUB3 . (,*eth-default-agg-subnet-weights* . ,*eth-default-aggregate-subnet-params*))))

(check= t (twpp *eth-twp*))

;; ---------------------- ATTACK ------------------------------



(set-ignore-ok t)

(skip-proofs
;; emit attack events from p1 to p2, such that p2 appears bad to p3 in the attacked topic top
(definecd emit-appear-bad-evnts (p1 p2 p3 :peer ts ats :lot n m elapsed :nat)
  :loev
  :function-contract-hints (("Goal" :in-theory (enable hbm-evntp evntp loevp)))
  (app
   (emit-meshmsgdeliveries-peer-topics p1 p2 ats m)
   (emit-meshmsgdeliveries-peer-topics p1 p2 (set-difference-equal ts ats) n)
   `((,p3 HBM ,elapsed)
     (,p2 HBM ,elapsed)))))


;; Optimized version of run-network, which only records violations
(skip-proofs
(definecd score-attack-violations (gr :group evnts evnts2 :loev j :nat twpm :twp s :nat acc :lob) :lob
    :ic (is-valid-twp twpm)
    :skip-tests t
    :body-contracts-strictp nil
    (cond
   ;;  ((== (second (car evnts)) 'HBM) gr)
     ((= 100000 j) (cons j (reverse acc)))
     ((endp evnts) (score-attack-violations gr evnts2 evnts2 j twpm s
                                          acc))
     (t (b* (((mv k s) (defdata::genrandom-seed
                         (1- (expt 2 31))
                         (mod s (expt 2 31))))
             (actor (caar evnts))
             (actor-state (lookup-state actor gr))
             (ev (car evnts))
             ((res4 next-actor-state evs) (transition
                                           actor actor-state ev
                                           twpm k))
             (next-actor-events (network-propagator evs nil))
             (newgrp (mset actor next-actor-state gr)))
          (score-attack-violations
           newgrp
           ;;mix generated events with remaining events
           (app next-actor-events (cdr evnts))
           evnts2
           (1+ j)
           twpm
           s
           (cons (take 4 (car evnts)) acc)))))
    )
)

;; Setup a grp for score attack
(defun pgrp ()
  (b* ((init-grp (initialize-group-of-meshpeers '(A B C)
                                                '(A B C)
                                                (topics)
                                                100))
       (astate (mget 'a init-grp))
       (astate (mset :nts
                     (mset :topic-mesh
                           '((sub1 b))
                           (mget :nts astate))
                     astate))
       (bstate (mget 'b init-grp))
       (bstate (mset :nts
                     '((:0tag . nbr-topic-state)
                       (:nbr-topicsubs (agg c a c)
                                       (blocks c a c)
                                       (sub1 c a c)
                                       (sub2 c a c)
                                       (sub3 c a c))
                       (:topic-mesh (agg c c)
                                    (blocks c c)
                                    (sub1 c a c)
                                    (sub2 c)
                                    (sub3 c c)))
                     bstate))
       (cstate (mget 'c init-grp))
       (cstate (mset :nts
                     '((:0tag . nbr-topic-state)
                       (:nbr-topicsubs (agg a b)
                                       (blocks a b)
                                       (sub1 a b)
                                       (sub2 a b)
                                       (sub3 a b))
                       (:topic-mesh (agg b)
                                    (blocks b)
                                    (sub1 b)
                                    (sub2 b)
                                    (sub3 b)))
                     cstate))
       (grp (mset 'a
                  astate
                  (mset 'b
                        bstate
                        (mset 'c
                              cstate
                              init-grp)))))
    grp))






 (time$
 (b* ((attacktopics '(SUB1))
      (grp (pgrp))
      (evnts (emit-appear-bad-evnts 'C 'B 'A (alltopics) attacktopics 20 1 18)))
   (take 1000 (score-attack-violations grp evnts evnts 0 *eth-twp* 42 nil))))




;; (b* ((twpm *eth-twp*)
;;      (gr (pgrp))
;;      (evnts '((b rcv c payload
;; 		 ((:0tag . payload-type)
;; 		  (:content . ba)
;; 		  (:origin . c)
;; 		  (:pid . ba)
;; 		  (:top . sub1)))))
;;      (actor (caar evnts))
;;      (actor-state (lookup-state actor gr))
;;      (ev (car evnts))
;;      ((res4 next-actor-state evs) (transition
;; 				   actor actor-state ev
;; 				   twpm 5))
;;      (next-actor-events (network-propagator evs nil))
;;      (newgrp (mset actor next-actor-state gr)))
;;   next-actor-events)
  
