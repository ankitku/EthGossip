(in-package "ACL2S")
(include-book "utils")
(include-book "network")
(include-book "attacks/attack")

;;---------------------------------------------------------------------------------
;; Setting custom enumerators for tctrs
;;---------------------------------------------------------------------------------
;;high values
(defdata lows (range integer (0 <= _ <= 1)))

;;low values
(defdata highs (range integer (300 < _ <= 400)))

(defdata-subtype lows nat)
(defdata-subtype highs nat)

;; setting badbehaviour as 0
(defun nth-bad-counters-custom (n)
  (tctrs 0 ;;setting invalid msg deliveries to 0, as penalty is too high
         (nth-lows (+ n 3)) 
         42
         (nth-lows (+ n 4))
         0))
;; for our scenario, meshfailurepenalty must be 0 since it is only incremented
;; when peer leaves.

;; keeping bad behaviors 0 as it drops score too much
(defun nth-good-counters-custom (n)
  (tctrs 0 (nth-highs (+ n 2)) (nth-highs (+ n 3)) (nth-highs (+ n 4)) 0))

(defun nth-counters-custom (n)
  (if (== 0 (mod n 4))
      (nth-bad-counters-custom n)
    (nth-good-counters-custom n)))

(property nth-pt-tctrs-mapp (n :nat)
          :proofs? nil
          (pt-tctrs-mapp (nth-pt-tctrs-map n)))

(defdata-attach tctrs :enumerator nth-counters-custom)

(set-ignore-ok t)
(defun nth-glb-counters-custom (n)
  (declare (irrelevant n))
  (gctrs 0 0 0))

(property nth-p-gctrs-mapp (n :nat)
          :proofs? nil
          (p-gctrs-mapp (nth-p-gctrs-map n)))

(defdata-attach gctrs :enumerator nth-glb-counters-custom)


(defun nth-good-params-custom (n)
  (declare (irrelevant n))
  '((:0tag . params)
    (:activationwindow . 1)
    (:behaviourpenaltydecay . 0)
    (:d . 0)
    (:decayinterval . 1)
    (:decaytozero . 0)
    (:dhigh . 0)
    (:dlazy . 0)
    (:dlow . 0)
    (:fanoutttl . 1)
    (:firstmessagedeliveriesdecay . 0)
    (:graylistthreshold . 0)
    (:hbminterval . 1)
    (:invalidmessagedeliveriesdecay . 0)
    (:mcachegsp . 1)
    (:mcachelen . 1)
    (:meshfailurepenaltydecay . 0)
    (:meshmessagedeliveriescap . 2)
    (:meshmessagedeliveriesdecay . 0)
    (:meshmessagedeliveriesthreshold . 1)
    (:meshtimequantum . 1)
    (:opportunisticgraftthreshold . 1)
    (:p2cap . 5)
    (:seenttl . 1)
    (:timequantainmeshcap . 2)
    (:topiccap . 5)
    (:topicweight . 1)))

;---------------------------------------------------------------------------------

;; Every message received will increase mmd counter
;; (property forward-counter (orig p self :peer topic :topic nts :nbr-topic-state mst :msgs-state
;;                            nbr-tctrs :pt-tctrs-map nbr-gctrs :p-gctrs-map
;;                            nbr-scores :peer-rational-map pld :payload-type
;;                            twpm :twp s :nat)
;;   :proofs? nil
;;   :check-contracts? nil
;;   :h (mget topic twpm)
;;   (< (tctrs-meshMessageDeliveries (lookup-tctrs p topic nbr-tctrs))
;;      (tctrs-meshMessageDeliveries
;;            (lookup-tctrs
;;             p
;;             topic
;;             (peer-state-nbr-tctrs
;;                   (res4-pst
;;                         (transition
;;                          self
;;                          (peer-state nts mst nbr-tctrs nbr-gctrs nbr-scores)
;;                          `(,self RCV ,p PAYLOAD ,(payload-type 'XX 'XX topic orig))
;;                          twpm
;;                          s)))))))


(property forward-counter2 (P O V :peer gr :group topic :topic
                              pld :payload-type twpm :twp s :nat)
  :proofs? nil
  :check-contracts? nil
  :h (is-valid-twp twpm)
  (b* ((evnts `((,O RCV ,V PAYLOAD ,(payload-type 'XX 'XX topic P))))
       (peer-state (lookup-state O gr))
       (nbr-tctrs (peer-state-nbr-tctrs peer-state)))
    (= (1+ (tctrs-meshMessageDeliveries
            (lookup-tctrs V topic nbr-tctrs)))
       (tctrs-meshMessageDeliveries
        (lookup-tctrs V
                      topic
                      (peer-state-nbr-tctrs
                       (lookup-state
                        O
                        (cdar
                         (run-network gr evnts 1 twpm s)))))))))




;; Pete : write b* in rev, which will be clearer
;; variables corresponding to state, make more sense
;; or use generate accessor

;; Pete : just call topic topic
;; ntst not shown to be associated.
;; So, we want every attacked topic message received to be forwarded.
;; Conditions required for a message to be forwarded
;; (property msg-forwards (self recvp origp fwdp :peer topic :topic ntst :nbr-topic-state s d :nat)
;;   :proofs? nil
;;   :check-contracts? nil
;;   :h (^
;;       (!= recvp fwdp)
;;       (!= self fwdp)
;;       (!= fwdp origp)
;;       (!= self origp)
;;       )
;;   (b* ((mesh-nbrs (mget topic (nbr-topic-state-topic-mesh ntst)))
;;        (fix-fwd-mesh-nbrs (mset topic (cons fwdp mesh-nbrs)
;;                                 (nbr-topic-state-topic-mesh ntst)))
;;        ;;fix fwdp as a peer in self's topic mesh
;;        (new-ntst (mset :topic-mesh fix-fwd-mesh-nbrs ntst)))
;;     (consp (mget :evs (forward-emission self
;;                                         `(,self RCV ,recvp PAYLOAD
;;                                                 ,(payload-type 'XX 'XX topic
;;                                                                origp))
;;                                         new-ntst
;;                                         nil s d)))))



;; Conditions for Forwarding of Messages to Happen in an AVO gadget.
(property msg-forwards (G A V O :peer pid :pid-type topic :topic 
                           twpm :twp s :nat)
  :proofs? nil
  :check-contracts? nil
  (let (;; initialize network gr where V and O are mesh neighbors
        ;; in the attacked topic, degree of each peer is set to 1
        (gr (initialize-group-of-meshpeers `(,V ,O) `(,V ,O) `(,topic) 1))
        ;; message m in topic "topic" originated at G
        (m (payload-type 'CONTENT pid topic G)))
    (=> (^ (is-valid-twp twpm)
           ;; m not already processed by V
           (! (in (payload2pid m)
                  (map* rs->pids
                        (acl2::alist-keys
                         (msgs-state-recently-seen
                          (peer-state-mst (mget V gr)))))))
           (!= A O)
           (!= V O)
           (!= O G)
           (!= V G))
        (in `(,O RCV ,V PAYLOAD ,m)
            (acl2::alist-keys
             (run-network gr `((,V RCV ,A PAYLOAD ,m)) 10 twpm s))))))

;; is msg-forwards used subsequently?


;; instead of cons, have record having weights and params
;; records for structured data


;; Do we want strict inequalities? Check for each condition. Weakening any hyp
;; should gen counterexamples.


(defdata-attach tctrs :enumerator nth-good-counters-custom)
(defdata-attach params :enumerator nth-good-params-custom)

;; Decreasing mmd will lower the score
(property dec-mmd-calcScore (p :peer topic :topic meshFailurePenalty imd mmd1
  mmd2 mt fmd mfp :non-neg-rational pt-ctrs :pt-tctrs-map gbmap :p-gctrs-map
  twpm :twp)
  :check-contracts? nil
  :proofs? nil
  :testing-timeout 600
  (let ((params (mget topic twpm)))
    (=> (^ (is-valid-twp twpm)       
           (>= (params-meshmessagedeliveriescap params)
               (params-meshmessagedeliveriesthreshold params))
           (> mt (params-activationWindow params))
           ;topic-cap > maximum possible score
           (> (params-topiccap params) 100000)
           (< mmd2 (params-meshmessagedeliveriesthreshold params))
           (>= mmd1 (params-meshmessagedeliveriesthreshold params)))
  (> (mget p (calc-nbr-scores-map
              (mset `(,p . ,topic) (tctrs imd mmd1 mt fmd mfp) pt-ctrs)
              gbmap
              twpm))
     (mget p (calc-nbr-scores-map
              (mset `(,p . ,topic) (tctrs imd mmd2 mt fmd mfp) pt-ctrs)
              gbmap
              twpm))))))




(include-book "eth2-twp")


;; We fix twp to that of Eth, to reduce the state space.
;; It is possible to decrease the peer score below 0, by just reducing mmd 
(property dec-mmd-calcScore2 (p :peer topic :topic mmd mt fmd :non-neg-rational)
  :check-contracts? nil
  :proofs? nil
  (>= (mget p (calc-nbr-scores-map
               (mset `(,p . ,topic) (tctrs 0 mmd mt fmd 0) '())
               nil
               *eth-twp*))
      0))


;; when writing topic down, convince that need each of the hyp and map to the
;; attack
;; have some properties and counterexamples to convince
;; 


(include-book "network")


(property gcmap-const-nbr-topic-state  (nts :nbr-topic-state
       nbr-scores :peer-rational-map
       tcmap :pt-tctrs-map
       gcmap :p-gctrs-map
       evnt :evnt
       twpm :twp
       s :nat)
  :h (is-valid-twp twpm)
  (== (mget :gcm (update-nbr-topic-state nts nbr-scores tcmap gcmap evnt twpm
                                         s))
      gcmap)
  :hints (("Goal" :in-theory (enable update-nbr-topic-state
                                     update-nbr-topic-state1
                                     update-nbr-topic-state2
                                     update-nbr-topic-state3))))


(property gcmap-const-update-msgs-state (mst :msgs-state evnt :evnt
                                             pcmap :pt-tctrs-map
                                             gcmap :p-gctrs-map
                                             twpm :twp)
  (== (mget :gcm (update-msgs-state mst evnt pcmap gcmap twpm))
      gcmap)
  :hints (("Goal" :in-theory (enable update-msgs-state
                                     update-msgs-state1
                                     update-msgs-state2))))


(property transition-gcmap-const (self :peer pstate :peer-state evnt :evnt twpm
                                       :twp s :nat)
  :h (is-valid-twp twpm)
  (== (mget :nbr-gctrs (mget :pst (transition self pstate evnt twpm s)))
      (mget :nbr-gctrs pstate))
  :hints (("Goal" :in-theory (enable transition))))

