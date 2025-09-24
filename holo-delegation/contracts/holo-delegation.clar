;; HoloDelegate - Liquid Democracy Governance Platform
;; A simplified implementation of multi-dimensional delegation and voting

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-PROPOSAL-ENDED (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-INVALID-DELEGATION (err u104))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u105))

;; Expertise domains
(define-constant DOMAIN-TECHNICAL u1)
(define-constant DOMAIN-FINANCIAL u2)
(define-constant DOMAIN-LEGAL u3)
(define-constant DOMAIN-COMMUNITY u4)

;; Data Variables
(define-data-var proposal-counter uint u0)

;; Data Maps
;; Proposals: id -> {creator, title, description, domain, end-block, yes-votes, no-votes, executed}
(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    domain: uint,
    end-block: uint,
    yes-votes: uint,
    no-votes: uint,
    executed: bool
  })

;; User delegations: {delegator, domain} -> delegate
(define-map delegations
  { delegator: principal, domain: uint }
  { delegate: principal })

;; User expertise scores: {user, domain} -> score
(define-map expertise-scores
  { user: principal, domain: uint }
  { score: uint })

;; User voting power: user -> power
(define-map voting-power
  { user: principal }
  { power: uint })

;; Vote records: {proposal-id, voter} -> {vote, power-used}
(define-map vote-records
  { proposal-id: uint, voter: principal }
  { vote: bool, power-used: uint })

;; Delegated voting power: {proposal-id, delegate} -> total-power
(define-map delegated-power
  { proposal-id: uint, delegate: principal }
  { total-power: uint })

;; Public Functions

;; Initialize user voting power
(define-public (set-voting-power (user principal) (power uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set voting-power { user: user } { power: power })
    (ok true)))

;; Set expertise score for a user in a domain
(define-public (set-expertise-score (user principal) (domain uint) (score uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set expertise-scores { user: user, domain: domain } { score: score })
    (ok true)))

;; Delegate voting power in a specific domain
(define-public (delegate-power (domain uint) (delegate principal))
  (begin
    (asserts! (not (is-eq tx-sender delegate)) ERR-INVALID-DELEGATION)
    (map-set delegations 
      { delegator: tx-sender, domain: domain } 
      { delegate: delegate })
    (ok true)))

;; Remove delegation in a specific domain
(define-public (remove-delegation (domain uint))
  (begin
    (map-delete delegations { delegator: tx-sender, domain: domain })
    (ok true)))

;; Create a new proposal
(define-public (create-proposal 
  (title (string-ascii 100)) 
  (description (string-ascii 500)) 
  (domain uint) 
  (voting-period uint))
  (let
    ((proposal-id (+ (var-get proposal-counter) u1))
     (end-block (+ block-height voting-period)))
    (map-set proposals
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        domain: domain,
        end-block: end-block,
        yes-votes: u0,
        no-votes: u0,
        executed: false
      })
    (var-set proposal-counter proposal-id)
    (ok proposal-id)))

;; Cast a direct vote on a proposal
(define-public (vote (proposal-id uint) (support bool))
  (let
    ((proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
     (user-power (default-to u0 (get power (map-get? voting-power { user: tx-sender }))))
     (expertise-multiplier (calculate-expertise-multiplier tx-sender (get domain proposal-data))))
    
    ;; Check if proposal is still active
    (asserts! (< block-height (get end-block proposal-data)) ERR-PROPOSAL-ENDED)
    
    ;; Check if user hasn't voted yet
    (asserts! (is-none (map-get? vote-records { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    ;; Check if user has voting power
    (asserts! (> user-power u0) ERR-INSUFFICIENT-VOTING-POWER)
    
    (let
      ((effective-power (* user-power expertise-multiplier))
       (updated-yes-votes (if support 
                           (+ (get yes-votes proposal-data) effective-power)
                           (get yes-votes proposal-data)))
       (updated-no-votes (if support 
                          (get no-votes proposal-data)
                          (+ (get no-votes proposal-data) effective-power))))
      
      ;; Record the vote
      (map-set vote-records
        { proposal-id: proposal-id, voter: tx-sender }
        { vote: support, power-used: effective-power })
      
      ;; Update proposal vote counts
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal-data 
          { yes-votes: updated-yes-votes, no-votes: updated-no-votes }))
      
      (ok effective-power))))

;; Vote on behalf of delegators (delegate voting)
(define-public (delegate-vote (proposal-id uint) (support bool))
  (let
    ((proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
     (total-delegated-power (calculate-delegated-power tx-sender (get domain proposal-data))))
    
    ;; Check if proposal is still active
    (asserts! (< block-height (get end-block proposal-data)) ERR-PROPOSAL-ENDED)
    
    ;; Check if delegate has voting power
    (asserts! (> total-delegated-power u0) ERR-INSUFFICIENT-VOTING-POWER)
    
    (let
      ((updated-yes-votes (if support 
                           (+ (get yes-votes proposal-data) total-delegated-power)
                           (get yes-votes proposal-data)))
       (updated-no-votes (if support 
                          (get no-votes proposal-data)
                          (+ (get no-votes proposal-data) total-delegated-power))))
      
      ;; Record delegated power used
      (map-set delegated-power
        { proposal-id: proposal-id, delegate: tx-sender }
        { total-power: total-delegated-power })
      
      ;; Update proposal vote counts
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal-data 
          { yes-votes: updated-yes-votes, no-votes: updated-no-votes }))
      
      (ok total-delegated-power))))

;; Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    ((proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND)))
    
    ;; Check if proposal has ended
    (asserts! (>= block-height (get end-block proposal-data)) ERR-PROPOSAL-ENDED)
    
    ;; Check if proposal hasn't been executed
    (asserts! (not (get executed proposal-data)) ERR-ALREADY-VOTED)
    
    ;; Check if proposal passed (simple majority for now)
    (asserts! (> (get yes-votes proposal-data) (get no-votes proposal-data)) ERR-NOT-AUTHORIZED)
    
    ;; Mark as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal-data { executed: true }))
    
    (ok true)))

;; Read-only Functions

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id }))

;; Get user's delegation for a domain
(define-read-only (get-delegation (delegator principal) (domain uint))
  (map-get? delegations { delegator: delegator, domain: domain }))

;; Get user's expertise score in a domain
(define-read-only (get-expertise-score (user principal) (domain uint))
  (default-to u1 (get score (map-get? expertise-scores { user: user, domain: domain }))))

;; Get user's voting power
(define-read-only (get-voting-power (user principal))
  (default-to u0 (get power (map-get? voting-power { user: user }))))

;; Get vote record for a user on a proposal
(define-read-only (get-vote-record (proposal-id uint) (voter principal))
  (map-get? vote-records { proposal-id: proposal-id, voter: voter }))

;; Check if proposal has passed
(define-read-only (proposal-passed? (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal-data (> (get yes-votes proposal-data) (get no-votes proposal-data))
    false))

;; Private Functions

;; Calculate expertise multiplier for a user in a domain
(define-private (calculate-expertise-multiplier (user principal) (domain uint))
  (let
    ((expertise-score (get-expertise-score user domain)))
    ;; Simple multiplier: 1x for score 1-3, 1.5x for score 4-6, 2x for score 7+
    (if (<= expertise-score u3)
      u1
      (if (<= expertise-score u6)
        u150  ;; 1.5x represented as 150/100
        u200)))) ;; 2x represented as 200/100

;; Calculate total delegated power for a delegate in a domain
(define-private (calculate-delegated-power (delegate principal) (domain uint))
  ;; This is simplified - in a full implementation, you'd iterate through all delegators
  ;; For now, return a fixed value or implement a more complex system
  u10)

;; Get current proposal counter
(define-read-only (get-proposal-counter)
  (var-get proposal-counter))