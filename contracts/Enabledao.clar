(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_PROPOSAL (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_PROPOSAL_EXPIRED (err u103))
(define-constant ERR_PROPOSAL_NOT_EXECUTABLE (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_MEMBER_NOT_FOUND (err u106))
(define-constant ERR_ALREADY_MEMBER (err u107))
(define-constant ERR_INVALID_AMOUNT (err u108))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u109))
(define-constant ERR_CANNOT_DELEGATE_TO_SELF (err u110))
(define-constant ERR_INVALID_DELEGATE (err u111))
(define-constant ERR_DELEGATE_NOT_FOUND (err u112))
(define-constant ERR_NOT_DELEGATED (err u113))
(define-constant ERR_ALREADY_DELEGATED (err u114))
(define-constant ERR_DELEGATE_CHAIN_TOO_LONG (err u115))

(define-data-var next-proposal-id uint u1)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var min-voting-period uint u144)
(define-data-var quorum-threshold uint u3)
(define-data-var max-delegation-chain uint u3)

(define-map members principal 
  {
    verified: bool,
    join-block: uint,
    aid-received: uint,
    proposals-created: uint
  }
)

(define-map proposals uint
  {
    creator: principal,
    title: (string-ascii 64),
    description: (string-ascii 256),
    amount: uint,
    recipient: principal,
    yes-votes: uint,
    no-votes: uint,
    created-at: uint,
    executed: bool,
    vote-end: uint
  }
)

(define-map votes {proposal-id: uint, voter: principal} bool)

(define-map verified-members principal bool)

(define-map delegations principal 
  {
    delegate: principal,
    delegated-at: uint,
    active: bool
  }
)

(define-map delegate-stats principal 
  {
    total-delegators: uint,
    active-delegators: uint,
    first-delegation: uint
  }
)

(define-map delegation-votes {proposal-id: uint, delegate: principal, delegator: principal} bool)

(define-private (is-member (user principal))
  (is-some (map-get? members user))
)

(define-private (is-verified-member (user principal))
  (default-to false (get verified (map-get? members user)))
)

(define-private (get-current-block)
  stacks-block-height
)

(define-private (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes {proposal-id: proposal-id, voter: voter}))
)

(define-private (has-active-delegation (user principal))
  (let ((delegation-data (map-get? delegations user)))
    (if (is-some delegation-data)
      (get active (unwrap-panic delegation-data))
      false)
  )
)



(define-private (calculate-vote-weight (voter principal))
  (let ((member-data (map-get? members voter)))
    (if (is-some member-data)
      (if (get verified (unwrap-panic member-data))
        u2
        u1)
      u0)
  )
)

(define-private (calculate-delegated-weight (delegate principal) (proposal-id uint))
  (let ((delegate-data (map-get? delegate-stats delegate)))
    (if (is-some delegate-data)
      (+ (calculate-vote-weight delegate) (get active-delegators (unwrap-panic delegate-data)))
      (calculate-vote-weight delegate)
    )
  )
)

(define-public (join-dao)
  (let ((current-block (get-current-block)))
    (if (is-member tx-sender)
      ERR_ALREADY_MEMBER
      (begin
        (map-set members tx-sender
          {
            verified: false,
            join-block: current-block,
            aid-received: u0,
            proposals-created: u0
          }
        )
        (var-set total-members (+ (var-get total-members) u1))
        (ok true)
      )
    )
  )
)

(define-public (verify-member (member principal))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (if (is-member member)
      (begin
        (map-set members member
          (merge (unwrap-panic (map-get? members member))
                 {verified: true}
          )
        )
        (map-set verified-members member true)
        (ok true)
      )
      ERR_MEMBER_NOT_FOUND
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (create-proposal (title (string-ascii 64)) 
                               (description (string-ascii 256))
                               (amount uint)
                               (recipient principal))
  (let ((proposal-id (var-get next-proposal-id))
        (current-block (get-current-block))
        (member-data (map-get? members tx-sender)))
    (if (and (is-verified-member tx-sender) (> amount u0))
      (begin
        (map-set proposals proposal-id
          {
            creator: tx-sender,
            title: title,
            description: description,
            amount: amount,
            recipient: recipient,
            yes-votes: u0,
            no-votes: u0,
            created-at: current-block,
            executed: false,
            vote-end: (+ current-block (var-get min-voting-period))
          }
        )
        (map-set members tx-sender
          (merge (unwrap-panic member-data)
                 {proposals-created: (+ (get proposals-created (unwrap-panic member-data)) u1)}
          )
        )
        (var-set next-proposal-id (+ proposal-id u1))
        (ok proposal-id)
      )
      ERR_UNAUTHORIZED
    )
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
  (let ((proposal (map-get? proposals proposal-id))
        (current-block (get-current-block))
        (vote-weight (calculate-vote-weight tx-sender)))
    (if (and (is-some proposal) 
             (is-member tx-sender)
             (not (has-voted proposal-id tx-sender))
             (< current-block (get vote-end (unwrap-panic proposal))))
      (let ((current-proposal (unwrap-panic proposal)))
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} vote-yes)
        (map-set proposals proposal-id
          (merge current-proposal
            {
              yes-votes: (if vote-yes (+ (get yes-votes current-proposal) vote-weight) (get yes-votes current-proposal)),
              no-votes: (if vote-yes (get no-votes current-proposal) (+ (get no-votes current-proposal) vote-weight))
            }
          )
        )
        (ok true)
      )
      (if (>= current-block (get vote-end (unwrap-panic (map-get? proposals proposal-id))))
        ERR_PROPOSAL_EXPIRED
        (if (has-voted proposal-id tx-sender)
          ERR_ALREADY_VOTED
          ERR_INVALID_PROPOSAL
        )
      )
    )
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (map-get? proposals proposal-id))
        (current-block (get-current-block)))
    (if (is-some proposal)
      (let ((current-proposal (unwrap-panic proposal))
            (total-votes (+ (get yes-votes current-proposal) (get no-votes current-proposal))))
        (if (and (>= current-block (get vote-end current-proposal))
                 (not (get executed current-proposal))
                 (>= (get yes-votes current-proposal) (get no-votes current-proposal))
                 (>= total-votes (var-get quorum-threshold))
                 (>= (var-get treasury-balance) (get amount current-proposal)))
          (begin
            (map-set proposals proposal-id
              (merge current-proposal {executed: true})
            )
            (var-set treasury-balance (- (var-get treasury-balance) (get amount current-proposal)))
            (let ((recipient-data (map-get? members (get recipient current-proposal))))
              (if (is-some recipient-data)
                (map-set members (get recipient current-proposal)
                  (merge (unwrap-panic recipient-data)
                         {aid-received: (+ (get aid-received (unwrap-panic recipient-data)) (get amount current-proposal))}
                  )
                )
                true
              )
            )
            (try! (as-contract (stx-transfer? (get amount current-proposal) tx-sender (get recipient current-proposal))))
            (ok true)
          )
          ERR_PROPOSAL_NOT_EXECUTABLE
        )
      )
      ERR_PROPOSAL_NOT_FOUND
    )
  )
)

(define-public (fund-treasury (amount uint))
  (if (> amount u0)
    (begin
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (var-set treasury-balance (+ (var-get treasury-balance) amount))
      (ok true)
    )
    ERR_INVALID_AMOUNT
  )
)

(define-public (emergency-withdraw (amount uint))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (if (and (> amount u0) (>= (var-get treasury-balance) amount))
      (begin
        (var-set treasury-balance (- (var-get treasury-balance) amount))
        (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
        (ok true)
      )
      ERR_INSUFFICIENT_FUNDS
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (update-voting-period (new-period uint))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (begin
      (var-set min-voting-period new-period)
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (update-quorum (new-quorum uint))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (begin
      (var-set quorum-threshold new-quorum)
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (delegate-voting-power (delegate principal))
  (let ((current-block (get-current-block))
        (existing-delegation (map-get? delegations tx-sender))
        (delegate-member-data (map-get? members delegate)))
    (if (and (is-member tx-sender) (is-member delegate) (not (is-eq tx-sender delegate)))
      (if (is-none existing-delegation)
        (begin
          (map-set delegations tx-sender
            {
              delegate: delegate,
              delegated-at: current-block,
              active: true
            }
          )
          (let ((current-stats (default-to {total-delegators: u0, active-delegators: u0, first-delegation: current-block} 
                                          (map-get? delegate-stats delegate))))
            (map-set delegate-stats delegate
              {
                total-delegators: (+ (get total-delegators current-stats) u1),
                active-delegators: (+ (get active-delegators current-stats) u1),
                first-delegation: (if (is-eq (get total-delegators current-stats) u0) 
                                   current-block 
                                   (get first-delegation current-stats))
              }
            )
          )
          (ok true)
        )
        ERR_ALREADY_DELEGATED
      )
      (if (is-eq tx-sender delegate)
        ERR_CANNOT_DELEGATE_TO_SELF
        ERR_INVALID_DELEGATE
      )
    )
  )
)

(define-public (revoke-delegation)
  (let ((delegation-data (map-get? delegations tx-sender)))
    (if (is-some delegation-data)
      (let ((current-delegation (unwrap-panic delegation-data))
            (delegate (get delegate current-delegation)))
        (if (get active current-delegation)
          (begin
            (map-set delegations tx-sender
              (merge current-delegation {active: false})
            )
            (let ((current-stats (unwrap-panic (map-get? delegate-stats delegate))))
              (map-set delegate-stats delegate
                (merge current-stats 
                       {active-delegators: (- (get active-delegators current-stats) u1)})
              )
            )
            (ok true)
          )
          ERR_NOT_DELEGATED
        )
      )
      ERR_NOT_DELEGATED
    )
  )
)

(define-public (vote-as-delegate (proposal-id uint) (vote-yes bool) (delegators (list 50 principal)))
  (let ((proposal (map-get? proposals proposal-id))
        (current-block (get-current-block)))
    (if (and (is-some proposal) 
             (is-member tx-sender)
             (< current-block (get vote-end (unwrap-panic proposal))))
      (let ((delegate-weight (calculate-delegated-weight tx-sender proposal-id)))
        (if (> delegate-weight u0)
          (begin
            (map-set votes {proposal-id: proposal-id, voter: tx-sender} vote-yes)
            (fold process-delegator-vote delegators 
                  {proposal-id: proposal-id, delegate: tx-sender, vote: vote-yes, success: true})
            (let ((current-proposal (unwrap-panic proposal)))
              (map-set proposals proposal-id
                (merge current-proposal
                  {
                    yes-votes: (if vote-yes (+ (get yes-votes current-proposal) delegate-weight) (get yes-votes current-proposal)),
                    no-votes: (if vote-yes (get no-votes current-proposal) (+ (get no-votes current-proposal) delegate-weight))
                  }
                )
              )
            )
            (ok true)
          )
          ERR_UNAUTHORIZED
        )
      )
      ERR_INVALID_PROPOSAL
    )
  )
)

(define-private (process-delegator-vote (delegator principal) (context {proposal-id: uint, delegate: principal, vote: bool, success: bool}))
  (let ((delegation-data (map-get? delegations delegator))
        (proposal-id (get proposal-id context))
        (delegate (get delegate context))
        (vote (get vote context)))
    (if (and (is-some delegation-data) 
             (get active (unwrap-panic delegation-data))
             (is-eq (get delegate (unwrap-panic delegation-data)) delegate))
      (map-set delegation-votes {proposal-id: proposal-id, delegate: delegate, delegator: delegator} vote)
      false
    )
    context
  )
)

(define-read-only (get-member-info (member principal))
  (map-get? members member)
)

(define-read-only (get-proposal-info (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-total-members)
  (var-get total-members)
)

(define-read-only (get-voting-period)
  (var-get min-voting-period)
)

(define-read-only (get-quorum-threshold)
  (var-get quorum-threshold)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (is-proposal-active (proposal-id uint))
  (let ((proposal (map-get? proposals proposal-id)))
    (if (is-some proposal)
      (let ((current-proposal (unwrap-panic proposal)))
        (and (< (get-current-block) (get vote-end current-proposal))
             (not (get executed current-proposal)))
      )
      false
    )
  )
)

(define-read-only (can-execute-proposal (proposal-id uint))
  (let ((proposal (map-get? proposals proposal-id)))
    (if (is-some proposal)
      (let ((current-proposal (unwrap-panic proposal))
            (total-votes (+ (get yes-votes current-proposal) (get no-votes current-proposal))))
        (and (>= (get-current-block) (get vote-end current-proposal))
             (not (get executed current-proposal))
             (>= (get yes-votes current-proposal) (get no-votes current-proposal))
             (>= total-votes (var-get quorum-threshold))
             (>= (var-get treasury-balance) (get amount current-proposal)))
      )
      false
    )
  )
)

(define-read-only (get-delegation-info (delegator principal))
  (map-get? delegations delegator)
)

(define-read-only (get-delegate-stats (delegate principal))
  (map-get? delegate-stats delegate)
)

(define-read-only (get-effective-vote-weight (voter principal))
  (if (has-active-delegation voter)
    u0
    (calculate-vote-weight voter)
  )
)

(define-read-only (get-delegate-vote-weight (delegate principal))
  (let ((delegate-data (map-get? delegate-stats delegate)))
    (if (is-some delegate-data)
      (+ (calculate-vote-weight delegate) (get active-delegators (unwrap-panic delegate-data)))
      (calculate-vote-weight delegate)
    )
  )
)

(define-read-only (is-delegated-to (delegator principal) (delegate principal))
  (let ((delegation-data (map-get? delegations delegator)))
    (if (is-some delegation-data)
      (let ((current-delegation (unwrap-panic delegation-data)))
        (and (get active current-delegation) (is-eq (get delegate current-delegation) delegate))
      )
      false
    )
  )
)

(define-read-only (get-delegation-vote (proposal-id uint) (delegate principal) (delegator principal))
  (map-get? delegation-votes {proposal-id: proposal-id, delegate: delegate, delegator: delegator})
)

(define-read-only (get-max-delegation-chain)
  (var-get max-delegation-chain)
)
