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
(define-constant ERR_INSUFFICIENT_REPUTATION (err u116))
(define-constant ERR_INVALID_REPUTATION_SCORE (err u117))
(define-constant ERR_REPUTATION_UPDATE_FAILED (err u118))

(define-data-var next-proposal-id uint u1)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var min-voting-period uint u144)
(define-data-var quorum-threshold uint u3)
(define-data-var max-delegation-chain uint u3)
(define-data-var reputation-decay-rate uint u50)
(define-data-var reputation-boost-multiplier uint u200)

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

(define-map reputation-scores principal
  {
    score: uint,
    participation-count: uint,
    successful-proposals: uint,
    failed-proposals: uint,
    last-activity: uint,
    positive-votes: uint,
    total-votes: uint,
    treasury-contributions: uint,
    reputation-level: uint
  }
)

(define-map reputation-history {member: principal, block: uint}
  {
    action: (string-ascii 20),
    score-change: int,
    new-score: uint
  }
)

(define-map proposal-outcomes uint
  {
    creator: principal,
    passed: bool,
    final-yes-votes: uint,
    final-no-votes: uint,
    execution-block: uint
  }
)

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



(define-private (get-reputation-level (score uint))
  (if (>= score u1000) u5
    (if (>= score u500) u4
      (if (>= score u200) u3
        (if (>= score u100) u2
          u1))))
)

(define-private (calculate-reputation-multiplier (member principal))
  (let ((reputation-data (map-get? reputation-scores member)))
    (if (is-some reputation-data)
      (let ((level (get reputation-level (unwrap-panic reputation-data))))
        (+ u100 (* level u25)))
      u100)
  )
)

(define-private (update-reputation-score (member principal) (action (string-ascii 20)) (score-change int))
  (let ((current-block (get-current-block))
        (current-reputation (default-to {score: u100, participation-count: u0, successful-proposals: u0, 
                                       failed-proposals: u0, last-activity: u0, positive-votes: u0, 
                                       total-votes: u0, treasury-contributions: u0, reputation-level: u1} 
                                      (map-get? reputation-scores member))))
    (let ((current-score (get score current-reputation))
          (new-score (if (< score-change 0)
                       (let ((abs-change (to-uint (- 0 score-change))))
                         (if (>= current-score abs-change)
                           (- current-score abs-change)
                           u0))
                       (+ current-score (to-uint score-change))))
          (new-level (get-reputation-level new-score)))
      (map-set reputation-scores member
        (merge current-reputation 
               {score: new-score, 
                last-activity: current-block, 
                reputation-level: new-level}))
      (map-set reputation-history {member: member, block: current-block}
        {action: action, score-change: score-change, new-score: new-score})
      (ok new-score)
    )
  )
)

(define-private (calculate-vote-weight (voter principal))
  (let ((member-data (map-get? members voter))
        (reputation-multiplier (calculate-reputation-multiplier voter)))
    (if (is-some member-data)
      (let ((base-weight (if (get verified (unwrap-panic member-data)) u2 u1)))
        (/ (* base-weight reputation-multiplier) u100))
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
        (map-set reputation-scores tx-sender
          {
            score: u100,
            participation-count: u0,
            successful-proposals: u0,
            failed-proposals: u0,
            last-activity: current-block,
            positive-votes: u0,
            total-votes: u0,
            treasury-contributions: u0,
            reputation-level: u1
          }
        )
        (var-set total-members (+ (var-get total-members) u1))
        (let ((reputation-result (update-reputation-score tx-sender "join" 0)))
          (ok true))
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
        (let ((reputation-result (update-reputation-score member "verification" 50)))
          (ok true))
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
        (let ((reputation-result (update-reputation-score tx-sender "create-proposal" 10)))
          (ok proposal-id))
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
        (let ((current-reputation (unwrap-panic (map-get? reputation-scores tx-sender))))
          (map-set reputation-scores tx-sender
            (merge current-reputation
                   {participation-count: (+ (get participation-count current-reputation) u1),
                    total-votes: (+ (get total-votes current-reputation) u1),
                    positive-votes: (if vote-yes (+ (get positive-votes current-reputation) u1) (get positive-votes current-reputation))}))
        )
        (let ((reputation-result (update-reputation-score tx-sender "vote" 5)))
          (ok true))
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
            (map-set proposal-outcomes proposal-id
              {
                creator: (get creator current-proposal),
                passed: true,
                final-yes-votes: (get yes-votes current-proposal),
                final-no-votes: (get no-votes current-proposal),
                execution-block: current-block
              }
            )
            (let ((creator-reputation (unwrap-panic (map-get? reputation-scores (get creator current-proposal)))))
              (map-set reputation-scores (get creator current-proposal)
                (merge creator-reputation
                       {successful-proposals: (+ (get successful-proposals creator-reputation) u1)}))
            )
            (let ((reputation-result (update-reputation-score (get creator current-proposal) "proposal-passed" 50)))
              (ok true))
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
      (let ((current-reputation (default-to {score: u100, participation-count: u0, successful-proposals: u0, 
                                            failed-proposals: u0, last-activity: u0, positive-votes: u0, 
                                            total-votes: u0, treasury-contributions: u0, reputation-level: u1} 
                                           (map-get? reputation-scores tx-sender))))
        (map-set reputation-scores tx-sender
          (merge current-reputation
                 {treasury-contributions: (+ (get treasury-contributions current-reputation) amount)}))
      )
      (let ((reputation-result (update-reputation-score tx-sender "fund-treasury" (to-int (/ amount u100000)))))
        (ok true))
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

(define-public (update-reputation-manually (member principal) (score-change int) (reason (string-ascii 20)))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (if (is-member member)
      (update-reputation-score member reason score-change)
      ERR_MEMBER_NOT_FOUND
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (decay-reputation (member principal))
  (if (is-member member)
    (let ((current-reputation (map-get? reputation-scores member))
          (current-block (get-current-block)))
      (if (is-some current-reputation)
        (let ((reputation-data (unwrap-panic current-reputation))
              (blocks-since-activity (- current-block (get last-activity reputation-data))))
          (if (> blocks-since-activity u1440)
            (let ((decay-amount (to-int (- u0 (/ (var-get reputation-decay-rate) u10)))))
              (update-reputation-score member "decay" decay-amount)
            )
            (ok (get score reputation-data))
          )
        )
        (ok u100)
      )
    )
    ERR_MEMBER_NOT_FOUND
  )
)

(define-public (boost-reputation-for-activity (member principal))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (if (is-member member)
      (let ((current-reputation (map-get? reputation-scores member)))
        (if (is-some current-reputation)
          (let ((reputation-data (unwrap-panic current-reputation))
                (activity-bonus (to-int (/ (* (get participation-count reputation-data) (var-get reputation-boost-multiplier)) u100))))
            (update-reputation-score member "activity-boost" activity-bonus)
          )
          (ok u100)
        )
      )
      ERR_MEMBER_NOT_FOUND
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (batch-update-reputation (member-list (list 20 principal)) (score-changes (list 20 int)) (reason (string-ascii 20)))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (if (is-eq (len member-list) (len score-changes))
      (begin
        (fold batch-reputation-updater 
              member-list 
              {reason: reason, success: true, scores: score-changes, index: u0})
        (ok true)
      )
      ERR_INVALID_REPUTATION_SCORE
    )
    ERR_UNAUTHORIZED
  )
)

(define-private (batch-reputation-updater (member principal) (context {reason: (string-ascii 20), success: bool, scores: (list 20 int), index: uint}))
  (let ((score-change (default-to 0 (element-at (get scores context) (get index context))))
        (reason (get reason context)))
    (let ((reputation-result (if (is-member member)
                               (update-reputation-score member reason score-change)
                               (ok u0))))
      {reason: reason, success: true, scores: (get scores context), index: (+ (get index context) u1)})
  )
)

(define-public (penalize-failed-proposal (proposal-id uint))
  (let ((proposal-outcome (map-get? proposal-outcomes proposal-id)))
    (if (and (is-some proposal-outcome) (is-eq tx-sender CONTRACT_OWNER))
      (let ((outcome-data (unwrap-panic proposal-outcome))
            (creator (get creator outcome-data)))
        (if (not (get passed outcome-data))
          (begin
            (let ((creator-reputation (unwrap-panic (map-get? reputation-scores creator))))
              (map-set reputation-scores creator
                (merge creator-reputation
                       {failed-proposals: (+ (get failed-proposals creator-reputation) u1)}))
            )
            (let ((reputation-result (update-reputation-score creator "proposal-failed" -20)))
              (ok true))
          )
          (ok true)
        )
      )
      ERR_PROPOSAL_NOT_FOUND
    )
  )
)

(define-read-only (get-reputation-score (member principal))
  (map-get? reputation-scores member)
)

(define-read-only (get-reputation-history (member principal) (block uint))
  (map-get? reputation-history {member: member, block: block})
)

(define-read-only (get-proposal-outcome (proposal-id uint))
  (map-get? proposal-outcomes proposal-id)
)

(define-read-only (calculate-reputation-weight (member principal))
  (let ((base-weight (calculate-vote-weight member))
        (reputation-data (map-get? reputation-scores member)))
    (if (is-some reputation-data)
      (let ((level (get reputation-level (unwrap-panic reputation-data))))
        (+ base-weight (/ level u2)))
      base-weight)
  )
)

(define-read-only (get-reputation-level-name (level uint))
  (if (is-eq level u5) "Expert"
    (if (is-eq level u4) "Advanced"
      (if (is-eq level u3) "Intermediate"
        (if (is-eq level u2) "Member"
          "Newcomer"))))
)

(define-read-only (get-member-reputation-summary (member principal))
  (let ((reputation-data (map-get? reputation-scores member))
        (member-data (map-get? members member)))
    (if (and (is-some reputation-data) (is-some member-data))
      (let ((rep-data (unwrap-panic reputation-data))
            (mem-data (unwrap-panic member-data)))
        (some {
          score: (get score rep-data),
          level: (get reputation-level rep-data),
          level-name: (get-reputation-level-name (get reputation-level rep-data)),
          participation-count: (get participation-count rep-data),
          successful-proposals: (get successful-proposals rep-data),
          failed-proposals: (get failed-proposals rep-data),
          positive-vote-ratio: (if (> (get total-votes rep-data) u0) 
                                 (/ (* (get positive-votes rep-data) u100) (get total-votes rep-data)) 
                                 u0),
          treasury-contributions: (get treasury-contributions rep-data),
          is-verified: (get verified mem-data),
          current-vote-weight: (calculate-reputation-weight member)
        })
      )
      none
    )
  )
)

(define-read-only (get-top-members-by-reputation (limit uint))
  (if (<= limit u50)
    (ok "Feature not implemented in this version")
    ERR_INVALID_REPUTATION_SCORE
  )
)


