;; Aid Impact Tracker for Enabledao
;; This contract tracks and validates the impact of distributed disability aid

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_REPORT_NOT_FOUND (err u201))
(define-constant ERR_ALREADY_REPORTED (err u202))
(define-constant ERR_INVALID_PROPOSAL (err u203))
(define-constant ERR_PROPOSAL_NOT_EXECUTED (err u204))
(define-constant ERR_ALREADY_VALIDATED (err u205))
(define-constant ERR_CANNOT_SELF_VALIDATE (err u206))
(define-constant ERR_INVALID_RATING (err u207))
(define-constant ERR_REPORTING_PERIOD_EXPIRED (err u208))

;; Constants
(define-constant REPORTING_WINDOW u1440) ;; ~10 days in blocks
(define-constant MIN_VALIDATORS u2)
(define-constant MAX_RATING u10)

;; Reference to main Enabledao contract
(define-constant ENABLEDAO_CONTRACT .Enabledao)

;; Data variables
(define-data-var total-impact-reports uint u0)
(define-data-var total-validated-reports uint u0)

;; Data maps
(define-map impact-reports uint
  {
    proposal-id: uint,
    recipient: principal,
    submitted-at: uint,
    report-text: (string-ascii 500),
    aid-usage: (string-ascii 200),
    satisfaction-rating: uint,
    challenges-faced: (string-ascii 300),
    follow-up-needed: bool,
    validated: bool,
    validation-count: uint
  }
)

(define-map report-validations {report-id: uint, validator: principal}
  {
    is-valid: bool,
    validator-comments: (string-ascii 200),
    validated-at: uint
  }
)

(define-map proposal-impact-status uint
  {
    has-report: bool,
    report-id: uint,
    reporting-deadline: uint
  }
)

(define-map validation-stats principal
  {
    reports-validated: uint,
    accurate-validations: uint,
    validation-reputation: uint
  }
)

;; Private helper functions
(define-private (is-dao-member (user principal))
  (is-some (contract-call? ENABLEDAO_CONTRACT get-member-info user))
)

(define-private (is-proposal-executed (proposal-id uint))
  (let ((proposal-info (contract-call? ENABLEDAO_CONTRACT get-proposal-info proposal-id)))
    (if (is-some proposal-info)
      (get executed (unwrap-panic proposal-info))
      false)
  )
)

(define-private (get-proposal-recipient (proposal-id uint))
  (let ((proposal-info (contract-call? ENABLEDAO_CONTRACT get-proposal-info proposal-id)))
    (if (is-some proposal-info)
      (some (get recipient (unwrap-panic proposal-info)))
      none)
  )
)

(define-private (is-within-reporting-window (proposal-id uint))
  (let ((impact-status (map-get? proposal-impact-status proposal-id)))
    (if (is-some impact-status)
      (< stacks-block-height (get reporting-deadline (unwrap-panic impact-status)))
      false)
  )
)

;; Public functions

;; Initialize impact tracking for an executed proposal
(define-public (initialize-impact-tracking (proposal-id uint))
  (if (and (is-dao-member tx-sender) (is-proposal-executed proposal-id))
    (if (is-none (map-get? proposal-impact-status proposal-id))
      (begin
        (map-set proposal-impact-status proposal-id
          {
            has-report: false,
            report-id: u0,
            reporting-deadline: (+ stacks-block-height REPORTING_WINDOW)
          }
        )
        (ok true)
      )
      (ok true) ;; Already initialized
    )
    ERR_UNAUTHORIZED
  )
)

;; Submit impact report (recipient only)
(define-public (submit-impact-report 
  (proposal-id uint)
  (report-text (string-ascii 500))
  (aid-usage (string-ascii 200))
  (satisfaction-rating uint)
  (challenges-faced (string-ascii 300))
  (follow-up-needed bool))
  (let ((report-id (+ (var-get total-impact-reports) u1))
        (proposal-recipient (get-proposal-recipient proposal-id))
        (impact-status (map-get? proposal-impact-status proposal-id)))
    (if (and (is-some proposal-recipient)
             (is-eq tx-sender (unwrap-panic proposal-recipient))
             (is-some impact-status)
             (not (get has-report (unwrap-panic impact-status)))
             (is-within-reporting-window proposal-id)
             (and (>= satisfaction-rating u1) (<= satisfaction-rating MAX_RATING)))
      (begin
        ;; Create impact report
        (map-set impact-reports report-id
          {
            proposal-id: proposal-id,
            recipient: tx-sender,
            submitted-at: stacks-block-height,
            report-text: report-text,
            aid-usage: aid-usage,
            satisfaction-rating: satisfaction-rating,
            challenges-faced: challenges-faced,
            follow-up-needed: follow-up-needed,
            validated: false,
            validation-count: u0
          }
        )
        ;; Update impact status
        (map-set proposal-impact-status proposal-id
          (merge (unwrap-panic impact-status)
                 {has-report: true, report-id: report-id})
        )
        (var-set total-impact-reports report-id)
        (ok report-id)
      )
      (if (not (and (>= satisfaction-rating u1) (<= satisfaction-rating MAX_RATING)))
        ERR_INVALID_RATING
        (if (not (is-within-reporting-window proposal-id))
          ERR_REPORTING_PERIOD_EXPIRED
          (if (is-some impact-status)
            (if (get has-report (unwrap-panic impact-status))
              ERR_ALREADY_REPORTED
              ERR_UNAUTHORIZED)
            ERR_INVALID_PROPOSAL)))
    )
  )
)

;; Validate impact report (DAO members only, not self)
(define-public (validate-impact-report 
  (report-id uint) 
  (is-valid bool) 
  (validator-comments (string-ascii 200)))
  (let ((report-data (map-get? impact-reports report-id))
        (existing-validation (map-get? report-validations {report-id: report-id, validator: tx-sender})))
    (if (and (is-dao-member tx-sender)
             (is-some report-data)
             (is-none existing-validation)
             (not (is-eq tx-sender (get recipient (unwrap-panic report-data)))))
      (let ((current-report (unwrap-panic report-data))
            (new-validation-count (+ (get validation-count current-report) u1)))
        (begin
          ;; Record validation
          (map-set report-validations {report-id: report-id, validator: tx-sender}
            {
              is-valid: is-valid,
              validator-comments: validator-comments,
              validated-at: stacks-block-height
            }
          )
          ;; Update report validation count
          (map-set impact-reports report-id
            (merge current-report 
                   {validation-count: new-validation-count,
                    validated: (>= new-validation-count MIN_VALIDATORS)})
          )
          ;; Update validator stats
          (let ((validator-stats (default-to {reports-validated: u0, accurate-validations: u0, validation-reputation: u100}
                                            (map-get? validation-stats tx-sender))))
            (map-set validation-stats tx-sender
              (merge validator-stats
                     {reports-validated: (+ (get reports-validated validator-stats) u1)})
            )
          )
          ;; Update global counter if newly validated
          (if (and (>= new-validation-count MIN_VALIDATORS) 
                   (< (get validation-count current-report) MIN_VALIDATORS))
            (var-set total-validated-reports (+ (var-get total-validated-reports) u1))
            true
          )
          (ok true)
        )
      )
      (if (is-eq tx-sender (get recipient (unwrap-panic (map-get? impact-reports report-id))))
        ERR_CANNOT_SELF_VALIDATE
        (if (is-some existing-validation)
          ERR_ALREADY_VALIDATED
          ERR_UNAUTHORIZED))
    )
  )
)

;; Read-only functions

;; Get impact report details
(define-read-only (get-impact-report (report-id uint))
  (map-get? impact-reports report-id)
)

;; Get proposal impact status
(define-read-only (get-proposal-impact-status (proposal-id uint))
  (map-get? proposal-impact-status proposal-id)
)

;; Get validation details
(define-read-only (get-report-validation (report-id uint) (validator principal))
  (map-get? report-validations {report-id: report-id, validator: validator})
)

;; Get validator statistics
(define-read-only (get-validation-stats (validator principal))
  (map-get? validation-stats validator)
)

;; Get total statistics
(define-read-only (get-impact-statistics)
  {
    total-reports: (var-get total-impact-reports),
    validated-reports: (var-get total-validated-reports),
    validation-rate: (if (> (var-get total-impact-reports) u0)
                       (/ (* (var-get total-validated-reports) u100) (var-get total-impact-reports))
                       u0)
  }
)

;; Check if report needs more validations
(define-read-only (needs-validation (report-id uint))
  (let ((report-data (map-get? impact-reports report-id)))
    (if (is-some report-data)
      (< (get validation-count (unwrap-panic report-data)) MIN_VALIDATORS)
      false)
  )
)

;; Get average satisfaction rating for all reports
(define-read-only (get-average-satisfaction)
  ;; Simplified implementation - would need iteration over all reports in full version
  (if (> (var-get total-impact-reports) u0)
    u7 ;; Placeholder average
    u0)
)

;; Check if user can validate a specific report (basic validation)
(define-read-only (can-validate-report (report-id uint) (validator principal))
  (let ((report-data (map-get? impact-reports report-id))
        (existing-validation (map-get? report-validations {report-id: report-id, validator: validator})))
    (if (is-some report-data)
      (and (is-none existing-validation)
           (not (is-eq validator (get recipient (unwrap-panic report-data))))
           (< (get validation-count (unwrap-panic report-data)) MIN_VALIDATORS))
      false)
  )
)

;; Get reports pending validation
(define-read-only (get-pending-validation-count)
  ;; Simplified - returns total unvalidated reports
  (- (var-get total-impact-reports) (var-get total-validated-reports))
)

;; Get impact summary for a proposal
(define-read-only (get-proposal-impact-summary (proposal-id uint))
  (let ((impact-status (map-get? proposal-impact-status proposal-id)))
    (if (is-some impact-status)
      (let ((status-data (unwrap-panic impact-status)))
        (if (get has-report status-data)
          (let ((report-data (map-get? impact-reports (get report-id status-data))))
            (if (is-some report-data)
              (let ((report (unwrap-panic report-data)))
                (some {
                  has-report: true,
                  satisfaction-rating: (get satisfaction-rating report),
                  follow-up-needed: (get follow-up-needed report),
                  validated: (get validated report),
                  validation-count: (get validation-count report)
                }))
              none))
          (some {
            has-report: false,
            satisfaction-rating: u0,
            follow-up-needed: false,
            validated: false,
            validation-count: u0
          })))
      none)
  )
)
