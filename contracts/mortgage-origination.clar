
;; title: mortgage-origination
;; version: 1.0.0
;; summary: Home loan processing system with income verification, property appraisal, and multi-lender bidding
;; description: A comprehensive mortgage origination platform with automated underwriting and transparent fee structures

;; constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_STATUS (err u104))
(define-constant ERR_INSUFFICIENT_INCOME (err u105))
(define-constant ERR_LOW_APPRAISAL (err u106))
(define-constant ERR_EXPIRED (err u107))

(define-constant CONTRACT_OWNER tx-sender)
(define-constant MIN_CREDIT_SCORE u600)
(define-constant MIN_INCOME_RATIO u3) ;; 3x monthly payment
(define-constant MAX_LOAN_TO_VALUE u80) ;; 80% LTV
(define-constant BID_DURATION u144) ;; ~24 hours in blocks

;; data vars
(define-data-var next-loan-id uint u1)
(define-data-var next-lender-id uint u1)
(define-data-var platform-fee-rate uint u50) ;; 0.5% in basis points

;; data maps
(define-map loan-applications
  uint
  {
    borrower: principal,
    property-address: (string-ascii 200),
    loan-amount: uint,
    property-value: uint,
    monthly-income: uint,
    credit-score: uint,
    status: (string-ascii 20),
    appraisal-value: uint,
    created-at: uint,
    expires-at: uint
  }
)

(define-map lender-profiles
  uint
  {
    lender: principal,
    name: (string-ascii 100),
    min-loan-amount: uint,
    max-loan-amount: uint,
    base-rate: uint, ;; basis points
    active: bool,
    total-loans: uint
  }
)

(define-map loan-bids
  { loan-id: uint, lender-id: uint }
  {
    interest-rate: uint, ;; basis points
    fees: uint,
    terms-months: uint,
    bid-amount: uint,
    expires-at: uint,
    status: (string-ascii 20)
  }
)

(define-map income-verifications
  uint ;; loan-id
  {
    verifier: principal,
    monthly-income: uint,
    employment-status: (string-ascii 50),
    verified: bool,
    verified-at: uint
  }
)

(define-map property-appraisals
  uint ;; loan-id
  {
    appraiser: principal,
    appraised-value: uint,
    appraisal-date: uint,
    verified: bool
  }
)

(define-map winning-bids
  uint ;; loan-id
  { lender-id: uint, interest-rate: uint, fees: uint, selected-at: uint }
)

;; public functions

;; Submit a new loan application
(define-public (submit-loan-application
  (property-address (string-ascii 200))
  (loan-amount uint)
  (property-value uint)
  (monthly-income uint)
  (credit-score uint))
  (let (
    (loan-id (var-get next-loan-id))
    (current-block stacks-block-height)
    (expires-at (+ current-block BID_DURATION))
  )
    (asserts! (> loan-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> property-value u0) ERR_INVALID_AMOUNT)
    (asserts! (>= credit-score MIN_CREDIT_SCORE) ERR_INSUFFICIENT_INCOME)
    (asserts! (>= monthly-income (/ loan-amount (* MIN_INCOME_RATIO u12))) ERR_INSUFFICIENT_INCOME)
    (asserts! (<= (/ (* loan-amount u100) property-value) MAX_LOAN_TO_VALUE) ERR_LOW_APPRAISAL)
    
    (map-set loan-applications loan-id {
      borrower: tx-sender,
      property-address: property-address,
      loan-amount: loan-amount,
      property-value: property-value,
      monthly-income: monthly-income,
      credit-score: credit-score,
      status: "pending",
      appraisal-value: u0,
      created-at: current-block,
      expires-at: expires-at
    })
    
    (var-set next-loan-id (+ loan-id u1))
    (ok loan-id)
  )
)

;; Register as a lender
(define-public (register-lender
  (name (string-ascii 100))
  (min-loan-amount uint)
  (max-loan-amount uint)
  (base-rate uint))
  (let (
    (lender-id (var-get next-lender-id))
  )
    (asserts! (> max-loan-amount min-loan-amount) ERR_INVALID_AMOUNT)
    (asserts! (> base-rate u0) ERR_INVALID_AMOUNT)
    
    (map-set lender-profiles lender-id {
      lender: tx-sender,
      name: name,
      min-loan-amount: min-loan-amount,
      max-loan-amount: max-loan-amount,
      base-rate: base-rate,
      active: true,
      total-loans: u0
    })
    
    (var-set next-lender-id (+ lender-id u1))
    (ok lender-id)
  )
)

;; Submit a bid for a loan
(define-public (submit-bid
  (loan-id uint)
  (lender-id uint)
  (interest-rate uint)
  (fees uint)
  (terms-months uint))
  (let (
    (loan (unwrap! (map-get? loan-applications loan-id) ERR_NOT_FOUND))
    (lender (unwrap! (map-get? lender-profiles lender-id) ERR_NOT_FOUND))
    (current-block stacks-block-height)
    (expires-at (+ current-block u72)) ;; ~12 hours
  )
    (asserts! (is-eq tx-sender (get lender lender)) ERR_UNAUTHORIZED)
    (asserts! (get active lender) ERR_INVALID_STATUS)
    (asserts! (is-eq (get status loan) "pending") ERR_INVALID_STATUS)
    (asserts! (< current-block (get expires-at loan)) ERR_EXPIRED)
    (asserts! (>= (get loan-amount loan) (get min-loan-amount lender)) ERR_INVALID_AMOUNT)
    (asserts! (<= (get loan-amount loan) (get max-loan-amount lender)) ERR_INVALID_AMOUNT)
    (asserts! (> interest-rate u0) ERR_INVALID_AMOUNT)
    (asserts! (> terms-months u0) ERR_INVALID_AMOUNT)
    
    (map-set loan-bids { loan-id: loan-id, lender-id: lender-id } {
      interest-rate: interest-rate,
      fees: fees,
      terms-months: terms-months,
      bid-amount: (get loan-amount loan),
      expires-at: expires-at,
      status: "active"
    })
    
    (ok true)
  )
)

;; Verify income for a loan application
(define-public (verify-income
  (loan-id uint)
  (verified-monthly-income uint)
  (employment-status (string-ascii 50)))
  (let (
    (loan (unwrap! (map-get? loan-applications loan-id) ERR_NOT_FOUND))
    (current-block stacks-block-height)
  )
    ;; In a real implementation, this would be restricted to authorized verifiers
    (asserts! (> verified-monthly-income u0) ERR_INVALID_AMOUNT)
    
    (map-set income-verifications loan-id {
      verifier: tx-sender,
      monthly-income: verified-monthly-income,
      employment-status: employment-status,
      verified: true,
      verified-at: current-block
    })
    
    (ok true)
  )
)

;; Submit property appraisal
(define-public (submit-appraisal
  (loan-id uint)
  (appraised-value uint))
  (let (
    (loan (unwrap! (map-get? loan-applications loan-id) ERR_NOT_FOUND))
    (current-block stacks-block-height)
  )
    ;; In a real implementation, this would be restricted to certified appraisers
    (asserts! (> appraised-value u0) ERR_INVALID_AMOUNT)
    
    (map-set property-appraisals loan-id {
      appraiser: tx-sender,
      appraised-value: appraised-value,
      appraisal-date: current-block,
      verified: true
    })
    
    ;; Update loan application with appraisal value
    (map-set loan-applications loan-id 
      (merge loan { appraisal-value: appraised-value })
    )
    
    (ok true)
  )
)

;; Select winning bid
(define-public (select-winning-bid
  (loan-id uint)
  (lender-id uint))
  (let (
    (loan (unwrap! (map-get? loan-applications loan-id) ERR_NOT_FOUND))
    (bid (unwrap! (map-get? loan-bids { loan-id: loan-id, lender-id: lender-id }) ERR_NOT_FOUND))
    (current-block stacks-block-height)
  )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status loan) "pending") ERR_INVALID_STATUS)
    (asserts! (is-eq (get status bid) "active") ERR_INVALID_STATUS)
    (asserts! (< current-block (get expires-at bid)) ERR_EXPIRED)
    
    ;; Record winning bid
    (map-set winning-bids loan-id {
      lender-id: lender-id,
      interest-rate: (get interest-rate bid),
      fees: (get fees bid),
      selected-at: current-block
    })
    
    ;; Update loan status
    (map-set loan-applications loan-id
      (merge loan { status: "approved" })
    )
    
    ;; Update lender stats
    (let (
      (lender (unwrap! (map-get? lender-profiles lender-id) ERR_NOT_FOUND))
    )
      (map-set lender-profiles lender-id
        (merge lender { total-loans: (+ (get total-loans lender) u1) })
      )
    )
    
    (ok true)
  )
)

;; read only functions

;; Get loan application details
(define-read-only (get-loan-application (loan-id uint))
  (map-get? loan-applications loan-id)
)

;; Get lender profile
(define-read-only (get-lender-profile (lender-id uint))
  (map-get? lender-profiles lender-id)
)

;; Get loan bid
(define-read-only (get-loan-bid (loan-id uint) (lender-id uint))
  (map-get? loan-bids { loan-id: loan-id, lender-id: lender-id })
)

;; Get income verification
(define-read-only (get-income-verification (loan-id uint))
  (map-get? income-verifications loan-id)
)

;; Get property appraisal
(define-read-only (get-property-appraisal (loan-id uint))
  (map-get? property-appraisals loan-id)
)

;; Get winning bid
(define-read-only (get-winning-bid (loan-id uint))
  (map-get? winning-bids loan-id)
)

;; Calculate monthly payment
(define-read-only (calculate-monthly-payment
  (loan-amount uint)
  (interest-rate uint)
  (term-months uint))
  (let (
    (monthly-rate (/ interest-rate (* u12 u10000))) ;; Convert annual rate to monthly decimal
    (payment-factor (/ (pow (+ u1 monthly-rate) term-months) (- (pow (+ u1 monthly-rate) term-months) u1)))
  )
    (* loan-amount (* monthly-rate payment-factor))
  )
)

;; Get platform fee
(define-read-only (get-platform-fee (loan-amount uint))
  (/ (* loan-amount (var-get platform-fee-rate)) u10000)
)

;; private functions

;; Check if loan qualifies for automated underwriting
(define-private (qualifies-for-automated-underwriting (loan-id uint))
  (let (
    (loan (unwrap! (map-get? loan-applications loan-id) false))
    (income-verification (map-get? income-verifications loan-id))
    (appraisal (map-get? property-appraisals loan-id))
  )
    (and
      (>= (get credit-score loan) u700)
      (is-some income-verification)
      (is-some appraisal)
      (match income-verification verified-income (get verified verified-income) false)
      (match appraisal appraisal-data (get verified appraisal-data) false)
    )
  )
)

