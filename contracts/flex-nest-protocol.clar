;; Flex Nest - Professional Coworking Reservation Protocol
;;
;; This smart contract facilitates the decentralized management of coworking spaces
;; where users can reserve time slots, list their own availability, and
;; participate in a peer-to-peer ecosystem of shared working environments.
;; 
;; The protocol enables users to:
;; - Acquire time slots with STX tokens
;; - List their unused time slots for others to reserve
;; - Transfer time slots between users
;; - Receive partial refunds for unused time

;; ------------------------------
;; Contract Configuration Section
;; ------------------------------

;; Contract administration constants
(define-constant admin-principal tx-sender)
(define-constant error-admin-restricted (err u100))

;; Error code definitions for robust operation
(define-constant error-insufficient-time-slots (err u101))
(define-constant error-booking-unsuccessful (err u102))
(define-constant error-self-transaction (err u107))
(define-constant error-allocation-exceeded (err u108))
(define-constant error-slot-pricing-invalid (err u103))
(define-constant error-time-duration-invalid (err u104))
(define-constant error-percentage-invalid (err u105))
(define-constant error-cancellation-failed (err u106))
(define-constant error-invalid-allocation-parameter (err u109))

;; ------------------------------
;; Protocol Parameters Section
;; ------------------------------

;; Time slot cost in microstacks (1,000,000 microstacks = 1 STX)
(define-data-var time-slot-rate uint u500)


;; Protocol economic parameters
(define-data-var protocol-fee-percentage uint u5)
(define-data-var cancellation-reimbursement-rate uint u90)

;; System capacity configuration
(define-data-var system-max-capacity uint u10000)
(define-data-var system-current-allocation uint u0)

;; Individual account limitations
(define-data-var individual-slot-ceiling uint u100)

;; ------------------------------
;; Data Storage Section
;; ------------------------------

;; Track user time slot allocations
(define-map user-time-allocation principal uint)

;; Track user token balances within protocol
(define-map user-token-balance principal uint)

;; Available time slots being offered by users
(define-map available-time-offerings {provider: principal} {slots: uint, rate: uint})

;; ------------------------------
;; Private Function Utilities
;; ------------------------------

;; Manage system-wide time allocation accounting
(define-private (adjust-system-allocation (adjustment int))
  (let (
    (existing-allocation (var-get system-current-allocation))
    (updated-allocation (if (< adjustment 0)
                         (if (>= existing-allocation (to-uint (- 0 adjustment)))
                             (- existing-allocation (to-uint (- 0 adjustment)))
                             u0)
                         (+ existing-allocation (to-uint adjustment))))
  )
    (asserts! (<= updated-allocation (var-get system-max-capacity)) error-allocation-exceeded)
    (var-set system-current-allocation updated-allocation)
    (ok true)))

;; Calculate protocol fee from transaction amount
(define-private (derive-protocol-fee (transaction-value uint))
  (/ (* transaction-value (var-get protocol-fee-percentage)) u100))

;; Calculate reimbursement amount for cancellations
(define-private (calculate-cancellation-credit (slots uint))
  (/ (* slots (var-get time-slot-rate) (var-get cancellation-reimbursement-rate)) u100))


;; ------------------------------
;; Time Slot Management Functions
;; ------------------------------

;; Request refund for unused time slots
(define-public (request-time-slot-refund (slots uint))
  (let (
    (user-slots (default-to u0 (map-get? user-time-allocation tx-sender)))
    (refund-value (calculate-cancellation-credit slots))
    (admin-token-balance (default-to u0 (map-get? user-token-balance admin-principal)))
  )
    (asserts! (> slots u0) error-time-duration-invalid)
    (asserts! (>= user-slots slots) error-insufficient-time-slots)
    (asserts! (>= admin-token-balance refund-value) error-cancellation-failed)

    ;; Update user's time allocation
    (map-set user-time-allocation tx-sender (- user-slots slots))

    ;; Process refund to user
    (map-set user-token-balance tx-sender (+ (default-to u0 (map-get? user-token-balance tx-sender)) refund-value))

    (ok true)))

;; Transfer time slots to another user
(define-public (transfer-time-slots (recipient principal) (slots uint))
  (let (
    (sender-allocation (default-to u0 (map-get? user-time-allocation tx-sender)))
    (recipient-allocation (default-to u0 (map-get? user-time-allocation recipient)))
    (recipient-new-allocation (+ recipient-allocation slots))
  )
    ;; Validation checks
    (asserts! (not (is-eq tx-sender recipient)) error-self-transaction)
    (asserts! (> slots u0) error-time-duration-invalid)
    (asserts! (>= sender-allocation slots) error-insufficient-time-slots)
    (asserts! (<= recipient-new-allocation (var-get individual-slot-ceiling)) error-allocation-exceeded)

    ;; Update sender's allocation
    (map-set user-time-allocation tx-sender (- sender-allocation slots))

    ;; Update recipient's allocation
    (map-set user-time-allocation recipient recipient-new-allocation)

    (ok true)))

;; Alternative time slot transfer function (duplicate functionality for demonstration)
(define-public (reallocate-time-slots (recipient principal) (slots uint))
  (let (
    (sender-allocation (default-to u0 (map-get? user-time-allocation tx-sender)))
    (recipient-allocation (default-to u0 (map-get? user-time-allocation recipient)))
    (recipient-new-allocation (+ recipient-allocation slots))
  )
    (asserts! (not (is-eq tx-sender recipient)) error-self-transaction)
    (asserts! (> slots u0) error-time-duration-invalid)
    (asserts! (>= sender-allocation slots) error-insufficient-time-slots)
    (asserts! (<= recipient-new-allocation (var-get individual-slot-ceiling)) error-allocation-exceeded)

    ;; Update sender's time allocation
    (map-set user-time-allocation tx-sender (- sender-allocation slots))

    ;; Update recipient's time allocation
    (map-set user-time-allocation recipient recipient-new-allocation)

    (ok true)))

;; ------------------------------
;; Direct Time Slot Acquisition
;; ------------------------------

;; Acquire time slots directly using STX tokens
(define-public (acquire-time-slots (slots uint))
  (let (
    (total-cost (* slots (var-get time-slot-rate)))
    (user-tokens (default-to u0 (map-get? user-token-balance tx-sender)))
    (user-slots (default-to u0 (map-get? user-time-allocation tx-sender)))
    (updated-allocation (+ user-slots slots))
  )
    ;; Input validation
    (asserts! (> slots u0) error-time-duration-invalid)
    (asserts! (>= user-tokens total-cost) error-insufficient-time-slots)
    (asserts! (<= updated-allocation (var-get individual-slot-ceiling)) error-allocation-exceeded)

    ;; Update user token balance
    (map-set user-token-balance tx-sender (- user-tokens total-cost))

    ;; Update user time allocation
    (map-set user-time-allocation tx-sender updated-allocation)

    ;; Credit admin account
    (map-set user-token-balance admin-principal 
             (+ (default-to u0 (map-get? user-token-balance admin-principal)) total-cost))

    (ok true)))

;; Share time slots with another user
(define-public (share-time-slots (recipient principal) (slots uint))
  (let (
    (sender-allocation (default-to u0 (map-get? user-time-allocation tx-sender)))
    (recipient-allocation (default-to u0 (map-get? user-time-allocation recipient)))
    (updated-recipient-allocation (+ recipient-allocation slots))
  )
    ;; Validation
    (asserts! (> slots u0) error-time-duration-invalid)
    (asserts! (not (is-eq tx-sender recipient)) error-self-transaction)
    (asserts! (>= sender-allocation slots) error-insufficient-time-slots)
    (asserts! (<= updated-recipient-allocation (var-get individual-slot-ceiling)) error-allocation-exceeded)

    ;; Update sender's allocation
    (map-set user-time-allocation tx-sender (- sender-allocation slots))

    ;; Update recipient's allocation
    (map-set user-time-allocation recipient updated-recipient-allocation)

    (ok true)))

;; ------------------------------
;; User Listing Management
;; ------------------------------

;; List time slots for reservation by others
(define-public (list-available-time-slots (slots uint) (rate uint))
  (let (
    (user-allocation (default-to u0 (map-get? user-time-allocation tx-sender)))
    (existing-listing (get slots (default-to {slots: u0, rate: u0} (map-get? available-time-offerings {provider: tx-sender}))))
    (total-listing (+ slots existing-listing))
  )
    (asserts! (> slots u0) error-time-duration-invalid)
    (asserts! (> rate u0) error-slot-pricing-invalid)
    (asserts! (>= user-allocation total-listing) error-insufficient-time-slots)
    (try! (adjust-system-allocation (to-int slots)))
    (map-set available-time-offerings {provider: tx-sender} {slots: total-listing, rate: rate})
    (ok true)))

;; Withdraw previously listed time slots
(define-public (withdraw-time-slot-listing (slots uint))
  (let (
    (current-listing (get slots (default-to {slots: u0, rate: u0} (map-get? available-time-offerings {provider: tx-sender}))))
  )
    (asserts! (>= current-listing slots) error-insufficient-time-slots)
    (try! (adjust-system-allocation (to-int (- slots))))
    (map-set available-time-offerings {provider: tx-sender} 
             {slots: (- current-listing slots), 
              rate: (get rate (default-to {slots: u0, rate: u0} (map-get? available-time-offerings {provider: tx-sender})))})
    (ok true)))

;; ------------------------------
;; Time Slot Acquisition Functions
;; ------------------------------

;; Reserve time slots from another user's listing
(define-public (reserve-listed-time-slots (provider principal) (slots uint))
  (let (
    (listing-data (default-to {slots: u0, rate: u0} (map-get? available-time-offerings {provider: provider})))
    (slot-cost (* slots (get rate listing-data)))
    (fee (derive-protocol-fee slot-cost))
    (total-transaction-cost (+ slot-cost fee))
    (provider-allocation (default-to u0 (map-get? user-time-allocation provider)))
    (requester-tokens (default-to u0 (map-get? user-token-balance tx-sender)))
    (provider-tokens (default-to u0 (map-get? user-token-balance provider)))
    (admin-tokens (default-to u0 (map-get? user-token-balance admin-principal)))
  )
    (asserts! (not (is-eq tx-sender provider)) error-self-transaction)
    (asserts! (> slots u0) error-time-duration-invalid)
    (asserts! (>= (get slots listing-data) slots) error-insufficient-time-slots)
    (asserts! (>= provider-allocation slots) error-insufficient-time-slots)
    (asserts! (>= requester-tokens total-transaction-cost) error-insufficient-time-slots)

    ;; Update provider allocation and listing
    (map-set user-time-allocation provider (- provider-allocation slots))
    (map-set available-time-offerings {provider: provider} 
             {slots: (- (get slots listing-data) slots), rate: (get rate listing-data)})

    ;; Update requester's token balance and time allocation
    (map-set user-token-balance tx-sender (- requester-tokens total-transaction-cost))
    (map-set user-time-allocation tx-sender (+ (default-to u0 (map-get? user-time-allocation tx-sender)) slots))

    ;; Distribute tokens to provider and admin
    (map-set user-token-balance provider (+ provider-tokens slot-cost))
    (map-set user-token-balance admin-principal (+ admin-tokens fee))

    (ok true)))

;; ------------------------------
;; Administrative Functions
;; ------------------------------

;; Configure protocol parameters
(define-public (configure-protocol-parameters 
                (new-rate (optional uint))
                (new-fee-percentage (optional uint))
                (new-reimbursement-rate (optional uint))
                (new-individual-ceiling (optional uint))
                (new-system-capacity (optional uint)))
  (begin
    ;; Admin authorization check
    (asserts! (is-eq tx-sender admin-principal) error-admin-restricted)

    ;; Update time slot rate if provided
    (if (is-some new-rate)
        (begin
          (asserts! (> (unwrap-panic new-rate) u0) error-slot-pricing-invalid)
          (var-set time-slot-rate (unwrap-panic new-rate)))
        true)

    ;; Update fee percentage if provided
    (if (is-some new-fee-percentage)
        (begin
          (asserts! (< (unwrap-panic new-fee-percentage) u100) error-percentage-invalid)
          (var-set protocol-fee-percentage (unwrap-panic new-fee-percentage)))
        true)

    ;; Update reimbursement rate if provided
    (if (is-some new-reimbursement-rate)
        (begin
          (asserts! (<= (unwrap-panic new-reimbursement-rate) u100) error-percentage-invalid)
          (var-set cancellation-reimbursement-rate (unwrap-panic new-reimbursement-rate)))
        true)

    ;; Update individual slot ceiling if provided
    (if (is-some new-individual-ceiling)
        (begin
          (asserts! (> (unwrap-panic new-individual-ceiling) u0) error-invalid-allocation-parameter)
          (var-set individual-slot-ceiling (unwrap-panic new-individual-ceiling)))
        true)

    ;; Update system capacity if provided
    (if (is-some new-system-capacity)
        (begin
          (asserts! (>= (unwrap-panic new-system-capacity) (var-get system-current-allocation)) error-invalid-allocation-parameter)
          (var-set system-max-capacity (unwrap-panic new-system-capacity)))
        true)

    (ok true)))

;; ------------------------------
;; Financial Functions
;; ------------------------------

;; Withdraw tokens from protocol balance
(define-public (withdraw-token-balance (amount uint))
  (let (
    (user-tokens (default-to u0 (map-get? user-token-balance tx-sender)))
  )
    ;; Validation checks
    (asserts! (> amount u0) error-slot-pricing-invalid)
    (asserts! (>= user-tokens amount) error-insufficient-time-slots)

    ;; Update user token balance
    (map-set user-token-balance tx-sender (- user-tokens amount))

    ;; Process STX transfer
    (as-contract 
      (try! (stx-transfer? amount tx-sender tx-sender))
    )

    ;; Return success response
    (ok true)))

;; Emergency system reset and refund function
(define-public (execute-emergency-system-reset)
  (let (
    (admin-tokens (default-to u0 (map-get? user-token-balance admin-principal)))
    (total-active-slots (var-get system-current-allocation))
    (total-refund-required (* total-active-slots (var-get time-slot-rate)))
  )
    ;; Admin authorization check
    (asserts! (is-eq tx-sender admin-principal) error-admin-restricted)

    ;; Verify sufficient admin tokens for refund
    (asserts! (>= admin-tokens total-refund-required) error-cancellation-failed)

    ;; Reset system allocation counter
    (var-set system-current-allocation u0)

    ;; Deduct tokens from admin balance
    (map-set user-token-balance admin-principal (- admin-tokens total-refund-required))

    ;; Return success - actual refunds would be processed separately
    (ok true)))

