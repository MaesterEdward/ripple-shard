;; RippleShard Disaster Relief Coordination Contract
;; A decentralized platform for coordinating disaster relief efforts

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-stake (err u104))
(define-constant err-already-exists (err u105))

;; Minimum stake required to validate relief efforts (in microSTX)
(define-constant min-validator-stake u1000000)

;; Data Variables
(define-data-var disaster-count uint u0)
(define-data-var relief-request-count uint u0)

;; Data Maps
;; Disaster tracking
(define-map disasters
    { disaster-id: uint }
    {
        location: (string-ascii 100),
        severity: uint,
        status: (string-ascii 20),
        timestamp: uint,
        coordinator: principal
    }
)

;; Relief requests
(define-map relief-requests
    { request-id: uint }
    {
        disaster-id: uint,
        requester: principal,
        resource-type: (string-ascii 50),
        quantity: uint,
        urgency: uint,
        status: (string-ascii 20),
        timestamp: uint
    }
)

;; Resource allocations
(define-map resource-allocations
    { allocation-id: uint }
    {
        request-id: uint,
        provider: principal,
        amount: uint,
        delivered: bool,
        verified: bool,
        timestamp: uint
    }
)

;; Validator stakes
(define-map validator-stakes
    { validator: principal }
    { stake-amount: uint, active: bool }
)

;; Impact scores
(define-map impact-scores
    { participant: principal }
    { score: uint }
)

;; Read-only functions
(define-read-only (get-disaster (disaster-id uint))
    (map-get? disasters { disaster-id: disaster-id })
)

(define-read-only (get-relief-request (request-id uint))
    (map-get? relief-requests { request-id: request-id })
)

(define-read-only (get-resource-allocation (allocation-id uint))
    (map-get? resource-allocations { allocation-id: allocation-id })
)

(define-read-only (get-validator-stake (validator principal))
    (map-get? validator-stakes { validator: validator })
)

(define-read-only (get-impact-score (participant principal))
    (default-to 
        { score: u0 }
        (map-get? impact-scores { participant: participant })
    )
)

(define-read-only (get-disaster-count)
    (ok (var-get disaster-count))
)

(define-read-only (get-relief-request-count)
    (ok (var-get relief-request-count))
)

;; Public functions

;; Register a new disaster
(define-public (register-disaster (location (string-ascii 100)) (severity uint))
    (let
        (
            (new-disaster-id (+ (var-get disaster-count) u1))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set disasters
            { disaster-id: new-disaster-id }
            {
                location: location,
                severity: severity,
                status: "active",
                timestamp: block-height,
                coordinator: tx-sender
            }
        )
        (var-set disaster-count new-disaster-id)
        (ok new-disaster-id)
    )
)

;; Create a relief request
(define-public (create-relief-request 
    (disaster-id uint)
    (resource-type (string-ascii 50))
    (quantity uint)
    (urgency uint))
    (let
        (
            (new-request-id (+ (var-get relief-request-count) u1))
            (disaster (unwrap! (get-disaster disaster-id) err-not-found))
        )
        (map-set relief-requests
            { request-id: new-request-id }
            {
                disaster-id: disaster-id,
                requester: tx-sender,
                resource-type: resource-type,
                quantity: quantity,
                urgency: urgency,
                status: "pending",
                timestamp: block-height
            }
        )
        (var-set relief-request-count new-request-id)
        (ok new-request-id)
    )
)

;; Stake tokens to become a validator
(define-public (stake-as-validator (amount uint))
    (begin
        (asserts! (>= amount min-validator-stake) err-insufficient-stake)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set validator-stakes
            { validator: tx-sender }
            { stake-amount: amount, active: true }
        )
        (ok true)
    )
)

;; Allocate resources to a relief request
(define-public (allocate-resources 
    (request-id uint)
    (allocation-id uint)
    (amount uint))
    (let
        (
            (request (unwrap! (get-relief-request request-id) err-not-found))
            (validator (unwrap! (get-validator-stake tx-sender) err-unauthorized))
        )
        (asserts! (get active validator) err-unauthorized)
        (map-set resource-allocations
            { allocation-id: allocation-id }
            {
                request-id: request-id,
                provider: tx-sender,
                amount: amount,
                delivered: false,
                verified: false,
                timestamp: block-height
            }
        )
        (ok allocation-id)
    )
)

;; Mark delivery as complete
(define-public (mark-delivered (allocation-id uint))
    (let
        (
            (allocation (unwrap! (get-resource-allocation allocation-id) err-not-found))
        )
        (asserts! (is-eq (get provider allocation) tx-sender) err-unauthorized)
        (map-set resource-allocations
            { allocation-id: allocation-id }
            (merge allocation { delivered: true })
        )
        (ok true)
    )
)

;; Verify delivery (validator function)
(define-public (verify-delivery (allocation-id uint))
    (let
        (
            (allocation (unwrap! (get-resource-allocation allocation-id) err-not-found))
            (validator (unwrap! (get-validator-stake tx-sender) err-unauthorized))
            (provider (get provider allocation))
            (current-score (get score (get-impact-score provider)))
        )
        (asserts! (get active validator) err-unauthorized)
        (asserts! (get delivered allocation) err-invalid-status)
        
        ;; Update allocation as verified
        (map-set resource-allocations
            { allocation-id: allocation-id }
            (merge allocation { verified: true })
        )
        
        ;; Award impact score to provider
        (map-set impact-scores
            { participant: provider }
            { score: (+ current-score u10) }
        )
        
        (ok true)
    )
)

;; Update disaster status
(define-public (update-disaster-status (disaster-id uint) (new-status (string-ascii 20)))
    (let
        (
            (disaster (unwrap! (get-disaster disaster-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get coordinator disaster)) err-unauthorized)
        (map-set disasters
            { disaster-id: disaster-id }
            (merge disaster { status: new-status })
        )
        (ok true)
    )
)

;; Update relief request status
(define-public (update-request-status (request-id uint) (new-status (string-ascii 20)))
    (let
        (
            (request (unwrap! (get-relief-request request-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get requester request)) err-unauthorized)
        (map-set relief-requests
            { request-id: request-id }
            (merge request { status: new-status })
        )
        (ok true)
    )
)

;; Withdraw validator stake (if no active allocations)
(define-public (withdraw-stake)
    (let
        (
            (validator (unwrap! (get-validator-stake tx-sender) err-not-found))
            (stake-amount (get stake-amount validator))
        )
        (asserts! (get active validator) err-invalid-status)
        (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
        (map-set validator-stakes
            { validator: tx-sender }
            { stake-amount: u0, active: false }
        )
        (ok stake-amount)
    )
)
