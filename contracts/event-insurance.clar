
;; Event Cancellation Insurance Smart Contract
;; Provides protection for events with weather monitoring and automated claims

;; Error constants
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-AMOUNT (err u400))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-EVENT-ENDED (err u410))
(define-constant ERR-CLAIM-PROCESSED (err u411))
(define-constant ERR-NO-COVERAGE (err u412))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures
(define-map events
  { event-id: uint }
  {
    organizer: principal,
    name: (string-ascii 100),
    date: uint,
    location: (string-ascii 50),
    premium: uint,
    coverage-amount: uint,
    weather-threshold: uint,
    status: (string-ascii 20),
    claim-processed: bool
  }
)

(define-map weather-reports
  { event-id: uint, report-date: uint }
  {
    temperature: int,
    precipitation: uint,
    wind-speed: uint,
    reporter: principal
  }
)

(define-map vendor-coordination
  { event-id: uint, vendor: principal }
  {
    service-type: (string-ascii 50),
    cost: uint,
    status: (string-ascii 20)
  }
)

;; Data variables
(define-data-var next-event-id uint u1)
(define-data-var total-premiums uint u0)

;; Public functions

;; Create new event insurance policy
(define-public (create-policy
    (name (string-ascii 100))
    (event-date uint)
    (location (string-ascii 50))
    (premium uint)
    (coverage-amount uint)
    (weather-threshold uint)
  )
  (let
    (
      (event-id (var-get next-event-id))
    )
    (asserts! (> premium u0) ERR-INVALID-AMOUNT)
    (asserts! (> coverage-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> event-date stacks-block-height) ERR-INVALID-AMOUNT)
    
    ;; Transfer premium to contract
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    
    ;; Create event record
    (map-set events
      { event-id: event-id }
      {
        organizer: tx-sender,
        name: name,
        date: event-date,
        location: location,
        premium: premium,
        coverage-amount: coverage-amount,
        weather-threshold: weather-threshold,
        status: "active",
        claim-processed: false
      }
    )
    
    ;; Update counters
    (var-set next-event-id (+ event-id u1))
    (var-set total-premiums (+ (var-get total-premiums) premium))
    
    (ok event-id)
  )
)

;; Submit weather report
(define-public (submit-weather-report
    (event-id uint)
    (temperature int)
    (precipitation uint)
    (wind-speed uint)
  )
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR-NOT-FOUND))
      (report-date stacks-block-height)
    )
    ;; Only contract owner or event organizer can submit weather reports
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get organizer event))) ERR-UNAUTHORIZED)
    
    (map-set weather-reports
      { event-id: event-id, report-date: report-date }
      {
        temperature: temperature,
        precipitation: precipitation,
        wind-speed: wind-speed,
        reporter: tx-sender
      }
    )
    
    (ok true)
  )
)

;; Register vendor
(define-public (register-vendor
    (event-id uint)
    (service-type (string-ascii 50))
    (cost uint)
  )
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR-NOT-FOUND))
    )
    ;; Only event organizer can register vendors
    (asserts! (is-eq tx-sender (get organizer event)) ERR-UNAUTHORIZED)
    (asserts! (> cost u0) ERR-INVALID-AMOUNT)
    
    (map-set vendor-coordination
      { event-id: event-id, vendor: tx-sender }
      {
        service-type: service-type,
        cost: cost,
        status: "registered"
      }
    )
    
    (ok true)
  )
)

;; Process claim automatically based on weather conditions
(define-public (process-claim (event-id uint))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR-NOT-FOUND))
      (weather (map-get? weather-reports { event-id: event-id, report-date: (get date event) }))
    )
    ;; Verify conditions
    (asserts! (is-eq tx-sender (get organizer event)) ERR-UNAUTHORIZED)
    (asserts! (not (get claim-processed event)) ERR-CLAIM-PROCESSED)
    (asserts! (>= stacks-block-height (get date event)) ERR-EVENT-ENDED)
    
    ;; Check if weather conditions meet threshold for claim
    (match weather
      some-weather
        (if (>= (get precipitation some-weather) (get weather-threshold event))
          (begin
            ;; Process payout
            (try! (as-contract (stx-transfer? (get coverage-amount event) tx-sender (get organizer event))))
            
            ;; Update event status
            (map-set events
              { event-id: event-id }
              (merge event { status: "claimed", claim-processed: true })
            )
            
            (ok (get coverage-amount event))
          )
          ERR-NO-COVERAGE
        )
      ERR-NOT-FOUND
    )
  )
)

;; Update event status
(define-public (update-event-status (event-id uint) (new-status (string-ascii 20)))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get organizer event)) ERR-UNAUTHORIZED)
    
    (map-set events
      { event-id: event-id }
      (merge event { status: new-status })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get event details
(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id })
)

;; Get weather report
(define-read-only (get-weather-report (event-id uint) (report-date uint))
  (map-get? weather-reports { event-id: event-id, report-date: report-date })
)

;; Get vendor information
(define-read-only (get-vendor (event-id uint) (vendor principal))
  (map-get? vendor-coordination { event-id: event-id, vendor: vendor })
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-events: (- (var-get next-event-id) u1),
    total-premiums: (var-get total-premiums),
    contract-balance: (stx-get-balance (as-contract tx-sender))
  }
)

;; Check if event qualifies for claim based on weather
(define-read-only (check-claim-eligibility (event-id uint))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR-NOT-FOUND))
      (weather (map-get? weather-reports { event-id: event-id, report-date: (get date event) }))
    )
    (match weather
      some-weather
        (ok (>= (get precipitation some-weather) (get weather-threshold event)))
      (ok false)
    )
  )
)
