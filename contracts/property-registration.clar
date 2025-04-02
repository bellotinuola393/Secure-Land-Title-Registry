;; property-registration.clar
;; Records land boundaries and ownership

;; Define data variables
(define-data-var registry-admin principal tx-sender)

;; Define data maps
(define-map properties
  { property-id: (string-ascii 36) }
  {
    owner: principal,
    boundaries: (string-ascii 256),
    registration-date: uint,
    last-updated: uint,
    status: (string-ascii 20)
  }
)

;; Define ownership history map
(define-map ownership-history
  { property-id: (string-ascii 36), index: uint }
  {
    owner: principal,
    transfer-date: uint
  }
)

;; Define ownership history counter
(define-map ownership-counter
  { property-id: (string-ascii 36) }
  { count: uint }
)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPERTY-EXISTS (err u101))
(define-constant ERR-PROPERTY-NOT-FOUND (err u102))
(define-constant ERR-INVALID-STATUS (err u103))

;; Check if caller is admin
(define-private (is-admin)
  (is-eq tx-sender (var-get registry-admin))
)

;; Register a new property
(define-public (register-property
    (property-id (string-ascii 36))
    (boundaries (string-ascii 256))
    (owner principal))
  (begin
    ;; Only admin can register properties
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)

    ;; Check if property already exists
    (asserts! (is-none (map-get? properties { property-id: property-id })) ERR-PROPERTY-EXISTS)

    ;; Add property to registry
    (map-set properties
      { property-id: property-id }
      {
        owner: owner,
        boundaries: boundaries,
        registration-date: block-height,
        last-updated: block-height,
        status: "active"
      }
    )

    ;; Initialize ownership history
    (map-set ownership-history
      { property-id: property-id, index: u0 }
      {
        owner: owner,
        transfer-date: block-height
      }
    )

    ;; Set counter to 1 (first entry)
    (map-set ownership-counter
      { property-id: property-id }
      { count: u1 }
    )

    (ok true)
  )
)

;; Update property boundaries
(define-public (update-boundaries
    (property-id (string-ascii 36))
    (new-boundaries (string-ascii 256)))
  (let (
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
  )
    ;; Only admin can update boundaries
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)

    ;; Update property boundaries
    (map-set properties
      { property-id: property-id }
      (merge property {
        boundaries: new-boundaries,
        last-updated: block-height
      })
    )

    (ok true)
  )
)

;; Update property owner
(define-public (update-owner
    (property-id (string-ascii 36))
    (new-owner principal))
  (let (
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (counter-data (default-to { count: u0 } (map-get? ownership-counter { property-id: property-id })))
    (current-count (get count counter-data))
  )
    ;; Update property owner
    (map-set properties
      { property-id: property-id }
      (merge property {
        owner: new-owner,
        last-updated: block-height
      })
    )

    ;; Record in ownership history
    (map-set ownership-history
      { property-id: property-id, index: current-count }
      {
        owner: new-owner,
        transfer-date: block-height
      }
    )

    ;; Update counter
    (map-set ownership-counter
      { property-id: property-id }
      { count: (+ current-count u1) }
    )

    (ok true)
  )
)

;; Change property status (active, inactive, disputed)
(define-public (change-property-status
    (property-id (string-ascii 36))
    (new-status (string-ascii 20)))
  (let (
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
  )
    ;; Only admin can change status
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)

    ;; Validate status
    (asserts! (or
      (is-eq new-status "active")
      (is-eq new-status "inactive")
      (is-eq new-status "disputed"))
      ERR-INVALID-STATUS)

    ;; Update property status
    (map-set properties
      { property-id: property-id }
      (merge property {
        status: new-status,
        last-updated: block-height
      })
    )

    (ok true)
  )
)

;; Get property details
(define-read-only (get-property (property-id (string-ascii 36)))
  (map-get? properties { property-id: property-id })
)

;; Get property owner
(define-read-only (get-property-owner (property-id (string-ascii 36)))
  (match (map-get? properties { property-id: property-id })
    property (some (get owner property))
    none
  )
)

;; Check if property exists
(define-read-only (property-exists? (property-id (string-ascii 36)))
  (is-some (map-get? properties { property-id: property-id }))
)

;; Transfer admin rights
(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-admin) ERR
