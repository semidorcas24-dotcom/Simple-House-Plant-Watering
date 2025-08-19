;; Simple House Plant Watering System
;; Manages plant care requests and neighbor assignments

(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_UNAUTHORIZED (err u403))
(define-constant ERR_ALREADY_EXISTS (err u409))

;; Plant care request structure
(define-map plant-requests
  { owner: principal, plant-id: uint }
  {
    care-instructions: (string-ascii 500),
    watering-frequency: uint, ;; days between watering
    start-block: uint,
    end-block: uint,
    assigned-neighbor: (optional principal),
    status: (string-ascii 20) ;; "pending", "assigned", "completed"
  }
)

;; Neighbor availability
(define-map neighbor-availability
  { neighbor: principal }
  { available-from: uint, available-until: uint }
)

;; Care completion tracking
(define-map care-completions
  { owner: principal, plant-id: uint, completion-id: uint }
  {
    caretaker: principal,
    completion-block: uint,
    notes: (string-ascii 300),
    photo-hash: (optional (string-ascii 64))
  }
)

;; Request plant care
(define-public (request-plant-care
  (plant-id uint)
  (instructions (string-ascii 500))
  (frequency uint)
  (duration-blocks uint))
  (let ((current-block burn-block-height))
    (match (map-get? plant-requests { owner: tx-sender, plant-id: plant-id })
      existing-request ERR_ALREADY_EXISTS
      (ok (map-set plant-requests
        { owner: tx-sender, plant-id: plant-id }
        {
          care-instructions: instructions,
          watering-frequency: frequency,
          start-block: current-block,
          end-block: (+ current-block duration-blocks),
          assigned-neighbor: none,
          status: "pending"
        }))
    )
  )
)

;; Set neighbor availability
(define-public (set-availability (from-block uint) (until-block uint))
  (ok (map-set neighbor-availability
    { neighbor: tx-sender }
    { available-from: from-block, available-until: until-block }
  ))
)

;; Assign neighbor to plant care
(define-public (assign-neighbor (owner principal) (plant-id uint))
  (match (map-get? plant-requests { owner: owner, plant-id: plant-id })
    request
    (if (is-eq (get status request) "pending")
      (ok (map-set plant-requests
        { owner: owner, plant-id: plant-id }
        (merge request {
          assigned-neighbor: (some tx-sender),
          status: "assigned"
        })
      ))
      ERR_UNAUTHORIZED
    )
    ERR_NOT_FOUND
  )
)

;; Log care completion
(define-public (log-care-completion
  (owner principal)
  (plant-id uint)
  (completion-id uint)
  (notes (string-ascii 300))
  (photo-hash (optional (string-ascii 64))))
  (match (map-get? plant-requests { owner: owner, plant-id: plant-id })
    request
    (if (is-eq (some tx-sender) (get assigned-neighbor request))
      (ok (map-set care-completions
        { owner: owner, plant-id: plant-id, completion-id: completion-id }
        {
          caretaker: tx-sender,
          completion-block: burn-block-height,
          notes: notes,
          photo-hash: photo-hash
        }
      ))
      ERR_UNAUTHORIZED
    )
    ERR_NOT_FOUND
  )
)

;; Complete care request
(define-public (complete-care-request (owner principal) (plant-id uint))
  (match (map-get? plant-requests { owner: owner, plant-id: plant-id })
    request
    (if (is-eq (some tx-sender) (get assigned-neighbor request))
      (ok (map-set plant-requests
        { owner: owner, plant-id: plant-id }
        (merge request { status: "completed" })
      ))
      ERR_UNAUTHORIZED
    )
    ERR_NOT_FOUND
  )
)

;; Read-only functions
(define-read-only (get-plant-request (owner principal) (plant-id uint))
  (map-get? plant-requests { owner: owner, plant-id: plant-id })
)

(define-read-only (get-neighbor-availability (neighbor principal))
  (map-get? neighbor-availability { neighbor: neighbor })
)

(define-read-only (get-care-completion (owner principal) (plant-id uint) (completion-id uint))
  (map-get? care-completions { owner: owner, plant-id: plant-id, completion-id: completion-id })
)
