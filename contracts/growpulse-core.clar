;; growpulse-core
;;
;; A smart contract to manage learning milestones, achievements, and visual tree 
;; structures for the GrowPulse learning tracker platform designed for children.
;; This contract enables the creation, verification, and organization of learning
;; milestones in tree-like structures, creating an immutable record of a child's
;; educational journey.
;; =============================
;; Constants & Error Codes
;; =============================
(define-constant CONTRACT-OWNER tx-sender)
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-NOT-FOUND (err u101))
(define-constant ERR-MILESTONE-NOT-FOUND (err u102))
(define-constant ERR-MILESTONE-ALREADY-EXISTS (err u103))
(define-constant ERR-FOREST-NOT-FOUND (err u104))
(define-constant ERR-FOREST-ALREADY-EXISTS (err u105))
(define-constant ERR-PARENT-MILESTONE-NOT-FOUND (err u106))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u107))
(define-constant ERR-PREREQUISITES-NOT-COMPLETED (err u108))
(define-constant ERR-INVALID-PARAMETERS (err u109))
(define-constant ERR-INVALID-USER-ROLE (err u110))
(define-constant ERR-CHILD-NOT-REGISTERED (err u111))
(define-constant ERR-DUPLICATE-RELATIONSHIP (err u112))
;; =============================
;; Data Maps & Variables
;; =============================
;; User roles: 1=Admin, 2=Educator, 3=Parent, 4=Child
(define-map users
  { user-id: principal }
  {
    role: uint,
    name: (string-ascii 100),
    registered-at: uint,
  }
)
;; Stores relationships: parent-child or educator-child
(define-map user-relationships
  {
    user-id: principal,
    related-user-id: principal,
  }
  { relationship-type: (string-ascii 20) } ;; "parent-child" or "educator-child"
)
;; Forests represent collections of milestone trees (e.g., "Mathematics", "Science")
(define-map forests
  { forest-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    created-by: principal,
    created-at: uint,
  }
)
;; Milestone definitions
(define-map milestones
  { milestone-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    category: (string-ascii 50),
    difficulty-level: uint, ;; 1-5 representing difficulty
    forest-id: uint,
    parent-milestone-id: (optional uint),
    created-by: principal,
    created-at: uint,
  }
)
;; Tracks milestone completion by users
(define-map milestone-completions
  {
    milestone-id: uint,
    user-id: principal,
  }
  {
    completed-at: uint,
    verified-by: principal,
    evidence-url: (optional (string-utf8 500)),
  }
)
;; Milestone prerequisites - what must be completed before attempting a milestone
(define-map milestone-prerequisites
  {
    milestone-id: uint,
    prerequisite-id: uint,
  }
  { added-at: uint }
)
;; Counters
(define-data-var milestone-id-counter uint u1)
(define-data-var forest-id-counter uint u1)
;; =============================
;; Private Functions
;; =============================
;; Check if a user is authorized to manage a child's milestones
(define-private (can-manage-child
    (manager-id principal)
    (child-id principal)
  )
  (or
    (is-eq manager-id CONTRACT-OWNER)
    (match (map-get? user-relationships {
      user-id: manager-id,
      related-user-id: child-id,
    })
      relationship
      true
      false
    )
  )
)

;; Increment milestone ID counter and return new value
(define-private (get-next-milestone-id)
  (let ((next-id (var-get milestone-id-counter)))
    (var-set milestone-id-counter (+ next-id u1))
    next-id
  )
)

;; Increment forest ID counter and return new value
(define-private (get-next-forest-id)
  (let ((next-id (var-get forest-id-counter)))
    (var-set forest-id-counter (+ next-id u1))
    next-id
  )
)

;; =============================
;; Read-Only Functions
;; =============================
;; Get user information
(define-read-only (get-user (user-id principal))
  (map-get? users { user-id: user-id })
)

;; Get milestone information
(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

;; Get forest information
(define-read-only (get-forest (forest-id uint))
  (map-get? forests { forest-id: forest-id })
)

;; Check if a milestone is completed by a user
(define-read-only (is-milestone-completed
    (milestone-id uint)
    (user-id principal)
  )
  (is-some (map-get? milestone-completions {
    milestone-id: milestone-id,
    user-id: user-id,
  }))
)

;; Get milestone completion details
(define-read-only (get-milestone-completion
    (milestone-id uint)
    (user-id principal)
  )
  (map-get? milestone-completions {
    milestone-id: milestone-id,
    user-id: user-id,
  })
)

;; Get relationship between two users
(define-read-only (get-user-relationship
    (user-id principal)
    (related-user-id principal)
  )
  (map-get? user-relationships {
    user-id: user-id,
    related-user-id: related-user-id,
  })
)

;; =============================
;; Public Functions
;; =============================
;; Register a new user
(define-public (register-user
    (name (string-ascii 100))
    (role uint)
  )
  (let ((user-id tx-sender))
    (asserts! (and (>= role u1) (<= role u4)) ERR-INVALID-USER-ROLE)
    (asserts! (is-none (map-get? users { user-id: user-id }))
      ERR-MILESTONE-ALREADY-EXISTS
    )
    (map-set users { user-id: user-id } {
      role: role,
      name: name,
      registered-at: block-height,
    })
    (ok true)
  )
)

;; Create a new milestone
(define-public (create-milestone
    (title (string-ascii 100))
    (description (string-ascii 500))
    (category (string-ascii 50))
    (difficulty-level uint)
    (forest-id uint)
    (parent-milestone-id (optional uint))
  )
  (let (
      (user-id tx-sender)
      (milestone-id (get-next-milestone-id))
    )
    ;; Ensure requester is admin, educator, or parent
    (asserts! (is-some (map-get? forests { forest-id: forest-id }))
      ERR-FOREST-NOT-FOUND
    )
    ;; Validate difficulty level (1-5)
    (asserts! (and (>= difficulty-level u1) (<= difficulty-level u5))
      ERR-INVALID-PARAMETERS
    )
    ;; If parent milestone is specified, ensure it exists
    (asserts!
      (match parent-milestone-id
        parent-id (is-some (map-get? milestones { milestone-id: parent-id }))
        true
      )
      ERR-PARENT-MILESTONE-NOT-FOUND
    )
    (map-set milestones { milestone-id: milestone-id } {
      title: title,
      description: description,
      category: category,
      difficulty-level: difficulty-level,
      forest-id: forest-id,
      parent-milestone-id: parent-milestone-id,
      created-by: user-id,
      created-at: block-height,
    })
    (ok milestone-id)
  )
)
