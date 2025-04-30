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
    registered-at: uint
  }
)

;; Stores relationships: parent-child or educator-child
(define-map user-relationships
  { user-id: principal, related-user-id: principal }
  { relationship-type: (string-ascii 20) } ;; "parent-child" or "educator-child"
)

;; Forests represent collections of milestone trees (e.g., "Mathematics", "Science")
(define-map forests
  { forest-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    created-by: principal,
    created-at: uint
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
    created-at: uint
  }
)

;; Tracks milestone completion by users
(define-map milestone-completions
  { milestone-id: uint, user-id: principal }
  {
    completed-at: uint,
    verified-by: principal,
    evidence-url: (optional (string-utf8 500))
  }
)

;; Milestone prerequisites - what must be completed before attempting a milestone
(define-map milestone-prerequisites
  { milestone-id: uint, prerequisite-id: uint }
  { added-at: uint }
)

;; Counters
(define-data-var milestone-id-counter uint u1)
(define-data-var forest-id-counter uint u1)

;; =============================
;; Private Functions
;; =============================

;; Check if a user exists and has one of the specified roles
(define-private (is-user-authorized (user-id principal) (allowed-roles (list 10 uint)))
  (match (map-get? users { user-id: user-id })
    user (fold and true (map (lambda (role) (is-eq (get role user) role)) allowed-roles))
    false
  )
)

;; Check if a user is authorized to manage a child's milestones
(define-private (can-manage-child (manager-id principal) (child-id principal))
  (or
    (is-eq manager-id CONTRACT-OWNER)
    (match (map-get? user-relationships { user-id: manager-id, related-user-id: child-id })
      relationship true
      false
    )
  )
)

;; Check if all prerequisites for a milestone have been completed by a user
(define-private (are-prerequisites-completed (milestone-id uint) (user-id principal))
  (fold and 
    true
    (map 
      (lambda (prereq-map) 
        (is-some (map-get? milestone-completions 
          { milestone-id: (get prerequisite-id prereq-map), user-id: user-id }))
      )
      (get-milestone-prerequisites milestone-id)
    )
  )
)

;; Get all prerequisites for a milestone
(define-private (get-milestone-prerequisites (milestone-id uint))
  (map-get? milestone-prerequisites { milestone-id: milestone-id })
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
(define-read-only (is-milestone-completed (milestone-id uint) (user-id principal))
  (is-some (map-get? milestone-completions { milestone-id: milestone-id, user-id: user-id }))
)

;; Get milestone completion details
(define-read-only (get-milestone-completion (milestone-id uint) (user-id principal))
  (map-get? milestone-completions { milestone-id: milestone-id, user-id: user-id })
)

;; Get relationship between two users
(define-read-only (get-user-relationship (user-id principal) (related-user-id principal))
  (map-get? user-relationships { user-id: user-id, related-user-id: related-user-id })
)

;; =============================
;; Public Functions
;; =============================

;; Register a new user
(define-public (register-user (name (string-ascii 100)) (role uint))
  (let ((user-id tx-sender))
    (asserts! (and (>= role u1) (<= role u4)) ERR-INVALID-USER-ROLE)
    (asserts! (is-none (map-get? users { user-id: user-id })) ERR-MILESTONE-ALREADY-EXISTS)
    
    (map-set users
      { user-id: user-id }
      {
        role: role,
        name: name,
        registered-at: block-height
      }
    )
    (ok true)
  )
)

;; Create a parent-child or educator-child relationship
(define-public (create-relationship (child-id principal) (relationship-type (string-ascii 20)))
  (let (
    (user-id tx-sender)
    (valid-types (list "parent-child" "educator-child"))
  )
    ;; Ensure requester is parent or educator
    (asserts! (is-user-authorized user-id (list u2 u3)) ERR-NOT-AUTHORIZED)
    ;; Ensure child exists and is role 4 (child)
    (asserts! (match (map-get? users { user-id: child-id })
                user (is-eq (get role user) u4)
                false) ERR-CHILD-NOT-REGISTERED)
    ;; Validate relationship type
    (asserts! (>= (len (filter (lambda (type) (is-eq type relationship-type)) valid-types)) u1) ERR-INVALID-PARAMETERS)
    ;; Check for duplicate relationship
    (asserts! (is-none (map-get? user-relationships { user-id: user-id, related-user-id: child-id })) ERR-DUPLICATE-RELATIONSHIP)
    
    (map-set user-relationships
      { user-id: user-id, related-user-id: child-id }
      { relationship-type: relationship-type }
    )
    (ok true)
  )
)

;; Create a new forest (collection of milestone trees)
(define-public (create-forest (name (string-ascii 100)) (description (string-ascii 500)))
  (let (
    (user-id tx-sender)
    (forest-id (get-next-forest-id))
  )
    ;; Ensure requester is admin, educator, or parent
    (asserts! (is-user-authorized user-id (list u1 u2 u3)) ERR-NOT-AUTHORIZED)
    
    (map-set forests
      { forest-id: forest-id }
      {
        name: name,
        description: description,
        created-by: user-id,
        created-at: block-height
      }
    )
    (ok forest-id)
  )
)

;; Create a new milestone
(define-public (create-milestone (title (string-ascii 100)) 
                                (description (string-ascii 500))
                                (category (string-ascii 50))
                                (difficulty-level uint)
                                (forest-id uint)
                                (parent-milestone-id (optional uint)))
  (let (
    (user-id tx-sender)
    (milestone-id (get-next-milestone-id))
  )
    ;; Ensure requester is admin, educator, or parent
    (asserts! (is-user-authorized user-id (list u1 u2 u3)) ERR-NOT-AUTHORIZED)
    ;; Ensure forest exists
    (asserts! (is-some (map-get? forests { forest-id: forest-id })) ERR-FOREST-NOT-FOUND)
    ;; Validate difficulty level (1-5)
    (asserts! (and (>= difficulty-level u1) (<= difficulty-level u5)) ERR-INVALID-PARAMETERS)
    ;; If parent milestone is specified, ensure it exists
    (asserts! (match parent-milestone-id
                parent-id (is-some (map-get? milestones { milestone-id: parent-id }))
                true) ERR-PARENT-MILESTONE-NOT-FOUND)
    
    (map-set milestones
      { milestone-id: milestone-id }
      {
        title: title,
        description: description,
        category: category,
        difficulty-level: difficulty-level,
        forest-id: forest-id,
        parent-milestone-id: parent-milestone-id,
        created-by: user-id,
        created-at: block-height
      }
    )
    (ok milestone-id)
  )
)

;; Add a prerequisite to a milestone
(define-public (add-milestone-prerequisite (milestone-id uint) (prerequisite-id uint))
  (let ((user-id tx-sender))
    ;; Ensure requester is admin, educator, or parent
    (asserts! (is-user-authorized user-id (list u1 u2 u3)) ERR-NOT-AUTHORIZED)
    ;; Ensure both milestones exist
    (asserts! (is-some (map-get? milestones { milestone-id: milestone-id })) ERR-MILESTONE-NOT-FOUND)
    (asserts! (is-some (map-get? milestones { milestone-id: prerequisite-id })) ERR-MILESTONE-NOT-FOUND)
    ;; Prevent self-prerequisite
    (asserts! (not (is-eq milestone-id prerequisite-id)) ERR-INVALID-PARAMETERS)
    
    (map-set milestone-prerequisites
      { milestone-id: milestone-id, prerequisite-id: prerequisite-id }
      { added-at: block-height }
    )
    (ok true)
  )
)

;; Mark a milestone as completed for a child
(define-public (complete-milestone (milestone-id uint) 
                                  (child-id principal) 
                                  (evidence-url (optional (string-utf8 500))))
  (let ((verifier-id tx-sender))
    ;; Ensure requester is authorized to manage this child
    (asserts! (can-manage-child verifier-id child-id) ERR-NOT-AUTHORIZED)
    ;; Ensure milestone exists
    (asserts! (is-some (map-get? milestones { milestone-id: milestone-id })) ERR-MILESTONE-NOT-FOUND)
    ;; Ensure milestone isn't already completed
    (asserts! (is-none (map-get? milestone-completions { milestone-id: milestone-id, user-id: child-id })) 
              ERR-MILESTONE-ALREADY-COMPLETED)
    ;; Ensure all prerequisites are completed
    (asserts! (are-prerequisites-completed milestone-id child-id) ERR-PREREQUISITES-NOT-COMPLETED)
    
    (map-set milestone-completions
      { milestone-id: milestone-id, user-id: child-id }
      {
        completed-at: block-height,
        verified-by: verifier-id,
        evidence-url: evidence-url
      }
    )
    (ok true)
  )
)

;; Self-completion for a milestone (for children to mark their own progress)
(define-public (self-complete-milestone (milestone-id uint) (evidence-url (optional (string-utf8 500))))
  (let ((child-id tx-sender))
    ;; Ensure user is a child
    (asserts! (match (map-get? users { user-id: child-id })
                user (is-eq (get role user) u4)
                false) ERR-INVALID-USER-ROLE)
    ;; Ensure milestone exists
    (asserts! (is-some (map-get? milestones { milestone-id: milestone-id })) ERR-MILESTONE-NOT-FOUND)
    ;; Ensure milestone isn't already completed
    (asserts! (is-none (map-get? milestone-completions { milestone-id: milestone-id, user-id: child-id })) 
              ERR-MILESTONE-ALREADY-COMPLETED)
    ;; Ensure all prerequisites are completed
    (asserts! (are-prerequisites-completed milestone-id child-id) ERR-PREREQUISITES-NOT-COMPLETED)
    
    (map-set milestone-completions
      { milestone-id: milestone-id, user-id: child-id }
      {
        completed-at: block-height,
        verified-by: child-id,
        evidence-url: evidence-url
      }
    )
    (ok true)
  )
)