;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_TIME (err u103))
(define-constant ERR_CAPACITY_FULL (err u104))
(define-constant ERR_NOT_PARTICIPANT (err u105))
(define-constant ERR_INVALID_STATUS (err u106))

;; Data Variables
(define-data-var contract-active bool true)
(define-data-var next-carpool-id uint u1)
(define-data-var next-route-id uint u1)

;; Data Maps
;; Parent profiles with emergency contact information
(define-map parents principal {
    name: (string-ascii 50),
    phone: (string-ascii 15),
    emergency-contact: (string-ascii 15),
    emergency-name: (string-ascii 50),
    active: bool,
    created-at: uint
})

;; Children profiles linked to parents
(define-map children uint {
    name: (string-ascii 50),
    parent: principal,
    grade: uint,
    pickup-location: (string-ascii 100),
    dropoff-location: (string-ascii 100),
    special-needs: (string-ascii 200),
    active: bool,
    created-at: uint
})

;; Carpool schedules and coordination
(define-map carpools uint {
    driver: principal,
    date: uint,
    pickup-time: uint,
    dropoff-time: uint,
    route-id: uint,
    capacity: uint,
    current-riders: uint,
    status: (string-ascii 20), ;; "scheduled", "in-progress", "completed", "cancelled"
    created-at: uint
})

;; Route information for tracking
(define-map routes uint {
    name: (string-ascii 50),
    waypoints: (list 10 (string-ascii 100)),
    estimated-duration: uint, ;; in minutes
    active: bool,
    created-by: principal,
    created-at: uint
})

;; Carpool participants (many-to-many relationship)
(define-map carpool-participants { carpool-id: uint, child-id: uint } {
    pickup-location: (string-ascii 100),
    dropoff-location: (string-ascii 100),
    status: (string-ascii 20), ;; "confirmed", "picked-up", "dropped-off", "cancelled"
    joined-at: uint
})

;; Child ID counter
(define-data-var next-child-id uint u1)

;; Read-only functions
(define-read-only (get-contract-info)
    {
        active: (var-get contract-active),
        total-carpools: (- (var-get next-carpool-id) u1),
        total-routes: (- (var-get next-route-id) u1),
        contract-owner: CONTRACT_OWNER
    }
)

(define-read-only (get-parent (parent-address principal))
    (map-get? parents parent-address)
)

(define-read-only (get-child (child-id uint))
    (map-get? children child-id)
)

(define-read-only (get-carpool (carpool-id uint))
    (map-get? carpools carpool-id)
)

(define-read-only (get-route (route-id uint))
    (map-get? routes route-id)
)

(define-read-only (get-carpool-participant (carpool-id uint) (child-id uint))
    (map-get? carpool-participants { carpool-id: carpool-id, child-id: child-id })
)

(define-read-only (is-parent-registered (parent-address principal))
    (is-some (map-get? parents parent-address))
)

;; Parent registration and management
(define-public (register-parent (name (string-ascii 50)) (phone (string-ascii 15)) (emergency-contact (string-ascii 15)) (emergency-name (string-ascii 50)))
    (begin
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? parents tx-sender)) ERR_ALREADY_EXISTS)
        (ok (map-set parents tx-sender {
            name: name,
            phone: phone,
            emergency-contact: emergency-contact,
            emergency-name: emergency-name,
            active: true,
            created-at: stacks-block-height
        }))
    )
)

(define-public (update-parent-info (phone (string-ascii 15)) (emergency-contact (string-ascii 15)) (emergency-name (string-ascii 50)))
    (let ((existing-parent (unwrap! (map-get? parents tx-sender) ERR_NOT_FOUND)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (ok (map-set parents tx-sender (merge existing-parent {
            phone: phone,
            emergency-contact: emergency-contact,
            emergency-name: emergency-name
        })))
    )
)

;; Child management
(define-public (add-child (name (string-ascii 50)) (grade uint) (pickup-location (string-ascii 100)) (dropoff-location (string-ascii 100)) (special-needs (string-ascii 200)))
    (let ((child-id (var-get next-child-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? parents tx-sender)) ERR_NOT_FOUND)
        (map-set children child-id {
            name: name,
            parent: tx-sender,
            grade: grade,
            pickup-location: pickup-location,
            dropoff-location: dropoff-location,
            special-needs: special-needs,
            active: true,
            created-at: stacks-block-height
        })
        (var-set next-child-id (+ child-id u1))
        (ok child-id)
    )
)

(define-public (update-child-locations (child-id uint) (pickup-location (string-ascii 100)) (dropoff-location (string-ascii 100)))
    (let ((existing-child (unwrap! (map-get? children child-id) ERR_NOT_FOUND)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get parent existing-child) tx-sender) ERR_UNAUTHORIZED)
        (ok (map-set children child-id (merge existing-child {
            pickup-location: pickup-location,
            dropoff-location: dropoff-location
        })))
    )
)

;; Route management
(define-public (create-route (name (string-ascii 50)) (waypoints (list 10 (string-ascii 100))) (estimated-duration uint))
    (let ((route-id (var-get next-route-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? parents tx-sender)) ERR_UNAUTHORIZED)
        (map-set routes route-id {
            name: name,
            waypoints: waypoints,
            estimated-duration: estimated-duration,
            active: true,
            created-by: tx-sender,
            created-at: stacks-block-height
        })
        (var-set next-route-id (+ route-id u1))
        (ok route-id)
    )
)

;; Carpool scheduling
(define-public (schedule-carpool (date uint) (pickup-time uint) (dropoff-time uint) (route-id uint) (capacity uint))
    (let ((carpool-id (var-get next-carpool-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? parents tx-sender)) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? routes route-id)) ERR_NOT_FOUND)
        (asserts! (> capacity u0) ERR_INVALID_STATUS)
        (asserts! (< pickup-time dropoff-time) ERR_INVALID_TIME)
        (map-set carpools carpool-id {
            driver: tx-sender,
            date: date,
            pickup-time: pickup-time,
            dropoff-time: dropoff-time,
            route-id: route-id,
            capacity: capacity,
            current-riders: u0,
            status: "scheduled",
            created-at: stacks-block-height
        })
        (var-set next-carpool-id (+ carpool-id u1))
        (ok carpool-id)
    )
)

;; Join carpool
(define-public (join-carpool (carpool-id uint) (child-id uint) (pickup-location (string-ascii 100)) (dropoff-location (string-ascii 100)))
    (let (
        (carpool-info (unwrap! (map-get? carpools carpool-id) ERR_NOT_FOUND))
        (child-info (unwrap! (map-get? children child-id) ERR_NOT_FOUND))
        (participant-key { carpool-id: carpool-id, child-id: child-id })
    )
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get parent child-info) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? carpool-participants participant-key)) ERR_ALREADY_EXISTS)
        (asserts! (< (get current-riders carpool-info) (get capacity carpool-info)) ERR_CAPACITY_FULL)
        (asserts! (is-eq (get status carpool-info) "scheduled") ERR_INVALID_STATUS)

        ;; Add participant
        (map-set carpool-participants participant-key {
            pickup-location: pickup-location,
            dropoff-location: dropoff-location,
            status: "confirmed",
            joined-at: stacks-block-height
        })

        ;; Update carpool rider count
        (map-set carpools carpool-id (merge carpool-info {
            current-riders: (+ (get current-riders carpool-info) u1)
        }))

        (ok true)
    )
)

;; Update carpool status (driver only)
(define-public (update-carpool-status (carpool-id uint) (new-status (string-ascii 20)))
    (let ((carpool-info (unwrap! (map-get? carpools carpool-id) ERR_NOT_FOUND)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get driver carpool-info) tx-sender) ERR_UNAUTHORIZED)
        (ok (map-set carpools carpool-id (merge carpool-info {
            status: new-status
        })))
    )
)

;; Update participant status (driver or parent)
(define-public (update-participant-status (carpool-id uint) (child-id uint) (new-status (string-ascii 20)))
    (let (
        (carpool-info (unwrap! (map-get? carpools carpool-id) ERR_NOT_FOUND))
        (child-info (unwrap! (map-get? children child-id) ERR_NOT_FOUND))
        (participant-key { carpool-id: carpool-id, child-id: child-id })
        (participant-info (unwrap! (map-get? carpool-participants participant-key) ERR_NOT_FOUND))
    )
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        ;; Either the driver or the child's parent can update status
        (asserts! (or (is-eq (get driver carpool-info) tx-sender) (is-eq (get parent child-info) tx-sender)) ERR_UNAUTHORIZED)
        (ok (map-set carpool-participants participant-key (merge participant-info {
            status: new-status
        })))
    )
)

;; Emergency: Cancel carpool (driver only)
(define-public (cancel-carpool (carpool-id uint))
    (let ((carpool-info (unwrap! (map-get? carpools carpool-id) ERR_NOT_FOUND)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get driver carpool-info) tx-sender) ERR_UNAUTHORIZED)
        (ok (map-set carpools carpool-id (merge carpool-info {
            status: "cancelled"
        })))
    )
)

;; Leave carpool (parent only)
(define-public (leave-carpool (carpool-id uint) (child-id uint))
    (let (
        (carpool-info (unwrap! (map-get? carpools carpool-id) ERR_NOT_FOUND))
        (child-info (unwrap! (map-get? children child-id) ERR_NOT_FOUND))
        (participant-key { carpool-id: carpool-id, child-id: child-id })
        (participant-info (unwrap! (map-get? carpool-participants participant-key) ERR_NOT_FOUND))
    )
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get parent child-info) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status participant-info) "confirmed") ERR_INVALID_STATUS)

        ;; Update participant status to cancelled
        (map-set carpool-participants participant-key (merge participant-info {
            status: "cancelled"
        }))

        ;; Decrease rider count
        (map-set carpools carpool-id (merge carpool-info {
            current-riders: (- (get current-riders carpool-info) u1)
        }))

        (ok true)
    )
)

;; Admin functions (contract owner only)
(define-public (toggle-contract (active bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-active active)
        (ok active)
    )
)

(define-public (deactivate-route (route-id uint))
    (let ((route-info (unwrap! (map-get? routes route-id) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (ok (map-set routes route-id (merge route-info {
            active: false
        })))
    )
)
