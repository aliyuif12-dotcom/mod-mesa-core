;; ModMesa Core - Hybrid Blockchain-DNS Infrastructure
;; A smart contract for bridging Web2 and Web3 naming systems

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-name (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-invalid-resolver (err u106))

;; Registration and renewal fees (in microSTX)
(define-constant registration-fee u1000000) ;; 1 STX
(define-constant renewal-fee u500000) ;; 0.5 STX
(define-constant min-stake-amount u5000000) ;; 5 STX for proxy nodes

;; Data Variables
(define-data-var total-domains uint u0)
(define-data-var total-proxy-nodes uint u0)

;; Domain Registry
;; Maps domain names to their registration details
(define-map domains
  { name: (string-ascii 64) }
  {
    owner: principal,
    resolver: principal,
    registered-at: uint,
    expires-at: uint,
    dns-record: (string-ascii 256),
    chain-id: (string-ascii 32)
  }
)

;; Multi-Chain Name Synthesis
;; Tracks domain ownership across chains to prevent conflicts
(define-map chain-domains
  { chain-id: (string-ascii 32), name: (string-ascii 64) }
  {
    owner: principal,
    verified: bool,
    registered-at: uint
  }
)

;; Resolver Reputation System
;; Time-weighted reputation for domain resolvers
(define-map resolver-reputation
  { resolver: principal }
  {
    total-resolutions: uint,
    successful-resolutions: uint,
    reputation-score: uint,
    last-update: uint
  }
)

;; DNS Proxy Nodes
;; Network of distributed DNS proxy operators
(define-map proxy-nodes
  { operator: principal }
  {
    stake-amount: uint,
    active: bool,
    total-requests: uint,
    successful-requests: uint,
    rewards-earned: uint,
    registered-at: uint
  }
)

;; Subdomain Delegation
;; Programmable subdomain access controls
(define-map subdomain-permissions
  { parent-domain: (string-ascii 64), subdomain: (string-ascii 64) }
  {
    delegated-to: principal,
    can-transfer: bool,
    can-modify: bool,
    created-at: uint
  }
)

;; Node Stake Tracking
(define-map node-stakes
  { operator: principal }
  { amount: uint }
)

;; Read-only functions

(define-read-only (get-domain (name (string-ascii 64)))
  (map-get? domains { name: name })
)

(define-read-only (get-domain-owner (name (string-ascii 64)))
  (match (map-get? domains { name: name })
    domain-info (ok (get owner domain-info))
    err-not-found
  )
)

(define-read-only (get-resolver-reputation (resolver principal))
  (default-to 
    { total-resolutions: u0, successful-resolutions: u0, reputation-score: u0, last-update: u0 }
    (map-get? resolver-reputation { resolver: resolver })
  )
)

(define-read-only (get-proxy-node (operator principal))
  (map-get? proxy-nodes { operator: operator })
)

(define-read-only (is-domain-available (name (string-ascii 64)))
  (match (map-get? domains { name: name })
    domain-info 
      (if (> block-height (get expires-at domain-info))
        (ok true)
        (ok false)
      )
    (ok true)
  )
)

(define-read-only (get-total-domains)
  (ok (var-get total-domains))
)

(define-read-only (get-total-proxy-nodes)
  (ok (var-get total-proxy-nodes))
)

(define-read-only (check-chain-domain (chain-id-param (string-ascii 32)) (name (string-ascii 64)))
  (map-get? chain-domains { chain-id: chain-id-param, name: name })
)

;; Public functions

;; Register a new domain
(define-public (register-domain 
    (name (string-ascii 64))
    (dns-record (string-ascii 256))
    (chain-id-param (string-ascii 32))
    (duration uint)
  )
  (let
    (
      (expires-at (+ block-height (* duration u144))) ;; Approximate blocks per day
    )
    ;; Check if domain is available
    (asserts! (unwrap! (is-domain-available name) err-invalid-name) err-already-exists)
    
    ;; Transfer registration fee
    (try! (stx-transfer? registration-fee tx-sender contract-owner))
    
    ;; Register domain
    (map-set domains
      { name: name }
      {
        owner: tx-sender,
        resolver: tx-sender,
        registered-at: block-height,
        expires-at: expires-at,
        dns-record: dns-record,
        chain-id: chain-id-param
      }
    )
    
    ;; Register in multi-chain synthesis
    (map-set chain-domains
      { chain-id: chain-id-param, name: name }
      {
        owner: tx-sender,
        verified: true,
        registered-at: block-height
      }
    )
    
    ;; Increment total domains
    (var-set total-domains (+ (var-get total-domains) u1))
    
    (ok true)
  )
)

;; Renew domain registration
(define-public (renew-domain (name (string-ascii 64)) (duration uint))
  (let
    (
      (domain-info (unwrap! (map-get? domains { name: name }) err-not-found))
      (new-expiry (+ (get expires-at domain-info) (* duration u144)))
    )
    ;; Check ownership
    (asserts! (is-eq tx-sender (get owner domain-info)) err-unauthorized)
    
    ;; Transfer renewal fee
    (try! (stx-transfer? renewal-fee tx-sender contract-owner))
    
    ;; Update expiry
    (map-set domains
      { name: name }
      (merge domain-info { expires-at: new-expiry })
    )
    
    (ok true)
  )
)

;; Update DNS record
(define-public (update-dns-record (name (string-ascii 64)) (new-record (string-ascii 256)))
  (let
    (
      (domain-info (unwrap! (map-get? domains { name: name }) err-not-found))
    )
    ;; Check ownership
    (asserts! (is-eq tx-sender (get owner domain-info)) err-unauthorized)
    
    ;; Update DNS record
    (map-set domains
      { name: name }
      (merge domain-info { dns-record: new-record })
    )
    
    (ok true)
  )
)

;; Set domain resolver
(define-public (set-resolver (name (string-ascii 64)) (new-resolver principal))
  (let
    (
      (domain-info (unwrap! (map-get? domains { name: name }) err-not-found))
    )
    ;; Check ownership
    (asserts! (is-eq tx-sender (get owner domain-info)) err-unauthorized)
    
    ;; Update resolver
    (map-set domains
      { name: name }
      (merge domain-info { resolver: new-resolver })
    )
    
    (ok true)
  )
)

;; Register as DNS proxy node operator
(define-public (register-proxy-node)
  (begin
    ;; Check minimum stake
    (asserts! (>= (stx-get-balance tx-sender) min-stake-amount) err-insufficient-balance)
    
    ;; Transfer stake
    (try! (stx-transfer? min-stake-amount tx-sender (as-contract tx-sender)))
    
    ;; Register node
    (map-set proxy-nodes
      { operator: tx-sender }
      {
        stake-amount: min-stake-amount,
        active: true,
        total-requests: u0,
        successful-requests: u0,
        rewards-earned: u0,
        registered-at: block-height
      }
    )
    
    ;; Track stake
    (map-set node-stakes
      { operator: tx-sender }
      { amount: min-stake-amount }
    )
    
    ;; Increment total proxy nodes
    (var-set total-proxy-nodes (+ (var-get total-proxy-nodes) u1))
    
    (ok true)
  )
)

;; Record successful resolution (called by resolver)
(define-public (record-resolution (resolver principal) (success bool))
  (let
    (
      (current-rep (get-resolver-reputation resolver))
      (new-total (+ (get total-resolutions current-rep) u1))
      (new-successful (if success (+ (get successful-resolutions current-rep) u1) (get successful-resolutions current-rep)))
      (new-score (/ (* new-successful u100) new-total))
    )
    (map-set resolver-reputation
      { resolver: resolver }
      {
        total-resolutions: new-total,
        successful-resolutions: new-successful,
        reputation-score: new-score,
        last-update: block-height
      }
    )
    (ok true)
  )
)

;; Delegate subdomain with permissions
(define-public (delegate-subdomain
    (parent-domain (string-ascii 64))
    (subdomain (string-ascii 64))
    (delegated-to principal)
    (can-transfer bool)
    (can-modify bool)
  )
  (let
    (
      (domain-info (unwrap! (map-get? domains { name: parent-domain }) err-not-found))
    )
    ;; Check ownership
    (asserts! (is-eq tx-sender (get owner domain-info)) err-unauthorized)
    
    ;; Create subdomain delegation
    (map-set subdomain-permissions
      { parent-domain: parent-domain, subdomain: subdomain }
      {
        delegated-to: delegated-to,
        can-transfer: can-transfer,
        can-modify: can-modify,
        created-at: block-height
      }
    )
    
    (ok true)
  )
)

;; Reward proxy node for processing requests
(define-public (reward-proxy-node (operator principal) (reward-amount uint))
  (let
    (
      (node-info (unwrap! (map-get? proxy-nodes { operator: operator }) err-not-found))
    )
    ;; Only contract owner can distribute rewards
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Update node statistics
    (map-set proxy-nodes
      { operator: operator }
      (merge node-info 
        {
          successful-requests: (+ (get successful-requests node-info) u1),
          total-requests: (+ (get total-requests node-info) u1),
          rewards-earned: (+ (get rewards-earned node-info) reward-amount)
        }
      )
    )
    
    ;; Transfer reward
    (as-contract (stx-transfer? reward-amount tx-sender operator))
  )
)

;; Deactivate proxy node and withdraw stake
(define-public (deactivate-proxy-node)
  (let
    (
      (node-info (unwrap! (map-get? proxy-nodes { operator: tx-sender }) err-not-found))
      (stake-info (unwrap! (map-get? node-stakes { operator: tx-sender }) err-not-found))
    )
    ;; Mark as inactive
    (map-set proxy-nodes
      { operator: tx-sender }
      (merge node-info { active: false })
    )
    
    ;; Return stake
    (as-contract (stx-transfer? (get amount stake-info) tx-sender tx-sender))
  )
)

;; Transfer domain ownership
(define-public (transfer-domain (name (string-ascii 64)) (new-owner principal))
  (let
    (
      (domain-info (unwrap! (map-get? domains { name: name }) err-not-found))
    )
    ;; Check ownership
    (asserts! (is-eq tx-sender (get owner domain-info)) err-unauthorized)
    
    ;; Transfer ownership
    (map-set domains
      { name: name }
      (merge domain-info { owner: new-owner })
    )
    
    (ok true)
  )
)