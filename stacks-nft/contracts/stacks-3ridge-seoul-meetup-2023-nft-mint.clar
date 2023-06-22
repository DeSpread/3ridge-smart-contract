;; Storage
(define-map presale-count principal uint)
(define-map treasure-count principal uint)

;; Define Constants
(define-constant CONTRACT-OWNER tx-sender)

;; Define error codes
(define-constant ERR-NOT-AUTHORIZED (err u201))
(define-constant ERR-SALE-NOT-ACTIVE (err u202))
(define-constant ERR-NO-TREASURE-AMOUNT-REMAINING (err u203))
(define-constant ERR-NO-PRE-SALE-REMAINING (err u204))

;; Define Variables
(define-data-var pre-sale-active bool false)
(define-data-var public-sale-active bool false)

;; Get activation of sale
(define-read-only (get-pre-sale-active)
  (ok (var-get pre-sale-active)))

(define-read-only (get-public-sale-active)
  (ok (var-get public-sale-active)))

;; Get balance of treasure
(define-read-only (get-treasure-balance (account principal))
  (default-to u0
    (map-get? treasure-count account)))

;; Get balance of pre sale
(define-read-only (get-presale-balance (account principal))
  (default-to u0
    (map-get? presale-count account)))

;; Mint: a new NFT
(define-public (mint)
  (if (var-get pre-sale-active)
    (pre-mint tx-sender)
    (public-mint tx-sender)))

(define-public (mint-two)
  (begin
    (try! (mint))
    (try! (mint))
    (ok true)))

(define-public (mint-three)
  (begin
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (ok true)))

(define-public (mint-four)
  (begin
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (ok true)))

(define-public (mint-five)
  (begin
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (ok true)))

(define-private (treasure-mint (new-owner principal))
  (begin
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner)) (try! (pre-treasure-mint new-owner))
    (ok true)))

;; Mint: reward NFT
(define-private (reward-mint (new-owner principal))
  (begin
    (try! (pre-treasure-mint new-owner))
    (ok true)))

;; Mint: treasure NFT
(define-private (pre-treasure-mint (new-owner principal))
  (let ((treasure-balance (get-treasure-balance new-owner)))
      (asserts! (> treasure-balance u0) ERR-NO-TREASURE-AMOUNT-REMAINING)
      (map-set treasure-count
                new-owner
                (- treasure-balance u1))
      (contract-call? .stacks-3ridge-seoul-meetup-2023-nft treasure-mint new-owner)))

;; Mint: pre sale NFT
(define-private (pre-mint (new-owner principal))
  (let ((presale-balance (get-presale-balance new-owner)))
    (asserts! (> presale-balance u0) ERR-NO-PRE-SALE-REMAINING)
    (map-set presale-count
              new-owner
              (- presale-balance u1))
  (contract-call? .stacks-3ridge-seoul-meetup-2023-nft mint new-owner)))

;; Mint: public sale NFT
(define-private (public-mint (new-owner principal))
  (begin
    (asserts! (var-get public-sale-active) ERR-SALE-NOT-ACTIVE)
    (contract-call? .stacks-3ridge-seoul-meetup-2023-nft mint new-owner)))

;; Flip flag for pre sale
(define-public (flip-pre-sale)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    ;; Disable the Public sale
    (var-set public-sale-active false)
    (var-set pre-sale-active (not (var-get pre-sale-active)))
    (ok (var-get pre-sale-active))))

;; Flip flag for public sale
(define-public (flip-public-sale)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    ;; Disable the public sale
    (var-set pre-sale-active false)
    (var-set public-sale-active (not (var-get public-sale-active)))
    (ok (var-get public-sale-active))))

;; Initialize address for minting
(as-contract (contract-call? .stacks-3ridge-seoul-meetup-2023-nft set-mint-address))

;; Treasure Mint Addresses
;; (map-set treasure-count CONTRACT-OWNER u50)
(map-set treasure-count 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 u3)
(map-set treasure-count 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG u2)

;; Treasure Mint
;; (treasure-mint tx-sender)
(reward-mint 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)
(reward-mint 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)
(reward-mint 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)
(reward-mint 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG)
(reward-mint 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG)