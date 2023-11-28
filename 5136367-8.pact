(module test-testing-testing-test GOVERNANCE 
@doc "rewards utils."

(defcap GOVERNANCE () (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;"test-sgk-weapons-1-1" "test-sgk-weapons-2-1" "test-sgk-weapons-3-1" 
(defconst COL_NAMES [
  "test-sgk-weapons-4-1" "test-sgk-weapons-5-1"];; "test-sgk-weapons-6-1" "test-sgk-weapons-7-1"
 ;; "test-sgk-weapons-8-1" "test-sgk-weapons-9-1" "test-sgk-weapons-10-1" "test-sgk-weapons-11-1" "test-sgk-weapons-12-1" "test-sgk-weapons-13-1" "test-sgk-weapons-14-1" 
  ;;"test-sgk-weapons-15-1" "test-sgk-weapons-16-1" "test-sgk-weapons-17-1" "test-sgk-weapons-18-1" "test-sgk-weapons-19-1" "test-sgk-weapons-20-1" "test-sgk-weapons-21-1" 
  ;;"test-sgk-weapons-22-1" "test-sgk-weapons-23-1" "test-sgk-weapons-24-1" "test-sgk-weapons-25-1" "test-sgk-weapons-26-1" "test-sgk-weapons-27-1" "test-sgk-weapons-28-1"
  ;;"test-sgk-weapons-29-1" "test-sgk-weapons-30-1"  "test-sgk-weapons-31-1" "test-sgk-weapons-32-1"]
  @doc "All collection names")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Pub method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun get-col-names()
      COL_NAMES
  )



  (defun enforce-mint:bool
      ( 
      purpose1:string

      )

       (let* 
                  (
                      (purpose (read-msg "purpose"))

                  )
                   (if (= purpose "gen-0-rewards" ) (meth-1 "")
                          [(enforce false "not available to mint now")]
                  )
              )
  )

  (defun enforce-mint-cond:bool
      ( 
      purpose1:string

      )

       (let* 
                  (
                      (purpose (read-msg "purpose"))

                  )

                  (cond 
                      ( (= purpose "gen-0-rewards" ) (meth-1 ""))
                      ( (= purpose "BH" ) (meth-2 "x"))
                      [(enforce false "not available to mint now")])
                  true
              )
  )
  
  
  (defun meth-1 (s:string)

   1

  )

  (defun meth-2 (s:string)
   s
  )
  
  (defun lol (s:string)
    s
  )
  )
