(module sgk-rewards-util GOVERNANCE
  @doc "rewards utils."
  
  
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")))


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
)
