(module hello-world 'admin-keyset
  "A smart contract to greet the world."
  (defun hello (name:string)
    "Do the hello-world dance"
    (format "Hello {}!" [name]))
)

;; and say hello!

