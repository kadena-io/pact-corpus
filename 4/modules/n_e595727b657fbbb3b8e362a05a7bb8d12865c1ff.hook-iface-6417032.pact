(interface hook-iface

    (use hyperlane-message [hyperlane-message])

    (defun post-dispatch:bool (id:string message:object{hyperlane-message}))
)
