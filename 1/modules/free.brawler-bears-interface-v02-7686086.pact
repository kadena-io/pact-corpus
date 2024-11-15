(interface brawler-bears-interface-v02
    (defun replenish-bulk:string (account:string attacks:list))
    (defun equip:string (bear-id:string id:string))
    (defun unequip:string (bear-id:string id:string))
    (defun get-item-details:object (id:string))
)

