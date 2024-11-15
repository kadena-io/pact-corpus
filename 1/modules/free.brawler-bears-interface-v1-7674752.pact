(interface brawler-bears-interface-v1

    (defun perform-attack:bool (id:string))
    (defun replenish-bulk:string (account:string attacks:list))
    (defun equip:string (bear-id:string id:string))
    (defun unequip:string (bear-id:string id:string))
  
)
