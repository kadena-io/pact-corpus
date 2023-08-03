(interface brawler-bears-interface

  (defun perform-attack:bool (nft-id:string battle-id:string attack-id:string default:bool))
  (defun replenish-bulk:string (attacks:list))
  (defun equip:string (bear-id:string id:string))
  (defun unequip:string (bear-id:string id:string))

)

