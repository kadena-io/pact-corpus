(interface brawler-bears-attacks-interface-v0
    (defun replenish-bulk:string (account:string attacks:list))
    (defun equip:string (bear-id:string id:string))
    (defun unequip:string (bear-id:string id:string))
)

(interface brawler-bears-interface-v0
    (defun perform-attack:string (nft-id:string battle-id:string attack-id:string default:bool))
)


