(interface brawler-bears-attacks-interface-v1
    (defun replenish-bulk (account:string attacks:list))
    (defun equip (bear-id:string id:string))
    (defun unequip (bear-id:string id:string))
)

(interface brawler-bears-interface-v4
    (defun perform-attack (nft-id:string battle-id:string attack-id:string default:bool))
)


