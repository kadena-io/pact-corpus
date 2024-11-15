(interface brawler-bears-interface-v2

    (defun perform-attack:string (nft-id:string battle-id:string attack-id:string default:bool))
    (defun replenish-bulk:string (account:string ids:list))
    (defun equip:string (bear-id:string id:string))
    (defun unequip:string (bear-id:string id:string))
)

