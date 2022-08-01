(module pactAgent18 GOVERNANCE

  (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")
  (defconst MILLION:decimal 1000000.0)
  (defconst REACTAW:decimal 0.001)
  (defconst CRANKAW:decimal 0.01)
  (defconst CRANKK01_BANK:string 'crankk01-bank)

  (defcap GOVERNANCE ()
  (with-read paAdmin18 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defcap INTERNAL ()
    "mark some functions as internal only"
    true
  )

  (defun init (aguard:guard)
    "Create Bank and set admin...."
    (insert paAdmin18 "admin" {
      "admin": (at "sender" (chain-data)),
      "aguard": aguard
      } )
  )

  (defschema admin
    admin:string
    aguard:guard
  )

  (deftable paAdmin18:{admin})

  (defschema node
     address: string
     pubkey: string
     random: decimal
     lastAction: time
  )    

  (deftable nodes182:{node})

  (defun get-my-pubkey ()
       (with-default-read nodes182 (at "sender" (chain-data)) 
           {"pubkey": ""} 
           {"pubkey":= pubkey}
           (format "{}" [pubkey])
       )   
  )

  (defun set-my-pubkey (pubkey)
       (update nodes182 (at "sender" (chain-data)) 
           {"pubkey": pubkey} 
       )   
  )

  (defun reactivate-node ()
    (with-capability (INTERNAL)
       (with-default-read nodes182 (at "sender" (chain-data)) 
           {"address": "", "lastAction": (get-time) } 
           {"address":= readAddress, "lastAction":= readLastAction}
           (if (> (length readAddress) 0)
              (update nodes182 readAddress {"lastAction": (get-time)})
              (insert nodes182 (at "sender" (chain-data)) {"address": (at "sender" (chain-data)), "pubkey":"", "random":0.0, "lastAction": (get-time)})
           )
           (if (> (get-time) (add-time readLastAction 540))
              (award REACTAW) 
              ""    
           )
           ; reselect executor if 5 min overdue    
           (let
             ((overdue (select modules18 
              (and?
                  (where 'crank (= true))
                  (where 'net (>= (add-time (get-time) -300)))))
              ))
             
             (if (> (length overdue) 0)
                (update modules18 (at "pactModule" (at 0 overdue))
                    {"executor": (at "sender" (chain-data))}
                )
                ""
             )
            )
        )
    )
  )

  (defun award (amount)
    (require-capability (INTERNAL))
    (install-capability (free.crankk01.TRANSFER CRANKK01_BANK (at "sender" (chain-data)) amount))
    (free.crankk01.transfer CRANKK01_BANK (at "sender" (chain-data)) amount)
  )    

  (defschema crankstats
    day:string
    count:integer
  )

  (deftable crankstats18:{crankstats})

  (defun update-daily-cranks ()
    (let
      ((day (format-time "%F" (get-time))))

      (with-default-read crankstats18 day
        {"day": "", "count": 0}
        {"day":= readDay, "count":= count}
        (if (> (length readDay) 0)
           (update crankstats18 day {"count": (+ count 1)})
           (insert crankstats18 day {"day": day, "count": (+ count 1)})
        )          
      )
    )
  )

  (defun get-todays-cranks ()
    (let
      ((day (format-time "%F" (get-time))))

      (with-default-read crankstats18 day
        {"count": 0}
        {"count":= count}
        (format "{}" [count])
      )
    )
  )

  (defschema nodestats
    day:string
    count:integer
  )

  (deftable nodestats18:{nodestats})

  (defun update-daily-nodes ()
    (let*
      ((day (format-time "%F" (get-time)))
       (start (time (+ day "T00:00:00Z")))
       (nodes (select nodes182 ['address] (where 'lastAction (<= start))))
       (count (length nodes))
      )

      (with-default-read nodestats18 day
        {"day": ""}
        {"day":= readDay}
        (if (> (length readDay) 0)
           (update nodestats18 day {"count": count})
           (insert nodestats18 day {"day": day, "count": count})
        )          
      )
    )
  )

  (defun substring (str:string from:integer to:integer)
    (let*
      ((strlist (str-to-list str))
       (res (fold (substr) {"i":0, "ss":"", "from":from, "to":to} strlist))
      )

      (at "ss" res)    
    )
  )

  (defschema substrcarry
     i: integer
     ss: string
     from: integer
     to: integer
  )   

  (defun substr:object{substrcarry} (sf:object{substrcarry} char:string)
    (bind sf {
      "i":= i,
      "ss":= ss,
      "from":= from,
      "to":= to
      }
      (let 
        ((i (+ i 1)))

          (if (and (>= i from) (<= i to))
            {"i":i, "ss":(+ ss char), "from": from, "to": to}
            {"i":i, "ss":ss, "from": from, "to": to}
          )
      )
    )
  )

  (defschema modules
    pactModule:string
    sender:string
    sguard:guard
    crank:bool
    frequency:integer
    net: time
    executor: string
    createdAt:time
  )

  (defun register-module (pactModule frequency)
    (insert modules18 pactModule {
      "pactModule": pactModule,
      "frequency": frequency,
      "sender": (at "sender" (chain-data)),
      "sguard": (read-keyset "keyset"),
      "crank": true,
      "net": (get-time),
      "executor": "",
      "createdAt": (get-time)}
    )
  )

  (defun set-module-next (pactModule)
    (with-capability (INTERNAL)
        (with-read modules18 pactModule {
            "frequency":= frequency,
            "executor":= executor} 
            (enforce (= executor (at "sender" (chain-data))) "Only executor can do this")
            (update modules18 pactModule {
              "net": (add-time (get-time) frequency),
              "executor": (select-executor)}    
            )
            (update-daily-cranks)
            (award CRANKAW)
        )
    )
  )

  (deftable modules18:{modules})

  (defschema scripts
    pactModule:string
    scriptId:string
    sender:string
    sguard:guard
    frequency:integer
    minExecutors:integer
    maxExecutors:integer
    percent:integer
    function:string
    input:string
    createdAt:time
  )

  (deftable scripts18:{scripts})

  (defschema tasks
    taskId:string
    scriptId:string
    sender:string
    sguard:guard
    input:string
    resultConsensus:string
    executions:integer
    active:bool
    arbitrate:bool
    net:time
    pubkey: string
    runCount:integer
    maxRuns:integer
    stopResult:string
    createdAt:time
  )

  (deftable tasks182:{tasks})

  (defschema taskResult
    executionId:string
    result:string
  )

  (defschema resultCount
    result:string
    count:integer
  )

  (defschema executions
    executionId:string
    scriptId:string
    taskId:string
    executor:string
    result:string
    createdAt:time
  )

  (deftable executions18:{executions})

  (defun register-crank() 
    (register-module "free.pactAgent18" 600)
  )

  (defun crank()
    (update-daily-nodes)
    (set-module-next "free.pactAgent18")
  )

  (defun get-time:time ()
    "Get block time"
    (at "block-time" (chain-data))
  )

  (defun get-formattedTime:string (t:time)
    "Get formatted time"
    (format-time "%Y-%m-%dT%H:%M:%S" t)
  )

  (defun create-script (pactModule scriptId frequency minExecutors maxExecutors percent function input)
    "Create a script"
    (insert scripts18 (compound-key2 pactModule scriptId)
            { "pactModule" : pactModule,
              "scriptId": scriptId,
              "sender": (at "sender" (chain-data)),
              "sguard": (read-keyset "keyset"),
              "frequency": frequency,
              "minExecutors": minExecutors,
              "maxExecutors": maxExecutors,
              "percent": percent,
              "function": function,
              "input": input,
              "createdAt": (get-time)
            }
    )
  )

  (defun update-script (pactModule scriptId input)
    "Update a script"
    ;Only the script creator can change it
    (with-read scripts18 (compound-key2 pactModule scriptId)
      { "sguard":= sguard }
    ;   (enforce-guard sguard)
        (update scripts18 (compound-key2 pactModule scriptId)
                { 
                  "input": input
                }
        )
    )
  )

  (defun create-task (pactModule scriptId taskId input maxRuns stopResult)
    "Create a task"
    ;Only the script creator can create a task for it - is that good?
    (with-read scripts18 (compound-key2 pactModule scriptId) 
      { "sguard":= sguard }
    ;   (enforce-guard sguard)
        (insert tasks182 (compound-key4 pactModule scriptId taskId "1")
                { 
                  "scriptId": scriptId,
                  "taskId": taskId,
                  "sender": (at "sender" (chain-data)),
                  "sguard": (read-keyset "keyset"),
                  "executions": 0, 
                  "resultConsensus": "",
                  "input": input,
                  "active": true,
                  "arbitrate": false,
                  "runCount": 1,
                  "maxRuns" : maxRuns,
                  "stopResult": stopResult,
                  "net": (get-time),
                  "pubkey": (select-arbiter),    
                  "createdAt": (get-time)
                }
        )
    )
  )

  (defun get-executions-for-task (pactModule:string scriptId:string taskId:string runCount:string)    
    (select executions18 (where 'executionId (= (compound-key4 pactModule scriptId taskId runCount))))
  )

  (defun create-execution (pactModule:string scriptId:string taskId:string runCount:string result:string)
    "Create an execution"
    ;Balance check of executor will come here 
    (with-read tasks182 (compound-key4 pactModule scriptId taskId runCount)
        {"net":= net,
         "stopResult":= stopResult,
         "active":= active}
        (enforce (= active true) "Only active task can be executed")
        (enforce (>= (get-time) net) "Not earlier than must be enforced")
        ; change key later on to allow one execution per task per executor (this is done already)
        (insert executions18 (compound-key5 pactModule scriptId taskId runCount (at "sender" (chain-data)))
                { "executionId": (compound-key4 pactModule scriptId taskId runCount),
                  "taskId": taskId,
                  "scriptId": scriptId,
                  "executor": (at "sender" (chain-data)),
                  "result": result,
                  "createdAt": (get-time)
                }
        )
        (with-read scripts18 (compound-key2 pactModule scriptId)  
          {"maxExecutors":= maxExecutors,
           "frequency":= frequency}
            (let*
              (
                ;all executions for the same task
                (execId (compound-key4 pactModule scriptId taskId runCount))
                (all (select executions18 ['executionId] 
                  (where 'executionId (= execId))))
                (executions (length all))
                ; deactivate if enough executions
                (active (if (>= executions maxExecutors) false true))
                (arbitrate (if (>= executions maxExecutors) true false))
                ; copy result if enough executions
                ; (resultConsensus (if (>= executions maxExecutors) (get-topResult execId) ""))
              )     
    
                ;Increment executions, deactivate if needed, update consensus result
              (update tasks182 (compound-key4 pactModule scriptId taskId runCount)
                {
                    "executions": executions,
                    "active": active,
                    "arbitrate": arbitrate
                    ; "resultConsensus": resultConsensus
                }
              )
    
              ;If deactivated, a new one can be created
            ;   (with-read tasks182 (compound-key4 pactModule scriptId taskId runCount)
            ;     {"sender":= sender,
            ;      "sguard":= sguard,
            ;      "runCount":= runCount,
            ;      "maxRuns" := maxRuns,
            ;      "input":= input}
            ;     ;Being deactivated; runCount allows, result not satisfied 
            ;     (if (= active false)
            ;     (if (!= stopResult resultConsensus)
            ;     (if (or (< runCount maxRuns) (= 0 maxRuns))
            ;       ;insert task, decrement runCount, add frequency 
            ;         (insert tasks182 (compound-key4 pactModule scriptId taskId (int-to-str 10 (+ runCount 1)))
            ;             { "taskId": taskId,
            ;               "scriptId": scriptId,
            ;               "sender": sender,
            ;               "sguard": sguard,
            ;               "executions": 0, 
            ;               "resultConsensus": "",
            ;               "input": input,
            ;               "active": true,
            ;               "runCount": (+ runCount 1),
            ;               "maxRuns": maxRuns,
            ;               "stopResult": stopResult,
            ;               "net": (add-time (get-time) frequency),
            ;               "pubkey": (select-arbiter),   
            ;               "createdAt": (get-time)
            ;             }
            ;         )
            ;     "" )
            ;     "" )
            ;     "" )
            ;   )
            )
        )
    )
  )

  (defun set-consensus-result (pactModule:string scriptId:string taskId:string runCount:string resultConsensus:string)
      (with-read tasks182 (compound-key4 pactModule scriptId taskId runCount)
        {"sender":= sender,
         "sguard":= sguard,
         "runCount":= runCountInt,
         "maxRuns" := maxRuns,
         "active" := active,
         "arbitrate":= arbitrate,
         "stopResult":= stopResult,
         "input":= input}
        (enforce (= active false) "Cannot be active")
        (enforce (= arbitrate true) "Must be open for arbitration")
        (update tasks182 (compound-key4 pactModule scriptId taskId runCount)
          {
              "arbitrate": false,
              "resultConsensus": resultConsensus
          }
        )

        (if (!= stopResult resultConsensus)
        (if (or (< runCountInt maxRuns) (= 0 maxRuns))
            ;insert task, decrement runCount, add frequency 
            (with-read scripts18 (compound-key2 pactModule scriptId)  
              {"frequency":= frequency}
                (insert tasks182 (compound-key4 pactModule scriptId taskId (int-to-str 10 (+ runCountInt 1)))
                    { "taskId": taskId,
                      "scriptId": scriptId,
                      "sender": sender,
                      "sguard": sguard,
                      "executions": 0, 
                      "resultConsensus": "",
                      "input": input,
                      "active": true,
                      "arbitrate": false,
                      "runCount": (+ runCountInt 1),
                      "maxRuns": maxRuns,
                      "stopResult": stopResult,
                      "net": (add-time (get-time) frequency),
                      "pubkey": (select-arbiter),   
                      "createdAt": (get-time)
                    }
                )
            )
        "" )
        "" )
      )
  )

  (defun get-scripts ()
    "Get all scripts"
    (select scripts18 (constantly true))
  )

  (defun get-tasks ()
    "Get all tasks"
    (select tasks182 (constantly true))
  )

  (defun get-result (taskId)
    "Get consensus result for a tasks"
    (let
      (
        (finished (reverse (sort ['createdAt]
            (select tasks182 ['resultConsensus 'scriptId 'net 'createdAt] 
                (and?
                (where 'taskId (= taskId))
                (where 'active (= false))
                )
            )
        )))
      )
      ;Return latest consensus result    
      (if (> (length finished) 0) (at "resultConsensus" (at 0 finished)) "")
    )
  )

  (defun get-executions ()
    "Get all executions"
    (select executions18 (constantly true))
  )

  (defun get-nodes ()
    "Get all nodes"
    (select nodes182 (constantly true))
  )

  (defun get-nodestats ()
    "Get all nodestats"
    (select nodestats18 (constantly true))
  )

  (defun get-crankstats ()
    "Get all crankstats"
    (select crankstats18 (constantly true))
  )

  (defun select-executor ()
    "Select executor"
    (let*
      ((activeNodes (get-activeNodes))
      (activeNodeCount (length activeNodes)) 
      (randomIndex (ceiling (* (/ (str-to-int (format-time "%v" (get-time))) MILLION) activeNodeCount))))
      
      (if (> activeNodeCount 0)
         (at "address" (at (- randomIndex 1) activeNodes))
         ""
      )
    )
  )

  (defun select-arbiter ()
    "Select arbiter"
    (let*
      ((arbiterNodes (get-activeArbiterNodes))
      (arbiterNodeCount (length arbiterNodes)) 
      (randomIndex (ceiling (* (/ (str-to-int (format-time "%v" (get-time))) MILLION) arbiterNodeCount))))
      
      (if (> arbiterNodeCount 0)
         (at "pubkey" (at (- randomIndex 1) arbiterNodes))
         ""
      )
    )
  )

  (defschema mapped-node
     index:integer
     address: string
  )    

  (defun select-executors (count:integer)
    "Select 'count' number of executors"
    (let*
      ((activeNodes (get-activeNodes))
      (activeNodeCount (length activeNodes)) 
      (randomIndex (ceiling (* (/ (str-to-int (format-time "%v" (get-time))) MILLION) activeNodeCount)))
      (tail (take (- randomIndex activeNodeCount) activeNodes)))

      (if (< count (length tail))
        (take count tail)
        (if (> count (length tail)) 
          tail
          tail
        )  
      )
    ;   (fold (map-node) [{"index":randomIndex, "address":""}] activeNodes)
    )
  )

;   (defun map-node:[object:{mapped-node}] (sf:[object:{mapped-node}] node:object{node})
;     (bind mapped-node { 
;       "index" := index }
;     (bind node { 
;       "address" := address }
;       (+ sf [{"index":0, "address":address}]) 
;     )
;     )
;   )


  (defun get-activeNodes ()
    "Get active nodes"
    (select nodes182 (where 'lastAction (<= (add-time (get-time) -900))))
  )

  (defun get-activeArbiterNodes ()
    "Get active arbiter nodes"
    (select nodes182 
      (and? 
        (where 'lastAction (<= (add-time (get-time) -900)))
        (where 'pubkey (!= ""))
      )
    )
  )

  (defun get-taskExecutions (taskId)
    "Get all executions for a task"
    (select executions18 (where 'taskId (= taskId)))
  )

  (defun count-items:object{resultCount} (sf:object{resultCount} taskResult:object{taskResult})
    "Count items"
    (bind sf { 
       "result" := oldResult,
       "count" := count }
    (bind taskResult { 
       "executionId" := executionId,
       "result" := result }
       (let*
         (     
           (results (select executions18
             (and?
             (where 'executionId (= executionId))
             (where 'result (= result))
             )
           ))
           (records (length results))
         )
         (if (> records count) {"result":result, "count":records} sf)
       )
    )
    )
  )

  (defun get-topResult (execId)
    "Get top result for a task"
    (let*
      (
        (all (select executions18 ['executionId 'result] (where 'executionId (= execId))))
        (dist (distinct all))   
        (res (fold (count-items) {"result":"", "count":0} dist))
      )     
      (at "result" res)
    )
  )

  (defun get-activeTasks ()
    "Get active tasks"
    (select tasks182 (where 'active (= true)))
  )

  (defun get-arbitrateTasks ()
    "Get arbitrate tasks"
    (select tasks182 (where 'arbitrate (= true)))
  )

  (defun get-activeModules ()
    "Get active modules"
    (select modules18 (where 'crank (= true)))
  )

  (defun get-cranks ()
    "Get cranks"
    (let*
        ((all (select modules18 
          (and?
          (where 'crank (= true))
          (where 'net (>= (get-time)))))
         )
        (mine (filter (where 'executor (= (at "sender" (chain-data)))) all)))
        ; return empty array if nothing
        (if (> (length mine) 0) mine [])
    )
  )

  (defun deactivate-activeTasks ()
    "Deactivate active tasks"
      (fold (deactivate-item) "" (keys tasks182))
  )

  (defun deactivate-item (sf key)
    (update tasks182 key
      {"active": false} 
    )  
  )

  (defun get-taskKeys ()
    (keys tasks182)
  )

  (defun get-scriptKeys ()
    (keys scripts18)
  )

  (defun update-finishedTasks ()
    "Update finished tasks"
    (select tasks182 (where 'active (= true)))
  )

  (defun read-admin ()
    "Read admin guard"
    (with-read paAdmin18 "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
  )

  (defun test-admin ()
    "Test admin guard"
  (with-read paAdmin18 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
      (format "Admin guard success! {}" [aguard] )
    )
  )

  (defun compound-key2:string
    ( part1:string
      part2:string )
    (format "{}{}{}" [part1 ROW_KEY_SEPARATOR part2])
  )

  (defun compound-key3:string
    ( part1:string
      part2:string
      part3:string )
    (format "{}{}{}{}{}" [part1 ROW_KEY_SEPARATOR part2 ROW_KEY_SEPARATOR part3])
  )

  (defun compound-key4:string
    ( part1:string
      part2:string
      part3:string
      part4:string )
    (format "{}{}{}{}{}{}{}" [part1 ROW_KEY_SEPARATOR part2 ROW_KEY_SEPARATOR part3 ROW_KEY_SEPARATOR part4])
  )

  (defun compound-key5:string
    ( part1:string
      part2:string
      part3:string
      part4:string 
      part5:string )
    (format "{}{}{}{}{}{}{}{}{}" [part1 ROW_KEY_SEPARATOR part2 ROW_KEY_SEPARATOR part3 ROW_KEY_SEPARATOR part4 ROW_KEY_SEPARATOR part5])
  )

  (defun show-msg ()
    (read-msg)
  )
  
  (defun init-bank ()
    (free.crankk01.create-account CRANKK01_BANK (create-module-guard "CRANKK01"))
  )

)
; create-table

