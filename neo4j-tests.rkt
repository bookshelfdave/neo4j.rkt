#lang racket
(require rackunit
         "neo4j.rkt")

(define conn (neo4j-init "http://localhost:7474/db/data"))

(define test-node 'nil)
(define test-node-with-props 'nil)
(define test-rel-node-1 'nil)
(define test-rel-node-2 'nil)
(define test-rel 'nil)

;test get-root?


(test-case
 "Connection" 
   (check-equal? 
    (neo4j-server-baseurl conn) "http://localhost:7474/db/data"))

(test-case
 "Connection fail" 
 (check-exn
  exn:fail?
  (lambda (x) 
    (neo4j-init "http://localhost:7475/db/data"))))

(test-case "Create Node"
 (let ([node (create-node conn)])
   (begin 
     (set! test-node node)
     (check-true (hash-has-key? node 'self)))))

(test-case
 "Create Node w/ props"
 (let* (
        [props (hash 'foo 1 'bar "two")]
        [node (create-node conn props)]
        [datanode (hash-ref node 'data)])   
   (begin 
     (check-true (hash-has-key? datanode 'foo))
     (check-true (hash-has-key? datanode 'bar))
     (set! test-node-with-props node)
 )))


(test-case
 "Get Node"
 (let* ([nodeid (get-node-id test-node)]
        [node (get-node conn nodeid)]
        [gnodeid (get-node-id node)])       
   (check-equal? nodeid gnodeid)))

(test-case
 "Delete Node"
 (let* ([nodeid (get-node-id test-node)]   
        [dn (delete-node conn nodeid) ])                       
     (check-exn exn:fail? (lambda (x) (get-node conn nodeid)))))

(test-case
 "Get Node Properties"
 (let* ([nodeid (get-node-id test-node-with-props)]
        [props  (get-node-props conn nodeid)])   
   (begin 
     (check-true (hash-has-key? props 'foo))
     (check-true (hash-has-key? props 'bar))      
     )))

(test-case
 "Set Node Properties"
 (let* ([nodeid (get-node-id test-node-with-props)]
        [newprops (hash 'baz "x" 'z 1)]
        [result  (set-node-props conn nodeid newprops)]
        [props (get-node-props conn nodeid)])  
   (begin 
     (check-true (hash-has-key? props 'baz))
     (check-true (hash-has-key? props 'z))      
     )))

(test-case
 "Remove Node Properties"
 (let* ([nodeid (get-node-id test-node-with-props)]
        [result (remove-node-props conn nodeid)]
        [props (get-node-props conn nodeid)])    
   (check-equal? (hash-count props) 0 )   
   ))

(test-case
 "Set Node Property"
 (let* ([nodeid (get-node-id test-node-with-props)]        
        [result  (set-node-prop conn nodeid "prop1" 1)]
        [props (get-node-props conn nodeid)])  
   (begin 
     (check-true (hash-has-key? props 'prop1))        
     (check-true (equal? (hash-ref props 'prop1) 1)))))

(test-case
 "Get Node Property"
 (let* ([nodeid (get-node-id test-node-with-props)]        
        [result  (set-node-prop conn nodeid "prop1" 1)]
        [prop (get-node-prop conn nodeid "prop1")])  
   (begin      
     (check-true (equal? prop 1)))))  


(test-case
 "Remove Node Property"
 (let* ([nodeid (get-node-id test-node-with-props)]        
        [result  (set-node-prop conn nodeid "prop1" 1)])           
   (check-exn exn:fail? 
              (lambda (x) (get-node-prop conn nodeid "prop1")))))

(test-case
 "Delete Node"
 (let* ([node-to-delete (create-node conn)]
        [nodeid (get-node-id node-to-delete)])
   (begin
     (delete-node conn nodeid)
     (check-exn exn:fail? 
                (lambda (x) (get-node-prop conn nodeid "prop1"))))))


(test-case
 "Create Relationship"
 (let* ([rn1 (create-node conn)]        
        [rn2 (create-node conn)]        
        [rn1id (get-node-id rn1)]
        [rn2id (get-node-id rn2)]
        [reltype "RELTEST"]
        [reldata (hash 'rd "RELDATA")]
        [newrel (create-relationship conn rn1id rn2id reltype reldata)]
        )
   (begin
     ;(display (string-append "Source node " rn1id " "))
     ;(display (string-append "Dest node " rn2id))     
     (check-true (hash-has-key? newrel 'data))         
     (set! test-rel-node-1 rn1)
     (set! test-rel-node-2 rn2)     
     (set! test-rel newrel)
     )))

;(display (string-append "Relationship node from:" (get-node-id test-rel-node-1) "\n"))
;(display (string-append "Relationship node to:" (get-node-id test-rel-node-2) "\n"))
;(display (string-append "[" (get-rel-id test-rel) "]" "\n"))

(test-case
 "Get Relationship Properties"
 (let* ([relid (get-rel-id test-rel)]
        [props (get-rel-props conn relid)])
   (begin
     (check-true (hash-has-key? props 'rd))
     (check-true (equal? (hash-ref props 'rd) "RELDATA")))))

(test-case
 "Set Relationship Properties"
 (let* ([relid (get-rel-id test-rel)]
        [nop (set-rel-props conn relid (hash 'newprop 123))]
        [props (get-rel-props conn relid)])
   (begin
     (check-true (hash-has-key? props 'newprop))
     (check-true (equal? (hash-ref props 'newprop) 123)))))


(test-case
 "Remove Relationship Properties"
 (let* ([relid (get-rel-id test-rel)]
        [nop (remove-rel-props conn relid)]
        [props (get-rel-props conn relid)])
   (check-equal? (hash-count props) 0 )))   


(test-case
 "Get Relationship Property"
 (let* ([relid (get-rel-id test-rel)]
        [nop (set-rel-props conn relid (hash 'newprop 456))]
        [prop (get-rel-prop conn relid "newprop")])
   (check-equal? prop 456)))
                     

(test-case
 "Set Relationship Property"
 (let* ([relid (get-rel-id test-rel)]
        [nop (set-rel-prop conn relid "newprop" 100)]
        [prop (get-rel-prop conn relid "newprop")])
   (check-equal? prop 100)))
     
(test-case
 "Remove Relationship Property"
 (let* ([relid (get-rel-id test-rel)]        
        [prop (remove-rel-prop conn relid "newprop")])           
        (check-exn exn:fail? (lambda (x) (get-rel-prop conn relid "newprop")))))


(test-case
 "Delete Relationship"
 (let* ([relid (get-rel-id test-rel)])        
   (begin
     (delete-rel conn relid)
        (check-exn exn:fail? (lambda (x) (get-rel-prop conn relid "x"))))))


(test-case
 "Get Relationship Types"
 (let* ([reltypes (get-rel-types conn)])        
   (check-equal? (car reltypes) "RELTEST")))


(test-case
 "Get Node Relationship - ALL"
 (let* ([rn1 (create-node conn)]        
        [rn2 (create-node conn)]        
        [rn1id (get-node-id rn1)]
        [rn2id (get-node-id rn2)]
        [reltype "XYZ"]
        [reldata (hash 'rd "RELDATA")]
        [newrel (create-relationship conn rn1id rn2id reltype reldata)]
        [nr (get-node-rel-all conn rn1id '())]
        )
   (begin
     ;(display (string-append "Source node " rn1id " "))
     ;(display (string-append "Dest node " rn2id))     
     ;(display nr)          
     (check-true (hash-has-key? (car nr) 'start))
     (check-equal? rn1id 
                   (last 
                    (regexp-split #rx"/" (hash-ref (car nr) 'start)))))))

(test-case
 "Get Node Relationship - OUT"
 (let* ([rn1 (create-node conn)]        
        [rn2 (create-node conn)]        
        [rn1id (get-node-id rn1)]
        [rn2id (get-node-id rn2)]
        [reltype "XYZ"]
        [reldata (hash 'rd "RELDATA")]
        [newrel (create-relationship conn rn1id rn2id reltype reldata)]
        [nr (get-node-rel-out conn rn1id '())]
        )
   (begin
     (check-true (hash-has-key? (car nr) 'start))
     (check-equal? rn1id 
                   (last 
                    (regexp-split #rx"/" (hash-ref (car nr) 'start))))
     (check-equal? rn2id 
                   (last 
                    (regexp-split #rx"/" (hash-ref (car nr) 'end)))))))


(test-case
 "Get Node Relationship - IN"
 (let* ([rn1 (create-node conn)]        
        [rn2 (create-node conn)]        
        [rn1id (get-node-id rn1)]
        [rn2id (get-node-id rn2)]
        [reltype "XYZ"]
        [reldata (hash 'rd "RELDATA")]
        [newrel (create-relationship conn rn1id rn2id reltype reldata)]
        [nr (get-node-rel-in conn rn2id '())]
        )
   (begin
     (check-true (hash-has-key? (car nr) 'start))
     (check-equal? rn1id 
                   (last 
                    (regexp-split #rx"/" (hash-ref (car nr) 'start))))
     (check-equal? rn2id 
                   (last 
                    (regexp-split #rx"/" (hash-ref (car nr) 'end)))))))

(test-case
 "Create Node Index"
 1)

(test-case
 "Create Relationship Index"
 1)

(test-case 
 "Get Node Indexes"
 (get-node-indexes conn)
 )

(test-case
 "Get Relationship Indexes" 
 (get-rel-indexes conn)
 )


;delete-index
;index_node
;index_remove_items
;index_remove_items_completely
;index_search_keyval
;index_search_query
;traverse
;path_between_nodes
;paths_between_nodes                          
