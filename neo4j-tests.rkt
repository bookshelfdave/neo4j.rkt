#lang racket
(require rackunit
         "neo4j.rkt")

(define conn (neo4j-init "http://localhost:7474/db/data"))

(define test-node 'nil)
(define test-node-with-props 'nil)


;test get-root?


(test-case
 "Connection" 
   (check-equal? (neo4j-server-baseurl conn) "http://localhost:7474/db/data"))

(test-case
 "Connection fail" 
 (check-exn
  exn:fail?
  (lambda (x) (neo4j-init "http://localhost:7475/db/data"))))

(test-case
 "Create Node"
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
   (check-exn exn:fail? (lambda (x) (get-node-prop conn nodeid "prop1")))))

(test-case
 "Delete Node"
 (let* ([node-to-delete (create-node conn)]
        [nodeid (get-node-id node-to-delete)])
   (begin
     (delete-node conn nodeid)
     (check-exn exn:fail? (lambda (x) (get-node-prop conn nodeid "prop1"))))))



;create-relationship
;set-relationship-properties
;get-relationship-properties
;remove-relationship-properties
;get-relationship-property
;set-relationship-property
;remove-relationship-property
;delete-relationship
;get-node-relationship
;get-relationship-types
;create-index
;delete-index
;list-node-indexes
;list-relationship-indexes
;index_node
;index_remove_items
;index_remove_items_completely
;index_search_keyval
;index_search_query
;traverse
;path_between_nodes
;paths_between_nodes                          
