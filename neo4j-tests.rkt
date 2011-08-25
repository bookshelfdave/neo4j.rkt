#lang racket
(require rackunit
         "neo4j.rkt")

(test-case
 "Connection test"
 (let ([conn (neo4j-init "http://localhost:7474/db/data")])         
   (equal? (neo4j-server-baseurl conn) "http://localhost:7474/db/data")))
   
;get-root
;create-node
;get-node
;set-node-properties
;get-node-properties
;remove-node-properties
;set-node-property
;get-node-property
;remove-node-property
;delete-node
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
