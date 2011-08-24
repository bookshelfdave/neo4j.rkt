#lang racket
(require net/url)
(require (planet dherman/json:3:0))
(require net/head)
;curl -H Accept:application/json -X POST http://localhost:7474/db/data/node

(struct neo4j-server (baseurl 
                      node 
                      node_index 
                      relationship_index
                      reference_node
                      extensions_info
                      extensions
                      relationship_types
                      handlers
                      ) #:transparent )

(struct response-handlers (
                          get-root
                          create-node
                          get-node
                          set-node-properties
                          get-node-properties
                          remove-node-properties
                          set-node-property
                          get-node-property
                          remove-node-property
                          delete-node
                          create-relationship
                          set-relationship-properties
                          get-relationship-properties
                          remove-relationship-properties
                          get-relationship-property
                          set-relationship-property
                          remove-relationship-property
                          delete-relationship
                          get-node-relationship
                          get-relationship-types
                          create-index
                          delete-index
                          list-node-indexes
                          list-relationship-indexes
                          index_node
                          index_remove_items
                          index_remove_items_completely
                          index_search_keyval
                          index_search_query
                          traverse
                          path_between_nodes
                          paths_between_nodes                          
                          ))


  
; the result of an http request
(struct neo4j-response (header body status) #:transparent)

  
; used to generically handle http response codes
(struct handler (status description converter) #:transparent)

; define one of these for each http response code you are expecting
(define generic-handler (lambda (x) (string-append "Generic handler:" x)))
(define (handle status description converter)
  (handler status description converter))
; make a hash out of the list of handlers, using the status code as the hash key
(define (handlers . l)
 (make-hash (map (lambda (y) (cons (handler-status y) y)) l)))


(define (handle-generic code msg)
  (handle code msg (lambda (x) (error (string-append "Error: " msg ": Server returned " code " -> " x)))))

(define (handle-json code msg)
  (handle code msg json->jsexpr))


(define (server-error msg)
  (lambda (x)
    (string-append "An error occurred:" msg)))

(define default-response-handlers
  (response-handlers
   ;get-root
   (handlers (handle-generic "404" "Root not found") 
             (handle "200" "OK" json->jsexpr))
   ;create-node
   (handlers)
   ;get-node  
   (handlers (handle-generic "404" "Node not found")
             (handle-json "200" "OK"))
   ;set-node-properties
   (handlers)
   ;get-node-properties
   (handlers)
   ;remove-node-properties
   (handlers)
   ;set-node-property
   (handlers)
   ;get-node-property
   (handlers)
   ;remove-node-property
   (handlers)
   ;delete-node
   (handlers)
   ;create-relationship
   (handlers)
   ;set-relationship-properties
   (handlers)
   ;get-relationship-properties
   (handlers)
   ;remove-relationship-properties
   (handlers)
   ;get-relationship-property
   (handlers)
   ;set-relationship-property
   (handlers)
   ;remove-relationship-property
   (handlers)
   ;delete-relationship
   (handlers)
   ;get-node-relationship
   (handlers)
   ;get-relationship-types
   (handlers)
   ;create-index
   (handlers)
   ;delete-index
   (handlers)
   ;list-node-indexes
   (handlers)
   ;list-relationship-indexes
   (handlers)
   ;index_node
   (handlers)
   ;index_remove_items
   (handlers)
   ;index_remove_items_completely
   (handlers)
   ;index_search_keyval
   (handlers)
   ;index_search_query
   (handlers)
   ;traverse
   (handlers)
   ;path_between_nodes
   (handlers)
   ;paths_between_nodes                          
   (handlers)
   ))
  



(define neo4j-request-headers (list "Accept:application/json"))

(define neo4j-post-headers (list "Content-Type:application/json"))
 
(define (read-header p h)
  (let ([line (read-line p)])
    (if (eof-object? line)
        (begin
          (close-input-port p)
          h)
        (if (equal? line "\r") ;/n is already stripped off
            h                        
            (read-header p (string-append h line))))))

(define (read-body p b)
  (let ([line (read-line p)])
    (if (eof-object? line)
        (begin
          (close-input-port p)
          b)
        (read-body p (string-append b line)))))

(define (get-status-from-header header)
  (car (regexp-split #rx"\r" header)))

(define (read-response p r)
  (let* ([header (read-header p "")]
         [body   (read-body p "")]
         [status (get-http-status (get-status-from-header header))])
    (neo4j-response header body status)))

(define (get-http-status txt)
  (car (regexp-match* #px"\\d\\d\\d" txt)))

; opens a port just using the baseurl 
(define (neo4j-request-port-raw n4j path)         
  (let* ([baseurl (neo4j-server-baseurl n4j)]
         [fullpath (string-append baseurl path)]
         [url (string->url fullpath)])
    (get-impure-port url neo4j-request-headers)))

(define (neo4j-request-port n4j fullpath)         
  (let ([url (string->url fullpath)])
    (get-impure-port url neo4j-request-headers)))

(define (handle-response r handler-hash)        
  (let ([respcode (neo4j-response-status r)])
    (cond [(hash-has-key? handler-hash respcode)            
           ((handler-converter (hash-ref handler-hash respcode)) (neo4j-response-body r))
           ]
          [else (string-append "Response handler not defined for status:" (neo4j-response-header r) respcode)])          
    ))

(define (neo4j-post n4j surl reqbody handler-hash)
  (let* 
      ([url (string->url surl)]
       [resp (read-response (post-impure-port url reqbody neo4j-post-headers) "" )])
    (handle-response resp handler-hash)))

(define (neo4j-get n4j surl handler-hash)  
  (let* 
      ([url (string->url surl)]
       [resp (read-response (get-impure-port url neo4j-request-headers) "" )])
    (handle-response resp handler-hash)))


; exported functions
(define (neo4j-init baseurl)  
  (with-handlers ([(lambda (v) (begin (display v) #t)) (lambda (v) "Failed to connect!")])    
        (let* ([n4j (neo4j-server baseurl "" "" "" "" "" "" "" default-response-handlers)]
               [body (neo4j-response-body (get-neo4j-root n4j))]
               [root (json->jsexpr body)])
          (begin 
            (display body)
            (struct-copy neo4j-server n4j 
                       ;[root-node root]
                       [node (hash-ref root 'node)]
                       [extensions (hash-ref root 'extensions)]
                       [node_index (hash-ref root 'node_index)]
                       [relationship_index (hash-ref root 'relationship_index)]
                       [reference_node (hash-ref root 'reference_node)]
                       [extensions_info (hash-ref root 'extensions_info)]
                       [relationship_types (hash-ref root 'relationship_types)]
                       )))))
  
        
(define (get-neo4j-root n4j)
  (read-response (neo4j-request-port-raw n4j "/") ""))



(define (get-node n4j nodeid)
  (let* ([snodeid (cond 
                    [(string? nodeid) nodeid]
                    [(integer? nodeid) (number->string nodeid)])]
         [url (string-append (neo4j-server-node n4j) "/" snodeid)])
         (neo4j-get n4j url (response-handlers-get-node (neo4j-server-handlers n4j)))))
         

(define (create-node n4j [props #f])
  (let* ([url (string->url (neo4j-server-node n4j))]
         [reqbody (string->bytes/locale (if (eq? props #f) ""
                                            (jsexpr->json props)))]        
         [resp (neo4j-post n4j url reqbody)]         
         [respcode (neo4j-response-status resp)]
         )             
    (cond [(equal? respcode "201") (json->jsexpr (neo4j-response-body resp))]
          [(equal? respcode "400") "Invalid data sent"]
          [else (error (string-append "Unrecognized response from server: " respcode))])))        


(define (set-node-props n4j nodeid [props #f])
  (let* ([url0 (string-append (neo4j-server-node n4j) "/" nodeid "/properties")]
         [url (string->url url0)]
         [reqbody (string->bytes/locale (if (eq? props #f) ""
                                            (jsexpr->json props)))]        
         [resp (neo4j-post n4j url reqbody)]         
         [respcode (neo4j-response-status resp)]
         )             
    (cond [(equal? respcode "204") "OK"]
          [(equal? respcode "400") "Invalid data sent"]
          [(equal? respcode "404") "Node not found"]
          [else (error (string-append "Unrecognized response from server: " respcode))])))        
    
;(post-impure-port (string->url "http://localhost:7474/db/data/node") (string->bytes/locale testbody) neo4j-post-headers ))
;(get-neo4j-root (neo4j-init "http://localhost:7474/db/data"))
;(define r (read-response (get-neo4j-port (neo4j-init "http://localhost:7474/db/data") "/node/1") ""))
;(define testbody (jsexpr->json (hasheq 'name "Dave Parfitt" 'profession "Hacker")))
;(define s (neo4j-init "http://localhost:7474/db/data"))


