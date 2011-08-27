#lang racket
(require net/url)
(require (planet dherman/json:3:0))
(require net/head)

; no transaction support :-(

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
  (handle code msg (lambda (x) msg)))

(define (handle-error code msg)
  (handle code msg (lambda (x) (error (string-append "Error: " msg ". Server returned " code " -> " x)))))


(define (handle-json code)
  (handle code "OK" json->jsexpr))

(define (handle-empty-json code)
  (handle code "OK" (lambda (x) (hash))))

(define (server-error msg)
  (lambda (x)
    (string-append "An error occurred:" msg)))


;; TODO: How do I handle 4xx errors? exceptions?
(define default-response-handlers
  (response-handlers
   ;get-root
   (handlers (handle-generic "404" "Root not found") 
             (handle-json "200"))
   ;create-node
   (handlers
    (handle-json "201")
    )
   ;get-node  
   (handlers 
    (handle-error "404" "Node not found")
    (handle-json "200"))
   ;set-node-properties
   (handlers    
    (handle-generic "204" "OK")
    (handle-error "400" "Invalid data sent")
    (handle-error "404" "Node not found")    
    )
   ;get-node-properties
   (handlers
    (handle-json "200" )
    (handle-empty-json "204")
    (handle-error "404" "Node not found")        
    )      
   ;remove-node-properties
   (handlers    
    (handle-generic "204" "OK")
    (handle-error "404" "Node not found")    
    )
   ;set-node-property
   (handlers    
    (handle-generic "204" "OK")
    (handle-error "404" "Node not found")    
    )   
   ;get-node-property
   (handlers
    (handle-json "200")
    (handle-error "404" "Node or property not found")
    )
   ;remove-node-property
   (handlers    
    (handle-generic "204" "OK")
    (handle-error "404" "Node or property not found")    
    )
   ;delete-node
   (handlers 
    (handle-generic "204" "OK")
    (handle-error "404" "Node not found")
    (handle-error "409" "Node could not be deleted (still has relationships?)"))
         
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
(define neo4j-post-headers (list "Content-Type:application/json" "Accept:application/json"))
 
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

(define (read-response-head-only p r)
  (let* ([header (read-header p "")]
         [body   ""]
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

(define (neo4j-put n4j surl reqbody handler-hash)
  (let* 
      ([url (string->url surl)]
       [resp (read-response-head-only (put-impure-port url reqbody neo4j-post-headers) "" )])    
    (handle-response resp handler-hash)))

(define (neo4j-delete n4j surl handler-hash)    
  (let* 
      ([url (string->url surl)]
       [resp (read-response-head-only (delete-impure-port url neo4j-request-headers) "" )])     
      (handle-response resp handler-hash)      
      ))


(define (nodeid->string v)
  (cond 
    [(string? v) v]
    [(integer? v) (number->string v)]))

; exported functions
(define (neo4j-init baseurl)  
  (with-handlers ([(lambda (v) (begin (display v) #t)) (lambda (v) "Failed to connect!")])    
        (let* ([n4j (neo4j-server baseurl "" "" "" "" "" "" "" default-response-handlers)]
               [body (neo4j-response-body (get-neo4j-root n4j))]
               [root (json->jsexpr body)])
          (begin             
            (struct-copy neo4j-server n4j                        
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
  (let* ([snodeid (nodeid->string nodeid)]
         [url (string-append (neo4j-server-node n4j) "/" snodeid)])
         (neo4j-get n4j url (response-handlers-get-node (neo4j-server-handlers n4j)))))


(define (delete-node n4j nodeid)
  (let* ([snodeid (nodeid->string nodeid)]
         [url (string-append (neo4j-server-node n4j) "/" snodeid)])
         (neo4j-delete n4j url (response-handlers-delete-node (neo4j-server-handlers n4j)))))


(define (create-node n4j [props #f])
  (let* ([url (neo4j-server-node n4j)]
         [reqbody (string->bytes/locale (if (eq? props #f) ""
                                            (jsexpr->json props)))])    
    (neo4j-post n4j url reqbody (response-handlers-create-node (neo4j-server-handlers n4j)))))
        
(define (set-node-props n4j nodeid [props #f])
  (let* ([snodeid (nodeid->string nodeid)]         
         [url (string-append (neo4j-server-node n4j) "/" snodeid "/properties")]         
         [reqbody (string->bytes/locale (if (eq? props #f) ""
                                            (jsexpr->json props)))])
    (neo4j-put n4j url reqbody (response-handlers-set-node-properties (neo4j-server-handlers n4j)))))

(define (get-node-props n4j nodeid)
  (let* ([snodeid (nodeid->string nodeid)]
         [url (string-append (neo4j-server-node n4j) "/" snodeid "/properties")])
         (neo4j-get n4j url (response-handlers-get-node-properties (neo4j-server-handlers n4j)))))

(define (remove-node-props n4j nodeid)
  (let* ([snodeid (nodeid->string nodeid)]
         [url (string-append (neo4j-server-node n4j) "/" snodeid "/properties")])
  (neo4j-delete n4j url (response-handlers-remove-node-properties (neo4j-server-handlers n4j)))))


; should i take a json expr AND name + value?
(define (set-node-prop n4j nodeid prop value)
  (let* ([snodeid (nodeid->string nodeid)]         
         [url (string-append (neo4j-server-node n4j) "/" snodeid "/properties/" prop)]                  
         [reqbody (string->bytes/locale (jsexpr->json value))])
    (neo4j-put n4j url reqbody 
               (response-handlers-set-node-property (neo4j-server-handlers n4j)))))

(define (get-node-prop n4j nodeid prop)
  (let* ([snodeid (nodeid->string nodeid)]
         [url (string-append (neo4j-server-node n4j) "/" snodeid "/properties/" prop)])
         (neo4j-get n4j url (response-handlers-get-node-property (neo4j-server-handlers n4j)))))


(define (remove-node-prop n4j nodeid prop)
  (let* ([snodeid (nodeid->string nodeid)]
         [url (string-append (neo4j-server-node n4j) "/" snodeid "/properties/" prop)])
  (neo4j-delete n4j url (response-handlers-remove-node-property (neo4j-server-handlers n4j)))))




(define (create-relationship n4j [props #f])
  (let* ([url (neo4j-server-node n4j)]
         [reqbody (string->bytes/locale (if (eq? props #f) ""
                                            (jsexpr->json props)))])    
    (neo4j-post n4j url reqbody (response-handlers-create-node (neo4j-server-handlers n4j)))))


(define (get-node-id node)
  (last (regexp-split #rx"/" (hash-ref node 'self)))
  )

;(define s (neo4j-init "http://localhost:7474/db/data"))
(provide 
 neo4j-init 
 create-node
 get-node
 delete-node
 get-node-props
 set-node-props
 get-node-id
 remove-node-props
 set-node-prop
 get-node-prop
 (struct-out neo4j-server))
