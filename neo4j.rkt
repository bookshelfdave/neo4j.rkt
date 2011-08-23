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
                      ) #:transparent )
                                    
(struct neo4j-response (header body status) #:transparent)

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


(define (neo4j-post n4j url reqbody)
  (let* 
      ([url (string->url (neo4j-server-node n4j))]
       [resp (read-response (post-impure-port url reqbody neo4j-post-headers) "" )])
    resp))

(define (neo4j-get n4j url)
  "FOO"
  )


; exported functions
(define (neo4j-init baseurl)  
  (with-handlers ([(lambda (v) (begin (display v) #t)) (lambda (v) "Failed to connect!")])    
        (let* ([n4j (neo4j-server baseurl "" "" "" "" "" "" "")]
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

(define (get-neo4j-node n4j nodeid)
  (let* ([snodeid (cond 
                    [(string? nodeid) nodeid]
                    [(integer? nodeid) (number->string nodeid)])]
         [url (string-append (neo4j-server-node n4j) "/" snodeid)]
         [resp (read-response (neo4j-request-port n4j url) "" )]
         [body (neo4j-response-body resp)]
         [hdr (get-status-from-header (neo4j-response-header resp))]
         [respcode (get-http-status hdr)])
    (cond [(equal? respcode "200") (json->jsexpr body)]
          [(equal? respcode "404") #f]
          [else (error (string-append "Unrecognized response from server: " respcode))])))



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

    
;(post-impure-port (string->url "http://localhost:7474/db/data/node") (string->bytes/locale testbody) neo4j-post-headers ))
;(get-neo4j-root (neo4j-init "http://localhost:7474/db/data"))
;(define r (read-response (get-neo4j-port (neo4j-init "http://localhost:7474/db/data") "/node/1") ""))
;(define testbody (jsexpr->json (hasheq 'name "Dave Parfitt" 'profession "Hacker")))
;(define s (neo4j-init "http://localhost:7474/db/data"))


