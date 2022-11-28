;; P3 -- Metacircular Interpreter for Scheme
;; 
;; Starter Code
#lang racket

(provide run)

;;
;; Language
;;

;; Scheme expressions
;
; e ::= (lambda (x ...) e)
;     | (let ([x e] ...) e)
;     | (begin e e ...)
;     | (if e e e)
;     | (set! x e)
;     | (op ...)
;     | (e ...)
;     | x | n | b
;
; op ::= + | - | * | / | equal? | add1 | sub1 | car | cdr
;      | cons | empty? | append
(define (expr? e)
  (match e
    [`(let ([,(? symbol?) ,(? expr?)] ...) ,(? expr?)) #t]
    [`(begin ,(? expr?) ,(? expr?) ...) #t]
    [`(if ,(? expr?) ,(? expr?) ,(? expr?)) #t]
    [`(set! ,(? symbol?) ,(? expr?)) #t]
    [`(lambda (,(? symbol?) ...) ,(? expr?)) #t]
    [(or (? symbol?) (? number?) (? boolean?)) #t]
    [`(,(? expr?) ,(? expr?) ...) #t]
    [_ #f]))

(define (builtin-op? op) (member op '(+ - * / equal? add1 sub1 car cdr cons empty? append)))

(define ops-tbl (hash '+ + '- - '* * '/ / 'equal? equal? 'add1 add1 'sub1 sub1))

;; helper function to apply a scheme operator to a list of numbers /
;; booleans
;; (apply-builtin-op '+ '(1 2)) --> 3
;; (apply-builtin-op 'equal? '(#t #t)) --> #t
;; (apply-builtin-op 'add1 '(5)) --> 6
(define (apply-builtin-op op lst)
  (apply (hash-ref ops-tbl op) lst))

;;
;; Interpreter values
;; 

;; Your interpreter will manifest three sorts of values: numbers,
;; booleans, and closures. The closure case is worth noting: when you
;; produce closures, they must be in precisely this format.
(define (value? v)
  (match v
    [(? number? v) #t]
    [(? boolean? v) #t]
    [`(closure ,(? expr? e) ,(? environment?)) #t]
    [_ #f]))

;; Environments are hashes from symbol? to address?

;; Addresses will just be symbols, which we will generate using gensym
(define address? symbol?)

;; See the README and video for why it's symbol? -> address?, rather
;; than (say) symbol? -> value?
(define (environment? env)
  (and (hash? env) (andmap symbol? (hash-keys env)) (andmap address? (hash-values env))))

;; Stores (/ heaps / etc...) are maps from addresses to values. 
(define (store? sto)
  (and (hash? sto) (andmap address? (hash-keys sto)) (andmap value? (hash-values sto))))

;; Your interpreter must produce a tagged result, `(result ,v ,store+)
;; of a final value and new (possibly updated) store.
(define (eval-result? r)
  (match r
    [`(result ,(? value? v) ,(? store? s)) #t]
    [_ #f]))

;; 
;; Allocation
;; 

;; Allocate a fresh address (to be put in the store) for some named
;; variable.
(define (allocate var) (gensym var))

;;
;; YOUR CODE HERE
;;







;; (interp e env sto) 
;; (-> expr? environment? store? eval-result?)
;;
;; You will build out the implementation of a big-step, metacircular
;; interpreter.
(define/contract (interp e env store)
  (-> expr? environment? store? eval-result?)
  (match e

    ;; You may wish to handle only one variable to start
    [`(let ([,xs ,es] ...) ,ebody)
     (interp `((lambda ,xs ,ebody) ,@es) env store)]

    ;;explained on whiteboard

    [`(begin ,e0 ,es ...)
     (match-define `(result ,v ,store+) (interp e0 env store))
     (interp (first es) env store+)(interp `(begin ,(second es) ,@es) env store+)
     (define (getHelp es env store)
       (cond [(< (length es) 1) (interp (first es) env store)(interp (rest es) env store)]
             [(equal? (length es) 2) (interp (first es) env store)(interp (last es) env store)]
             [else (interp es env store)]))
     (getHelp `(,@es) env store+)]
        
    

    ;; Hint: use hash-ref for both env *and* store
    [(? symbol? x) `(result ,(hash-ref store (hash-ref env x)) ,store)]

    ;; These two are done for you: since the store doesn't change we
    ;; simply pass it back
    [(? number? n) `(result ,n ,store)]
    ;;[`(+ ,e0 ,e1)
     ;;(match-define `(result ,n0 ,store+) (interp e0 env store))
     ;;(match-define `(result ,n1 ,store++) (interp e1 env store+))
     ;;`(result ,(+ n0 n1) ,store++)]
    [(? boolean? b) `(result ,b ,store)]

    ;; Evaluate to a closure, don't forget to store the environment
    [`(lambda (,xs ...) ,ebody) `(result (closure ,e ,env) ,store)]


    ;; Evaluate guard (ec) and then either evaluate et or ef
    [`(if ,ec ,et ,ef)
     (let ([guard-result (interp ec env store)])
       (match guard-result
         [`(result ,result-value ,store+)
          (if (equal? result-value 0)
              (interp et env store+)
              (interp ef env store+))]))]


    ;; Evaluate e, then set x's address in the store, return
    ;; updated store
    [`(set! ,x ,e)
     (match-define `(result ,v ,store+) (interp e env store))
     `(result ,v ,(hash-set store+ (hash-ref env x) v))]


    ;; Evaluate each of the arguments to a value, then call 
    ;; apply-builtin-op to get the final result
    [`(,(? builtin-op? op) ,es ...)
     'todo]


    ;; See the lecture on closures for how to handle this one
    [`(,e0 ,e1)
     (let ([v0 (interp e0 env store)])
       (match v0
         [`(result (closure (lambda (,x) ,e-body) ,env+) ,store+)
          (let ([v-arg (interp e1 env store+)])
            (match v-arg
              [`(result ,v ,store++)
               (define v-addr (gensym x))
               (interp e-body (hash-set env+ x v-addr)
                       (hash-set store++ v-addr v))]))]))]))

(define (run e)
  (match-define `(result ,v ,_) (interp e (hash) (hash)))
  v)
