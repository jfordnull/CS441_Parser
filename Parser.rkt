#lang racket
(require racket/list)

;Unfortunately, this parser is not fully-functioning. I didn't have time to get functions or while loops to parse
;correctly. I stubbornly chose not to rely on any libraries, which after talking to a few peers I think was a mistake.
;This parser works by reading in a file and separating each line of the file into a list of strings, separated by
;whitespace. Those lists (lines) are appended into one superlist of lists which represent a program. Those strings are
;then mapped to tokens. I thought this was a smart shortcut, but it instead created a new set of problems - some of which
;I solved and some of which I didn't. I wonder if there's just a conceptual problem too with trying to map strings as
;tokens before I know if they're part of higher-order constructs in the language. (Like while loops and functions)
;
;I hope the evidence of effort and some structure here is enough to earn me a few points. All you need to do to parse
;is: (parse <yourfilenameinquotes>), assuming Parser.rkt is in the same working directory as your program.
;
;If you're curious about the representation of lines as lists of tokens, I recommend uncommenting the (displayln tokens)
;function within the isLine? function.

(define (parse filetoParse)
  (let ((split-lines (for/list ([line (in-list (file->lines filetoParse))])
                       (string-split line))))
    (isProgram? (reverse-if-contains-while (remove-empty-lists (combine-between-while-endwhile split-lines))))))

(define (tokenize input)
  (cond
    [(string=? "if" input) 'if]
    [(string=? "while" input) 'while]
    [(string=? "endwhile" input) 'endwhile]
    [(string=? "read" input) 'read]
    [(string=? "write" input) 'write]
    [(string=? "goto" input) 'goto]
    [(string=? "gosub" input) 'gosub]
    [(string=? "return" input) 'return]
    [(string=? "break" input) 'break]
    [(string=? "end" input) 'end]
    [(string=? "true" input) 'true]
    [(string=? "false" input) 'false]
    [(string=? "+" input) 'add]
    [(string=? "-" input) 'sub]
    [(string=? "*" input) 'mult]
    [(string=? "/" input) 'div]
    [(string=? "<" input) 'lt]
    [(string=? ">" input) 'gt]
    [(string=? "<=" input) 'lte]
    [(string=? ">=" input) 'gte]
    [(string=? "<>" input) '!equal]
    [(string=? "=" input) 'equal]
    [(string=? "(" input) 'l_paren]
    [(string=? ")" input) 'r_paren]
    [(string=? ";" input) 'semicolon]
    [(string=? ":" input) 'colon]
    [(isId? input) 'id]
    [(isNum? input) 'num]
    [(isLabel? input) 'label]
    [(string=? "$$" input) 'PROGRAMEND]
    [else 'TOKENERROR]))

; id -> [a-zA-Z][a-zA-Z0-9]*
(define (isId? word)
  (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9]*$" word))

; num -> numsign digit digit*
(define (isNum? word)
  (or (regexp-match? #rx"^[+-]?[1-9][0-9]*$" word)
      (string=? "0" word)))

; label -> id: (label as epsilon is handled by isLine?)
(define (isLabel? word)
  (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9]*:$" word))

; bool-op -> < | > | >= | <= | <> | =
(define (isBoolOp? word)
  (or (equal? word 'lt)
      (equal? word 'gt)
      (equal? word 'lte)
      (equal? word 'gte)
      (equal? word '!equal)
      (equal? word 'equal)))

; Handle-contiguity solves a problem my supposed shortcut created.
; What I'm doing here is breaking up strings at the occurence of
; specific characters for which I assume direct contiguity is legal
; This is a higher order procedure so that I can call it from
; isLine? for different characters
(define (handle-contiguity char)
  (lambda (line)
    (define (splitter str)
      (regexp-split (make-regexp char) str))

    (define (replace-empty-with-char str)
      (if (string=? str "") char str))

    (define (process-str str)
      (let loop ((parts (splitter str))
                 (result '()))
        (cond
          ((null? parts) (reverse result))
          (else
           (let ((part (car parts)))
             (if (string=? part "")
                 (loop (cdr parts) (cons char result))
                 (loop (cdr parts) (cons part result))))))))

    (apply append (map process-str line))))

(define (make-regexp char)
  (regexp (string-append "\\" char)))

(define (isProgram? input)
  ;(displayln "Program = ")
  ;(displayln input)
  (define program-end '("$$"))
  (cond
    ; Program should end in "$$"
    [(not (equal? (car (reverse input)) program-end))
     (displayln "Program lacks '$$' at EOF") false]
    ; Drop ("$$")
    [(isLine-List? (drop-right input 1))
     (displayln "Accept") true]))

(define (isLine-List? input)
  (andmap isLine? input))

(define (isLine? input)
  (define process-semicolon (handle-contiguity ";"))
  (define process-rparen (handle-contiguity ")"))
  (define process-lparen (handle-contiguity "("))

  ; Map each line (list of strings) to a list of tokens (symbols)
  (define tokens (map tokenize (process-semicolon (process-rparen (process-lparen input)))))
  ; (displayln tokens)

  (cond
    [(or (and (equal? (car tokens) 'label)
              (isStmt? (cdr tokens)))
         (isStmt? tokens)) true]
    [else (displayln "Error on line:")
          (displayln input) false]))

(define (isStmt? line)
  (cond
    
    ; Plan was to handle the while loops separately given the multiple instructions/semicolons
    [(and (equal? (car line) 'while)
          (isBoolWhile? (cdr line))) true]
    
    ; Basically check that each line does end in a semicolon
    ; Then if you need to pass to a different function, drop the semicolon
    [(and (equal? (car (reverse line)) 'semicolon)
              ; stmt -> id = expr
          (or (and (equal? (car line) 'id)
                   (equal? (cadr line) 'equal)
                   (isExpr? (drop-right (cddr line) 1)))
              ; stmt -> if (boolean) stmt
              (and (equal? (car line) 'if)
                   (isBoolStmt? (drop-right (cdr line) 1)))
              ; stmt -> read id
              (and (equal? (car line) 'read)
                   (equal? (cadr line) 'id)
                   (empty? (drop-right (cddr line) 1)))
              ; stmt -> write expr
              (and (equal? (car line) 'write)
                   (isExpr? (drop-right (cdr line) 1)))
              ; stmt -> goto id
              (and (equal? (car line) 'goto)
                   (equal? (cadr line) 'id)
                   (empty? (drop-right (cddr line) 1)))
              ; stmt -> gosub id
              (and (equal? (car line) 'gosub)
                   (equal? (cadr line) 'id)
                   (empty? (drop-right (cddr line) 1)))
              ; stmt -> return
              (and (equal? (car line) 'return)
                   (empty? (drop-right (cdr line) 1)))
              ; stmt -> break
              (and (equal? (car line) 'break)
                   (empty? (drop-right (cdr line) 1)))
              ; stmt -> end
              (and (equal? (car line) 'end)
                   (empty? (drop-right (cdr line) 1))))) true]
    [else false]))

(define (isExpr? line)
      ; expr -> id etail
  (or (and (equal? (car line) 'id)
           (isEtail? (cdr line)))
      ; expr -> num etail
      (and (equal? (car line) 'num)
           (isEtail? (cdr line)))
      ; expr -> (expr)
      (and (equal? (car line) 'l-paren)
           (equal? (car (reverse line)) 'r-paren)
           (isExpr? (drop-right (cdr line) 1)))))

; This is an absolute mess, but it's working!
(define (isBoolStmt? line)
  (cond
    [(equal? (car line) 'true) true]
    [(let ((result (split-at-bool-match line)))
       (if result
           (let ((sublist1 (car result))
                 (sublist2 (cadr result)))

             ; First, I split the list (line) at the bool operator
             ; Here I split the second sublist at the r-parenthesis
             ; The idea is to get an expr I can evaluate on the left
             ; And a stmt I can evaluate on the right to satisfy
             ; production rules
             (define (split-sublist2-at-r-paren lst)
               (let loop ((lst lst) (before '()) (after '()))
                 (cond
                   ((null? lst) (if (not (null? before)) 
                                    (list (reverse before) after) 
                                    false))
                   ((equal? (car lst) 'r-paren)
                    (if (not (null? before))
                        (list (reverse before) (cdr lst))
                        (loop (cdr lst) '() '())))
                   (else (loop (cdr lst) (cons (car lst) before) after)))))

             (let ((sublist2-split (split-sublist2-at-r-paren sublist2)))
               (if (and (isExpr? sublist1) 
                        sublist2-split)
                   (let ((sublist2-part1 (car sublist2-split))
                         (sublist2-part2 (cadr sublist2-split)))
                     (if (and (isExpr? sublist2-part1) 
                              (isStmt? sublist2-part2))
                         true
                         false))
                   false)))
       false))]))

; I just didn't have time to get this to work correctly. So right now this
; will always pass even when it shouldn't (as a placeholder). 
(define (isBoolWhile? line)
  (lambda (_) true))

(define (isEtail? line)
  (cond
    ; etail -> epsilon
    [(empty? line) true]
    ; etail -> + expr | - expr | * expr | / expr
    [(and (or (equal? (car line) 'add)
              (equal? (car line) 'sub)
              (equal? (car line) 'mult)
              (equal? (car line) 'div))
          (isExpr? (cdr line))) true]
    [else false]))

; Another example of a solution for a problem with my supposed shortcut.
; Here I'm appending every list of strings from the first list beginning with
; "while" to "endwhile" into its own "while-loop" line/list of strings...
; In theory so it can be more easily parsed in one call to the isLine? funtion
; It works! If you want to see what this looks like, I recommend uncommenting
; the displayln func within isLine?
(define (combine-between-while-endwhile program)
  (let loop ((lst program) (result '()) (current '()) (flag #f))
    (cond
      ((null? lst) (reverse (cons (reverse current) result)))
      ((and flag (member "endwhile;" (car lst)))
       (loop (cdr lst) (cons (reverse (append current (car lst))) result) '() #f))
      ((and flag (not (member "endwhile;" (car lst))))
       (loop (cdr lst) result (append current (car lst)) #t))
      ((member "while" (car lst))
       (loop (cdr lst) (cons current result) (car lst) #t))
      (else (loop (cdr lst) (cons current result) (car lst) flag)))))

; This fixes a side effect of the combine function. Functional eqv of duct tape, seems to work!
; I'm so bad at tracing Racket I found this easier than modifying the original function once I finally
; had something resembling the behavior I wanted
(define (reverse-if-contains-while program)
  (map (lambda (lst)
         (if (member "while" lst)
             (reverse lst)
             lst))
       program))

(define (remove-empty-lists program)
  (filter (lambda (sublist) (not (null? sublist))) program))

; This function splits a list of symbols at an instance of a bool-op. I use it to split either
; side of a boolean expr into expr's that I can evaluate using isExpr? Messy workaround, but it works!
(define (split-at-bool-match line)
  (let loop ((lst line) (before '()) (after '()))
    (cond
      ((null? lst) (if (not (null? before)) 
                       (list (reverse before) after) 
                       false)) ; Return false if no matches found
      ((isBoolOp? (car lst))
       (if (not (null? before))
           (list (reverse before) (cdr lst))
           (loop (cdr lst) '() '())))
      (else (loop (cdr lst) (cons (car lst) before) after)))))