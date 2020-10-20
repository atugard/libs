;;tree of life 

(define (tree? t)
 (and (eq? (length t) 3) (eq? 'tree (car tree)) (trees? (caddr tree)))) 

(define (trees? . ts)
  (type-list? ts tree?))

(define (tree r . subtrees)
  (if (null? subtrees)
    (list 'tree r null null)
    (if (trees? subtrees)
      (list 'tree r subtrees)
      (error "tree is an n-ary procedure whose first argument is of type any and whose n-1 remaining arguments are of type tree. You gave: " (list r subtrees)))))

(define (root t)
  (if (tree? t)
    (cadr t)
    (error "get-root takes one argument of type tree. you gave: " t)))

(define (subtrees t)
  (if (tree? t)
    (cddr t)
    (error "sub-trees takes one argument of type tree. you gave: " t)))

(define (leafs t)
  (display 'tbd))

(define (dfs node tree)
  (let* ((_subtrees (subtrees t))
         (_first (car _subtrees))
         (_root (root first)))
    (cond [(equal? node _root) _first ]
          [(null? _subtrees) false]
          [else
            (let ((_next  (dfs node _subtrees))) ;;either false or a tree.
              (if _next
                _next
                (dfs node (cadr _subtrees))))])))


        

      

;;(a+b)^p. A choice tree of height p gives you all the info to compute this. 




