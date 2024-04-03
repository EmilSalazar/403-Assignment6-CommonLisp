;Emil Baez Salazar

;(1) Function that creates the following list '(4 (7 22) "art" ("math" (8) 99) 100)'.
(defun myList ()
  (list 4 (list 7 22) "art" (list "math" (list 8) 99) 100))

;(2) Function that returns an ordered list containing all leap years.
(defun leapYear ()
  (loop for year from 1800 to 2021
        when (or (and (zerop (mod year 4)) ; divides by 4 but not 100
                      (not (zerop (mod year 100))))
                 (zerop (mod year 400))) ; or divides by 400
        collect year)) ;collects years

;(3) Function that returns a single list which contains the separate unique entities from two lists.
(defun union- (list1 list2)
  (let ((combined (append list1 list2))) ;appends list into one
    (labels ((add-if-not-present (element result)
               (if (member element result) ;checks if its already om the list
                   result
                   (cons element result)))) ;adds to list if its not already there
      (reduce (lambda (res elem) (add-if-not-present elem res)) ;reduced the list
              combined
              :initial-value nil))))

;(4) Function that eturns the average of all elements in a List.
(defun avg (alist)
  (if (null alist) ;check if list is empty
      nil
      (let ((sum 0) (count 0)) ;initilize variables
        (labels ((aux-avg (lst acc cnt) 
                   (if (null lst) ;if empty return average
                       (/ acc cnt)
                       (aux-avg (rest lst) (+ acc (first lst)) (1+ cnt))))) ;recursive part (change sum and add to count)
          (aux-avg alist sum count)))))

;(5) Function that returns true if the parameters data type is the data type specified.
(defun isType (data-type)
  (lambda (param) ;uses lambda to check if parameters data type is the data type
    (typep param data-type)))

;(6) Function that returns a list with the same elements and ordering of the values.
(defun taxCalculator (limit rate values)
  (labels ((aux-tax-calc (vals acc)
             (if (null vals) ;if list is empty returns the total after reversing
                 (nreverse acc)
                 (let ((current-value (first vals)) ;calculates value using limits and rates
                       (adjusted-value (if (> (first vals) limit)
                                           (* (first vals) rate)
                                           (first vals))))
                   (aux-tax-calc (rest vals) (cons adjusted-value acc))))))
    (aux-tax-calc values nil)))

;(7) Function that returns a list which contains all values in a List which, when passed to aFunc, return true.
(defun clean (a-func a-list)
  (cond
    ((null a-list) nil)  ; If list is empty return nil
    ((listp (first a-list))  ; If the first item is a sublist
     (cons (clean a-func (first a-list))  ; Recursively clean the sublist
           (clean a-func (rest a-list))))  
    ((funcall a-func (first a-list))  ; If a-func returns true for the first item
     (cons (first a-list)  
           (clean a-func (rest a-list))))  
    (t (clean a-func (rest a-list)))))  ; else skip the first item and continue

;(8) Function that evaluates the conditional in parameter x and if it evaluates to true, execute all subsequent sublists in parameter.
(defmacro threeWayBranch (x y z)
  `(if ,(car x)
       (progn ,@(cdr x)) ;If x condition is true evaluate everything in list x
       (if ,(car y) ;if false go to next list
           (progn ,@(cdr y)) ;If y condition is true evaluate everything in list y
           (when ,(car z) ;if false go to next list
             (progn ,@(cdr z)))))) ;If z condition is true evaluate everything in list z




