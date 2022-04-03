#lang scheme
#|
CSI 2520 - Projet integrateur partie Scheme
Etudiante: Celine Wan Min Kee
Numero etudiant: 300193369
|#

; reads a file
; filename: name of the file to read
(define (readlist filename)
 (call-with-input-file filename
  (lambda (in)
    (read in))))


; saves the clusterlist in a file
; fileName: name of the file where the output is gonna be written
; L: list to write in the file filename
(define (saveList filename L)
  (call-with-output-file filename
    (lambda (out)
      (write L out))))


; import
; returns a list of lists containing the points in each partitions
; each sublist is the format: (PARTITION_ID POINT_ID X Y CLUSTER_ID)
(define (import)
  (let ((p65 (readlist "partition65.scm"))
        (p74 (readlist "partition74.scm")) 
        (p75 (readlist "partition75.scm"))
        (p76 (readlist "partition76.scm"))
        (p84 (readlist "partition84.scm"))
        (p85 (readlist "partition85.scm"))
        (p86 (readlist "partition86.scm")))
    (append p65 p74 p75 p76 p84 p85 p86)))

; mergeClusters
; L: list is of format (PARTITION_ID POINT_ID X Y CLUSTER_ID) and contains all the the points in each partition
; merge the clusters L and return the new list by calling populateClusterList
; we also initialise the ClusterList to an empty list
; we also reverse the ClusterList to get the result in ascending order
; the result returned is a list of format (POINT_ID X Y CLUSTER_ID) to match the Prolog project
(define (mergeClusters L) (reverse (populateClusterList (removePartitionId L) '())))

; removePartitionId
; lst: list is of format (PARTITION_ID POINT_ID X Y CLUSTER_ID)
; lst is then transformed into format (POINT_ID X Y CLUSTER_ID)
; this function is to work with a list of the same format as in the Prolog project
; (POINT_ID X Y CLUSTER_ID)
(define (removePartitionId lst)
  (if (null? lst) '()  ; base case: if the list is null
      (cons (cdar lst) (removePartitionId (cdr lst)))))
      ; remove the partition id by keeping the cdr of the car of the list
      ; (meaning it removes all the first element of each sublist)
      ; + recursion call on the cdr of the list to keep looping through the list

; populateClusterList
; db: list returned by import function (contains all the the points in each partition)
; cl: ClusterList that will be the list after the merge
(define (populateClusterList db cl)
   (cond
    ((null? db) cl) ; base case: when already looped through the whole list
    
    ; case where the point is not in ClusterList, so we just add the point to ClusterList
    ((= (isIdPresent (caar db) cl) -1) (populateClusterList (cdr db) (cons (car db) cl))) 

    ; % case where the point is already in the ClusterList meaning it's intersecting, so we need to relabel the points
    (else (populateClusterList (cdr db) (relabel (isIdPresent (caar db) cl) (getClusterId (car db)) cl))))) ; recursive call on cdr of db to continue looping


; isIdPresent
; id: id used to do the checking
; lst: ClusterList
; checks if the point with id is already in the CluserList
; returns -1 if not present, and the label if present
; input: (isIdPresent 2 '((1 2 3 4) (2 7 9 10) (5 6 11 12))) output: 10
; input: (isIdPresent 11 '((1 2 3 4) (2 7 9 10) (5 6 11 12))) output: -1
(define (isIdPresent id lst)
  (cond
    ((null? lst) -1) ; if the point is not in ClusterList, we return -1
    (else
      (if (= (caar lst) id) (getClusterId (car lst)) ; if the point is already in ClusterList, we return the label
       (isIdPresent id (cdr lst)))))) ; recursive call
      

; relabel
; o: label to change
; l: label used to replace o
; lst: ClusterList
; loops through ClusterList and relabel the points with label o with label l and returns a new list
; input: (relabel 8 1 '((1 2 3 4) (5 6 7 8) (4 3 2 4))) output: ((1 2 3 4) (5 6 7 1) (4 3 2 4))
; input: (relabel 4 1 '((1 2 3 4) (5 6 7 8) (4 3 2 4))) output: ((1 2 3 1) (5 6 7 8) (4 3 2 1))
(define (relabel o l lst)
  (cond
    ((null? lst) '())  ; base case: if list is null (when already looped through the whole list)
     (else
       (if (= (getClusterId (car lst)) o) ;  % case where the label is O and has to be changed to R
           (cons (append (findFirstN (car lst) 3) (list l)) (relabel o l (cdr lst))) ; construct list with first 3 elements and new label l 
           (cons (car lst) (relabel o l (cdr lst))))))) ; case where the label doesn't need to be changed                

; getClusterId
; lst: list to process
; returns the last element of a list (meaning the cluster id)
; format of list (POINT_ID X Y CLUSTER_ID)
; input: (getClusterId '(1 2 3 4)) output: 4
(define (getClusterId lst)
  (if (null? (cdr lst)) (car lst) ; case where list is of lenght 1 meaning we're at the last elemt
        (getClusterId (cdr lst)))) ; recursive call to continue looping through list

; findFirstN
; lst: list to process
; n: number of element to get (in this case n is gonna be 3)
; returns a list with first first N elements
; input: (findFirstN '(1 2 3 4) 3) output: (1 2 3)
(define (findFirstN lst n)
  (if (equal? n 0) '() ; goal to get the N first elements reached
      (cons (car lst) (findFirstN (cdr lst) (- n 1))))) ; adding to list + recursive call
 